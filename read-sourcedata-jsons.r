#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

source("common.functions.r")

library(RColorBrewer)
library(lubridate)
library(oro.dicom)
library(readxl)
library(tidyverse)

## turn off messages about col type guesses from readr
options(readr.show_col_types = FALSE)

## turn off progressbars in readr
options(readr.show_progress = FALSE)

cli_h1("Extracting metadata from DICOMs, and BIDS JSON and NIfTI files")
json.files.glob="../sourcedata/sub-*/ses-*/*/*.json"
## json.files.glob="../../new.turmeric/sourcedata/sub-*/ses-*/*/*.json"
json.files=grep("dcm2nii", Sys.glob(json.files.glob), invert=TRUE, value=TRUE)
## keep only T1w, T2w, and bold suffixed files. This will eliminate
## things like events and DWI etc
json.files=grep("(T[12]w|bold)", json.files, value=TRUE)

## FSU site
dicom.files.glob="../rawdata/sub-*/ses-*/*AHN*/HEADS_*/LOCALIZER_0001/*0001.0001*"
dicom.files=grep("test", Sys.glob(dicom.files.glob), invert=TRUE, value=TRUE)
## U of Az site
dicom.files.glob="../rawdata/sub-*/ses-*/*AHN*/LOCALIZER_0001/*0001.0001*"
dicom.files=c(dicom.files, grep("test", Sys.glob(dicom.files.glob), invert=TRUE, value=TRUE))

##                         subject ID         session ID                   subject ID    year       month      day        remainder
dicom.pattern=".*/rawdata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/.*AHN.*(/HEADS_(.*))?/LOCALIZER_0001/.*0001\\.0001.*"
##dicom.pattern=".*/sourcedata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/AHN_PAIN_([[:alnum:]]+)_(.*)"

acq.dates.df=dicom.files %>%
    map(function(ff) {
        m=regexec(dicom.pattern, ff)
        matches=regmatches(ff, m)[[1]]
        dcm=readDICOMFile(ff)
        acq.date=extractHeader(dcm$hdr, "AcquisitionDate")
        ## institution.name=extractHeader(dcm$hdr, "InstitutionName")
        
        c("subject"=matches[2],
          "session"=matches[3],
          ## "site"=case_match(
          ##     institution.name,
          ##     "Florida State Univ. - College of Medicine" ~ "FSU",
          ##     "University of Arizona" ~ "UA",
          ##     .default= "UNKNOWN"
          ## ),
          "acquisition.date"=acq.date
          )
    }, .progress=list(
           type="iterator",
           format = "Creating acquisition date data frame {cli::pb_bar} {cli::pb_percent}",
           clear = FALSE,
           show_after=0)) %>%
    bind_rows() %>%
    mutate(across(acquisition.date, as_date)) %>%
    arrange(subject, session) %>%
    unique() %>%
    pivot_wider(names_from=session, values_from="acquisition.date") %>%
    mutate(interval=`ses-followup`-`ses-baseline`) %>%
    mutate(across(interval, as.numeric)) %>%
    pivot_longer(cols=starts_with("ses-"), names_to="session", values_to="acquisition.date") %>%
    relocate(interval, .after=last_col())
## 137 followup and 147 followup contain 2 files that match the DICOM
## patterns, so just use unique to remove the duplicates

##                                                                                                                   other entities 
##                           subject ID         session ID         data type      subject ID       session ID optional   |    suffix           extension
## json.pattern=".*/sourcedata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/([[:alnum:]]+)/(sub-[[:alnum:]]+(_ses-[[:alnum:]]+)?)_(.*)_([[:alnum:]]+)\\.([[:alnum:]]+)"
##                                                                                               other entities 
##                           subject ID         session ID         data type      subject ID          |   suffix           extension
json.pattern=".*/sourcedata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/([[:alnum:]]+)/(sub-[[:alnum:]]+)_(.*)_([[:alnum:]]+)\\.([.A-Za-z0-9]+)"
## json.pattern=".*/sourcedata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/([[:alnum:]]+)/(([[:alnum:]]+)-([[:alnum:]]+)_?)+_([[:alnum:]]+)\\.([[:alnum:]]+)"
bids.df=json.files %>%
    map(function(ff) {
        m=regexec(json.pattern, ff)
        matches=regmatches(ff, m)[[1]]
        ll=length(matches)

        ## this assumes that there are sessions within subject and
        ## that there is at least one BIDS entity to be matched by the
        ## entities group in the json.pattern regex. The regex is
        ## coded such that subject directories are mandatory and
        ## session entities are optional
        rr=c("subject"=matches[2],
             "session"=matches[3],
             "data.type"=matches[4],
             "entities"=matches[ll-2],
             ## the following recursively splits into key-value pairs
             ## based on the _ separator, then further splits based on
             ## the - separator, names the value with it's key name
             ## and returns the named values all while unlisting to
             ## ultimately yield a vector of values named with their
             ## keys
             unlist(
                 lapply(
                     unlist(str_split(matches[ll-2], "_")),
                     function(xx) {
                         pp=unlist(str_split(xx, "-"));
                         rr=pp[[2]]; names(rr)=pp[[1]];
                         rr
                     })),
             "suffix"=matches[ll-1],
             "ext"=matches[ll],
             "filename" = matches[1]
             )
        rr
    }, .progress=list(
           type="iterator",
           format = "Creating subject-session data frame {cli::pb_bar} {cli::pb_percent}",
           clear = FALSE,
           show_after=0)) %>%
    bind_rows() %>%
    mutate(across(filename,  R.utils::getAbsolutePath)) %>%
    mutate(across(c(run, echo), as.numeric)) %>%
    relocate(filename,       .after=last_col()) %>%
    relocate(c(suffix, ext), .before=filename)

meta.data=bids.df %>%
    pull(filename) %>%
    map(function(xx) {
        js.file=rjson::fromJSON(file=xx)
        json.attrs=unlist(js.file[c("InstitutionName",
                                    "SliceThickness",
                                    "RepetitionTime",
                                    "FlipAngle",
                                    "ReconMatrixPE",
                                    "BaseResolution",
                                    "SpacingBetweenSlices")])
        dd=system2("3dinfo", c("-n4 -ad3 -space", str_replace(xx, "json", "nii.gz")),
                   stdout=TRUE, stderr=NULL)
        ss=scan(textConnection(dd), sep="\t", what=character(),  quiet=TRUE)
        names(ss)=c("nx", "ny", "nz", "nt", "di", "dj", "dk", "space")
        c(json.attrs, ss)
    }, .progress=list(
           type="iterator",
           format = "Extracting DICOM tags {cli::pb_bar} {cli::pb_percent}",
           clear = FALSE,
           show_after=0)) %>%
    bind_rows() %>%
    mutate(across(c(SliceThickness:dk, SpacingBetweenSlices), as.numeric)) %>%
    mutate(site=case_match(InstitutionName,
                           "Florida State Univ. - College of Medicine" ~ "FSU",
                           "University of Arizona" ~ "UA")) %>%
    select( ! InstitutionName)

    ## mutate(site=if_else(str_detect(subject, "sub-10[0-9][0-9][0-9]"),
    ##                     "UA", "FSU"),
    ##        .after=session) %>%

all.meta.data.df=bids.df %>%
    left_join(acq.dates.df, by = join_by(subject, session)) %>%
    bind_cols(meta.data) %>%
    select(!c(ses, entities, ext)) %>%
    relocate(site, .after="session") %>%
    relocate(interval, .before="data.type") %>%
    relocate(acquisition.date, .after="site") %>%
    relocate(filename, .after=last_col())
## set the interval for baseline =0
## df[df$session=="ses-baseline", "interval"]=0
df.filename="../derivatives/functional-qc/meta.data.RData.gz"
if (! dir.exists(dirname(df.filename))) {
    dir.create(dirname(df.filename))
}
info.message(paste("Saving meta data to", df.filename))

pigz.save(all.meta.data.df, file=df.filename, ncores=4, verbose=TRUE)

cli_h1("Interval between scans")
my.base.size=16
my.theme=
    theme_bw(base_size =  my.base.size) +
    theme(
        ## legend.position="none",
        legend.position="bottom",        
        ## panel.grid.major = element_blank(),
        ## panel.grid.minor = element_blank(),
        
        ##remove the panel border
        ## panel.border = element_blank(),
        
        ## add back the axis lines
        axis.line=element_line(colour = "grey50"),
        
        ##axis.title.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=my.base.size, vjust=0.4, angle =  90),
        plot.title=element_text(size=my.base.size*1.2, vjust=1))


ss=all.meta.data.df %>%
    filter(session=="ses-followup") %>%
    select(c(subject, interval)) %>%
    unique() %>%
    drop_na()
interval.summary=ss %>%
    summarise(min=min(interval),
              max=max(interval),
              mean=mean(interval),
              sd=sd(interval),
              median=median(interval),
              iqr=IQR(interval))
print(interval.summary)

pd <- position_jitterdodge(dodge.width=0.2) # move them .1 to the left and right, and up or down
colorCount=length(unique(ss$subject))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
interval.graph=ggplot(interval.summary,
                      aes(x=factor(0), y=mean)) %+%
    geom_point(data=ss,
               aes(x=factor(0), y=interval, color=subject),
               position=pd, size=2) %+%
    scale_color_manual(name="Subject", values=getPalette(colorCount),
                       aesthetics = c("color")) %+%
    scale_x_discrete(breaks = NULL) %+%
    geom_point(size=3) %+%
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width=.3)) %+%
    labs(x=NULL, y="Interval", title="Interval in days between baseline and follow-up scans") %+%
    my.theme

if (isTRUE(! is.na(Sys.getenv("DISPLAY", unset=NA)))) {
    dev.new(width=8, height=10, unit="in")
    print(interval.graph)
}
interval.graph.filename=file.path("../derivatives/functional-qc", "interval.graph.pdf")
info.message(c("Saving", interval.graph.filename))
ggsave(interval.graph.filename, interval.graph, width=8, height=8, units="in")

