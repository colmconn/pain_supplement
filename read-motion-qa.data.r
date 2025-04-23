#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

source("common.functions.r")

library(tidyverse)
library(magrittr)
library(corrr)
library(readxl)
## library(compute.es)
## library(orddom)

## library(R.utils)

## turn off messages about col type guesses from readr
options(readr.show_col_types = FALSE)

## turn off progressbars in readr
options(readr.show_progress = FALSE)

####################################################################################################
### START OF FUNCTIONS
####################################################################################################

make.review.df <- function(in.review.files, in.excessive.motion.threshold.percentage, in.exclusion.fraction) {

    out.ss_review.pattern=paste0(".*/derivatives/afni-([-A-Za-z0-9]+)/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/",
                                 preprocessed.data.dir,
                                 "/.*")

    out.ss_review.df=in.review.files  %>%
        map(function(ff) {
            m=regexec(out.ss_review.pattern, ff)
            matches=regmatches(ff, m)[[1]]
            ll=length(matches)
            ## dna= Do Not Analyze
            ## dna.file=file.path(dirname(matches[1]),
            ##                    sprintf("00_DO_NOT_ANALYSE_%s_%s_%dpercent.txt",
            ##                            matches[3],
            ##                            matches[4],
            ##                            in.excessive.motion.threshold.percentage))
            ## this assumes that there are sessions within subject and
            ## that there is at least one BIDS entity to be matched by the
            ## entities group in the json.pattern regex. The regex is
            ## coded such that subject directories are mandatory and
            ## session entities are optional
            rr=c("subject"=matches[3],
                 "session"=matches[4],
                 "task"=str_remove(matches[2], "task-"),
                 "file"=matches[1])## ,
                 ## "dna.file"=dna.file,
                 ## "dna.exists"=file.exists(dna.file))
            if (matches[[1]][1] != -1) {
                aa=read_delim(matches[1], delim=":",
                              col_names=c("key", "value")) %>%
                    mutate(across(everything(), stringr::str_squish)) %>%                                
                    pivot_wider(names_from=key) # %>%
                    ##select(c("num TRs per run", "num TRs per run (applied)", "censor fraction", "num TRs per stim (orig)", "num TRs censored per stim"))
                rr=c(rr, aa)
            }
            rr
        }, .progress=list(
               type="iterator",
               format = "Creating subject review data frame {cli::pb_bar} {cli::pb_percent}",
               clear = FALSE,
               show_after=0)) %>%
        bind_rows() %>%
        mutate(site=if_else(str_detect(subject, "sub-10[0-9][0-9][0-9]"),
                            "UA", "FSU"),
               .after=session) %>%
        mutate(suppressWarnings(across(everything(), function(xx) {
            ifelse(!str_detect(xx, "[ _A-Za-z]+"),
                   as.numeric(xx), xx)
        } ))) %>% 
        mutate("exclude"=`censor fraction` > in.exclusion.fraction) %>%
        mutate("fraction_stim_censored"=`num TRs censored per stim`/`num TRs per stim (orig)`) ## %>%

    ##mutate(across("dna.exists", as.logical)) %>%
    ## select(!c(file, dna.file))

    cli::cli_progress_bar(type="iterator",
                     format="Updating multi-band and slice timing pattern data {cli::pb_bar} {cli::pb_percent}",
                     total=dim(out.ss_review.df)[1],
                     clear=FALSE)
    for (rr in seq.int(dim(out.ss_review.df)[1])) {
        if (substr(out.ss_review.df[rr, "slice timing pattern"], 1, 1) == "@") {
            dfile=pull(out.ss_review.df[rr, "slice timing pattern"])
            dd=system2("1d_tool.py", c("-show_slice_timing_pattern",
                                       "-infile",
                                       substr(dfile, 2, nchar(dfile)),
                                       "-verb",
                                       "0"),
                       stdout=TRUE, stderr=NULL)
            ss=scan(textConnection(dd), sep=" ", what=character(), quiet=TRUE)
            if (length(ss) ==2) {
                out.ss_review.df[rr, "multiband level"] = as.numeric(ss[1])
                out.ss_review.df[rr, "slice timing pattern"] = ss[2]
            }
        }
        cli::cli_progress_update()
    }
    cli::cli_progress_done()
    
    out.ss_review.df
}

make.afni.qc.df <- function(in.review.files) {
    qc_review.pattern=paste0(".*/derivatives/afni-([-A-Za-z0-9]+)/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/", preprocessed.data.dir, "/QC.*/apqc_[-_A-Za-z0-9]*.json")
    in.review.files %>%
        map(function(ff) {
            js.file=jsonlite::fromJSON(ff)
            colnames(js.file)=c("section", "rating", "comment")
            m=regexec(qc_review.pattern, ff)
            matches=regmatches(ff, m)[[1]]
            subject=matches[3]
            session=matches[4]
            js.df=js.file %>%
                as_tibble(validate=FALSE) %>%
                mutate("subject"=subject, "session"=session, .before=1)
            
            js.df
        }, .progress=list(
               type="iterator",
               format = "Creating AFNI QC data frame {cli::pb_bar} {cli::pb_percent}",
               clear = FALSE,
               show_after=0)
        )  %>%
        bind_rows() %>%
        mutate(site=if_else(str_detect(subject, "sub-10[0-9][0-9][0-9]"),
                            "UA", "FSU"),
               .after=session)
}


read.afni.motion.files <- function(in.motion.dfiles) {
    subject.session.pattern=".*/afni-([-A-Za-z0-9]+)/(sub-([[:alnum:]]*))/(ses-([[:alnum:]]*))/.*"

    info.message("Guessing column layout of motion dfiles")
    motion.dfile.col.positions=fwf_empty(motion.dfiles[1],
                                         col_names=c("roll", "pitch", "yaw", "dx", "dy", "dz"))
    motion.df <-
        in.motion.dfiles %>%
        map(function(ff) {
            m=regexec(subject.session.pattern, ff)
            matches=regmatches(ff, m)[[1]]
            subject=matches[3]
            session=matches[5]
            
            mm=read_fwf(ff,
                        col_positions=motion.dfile.col.positions,
                        col_types=paste(rep("d", 6), collapse="")
                        ) %>%
                mutate("subject"=subject,
                       "session"=session,
                       .before=1) %>%
                mutate(site=if_else(str_detect(subject, "sub-10[0-9][0-9][0-9]"),
                                    "UA", "FSU"),
                       .after=session)
            mm
            }, .progress=list(
               type="iterator",
               format = "Creating framewise motion data frame {cli::pb_bar} {cli::pb_percent}",
               clear = FALSE,
               show_after=0)
        ) %>%
        bind_rows()

    motion.df
}


graph.motion <- function(in.motion.estimates.df, in.graph.filename, in.show.graph=FALSE) {
    info.message("Graphing motion summary statistics")
    ## A theme for the graphs
    my.base.size=18
    my.theme=
        theme_bw(base_size =  my.base.size) +
        theme(
            ##legend.position="none",
            legend.position="bottom",        
            ## panel.grid.major = element_blank(),
            ## panel.grid.minor = element_blank(),
            
            ##remove the panel border
            ##panel.border = element_blank(),
            
            ## add back the axis lines
            axis.line=element_line(colour = "grey50"),
            ## legend.text = element_text(angle=45),
            
            ##axis.title.x=element_blank(),
            axis.title.x = element_text(size=my.base.size, vjust=0),
            axis.title.y = element_text(size=my.base.size, vjust=0.4, angle =  90),
            plot.title=element_text(size=my.base.size*1.2, vjust=1))

    my.dodge=position_dodge(.2)

    in.motion.estimates.long.df=in.motion.estimates.df %>%
        pivot_longer(cols=roll:enorm, names_to="measurement")

    motion.boxplot.graph=
        in.motion.estimates.long.df %>%
        ggplot(aes(measurement, value)) +
        geom_violin() +
        facet_grid(rows=vars(session), cols=vars(site),
                   labeller=as_labeller(str_to_title)) +
        scale_x_discrete(limits = c("roll", "pitch", "yaw", "dx", "dy", "dz", "enorm")) +
        labs(x="Motion Measurement", y="Motion Metric Value") +
        ggtitle("Motion excursion") +
        my.theme
    if (in.show.graph) {
        dev.new(width=8, height=10, unit="in")
        print(motion.boxplot.graph)
    }
    motion.graph.filename=file.path(derivative.data, "qc-inspection", in.graph.filename)
    info.message(c("Saving", motion.graph.filename))
    ggsave(motion.graph.filename, motion.boxplot.graph, width=8, height=8, units="in")
}

analyze.motion <- function(in.motion.estimates.df) {

    info.message("Creating summary statistics from motion dfiles")
    conf.interval=0.95
    motion.estimates.summary.df=
        in.motion.estimates.df %>%
        group_by(subject, session, site) %>%
        pivot_longer(cols=roll:dz, names_to=NULL) %>%
        summarise_at(
            vars(value),
            list(
                min   =~ min(.,    na.rm=TRUE),
                max   =~ max(.,    na.rm=TRUE),
                mean  =~ mean(.,   na.rm=TRUE),
                sd    =~ sd(.,     na.rm=TRUE),
                se    =~ sd(.,     na.rm=TRUE)/sqrt(n()),
                ci    =~ (sd(.,    na.rm=TRUE)/sqrt(n())) * (qt(conf.interval/2 + 0.5, n()-1)),
                median=~ median(., na.rm=TRUE),
                IQR   =~ IQR(.,    na.rm=TRUE),
                mad   =~ mad(.,    na.rm=TRUE)
            ))

    library (nlme)
    library(emmeans)
    for (measurement in c("min", "mean", "max")) {
        cli_h1(paste("LME model for", measurement, "motion excursion"))
        
        motion.model=lme(data=motion.estimates.summary.df,
                         as.formula(paste(measurement, "session*site", sep="~")),
                         random=~1|subject)
        print(anova(motion.model))
        emm=emmeans(motion.model, ~ session*site)

        print(pairs(emm, simple="session"))
        print(pairs(emm, simple="site"))
        
    }
}


## this works for small numbers but not large because of overflow. See
## below for more
## https://stackoverflow.com/questions/10933945/how-to-calculate-the-euclidean-norm-of-a-vector-in-r
norm.vector <- function(x) sqrt(sum(x^2))

make.table.string <- function(inGroup, inMean, inSe, inMin, inMax, inNaCount, inMissingData=TRUE) {
    ##  st=paste(round(inMean, 1), " / ", round(inSe, 1),    

    st=paste(round(inMean, 1), " ± ", round(inSe, 1),
        " (", round(inMin, 1), "-", round(inMax, 1), ")", ifelse(inMissingData & inNaCount > 0, paste(" [", inNaCount, "]", sep=""), ""), sep="")
    return(st)
}

analyze.demographics.stats <- function(in.df){

    mystack <- stack()
    ##    header=sprintf("Characteristic,%s,%s,Stat.,pValue,Effect Size (95%% CI),Signif.", group1, group2)
    
    cli_h1("Demographics statistics")
    cli_h1("Site")

    info.message("Sex")
    sex.table=table(in.df[, c("site", "History.Gender")])
    sex.test=chisq.test(sex.table)
    sex.table=addmargins(sex.table)
    print(sex.table)
    print(sex.test)

    info.message("Race")
    race.table=table(in.df[, c("site", "History.Race")])
    race.test=chisq.test(race.table)
    race.table=addmargins(race.table)
    print(race.table)
    print(race.test)

    info.message("Index Knee")
    knee.table=table(in.df[, c("site", "Index.knee")])
    knee.test=chisq.test(knee.table)
    knee.table=addmargins(knee.table)
    print(knee.table)
    print(knee.test)

    info.message("Marital.status")
    marital.status.table=table(in.df[, c("site", "Marital.status")])
    marital.status.test=chisq.test(marital.status.table)
    marital.status.table=addmargins(marital.status.table)
    print(marital.status.table)
    print(marital.status.test)
    
    info.message("Age")
    age.var.test=var.test(History.Age ~ site, in.df)
    age.t.test=t.test(History.Age ~ site, in.df)
    print(age.var.test)
    print(age.t.test)    

    info.message("Length of knee pain (months)")
    pain.length.var.test=var.test(OAmonth ~ site, in.df)
    pain.length.t.test=t.test(OAmonth ~ site, in.df)
    print(pain.length.var.test)
    print(pain.length.t.test)    

    info.message("BMI")
    bmi.var.test=var.test(bmi ~ site, in.df)
    bmi.t.test=t.test(bmi ~ site, in.df)
    print(bmi.var.test)
    print(bmi.t.test)

    
    cli_h1("Treatment Group")

    info.message("Sex")
    sex.table=table(in.df[, c("group", "History.Gender")])
    sex.test=chisq.test(sex.table)
    sex.table=addmargins(sex.table)
    print(sex.table)
    print(sex.test)

    info.message("Race")
    race.table=table(in.df[, c("group", "History.Race")])
    race.test=chisq.test(race.table)
    race.table=addmargins(race.table)
    print(race.table)
    print(race.test)

    info.message("Index Knee")
    knee.table=table(in.df[, c("group", "Index.knee")])
    knee.test=chisq.test(knee.table)
    knee.table=addmargins(knee.table)
    print(knee.table)
    print(knee.test)

    info.message("Marital.status")
    marital.status.table=table(in.df[, c("group", "Marital.status")])
    marital.status.test=chisq.test(marital.status.table)
    marital.status.table=addmargins(marital.status.table)
    print(marital.status.table)
    print(marital.status.test)
    
    info.message("Age")
    ## age.var.test=var.test(age ~ group, in.df)
    age.lm.model=lm(History.Age ~ group, in.df)
    age.aov.test=anova(age.lm.model)
    ## print(age.var.test)
    print(age.aov.test)    

    emm=emmeans(age.lm.model, ~ "group")
    print(pairs(emm, simple="group"))

    info.message("Length of knee pain (months)")
    pain.length.lm.model=lm(OAmonth ~ group, in.df)
    pain.length.aov.test=anova(pain.length.lm.model)
    ## print(pain.length.var.test
    print(pain.length.aov.test)    

    emm=emmeans(pain.length.lm.model, ~ "group")
    print(pairs(emm, simple="group"))

    info.message("BMI")
    bmi.lm.model=lm(bmi ~ group, in.df)
    bmi.aov.test=anova(bmi.lm.model)
    ## print(pain.length.var.test
    print(bmi.aov.test)

    emm=emmeans(bmi.lm.model, ~ "group")
    print(pairs(emm, simple="group"))

}    

build.input.file <- function(xx, in.task.dir, in.preprocessed.dir, in.subbrik) {

    ## stats.sub-10202_ses-followup_REML+tlrc.HEAD
    stats.pattern="stats.(sub-[[:alnum:]]+)_(ses-[[:alnum:]]+)(.*)"

    yy=xx %>%
        map(function(ff) {
            m=regexec(stats.pattern, ff)
            matches=regmatches(ff, m)[[1]]
            stats.file=matches[1]
            subject=matches[2]
            session=matches[3]
            
            file.path(in.task.dir, subject, session, in.preprocessed.dir, paste0(stats.file, in.subbrik))
        })
    unlist(yy)
}

fix.names <- function(in.names)
{
    gsub("+", "and", gsub(" ", ".", in.names, fixed=TRUE), fixed=TRUE)
}

####################################################################################################
### END OF FUNCTIONS
####################################################################################################

study.name="pain_supplement"
task="task-tapping"

root=file.path("/data/colmconn", study.name)

data=file.path(root, "data")
derivative.data=file.path(root, "derivatives")

cli_h1("START MOTION THRESHOLD INFO")
## these must be synchronized with the preprocessing scripts!

## the motion threshold for individual BOLD volumes. Where the enorm
## of the motion is greater than this threshold, the volume in
## censored
mt=0.35
info.message("Motion threshold: {mt}")
             
## the fraction of a subjects entire BOLD timeseries that must be
## censored in order to exclude the subject entirely
ex=0.3
info.message("Excessive motion threshold fraction: {ex}")

## this data directory may need to include the details above to pull
## in data from a directory created by running the regress-only
## scripts.

## preprocessed.data.dir="task-tapping-preprocessed-polortA-NL"
preprocessed.data.dir="task-tapping-preprocessed-polortA-mt0_35-regress-only-NL"
info.message("Preprocessed data directory: {preprocessed.data.dir}")

cli_h1("END MOTION THRESHOLD INFO")


df.filename=file.path(derivative.data, "functional-qc/meta.data.RData.gz")
if (file.exists(df.filename)) {
    pigz.load(df.filename)
} else {
    stop(sprintf("Could not load %s. Run read-sourcedata-jsons.r first to create it\n", df.filename))
}

show.graph=FALSE

cli_h1("Reading demographics file")
## demographics=read_excel("../rawdata/R01\ supplement_MRI\ pt\ demographic.xlsx") %>%
##     mutate(ID=paste0("sub-", ID)) %>%
##     rename(subject=ID, age=Age, sex=Sex, race=Race,
##            pain.length="Length of knee pain (month)",
##            knee="Index knee",
##            group="Treatment group") %>%
##     select(!"Data collection site")

demographics=read_excel("../rawdata/Excel data sheet_R01_20250422.xlsx",
                        sheet="Sheet1")  %>%
    rename(group = tx.group,
           site = Site,
           subject = ID,
           Marital.status = `Marital status`,
           Index.knee = `Index knee`) %>%
    mutate(
        subject=paste0("sub-", as.character(subject)),
        group=factor(group,
                     levels=c(0, 1, 2, 3),
                     labels=c("sham", "experimental", "tDCS", "meditation")),
        History.Gender=factor(
            History.Gender,
            levels=c(0, 1),
            labels=c("male", "female")),
        History.Race=factor(
            History.Race,
            levels=c(1, 2, 3, 4),
            labels=c("Asian", "Black.African.American", "White", "Hispanic.or.Latino")),
        Marital.status=factor(
            Marital.status,
            levels=c(1, 2, 3, 4, 5, 6, 7, 8),
            labels=c("married", "widowed", "divorced",
                     "separated", "never.married", "living.with.partner",
                     "refused", "unknown"))) %>%
    mutate(bmi=History.Weight / History.Height^2 * 703,
           .after=`History.Race`) %>%
    mutate(subject=case_when(
               site=="FSU" ~ str_replace(subject, "(sub-)(10)([0-9]{3})", "\\1\\3"),
               .default = subject)) %>%
    filter(site %in% c("FSU", "UA")) %>%
    select(site:Index.knee, OAmonth)

cli_h1("Reading motion data files")

## "/data/colmconn/pain_supplement/derivatives/afni-task-tapping/sub-10186/ses-baseline/task-tapping-preprocessed-polortA-NL/out.ss_review.sub-10186_ses-baseline.txt"
task.dir=file.path(derivative.data, paste0("afni-", task))

## quality control files automicatally genberated by afni_proc.py
out.ss_review.files.glob=paste0(task.dir,
                                "/sub-*/ses-*/",
                                preprocessed.data.dir,
                                "/out.ss_review.sub-*_ses-*.txt")
out.ss_review.files=grep("test", Sys.glob(out.ss_review.files.glob),
                         invert=TRUE, value=TRUE)

## quality control files created by visual inspection of the  afni_proc.py QC HTML reports
qc.review.files.glob=paste0(task.dir,
                            "/sub-*/ses-*/",
                            preprocessed.data.dir,
                            "/QC*/apqc_*.json")
qc.review.files=grep("test", Sys.glob(qc.review.files.glob),
                     invert=TRUE, value=TRUE)

## motion files generated by 3dvolreg
motion.dfiles.glob=paste0(task.dir,
                            "/sub-*/ses-*/",
                            preprocessed.data.dir,
                            "/dfile_rall.1D")
motion.dfiles=grep("test", Sys.glob(motion.dfiles.glob),
                     invert=TRUE, value=TRUE)

motion.estimates.df=read.afni.motion.files(motion.dfiles) %>%
    rowwise() %>%
    mutate(enorm=norm.vector(c(roll, pitch, yaw, dx, dy, dz)))

cli_h1("Graphing and analyzing motion-comparison.r on for all subjects")
graph.motion(motion.estimates.df,
             in.graph.filename="motion_graph_before_excluding_excessive_censoring.pdf",
             in.show.graph=show.graph)
analyze.motion(motion.estimates.df)

##exclusion.lists=list()
##for (ex in c(0.1, 0.2, 0.3, 0.35, 0.4)) {
## ex=0.3
## this must be synchronized with the preprocessing scripts!
excessive.motion.threshold.fraction=ex
excessive.motion.threshold.percentage=excessive.motion.threshold.fraction*100

cli_h1(sprintf("Tabulating subjects excluded at more than %d%% of volumes motion contaminated",
               excessive.motion.threshold.percentage))

out.ss_review.df=make.review.df(out.ss_review.files,
                                excessive.motion.threshold.percentage,
                                excessive.motion.threshold.fraction) %>%
    left_join(demographics, by=c("subject", "site")) %>%
    select(! c(`subject ID`))

cli_h2("Finding columns with NAs")
info.message("If this list includes more than echo_times, stop and examine the data frame before going further.")
info.message("It's OK for echo_times to be on this list because echo_times are not supplied to afni_proc.py for single echo data and thus will be NA.")
na.cols=out.ss_review.df %>%
    select(where(~ isTRUE(any(is.na(.x))))) %>%
    colnames()
warn.message(na.cols)
warn.message("The following subject(s) have NA values in at least one column")
out.ss_review.df %>%
    filter(if_any(everything(), is.na)) %>%
    select(c("subject", "session", "site", where(~ isTRUE(any(is.na(.x)))))) %>%
    print(n=Inf)

if (preprocessed.data.dir == "task-tapping-preprocessed-polortA-NL") {
    qc.review.df=make.afni.qc.df(qc.review.files)
    bad.ve2a=qc.review.df %>% filter(section=="ve2a" & rating!="good")
    if (dim(bad.ve2a)[1] > 1) {
        print(bad.ve2a, n=Inf)    
        stop("Some subjects have bad ratings for alignment between EPI and anatomy. Fix these before continuing")
    }
} else {
    warn.message("Skipping check of EPI to ANAT rating")
}

cli_h2("Correlation between fraction of TRs censored and fraction of tapping stimulus censored in subjects above exclusion threshold")
out.ss_review.df%>%
    filter(`censor fraction` > ex) %>%
    select(`censor fraction`, fraction_stim_censored) %>%
    correlate() %>%
    print(n=Inf)

cli_h2("Statistics for all subjects at baseline")
out.ss_review.df %>%
    filter(session=="ses-baseline") %>%
    analyze.demographics.stats()

exclusion.df=out.ss_review.df %>%
    pivot_wider(names_from="session", values_from=exclude) %>%
    select(subject, site, starts_with("ses-")) %>%
    group_by(subject) %>%
    mutate(session.count=n()) %>%
    filter(session.count==2) %>%    
    summarise(across(starts_with("ses-"), ~sum(., na.rm = TRUE))) %>%
    rowwise() %>%
    mutate(exclude=any(c_across(starts_with("ses-")))) %>%
    mutate(site=if_else(str_detect(subject, "sub-10[0-9][0-9][0-9]"),
                        "UA", "FSU"),
           .after=subject) %>%
    left_join(demographics, by=c("subject", "site"))

included.subjects=exclusion.df %>%
    filter(exclude==FALSE) %>%
    pull(subject)

exclusion.sumary.table=exclusion.df %>%
    select(subject, site, exclude) %>%
    group_by(site) %>%
    summarise(count=n(),
              count.excluded=sum(exclude),
              count.included=count-count.excluded) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total"))) %>%
    mutate(pct.excluded=(count.excluded/count)*100) %>%
    relocate(count.excluded, .before="pct.excluded")

exclusion.acf.summary.table=exclusion.df %>%
    filter(exclude==FALSE) %>%
    select(starts_with("acf.")) %>%
    summarize(across(everything(), ~mean(., na.rm=TRUE)))

cli_h1(sprintf("Graphing and analyzing motion for subjects with less than %02f of volumes excluded", ex))
clean.motion.estimates.df=motion.estimates.df %>%
    filter(subject %in% included.subjects)
       
## graph.motion(clean.motion.estimates.df,
##              in.graph.filename="motion_graph_after_excluding_excessive_censoring.pdf",
##              in.show.graph=show.graph)
## analyze.motion(clean.motion.estimates.df)




## exclusion.lists[as.character(excessive.motion.threshold.fraction)] =
##     list(list(
##         "exclusion.df" = exclusion.df,
##         "exclusion.sumary.table" = exclusion.sumary.table
##     ))

cli_h1("Stats and counts of included and exluded subjects by site")
print(exclusion.sumary.table)
exclusion.sumary.table %>%
    filter(site!="Total") %>%
    select(count.included, count.excluded) %>%
    as.matrix() %>%
    chisq.test() %>%
    print()

info.message("Subjects with only one session")
info.message("This explains the difference between the total n in the table above and those in site or treatment group tables above")
out.ss_review.df %>%
    group_by(subject) %>%
    summarize(n=n()) %>%
    filter(n!=2) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(where(is.character), ~"Total"))) %>%
    print(n=Inf)

## now build the data frame from which we will generate the lists of
## subjects for voxelwise group analysis
ttest.df=exclusion.df %>%
    inner_join(out.ss_review.df, by=c("subject", "site")) %>%
    select(subject, session, exclude.x, exclude.y, file, `final stats dset`) %>%
    mutate(file=dirname(file)) %>%
    mutate(`final stats dset`=file.path(file, `final stats dset`)) %>%
    select(!file) %>%
    filter(exclude.x==FALSE)

cli_h1("Saving lists of files for group-wise ttests")
path=file.path(derivative.data, sprintf("afni-task-tapping-ttests-mt%0.2f-ex%0.2f", mt, ex))
if (! dir.exists(path)) {
    info.message(c("Creating", path))
    dir.create(path)
}
baseline.stats.file=file.path(path, "baseline_stats_files.txt")
info.message(c("Saving", baseline.stats.file))
ttest.df %>%
    filter(session=="ses-baseline") %>%
    pull(`final stats dset`) %>%
    write_lines(file=baseline.stats.file)

followup.stats.file=file.path(path, "followup_stats_files.txt")
info.message(c("Saving", followup.stats.file))
ttest.df %>%
    filter(session=="ses-followup") %>%
    pull(`final stats dset`) %>%
    write_lines(file=followup.stats.file)
##}
cli_h2("Statistics for subjects with acceptable censoring and data at both baseline and followup")
exclusion.df %>%
    filter(exclude==FALSE) %>%
    analyze.demographics.stats()

cli_h2("Making data table for voxelwise LMEs")

lme.df=out.ss_review.df %>%
    select(subject, session, site, "final stats dset") %>%
    left_join(exclusion.df, by=c("subject", "site")) %>%
    select(!c("ses-baseline", "ses-followup")) %>%
    relocate("final stats dset", .after=last_col()) %>%
    filter(exclude==FALSE) %>%
    select(!exclude) %>%
    left_join(out.ss_review.df[, c("subject", "session", "blur estimates (ACF)")], by=c("subject", "session")) %>%
    separate_wider_delim("blur estimates (ACF)", " ", names=c("acf.a", "acf.b", "acf.c")) %>%
    mutate(across(starts_with("acf."), as.numeric)) %>%
    rename("Subj"="subject", "intervention"="group", "InputFile"="final stats dset") %>%
    mutate("intervention"=fix.names(intervention)) %>%
    mutate("InputFile"=build.input.file(InputFile, task.dir, preprocessed.data.dir, "[tapping#0_Coef]"))

path=file.path(derivative.data, sprintf("afni-task-tapping-lmes-mt%0.2f-ex%0.2f", mt, ex))
if (! dir.exists(path)) {
    info.message(c("Creating", path))
    dir.create(path)
}
lme.data.table.file=file.path(path, "lme_data_table.tsv")
info.message("Saving LME data table to", lme.data.table.file)
lme.df %>%
    select(!starts_with("acf.")) %>%
    write_tsv(lme.data.table.file)
info.message("Intervention levels:", paste(unique(lme.df$intervention), collapse=" "))

lme.acf.data.table.file=file.path(path, "lme_acf_data_table.tsv")
info.message("Saving LME ACF data to", lme.acf.data.table.file)
lme.df %>%
    select(starts_with("acf.")) %>%
    summarize(across(everything(), ~mean(., na.rm=TRUE))) %>%
    write_tsv(lme.acf.data.table.file, col_names=FALSE)

