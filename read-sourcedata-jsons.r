#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

source("common.functions.r")

library(tidyverse)
library(readxl)

## turn off messages about col type guesses from readr
options(readr.show_col_types = FALSE)

## turn off progressbars in readr
options(readr.show_progress = FALSE)

json.files.glob="../sourcedata/sub-*/ses-*/*/*.json"
## json.files.glob="../../new.turmeric/sourcedata/sub-*/ses-*/*/*.json"
json.files=grep("dcm2nii", Sys.glob(json.files.glob), invert=TRUE, value=TRUE)

##                                                                                                                   other entities 
##                           subject ID        session ID         data type      subject ID       session ID optional    |    suffix           extension
json.pattern=".*/sourcedata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/([[:alnum:]]+)/(sub-[[:alnum:]]+(_ses-[[:alnum:]]+)?)_(.*)_([[:alnum:]]+)\\.([[:alnum:]]+)"
## json.pattern=".*/sourcedata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/([[:alnum:]]+)/(([[:alnum:]]+)-([[:alnum:]]+)_?)+_([[:alnum:]]+)\\.([[:alnum:]]+)"
df=json.files %>%
    map(function(ff) {
        m=regexec(json.pattern, ff)
        matches=regmatches(ff, m)[[1]]
        ll=length(matches)

        ## this assumes that there are sessions within subject and
        ## that there is at least one BIDS entiry to be matched by the
        ## entities group in the json.pattern regex. The regex is
        ## coded such that session directories are mandatory and
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
             unlist(lapply(matches[ll-2],
                           function(yy) {
                               unlist(
                                   lapply(
                                       unlist(str_split(yy, "_")),
                                       function(xx) {
                                           pp=unlist(str_split(xx, "-"));
                                           rr=pp[[2]]; names(rr)=pp[[1]];
                                           rr
                                       }) )
                           })),
             "suffix"=matches[ll-1],
             "ext"=matches[ll],
             "filename" = matches[1]
             )
        rr
    }) %>%
    bind_rows() %>%
    mutate(across(filename, R.utils::getAbsolutePath)) %>%    
    relocate(filename,      .after=last_col()) %>%
    relocate(c(suffix, ext),.before=filename)


meta.data=df %>%
    pull(filename) %>%
    map(function(xx) {
        js.file=rjson::fromJSON(file=xx)
        json.attrs=unlist(js.file[c("SliceThickness",
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
    }) %>%
    bind_rows() %>%
    mutate(across(c(SliceThickness:dk, SpacingBetweenSlices), as.numeric))

df=df %>%
    bind_cols(meta.data) %>%
    relocate(filename, .after=last_col())

    
