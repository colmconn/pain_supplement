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

info.message("Reading data table file")
init.data.table.file="../derivative/afni-task-tapping-3dmss-basis-tent/init_datatable.tsv"
init.data.table=read_table(init.data.table.file)
## ,
##                       skip=1, ## ignore the column names in the table
##                       col_names=c("subject", "session", "InputFile"))

init.data.table=init.data.table %>%
    mutate(session=paste0("ses-", session)) %>%
    rename(source.file="InputFile")

info.message("Reading pain ratings file")
pain.ratings.file="../derivative/pain_ratings/pain_ratings.csv"
pain.ratings = read_csv(pain.ratings.file,
                        col_types=cols(
                            col_character(), col_character(), col_character(),
                            col_integer(), col_integer()),
                        col_names=c("subject", "session", "task", "run", "rating"))
##     filter(! (task=="kneeTapping" & run == 1)) %>%
delta.pain.rating=pain.ratings %>%
    mutate(subject=paste0("sub-", subject), 
           session=paste0("ses-", session)) %>%
    mutate(task=paste0(task, run)) %>%
    select(! run) %>%
    pivot_wider(id_cols = subject:session, names_from=task, values_from = rating) %>%
    mutate(dPainRating=kneeTapping2-initialpainrating1) #%>%
##    select(! c(initialpainrating1, kneeTapping2))

info.message("Reading demographics file")
demographics=read_excel("../rawdata/Ahn_MRI pt demographic.xlsx")
demographics=demographics %>%
    mutate(ID=paste0("sub-", ID)) %>%
    rename(subject=ID, "pain_length"='Length of knee pain (month)', leg='Index knee') %>%
    rename_with(tolower) %>%
    select(! race)

info.message("Creating 3dMSS data table file")
mss.data.table=left_join(init.data.table, delta.pain.rating, b=c("subject", "session"))

mss.data.table=left_join(mss.data.table, demographics, by=c("subject")) %>%
    relocate(source.file, .after=last_col()) %>%
    relocate(dPainRating, .before=source.file) %>%
    mutate(sex     = case_when(sex     == "female"       ~ -1,
                               sex     == "male"         ~  1),
           leg     = case_when(leg     == "left"         ~ -1,
                               leg     == "right"        ~  1),
           session = case_when(session == "ses-baseline" ~ -1,
                               session == "ses-followup" ~  1))

make.tent.knot.filename <- function(.x, n.knots, coef.dir) {
    dn=dirname(.x)
    fn=basename(.x)
    fn=str_replace(fn, "\\+tlrc\\.HEAD", "")

    knots=seq.int(0, n.knots-1)
    if (is.null(coef.dir))
         fp=file.path(dn, sprintf("%s_tr-%02d+tlrc.HEAD", fn, knots))
    else
        fp=file.path(dn, coef.dir, sprintf("%s_tr-%02d+tlrc.HEAD", fn, knots))

    ## fp=sprintf("%s_tr-%02d+tlrc.HEAD", fn, knots)
    ## fp=sprintf("%s[tapping#%d_Coef]", fn, knots)
    names(fp)=sprintf("tr-%02d", knots)
    return(fp)
}

extract.coefficient.briks <- function(.x, n.knots, coef.dir) {
    dn=dirname(.x)
    fn=basename(.x)
    if (is.null(coef.dir))
        coef.dir=dn
    else
        coef.dir=file.path(dn, coef.dir)
    
    if (! dir.exists(coef.dir))
        dir.create(coef.dir)

    knots=seq.int(0, n.knots-1)
    ## base filename
    bfn=str_replace(fn, "\\+tlrc\\.HEAD", "")
    ffn=file.path(coef.dir, sprintf("%s_tr-%02d+tlrc.HEAD", bfn, knots))
    cmd.args=sprintf("-session %s -prefix %s_tr-%02d %s[tapping#%d_Coef]",
                     coef.dir, bfn, knots, .x, knots)

    ## disable AFNI compression of output files
    Sys.unsetenv("AFNI_COMPRESSOR")
    ## cac = cmd args count
    cac=1
    for (args in cmd.args) {
        ## execute commands
        ## cat(c("3dbucket", args), "\n")
        if ( ! file.exists(ffn[cac])) {
            system2("3dbucket", args)
        }
        cac=cac+1    
    }
}

n.tent.knots=21
acq.tr=1.6 # acquisition TR in milliseconds

nn=mss.data.table %>%
    pull("source.file") %>%
    map(extract.coefficient.briks,  n.knots=n.tent.knots, coef.dir="basis-tent_coef")
nn=NULL

per.tr.filenames=mss.data.table %>%
    pull("source.file") %>%
    map(make.tent.knot.filename, n.knots=n.tent.knots, coef.dir="basis-tent_coef") %>%
    bind_rows()

## the above pipe is equivalent of the following code, However, map_df
## is superceded by the above map-followed-by-bind_rows idiom in a
## manner similar to do.call(rbind, ...)
##
## per.tr.filenames=mss.data.table %>%
##     pull("InputFile") %>%
##     map_df(make.tent.knot.filename, in.n.knots=n.tent.knots)
##    select(! source.file) %>%

mss.data.table=mss.data.table %>%
    bind_cols(per.tr.filenames) %>%
    pivot_longer(cols=starts_with("tr-"),
                 names_to="n.tr",
                 names_prefix="tr-",
                 names_transform=list(n.tr=as.numeric),
                 values_to="InputFile") %>%
    mutate(tr=n.tr*acq.tr) %>%
    mutate(across(tr, ~ round(., digits = 4))) %>%
    relocate(tr, .before=n.tr)

data.table.file=str_replace(init.data.table.file, "init_", "")
pred.data.table.file=str_replace(init.data.table.file, "init_datatable", "pred_datatable")

info.message(str_glue("Writing data table: {data.table.file}"))
## select(subject, session, age, pain_length, leg,
##        dPainRating, n.tr, InputFile) %>%
mss.data.table %>%
    select(subject, session, n.tr, InputFile) %>%
    rename(TR=n.tr) %>%
    write_delim(file=data.table.file, delim=" ", quote="none")

info.message(str_glue("Writing HRF prediciton data table: {pred.data.table.file}"))
## select(subject, session, age, pain_length, leg,
##        dPainRating, tr) %>%

mss.data.table %>%
    select(subject, session, tr) %>%
    rename(label=subject, TR=tr) %>%
    write_delim(file=pred.data.table.file, delim=" ", quote="none")
                      
## for (xx in c("age", "pain_length")) {
##     for (yy in c("initialpainrating1", "kneeTapping1", "kneeTapping2", "dPainRating", "pain_length")) {
##         if (xx != yy) {
##             cat(str_glue("Spearman correlation between {xx} and {yy}\n")) 

##             cor.test.result=suppressWarnings(cor.test(mss.data.table[[xx]], mss.data.table[[yy]], method="spearman"))

##             print(cor.test.result)
##         }
##     }
## }

        
