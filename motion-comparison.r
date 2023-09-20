#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
## library(ggforce)
library(R.utils)

source("common.functions.r")
## turn off messages about col type guesses from readr
options(readr.show_col_types = FALSE)

## turn off progressbars in readr
options(readr.show_progress = FALSE)


study.name="pain_supplement"
task="task-tapping"

root=file.path("/data/colmconn", study.name)

data=file.path(root, "data")
derivative.data=file.path(root, "derivative")

####################################################################################################
### START OF FUNCTIONS
####################################################################################################

summarize.df <- function(in.data, in.group.var, in.var, in.conf.interval=0.95) {
    in.data %>%
        group_by(.data[[in.group.var]]) %>%
        summarise(n=n(),
                  min=min(.data[[in.var]],   na.rm=TRUE),
                  max=max(.data[[in.var]],   na.rm=TRUE),
                  mean=mean(.data[[in.var]], na.rm=TRUE),
                  sd=sd(.data[[in.var]],     na.rm=TRUE),
                  se=sd(.data[[in.var]],     na.rm=TRUE)/sqrt(n()),
                  ci=(sd(.data[[in.var]],    na.rm=TRUE)/sqrt(n())) * (qt(in.conf.interval/2 + 0.5, n()-1)),
                  median=median(.data[[in.var]], na.rm=TRUE),
                  IQR=IQR(.data[[in.var]],       na.rm=TRUE),
                  mad=mad(.data[[in.var]],       na.rm=TRUE),
                  na.count=sum(is.na(.data[[in.var]])))    
}

####################################################################################################
### END OF FUNCTIONS
####################################################################################################


## the grep here is to discard the unwanted test01 subject
info.message("Finding all dfile_rall.1D files")
motion.dfiles=grep("test01",
                   list.files(file.path(derivative.data,
                                        paste0("afni-", c("task-tapping",
                                                          "resting-state"))),
                              full.names=TRUE, pattern="dfile_rall.1D", recursive=TRUE),
                   invert=TRUE, value=TRUE)

subject.session.pattern=".*/afni-((task-tapping)|(resting-state))/(sub-([[:alnum:]]*))/(ses-([[:alnum:]]*))/.*"
subject.session.df <-
    motion.dfiles %>%
    map(function(ff) {
        m=regexec(subject.session.pattern, ff)
        matches=regmatches(ff, m)[[1]]
        line.count=countLines(ff)[1]
        return(list("subject"=rep(matches[5], line.count),
                    "session"=rep(matches[8], line.count),
                    "task"   =rep(matches[2], line.count)))
    }, .progress=list(
           type="iterator",
           format = "Creating subject-session data frame {cli::pb_bar} {cli::pb_percent}",
           clear = FALSE,
           show_after=0)
    ) %>%
    bind_rows() %>%
    mutate(across(c(subject, session, task), as.factor))
subject.sessions.acquired.df=unique(subject.session.df[, c("subject", "session", "task")])

## the grep here is to discard the unwanted test01 subject
info.message("Finding all 00_DO_NOT_ANALYSE files")
donot_analyze.files=grep("test01",
                   list.files(file.path(derivative.data,
                                        paste0("afni-", c("task-tapping",
                                                          "resting-state"))),
                              full.names=TRUE, pattern="00_DO_NOT_ANALYSE.*", recursive=TRUE),
                   invert=TRUE, value=TRUE)

analyzable.pattern=".*/afni-((task-tapping)|(resting-state))/(sub-([[:alnum:]]*))/(ses-([[:alnum:]]*))/.*/00_DO_NOT_ANALYSE_.*"
not.analyzable=
    donot_analyze.files %>%
     map(function(ff) {
        m=regexec(subject.session.pattern, ff)
        matches=regmatches(ff, m)[[1]]
        return(list("subject"=matches[5],
                    "session"=matches[8],
                    "task"=matches[2],
                    "analyzable"=FALSE))
    }, .progress=list(
           type="iterator",
           format = "Creating not analyzable data frame {cli::pb_bar} {cli::pb_percent}",
           clear = FALSE,
           show_after=0)
    ) %>%
    bind_rows() %>%
    mutate(across(c(subject, session, task), as.factor)) %>%
    arrange(subject, session)

analyzable.df=left_join(subject.sessions.acquired.df, not.analyzable) %>%
    replace_na(list(analyzable=TRUE))

## The motion 1D files produced by 3dvolreg are a special beast. They
## use multiple space delimeters between each column. The first column
## of each line contains a space. Consequently, only read_fwf from
## readr can read these files and parse the lines correctly. Here we
## use the fwf_empty function to guess the start end end columns of
## each field start end end columns within a sample of 100 lines. It
## appears to work very well.
info.message("Guessing column layout of motion dfiles")
motion.dfile.col.positions=fwf_empty(motion.dfiles[1],
                                col_names=c("roll", "pitch", "yaw", "dx", "dy", "dz"))
motion.estimates.df <-
    motion.dfiles %>%
    map(read_fwf,
        col_positions=motion.dfile.col.positions,
        col_types=paste(rep("d", 6), collapse=""),
        .progress=list(
            type="iterator",
            format = "Reading motion data files {cli::pb_bar} {cli::pb_percent}",
            clear = FALSE,
            show_after=0)) %>%
    bind_rows()

motion.estimates.df=bind_cols(subject.session.df, motion.estimates.df)

## using the pivot_longer takes the columns roll:dz, and places then
## in one column named value. the column names are dropped by virtue
## of the names_to=NULL argument. The use summarize_at to aplpy
## multiple summary functions. The vars(value) is used to get at teh
## value column which is created by the immediatly preceding call to
## pivot_longer
info.message("Creating summary statistics from motion dfiles")
conf.interval=0.95
motion.estimates.summary.df=
    motion.estimates.df %>%
    group_by(subject, session, task) %>%
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

motion.estimates.long.df=motion.estimates.df %>%
    pivot_longer(cols=roll:dz, names_to="measurement")

motion.boxplot.graph=
    motion.estimates.long.df %>%
    ggplot(aes(measurement, value)) +
    geom_violin() +
    facet_grid(rows=vars(session), cols=vars(task),
               labeller=as_labeller(str_to_title)) +
    scale_x_discrete(limits = c("roll", "pitch", "yaw", "dx", "dy", "dz")) +
    labs(x="Motion Measurement", y="Motion Metric Value") +
    ggtitle("Motion excursion") +
    my.theme
graphics.off()
dev.new(width=8, height=10, unit="in")
print(motion.boxplot.graph)


motion.estimates.select.summary.df=
    motion.estimates.summary.df %>%
    select(subject:mean)

library (nlme)
library(emmeans)
for (measurement in c("min", "mean", "max")) {
    cli_h1(paste("LME model for", measurement, "motion excursion"))
    
    motion.model=lme(data=motion.estimates.select.summary.df,
                     as.formula(paste(measurement, "session*task", sep="~")),
                     random=~1|subject)
    print(anova(motion.model))
    emm=emmeans(motion.model, ~ session*task)

    print(pairs(emm, simple="session"))
    print(pairs(emm, simple="task"))
    
}
