#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

library(RColorBrewer)
library(emmeans)
library(ggplot2)
library(magrittr)
library(nlme)
library(tidyverse)
library(cli)

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
        axis.title.x = element_text(size=my.base.size, vjust=0),
        axis.title.y = element_text(size=my.base.size, vjust=0.4, angle =  90),
        plot.title=element_text(size=my.base.size*1.2, vjust=1))

pain.ratings.file="../derivatives/r-pain-ratings/pain_ratings.csv"
pain.ratings = read_csv(pain.ratings.file,
                        col_types=cols(
                            col_character(), col_character(), col_character(),
                            col_integer(), col_integer(), col_integer(), col_character()))
##                        col_names=c("subject", "session", "task", "run", "rating", "file.size", "file"))

## the 6 in the filter comes from 2 sessions X 3 ratings (initial,
## after tapping1 and after tapping 2)
complete.pain.ratings=pain.ratings %>%
    group_by(subject) %>%
    summarize(nn=n())
if (any(complete.pain.ratings$nn > 6)) {
    error.message.ln("Some subject(s) have more than 6 ratings (3 ratings (1 initial, 2 knee tapping) x 2 sessions)!")
    complete.pain.ratings %>%
        filter(nn==6) %>%
        print(n=Inf)
    stop()
}

## stop()
## keep only subjects with complete data
## pain.ratings=pain.ratings %>%
##     filter(subject %in% complete.pain.ratings) %>%
##     arrange(subject, session)

pd <- position_jitterdodge(jitter.height=0.0, dodge.width=0.0) # move them .1 to the left and right, and up or down
colorCount=length(unique(pain.ratings$subject))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

graph =
    pain.ratings %>%
    mutate(task=paste(task, run)) %>%
    mutate(task = case_when(task == "initialpainrating 1" ~ "Initial",
                            task == "kneeTapping 1"       ~ "Tapping 1",
                            task == "kneeTapping 2"       ~ "Tapping 2")) %>%
    ggplot(aes(task, rating, color=subject, group=subject)) %+%
    geom_line(position=pd) %+%    
    geom_point(size=3, position=pd) %+%
    scale_color_manual(name="Subject",
                       values=getPalette(colorCount),
                       aesthetics = c("color")) %+%
    labs(title="Pain Ratings",
         x="Task",
         y="Rating") %+%
    facet_wrap(vars(session), labeller=as_labeller(str_to_title)) %+%
    my.theme
##    scale_x_discrete(labels=c("Initial", "Tapping 1", "Tapping 2")) %+%

dev.new(width=8, height=10, unit="in")
print(graph)
graph.filename="../derivatives/r-pain-ratings/pain_ratings.pdf"
ggsave(graph.filename, graph, width=8, height=8, units="in")


## pr = pain ratings
pr.wide=pain.ratings %>%
    mutate(task=paste0(task, run)) %>% 
    pivot_wider(id_cols = subject:session, names_from=task, values_from = rating) %>%
    mutate(i_to_k1=kneeTapping1-initialpainrating1, k1_to_k2=kneeTapping2-kneeTapping1) %>%
    select(! initialpainrating1:kneeTapping2)

pr.long = pr.wide %>%
    pivot_longer(cols=i_to_k1:k1_to_k2, names_to="Difference", values_to = "Change")

change.graph = pr.long %>%
    ggplot(aes(Difference, Change, color=subject, group=subject)) %+%
    geom_line(position=pd) %+%    
    geom_point(size=3, position=pd) %+%
    scale_color_manual(name="Subject", values=getPalette(colorCount),
                       aesthetics = c("color")) %+%
    scale_x_discrete(labels=c("Initial PR to\nTapping 1", "Tapping 1 to\nTapping2")) %+%
    labs(title="Change in Pain Ratings",
         x="Difference Between Runs",
         y="Rating Change") %+%
    facet_wrap(vars(session), labeller=as_labeller(str_to_title)) %+%
    my.theme

dev.new(width=8, height=10, unit="in")
print(change.graph)
change.graph.filename="../derivatives/r-pain-ratings/pain_ratings_change.pdf"
ggsave(change.graph.filename, change.graph, width=8, height=8, units="in")

cli_h1("Baseline pain ratings model")

baseline.pain.ratings=pain.ratings %>%
    filter(session=="baseline") %>%
    mutate(task=paste0(task, run)) %>%
    select(! c("run", "file.size", "file"))

baseline.pain.ratings %>%
    drop_na() %>%
    group_by(session, task) %>%
    summarize(min=min(rating),
              mean=mean(rating),
              median=median(rating),
              sd=sd(rating),
              max=max(rating)) %>%
    print(n=Inf)

baseline.model=lme(data=baseline.pain.ratings,
                   rating ~ task,
                   random=~1|subject, na.action=na.omit)
cli_h2("LME Summary")
print(summary(baseline.model))
cli_h2("ANOVA Summary")
print(anova(baseline.model))
cli_h2("Pairwise comparisons")
baseline.emm=emmeans(baseline.model, "task")
print(contrast(baseline.emm, "pairwise"))

cli_h1("Follow-up pain ratings model")
followup.pain.ratings=pain.ratings %>%
    filter(session=="followup") %>%
    mutate(task=paste0(task, run)) %>%
    select(! run, file, file.size)
followup.pain.ratings %>%
    group_by(session, task) %>%
    summarize(min=min(rating),
              mean=mean(rating),
              median=median(rating),
              sd=sd(rating),
              max=max(rating)) %>%
    print(n=Inf)

followup.model=lme(data=followup.pain.ratings,
                   rating ~ task,
                   random=~1|subject,
                   na.action=na.omit)
cli_h2("LME Summary")
print(summary(followup.model))
cli_h2("ANOVA Summary")
print(anova(followup.model))
cli_h2("Pairwise comparisons")
followup.emm=emmeans(followup.model, "task")
print(contrast(followup.emm, "pairwise"))


cli_h1("Baseline to Follow-up mean pain ratings model") 
mean.pain.ratings=pain.ratings %>%
    select (! c("file", "file.size")) %>%
    mutate(task=paste0(task, run)) %>%
    select(!run) %>%
    pivot_wider(names_from="task", values_from="rating") %>%
    rowwise() %>%
    mutate(mean.pain.rating=mean(c_across(!c(subject, session))))
mean.pain.ratings %>%
    drop_na() %>%
    group_by(session) %>%
    summarize(min=min(mean.pain.rating),
              mean=mean(mean.pain.rating),
              median=median(mean.pain.rating),
              sd=sd(mean.pain.rating),
              max=max(mean.pain.rating)) %>%
    print(n=Inf)

pd <- position_jitterdodge(jitter.height=0.0, dodge.width=0.1) # move them .1 to the left and right, and up or down
mean.pain.ratings.graph=mean.pain.ratings %>%
    ggplot(aes(session, mean.pain.rating, color=subject, group=subject)) %+%
    geom_line(position=pd) %+%    
    geom_point(size=3, position=pd) %+%
    scale_color_manual(name="Subject",
                       values=getPalette(colorCount),
                       aesthetics = c("color")) %+%
    scale_x_discrete(labels=c("Baseline", "Follow up")) %+%
    labs(title="Mean Pain Ratings",
         x="Session",
         y="Mean Pain Rating") %+%
    my.theme
## dev.new(width=8, height=10, unit="in")
## print(mean.pain.ratings.graph)
mean.pain.ratings.graph.filename="../derivatives/r-pain-ratings/mean_pain_ratings.pdf"
ggsave(mean.pain.ratings.graph.filename, mean.pain.ratings.graph, width=8, height=8, units="in")

timepoint.model=lme(data=mean.pain.ratings,
                    mean.pain.rating ~ session,
                    random=~1|subject,
                    na.action=na.omit)
cli_h2("LME Summary")
print(summary(timepoint.model))
cli_h2("ANOVA Summary")
print(anova(timepoint.model))
cli_h2("Pairwise comparisons")
timepoint.emm=emmeans(timepoint.model, "session")
print(contrast(timepoint.emm, "pairwise"))
