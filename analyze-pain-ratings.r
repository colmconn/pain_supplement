#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

library(tidyverse)
library(RColorBrewer)
## library(ggplot2)
library(magrittr)
## library(ggforce)


## from https://stackoverflow.com/questions/7705345/how-can-i-extract-plot-axes-ranges-for-a-ggplot2-object
get_plot_limits <- function(plot) {
    gb = ggplot_build(plot)
    xmin = gb$layout$panel_params[[1]]$x.range[1]
    xmax = gb$layout$panel_params[[1]]$x.range[2]
    ymin = gb$layout$panel_params[[1]]$y.range[1]
    ymax = gb$layout$panel_params[[1]]$y.range[2]
    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}


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

pain.ratings.file="../derivative/pain_ratings/pain_ratings.csv"
pain.ratings = read_csv(pain.ratings.file,
                        col_types=cols(
                            col_factor(), col_factor(), col_character(),
                            col_integer(), col_integer()),
                        col_names=c("subject", "session", "task", "run", "rating"))

pd <- position_jitterdodge(jitter.height=0.1, dodge.width=0.1) # move them .1 to the left and right, and up or down
colorCount=length(unique(pain.ratings$subject))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

## geom_point(size=3, position=pd, shape=21, fill="white") %+%
graph =
    pain.ratings %>%
    mutate(task=paste(task, run)) %>% 
    ggplot(aes(task, rating, color=subject, group=subject)) %+%
    geom_line(position=pd) %+%    
    geom_point(size=3, position=pd) %+%
    scale_color_manual(name="Subject", values=getPalette(colorCount),
                       aesthetics = c("color")) %+%
    scale_x_discrete(labels=c("Initial", "Tapping 1", "Tapping 2")) %+%
    labs(title="Pain Ratings",
         x="Task",
         y="Rating") %+%
    facet_wrap(vars(session), labeller=as_labeller(str_to_title)) %+%
    my.theme

dev.new(width=8, height=10, unit="in")  
print(graph)

## pr = pain ratings
pr.wide=pain.ratings %>% mutate(task=paste0(task, run)) %>% 
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

plot.limits=get_plot_limits(change.graph)
## change.graph = change.graph %+%
##     annotate("text", x=plot.limits$xmin-1.5, y=2,  label="Follow-up > Baseline", angle=90, hjust="left") %+%
##     annotate("text", x=plot.limits$xmin-1.5, y=-1, label="Follow-up < Baseline", angle=90, hjust="left") %+%
##     coord_cartesian(xlim=c(plot.limits$xmin, plot.limits$xmax), clip="off")

dev.new(width=8, height=10, unit="in")

## change.graph = change.graph %+%
    
print(change.graph)


library (nlme)

baseline.pain.ratings=pain.ratings %>%
    filter(session=="baseline") %>%
    mutate(task=paste0(task, run)) %>%
    select(! run)

baseline.model=lme(data=baseline.pain.ratings, rating ~ task, random=~1|subject)
