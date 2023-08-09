#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

source("common.functions.r")

library(tidyverse)
library(readxl)
library(RColorBrewer)
####################################################################################################
### Start of functions
####################################################################################################

stack <- function(){ 
    it <- list() 
    res <- list( 
        push=function(x){ 
            it[[length(it)+1]] <<- x 
        }, 
        pop=function(){ 
            val <- it[[length(it)]] 
            it <<- it[-length(it)] 
            return(val) 
        }, 
        value=function(){ 
            return(it) 
        } 
    ) 
    class(res) <- "stack" 
    res 
}

print.stack <- function(x,...){ 
    print(x$value()) 
}

push <- function(stack,obj){ 
    stack$push(obj) 
}

pop <- function(stack){ 
    stack$pop() 
}

make.significance.indications <- function(pValues, which.pValues=c(1)) {

    Signif=symnum(pValues, corr=FALSE, na=FALSE, cutpoints = c(0,  .001,.01, .05, .1, 1),
                  symbols   =  c("***", "**", "*", ".", " "))
    f=format(Signif)

    ## only return the first one as we're only interested in marking significant group effects
    return(f[which.pValues])
}

substitute.short.labels <- function(in.region.names) {

    return.substituted.label=in.region.names %>%
        str_replace_all("_", " ") %>%
        str_replace("[0-9]+ ", "") %>%
        str_replace("Inf[[:space:]]+", "Inferior ") %>%
        str_replace("Sup[[:space:]]+", "Superior ") %>%
        str_replace("Gy", "Gyrus") %>%
        str_replace("^R", "Right") %>%
        str_replace("^L", "Left") %>%
        str_replace("[[:space:]]+\\([^\\)]+$", "")
        
    return (return.substituted.label)
}

read.summary.stats.table <- function(inFilename) {
    info.message("Reading" , inFilename)
    statsTable=read_delim(inFilename, delim="\t", name_repair=str_squish) %>%
        select(starts_with("Mean_"))

    return(statsTable)
}

read.stats.table <- function (inFilename) {

    info.message("Reading" , inFilename)
    statsTable=read_delim(inFilename, delim="\t", name_repair=str_squish) %>%
        separate_wider_regex(File,
                             c(".*/afni-task-tapping/",
                               subject="sub-[[:alnum:]]*",
                               "/",
                               session="ses-[[:alnum:]]*",
                               "/.*"))    
    return(statsTable)
}

read.clusters.table <- function (inFilename){
    info.message("Reading", file.path(inFilename))
    clusters=read.table(file.path(inFilename))
    colnames(clusters) = clust.header
    return (as_tibble(clusters))
}

read.clusters.locations <- function (inFilename) {
    info.message("Reading cluster locations from", inFilename)
    ## the gsub here chews up multiple consequtive spaces and replaces them with a single space
    clusterWhereAmI=gsub(" +", " ", scan(file=inFilename, what='character', sep=','))

    return (clusterWhereAmI)
}


make.publication.table <- function(in.clusters.locations, in.clusters.table, in.com=TRUE) {
    ## hemisphere=gsub("(Right)|(Left)", "", substr(in.clusters.locations, 1, 1))
    hemisphere=in.clusters.locations %>%
        str_extract("(Right)|(Left)") %>%
        str_replace_na(replacement="") %>%
        substr(1, 1)
    
    if ( in.com ) {
        locations=cbind(gsub("^[RL] ", "", in.clusters.locations), hemisphere, round(in.clusters.table[, c("Volume", "CM RL", "CM AP", "CM IS")], 0))
    } else {
        locations=cbind(gsub("^[RL] ", "", in.clusters.locations), hemisphere, round(in.clusters.table[, c("Volume", "MI RL", "MI AP", "MI IS")], 0))
    }
    publication.table.header=c("Structure", colnames(locations)[-1])

    colnames(locations)=publication.table.header
    rownames(locations)=NULL
    return(locations)
}
####################################################################################################
### End of functions
####################################################################################################


## turn off messages about col type guesses from readr
options(readr.show_col_types = FALSE)

## turn off progressbars in readr
options(readr.show_progress = FALSE)

derivative.data="../derivative/afni-task-tapping-ttests/"

clust.header = c("Volume", "CM RL", "CM AP", "CM IS", "minRL",
                 "maxRL", "minAP", "maxAP", "minIS", "maxIS", "Mean", "SEM", "Max Int",
                 "MI RL", "MI AP", "MI IS")

info.message("Reading data table file")
data.table.file=file.path(derivative.data, "datatable.tsv")
data.table=read_table(data.table.file)

data.table=data.table %>%
    mutate(session=paste0("ses-", session)) %>%
    rename(subject="Subj") %>%
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

info.message("Creating joined data table")
data.table=left_join(data.table, delta.pain.rating, b=c("subject", "session"))
data.table=left_join(data.table,  demographics, by=c("subject")) %>%
    mutate(label=paste(str_extract(subject, "[0-9]+"),
                       str_replace(session, "ses-", "" ), sep="_")) %>%
    relocate(label, .after="session") %>%
    relocate(source.file, .after=last_col()) %>%
    relocate(dPainRating, .before=source.file) %>%
    arrange(subject, session)


clusters.table.file=    file.path(derivative.data, "clusters.table.baseline-followup.txt")
clusters.locations.file=file.path(derivative.data, "clusters.locations.baseline-followup.csv")
rois.stats.file        =file.path(derivative.data, "roi.stats.baseline-followup.txt")
rois.average.contrast.values.file=file.path(derivative.data, "roi.stats.baseline-followup.averageContrastValue.txt")
rois.average.z.scores.file=file.path(derivative.data, "roi.stats.baseline-followup.averageZscore.txt")

clusters.table=read.clusters.table(clusters.table.file)
clusters.locations=substitute.short.labels(read.clusters.locations(clusters.locations.file))
clusters.locations=paste(seq.int(1, length(clusters.locations)), clusters.locations, sep=" ")
## useful to name the cluster locations by their corresponding
## Mean_? name form teh roistats file. this them permits it to be used
## as a labeller for the facets of the graph later
names(clusters.locations)=paste0("Mean_", seq.int(1, length(clusters.locations)))

rois.stats=read.stats.table(rois.stats.file)  %>%
    filter(`Sub-brick`=="1[tapping#0]") %>%
    select (!`Sub-brick`)##  %>%
    ## pivot_longer(cols=!c(subject, session)) %>%
    ## pivot_wider(id_cols=c(subject, name), names_from=session) %>%
    ## mutate(contrast=`ses-baseline`-`ses-followup`) %>%
    ## select(!starts_with("ses-")) %>%
    ## pivot_wider(id_cols=subject, values_from=contrast)##  %>%
    ## rename_with(.col = starts_with("Mean_"), ~ clusters.locations)

rois.average.contrast.values=read.summary.stats.table(rois.average.contrast.values.file)
rois.average.z.scores=read.summary.stats.table(rois.average.z.scores.file)

## A theme for the graphs
my.base.size=16
my.theme=
    theme_bw(base_size =  my.base.size) +
    theme(
        legend.position="none",
        ## legend.position="bottom",        
        ## panel.grid.major = element_blank(),
        ## panel.grid.minor = element_blank(),
        
        ##remove the panel border
        ##panel.border = element_blank(),

        ## facet strip label text size
        strip.text.x=element_text(size=my.base.size*0.8),
        
        ## add back the axis lines
        axis.line=element_line(colour = "grey50"),
        ## legend.text = element_text(angle=45),
        
        ##axis.title.x=element_blank(),
        axis.title.x = element_text(size=my.base.size, vjust=0),
        axis.title.y = element_text(size=my.base.size, vjust=0.4, angle =  90),
        plot.title=element_text(size=my.base.size*1.2, vjust=1))

pd <- position_jitterdodge(jitter.height=0.1, dodge.width=0.1) # move them .1 to the left and right, and up or down
colorCount=length(unique(pain.ratings$subject))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

rois.stats.summary=rois.stats %>%
    pivot_longer(cols=!c(subject, session), names_to="cluster") %>%
    group_by(session, cluster) %>% 
    summarize(mean=mean(value), sd=sd(value))

facetted.graph=ggplot(rois.stats.summary, aes(session, mean, group=1)) %+%
    geom_point(data=pivot_longer(rois.stats,
                                 cols=!c(subject, session),
                                 names_to="cluster"),
               aes(x=session, y=value, color=subject),
               position=pd, size=3) %+%
    scale_color_manual(name="Subject", values=getPalette(colorCount),
                       aesthetics = c("color")) %+%
    scale_x_discrete(labels = function(x) str_to_title(str_remove(x, "ses-"))) %+%
    geom_point(size=3) %+%
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width=.3)) %+%
    geom_line() %+%
    labs(x="Session",
         y="Average Beta") %+%
    facet_wrap(vars(cluster), labeller=as_labeller(clusters.locations)) %+%
    my.theme
dev.new(width=8, height=10, unit="in")
print(facetted.graph)

graph.dir=file.path(derivative.data, "graphs")
if ( ! dir.exists(graph.dir)) {
    info.message(c("Creating", graph.dir))
    dir.create(graph.dir)
}
facetted.graph.filename=file.path(graph.dir, "facetted.graph.pdf")
info.message(c("Saving", facted.graph.filename))
ggsave(facetted.graph.filename, facetted.graph, width=8, height=8, units="in")

for (ii in seq.int(1, length(clusters.locations))) {
    roi=paste0("Mean_", ii)
    graph.filename=file.path(graph.dir,
                             paste0(str_replace_all(clusters.locations[roi], " ", "_"),
                                    ".pdf"))
    cluster.graph=rois.stats %>%
        select(c(subject, session, all_of(roi))) %>%
        pivot_longer(cols=!c(subject, session), names_to="cluster") %>%
        group_by(session, cluster) %>%
        summarize(mean=mean(value), sd=sd(value)) %>%
        ggplot(aes(session, mean, group=1)) %+%
        geom_point(data=filter(pivot_longer(rois.stats,
                                     cols=!c(subject, session),
                                     names_to="cluster"), cluster==roi),
                   aes(x=session, y=value, color=subject),
                   position=pd, size=3) %+%
        scale_color_manual(name="Subject", values=getPalette(colorCount),
                           aesthetics = c("color")) %+%
        scale_x_discrete(labels = function(x) str_to_title(str_remove(x, "ses-"))) %+%
        geom_point(size=3) %+%
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, width=.3)) %+%
        geom_line() %+%
        labs(title=str_remove(clusters.locations[roi], "^[0-9]*[[:space:]]*"),
             x="Session",
             y="Average Beta") %+%
        my.theme
    
    dev.new(width=8, height=10, unit="in")
    print(cluster.graph)
    ggsave(graph.filename, cluster.graph, width=8, height=8, units="in")
}

