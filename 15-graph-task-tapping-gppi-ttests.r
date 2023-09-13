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
    clusterWhereAmI=gsub(" +", " ", scan(file=inFilename, what='character', sep=',', quiet=TRUE))

    return (clusterWhereAmI)
}

make.publication.table <- function(in.clusters.locations,
                                   in.clusters.table,
                                   in.contrast.values=NULL,
                                   in.stats.values=NULL,
                                   in.stats=NULL,
                                   in.stats.values.name="Z score",
                                   in.com=TRUE,
                                   in.volume.voxel.scale.factor=1) {

    hemisphere=in.clusters.locations %>%
        str_extract("(Right)|(Left)") %>%
        str_replace_na(replacement="") %>%
        substr(1, 1)
    
    if ( in.com ) {
        locations=bind_cols(list("Structure"=str_trim(str_replace(in.clusters.locations, "^(Right)|(Left)[[:space:]]*", "")),
                                 "Hemisphere"=hemisphere,
                                 in.clusters.table[, c("Volume", "CM RL", "CM AP", "CM IS")]))
    } else {
        locations=bind_cols(list("Structure"=str_trim(str_replace(in.clusters.locations, "^(Right)|(Left)[[:space:]]*", "")),
                                 "Hemisphere"=hemisphere,
                                 in.clusters.table[, c("Volume", "MI RL", "MI AP", "MI IS")]))
    }
    ## locations=locations %>%
    ##     mutate(across(where(is.numeric), ~ round(., digits = 0)))
    ## convert volume on number of voxels to (e.g.) SI units by
    ## multiplying by a scale factor
    if (in.volume.voxel.scale.factor != 1) {
        locations=locations %>%
            mutate(`Volume (muL)`=Volume*in.volume.voxel.scale.factor, .after=Volume)# %>%
            #mutate(across(`Volume (muL)`, ~ round(., digits = 2)))
    }
    
    if (! is.null(in.contrast.values)) {
        locations=bind_cols(locations,
                            pull(pivot_longer(in.contrast.values, cols=starts_with("Mean_")),
                                 "value"),
                            .name_repair="minimal")
        colnames(locations)=c(head(colnames(locations), -1), "Average Contrast")
        ## locations=locations %>%
        ##     mutate(across(last_col(), ~ round(., digits = 2)))
    }

    if (! is.null(in.stats.values)) {
        locations=bind_cols(locations,
                            pull(pivot_longer(in.stats.values, cols=starts_with("Mean_")),
                                 "value"),
                            .name_repair="minimal")
        colnames(locations)=c(head(colnames(locations), -1), in.stats.values.name)
        ## locations=locations %>%
        ##     mutate(across(last_col(), ~ round(., digits = 2)))
    }

    if ( ! is.null(in.stats)) {
        ss=in.stats %>%
            pivot_longer(cols=starts_with("Mean_"), names_to="cluster") %>%
            group_by(session, cluster) %>% summarize(mean=mean(value)) %>%
            pivot_wider(names_from=session, values_from=mean) %>%
            separate_wider_delim(cluster, delim="_", names=c(NA, "order"), cols_remove=FALSE) %>%
            mutate(across(order, as.numeric)) %>%
            arrange(order) %>%
            select(!c(cluster, order)) %>%
            rename_with(function(x) str_to_title(str_remove(x, "ses-")))
        locations=bind_cols(locations, ss)
##            mutate(across(unique(in.stats$session), ~ round(., digits = 2))) %>%
        
    }
        
    rownames(locations)=NULL
    return(locations)
}

seed.name.to.filename <- function(in.string) {
    out.string=str_replace_all(in.string, " ", "_")
    out.string=str_replace_all(out.string, "[(]|[)]", "")

    return(out.string)
}

run.correlations <- function(clusters.locations, rois.stats, data.table, make.graphs=FALSE) {

    cor.data.table=inner_join(rois.stats, data.table, by=c("subject", "session")) %>%
        select(!c(label, initialpainrating1, kneeTapping1, kneeTapping2, source.file)) %>%
        pivot_longer(cols=starts_with("Mean_"), names_to="cluster")

    ## now compute differences between baseline and followup for pain rating and cluster
    cor.data.table=cor.data.table %>%
        pivot_wider(id_cols=c(subject, age, sex, pain_length, leg, cluster),
                    names_from=session,
                    values_from=c(mean.pain.rating, value)) %>%
        mutate(dPainRating=`mean.pain.rating_ses-baseline`-`mean.pain.rating_ses-followup`) %>%
        mutate(dvalue=`value_ses-baseline`-`value_ses-followup`)

    names(clusters.locations)=NULL
    corr.strings=list()
    for (ii in seq.int(1, length(clusters.locations))) {
        info.message("############################################################")
        msg=sprintf("Running correlations for ROI %02d: %s", ii, clusters.locations[ii])
        info.message(msg)
        roi=paste0("Mean_", ii)

        ss=cor.data.table %>%
            filter(cluster==roi)

        ct=cor.test(ss$dPainRating, ss$dvalue)
        ##cor.str=sprintf("%d,%s,%0.3f,%0.3f,%02d,%0.3f",
        conf.level.str=format(attr(ct$conf.int, "conf.level")*100, digits=2)
        cor.str=c(ii,
                  clusters.locations[ii],
                  ct$estimate,
                  ct$statistic,
                  ct$parameter,
                  ct$p.value,
                  ct$conf.int[1],
                  ct$conf.int[2])
        names(cor.str)=c("ID", "Structure",  "Correlation",  "T value", "DoF", "p value",
                         sprintf("%s%% CI LB",  conf.level.str),
                         sprintf("%s%% CI UB",  conf.level.str))
        corr.strings[[ii]]=cor.str
        ## print(ct)

        if (ct$p.value <= 0.1 && isTRUE(make.graphs)) {
            info.message("P value < 0.01. Creating correlation graph")

            ## A theme for the graphs
            my.base.size=16
            my.theme=
                theme_bw(base_size =  my.base.size) +
                theme(
                    ## legend.position="none",
                    legend.position="bottom",        
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
            
            pd <- position_jitterdodge(jitter.height=0.05, dodge.width=0.05) # move them .1 to the left and right, and up or down
            colorCount=length(unique(ss$subject))
            getPalette = colorRampPalette(brewer.pal(9, "Set1"))

            corr.graph=ggplot(ss, aes(dPainRating, dvalue, color=subject)) %+%
                scale_color_manual(name="Subject", values=getPalette(colorCount),
                                   aesthetics = c("color")) %+%                
                geom_jitter(size=3) %+%
                geom_smooth(method=lm, se=FALSE, color="black", group=1) %+%               
                labs(x="Change in mean pain rating",
                     y="Change in Beta value",
                     title=clusters.locations[ii],
                     subtitle="Baseline - Follow-up") %+%
                my.theme

            if (isTRUE(! is.na(Sys.getenv("DISPLAY", unset=NA)))) {
                dev.new(width=8, height=10, unit="in")
                print(corr.graph)
            }
            graph.filename=file.path(graph.dir,
                                     sprintf("%02d_corr_%s.%s", ii,
                                             paste0(str_replace_all(clusters.locations[ii], " ", "_")),
                                             "pdf"))
            info.message(c("Saving", graph.filename))
            ggsave(graph.filename, corr.graph, width=8, height=8, units="in")
        }
    }

    corr.data.table=
        corr.strings %>%
        bind_rows()  %>%
        mutate(across(!c(Structure), as.numeric))

    return(corr.data.table)

}

####################################################################################################
### End of functions
####################################################################################################


## turn off messages about col type guesses from readr
options(readr.show_col_types = FALSE)

## turn off progressbars in readr
options(readr.show_progress = FALSE)

## suppress summarise()` has grouped output by ... messages
options(dplyr.summarise.inform = FALSE)

task.ttest.pipeline.dir="../derivative/afni-task-tapping-ttests/"
derivative.data="../derivative/afni-task-tapping-gppi-ttests/"

clust.header = c("Volume", "CM RL", "CM AP", "CM IS", "minRL",
                 "maxRL", "minAP", "maxAP", "minIS", "maxIS", "Mean", "SEM", "Max Int",
                 "MI RL", "MI AP", "MI IS")

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
    rowwise() %>%
    mutate(mean.pain.rating=mean(c_across(!c(subject, session))))

##    mutate(dPainRating=kneeTapping2-initialpainrating1) #%>%
##    select(! c(initialpainrating1, kneeTapping2))

info.message("Reading demographics file")
demographics=read_excel("../rawdata/Ahn_MRI pt demographic.xlsx")
demographics=demographics %>%
    mutate(ID=paste0("sub-", ID)) %>%
    rename(subject=ID, "pain_length"='Length of knee pain (month)', leg='Index knee') %>%
    rename_with(tolower) %>%
    select(! race)

## make list of seeds here
seed.roi.glob="../derivative/afni-task-tapping-ttests/roi*HEAD"
seed.roi.files=Sys.glob(seed.roi.glob) 
n.seed.rois=length(seed.roi.files)
info.message(sprintf("Got %02d ROIs", n.seed.rois))
if (n.seed.rois == 0) {
    error.message("Got no seed ROIs. Cannot continue")
    stop()
}

seed.locations.file=file.path(task.ttest.pipeline.dir, "clusters.locations.baseline-followup.csv")
seed.names=read.clusters.locations(seed.locations.file)

t.label.prefix="baseline-followup"

## A theme for the graphs
my.base.size=16
my.theme=
    theme_bw(base_size =  my.base.size) +
    theme(
        ## legend.position="none",
        legend.position="bottom",        
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

seed.count=1
for (seed.name in seed.names) {
    infix=sprintf("count-%02d_seed-%s", seed.count,
                  seed.name.to.filename(seed.name))
    cli_h1(sprintf("Graphing t-tests for GPPI seed %02d : %s", seed.count, seed.name))

    info.message("Reading data table file")
    data.table.file=file.path(derivative.data, sprintf("datatable_%s.tsv", infix))
    data.table=read_table(data.table.file)

    data.table=data.table %>%
        mutate(session=paste0("ses-", session)) %>%
        rename(subject="Subj") %>%
        rename(source.file="InputFile")

    info.message("Creating joined data table")
    data.table=left_join(data.table, delta.pain.rating, b=c("subject", "session"))
    data.table=left_join(data.table,  demographics, by=c("subject")) %>%
        mutate(label=paste(str_extract(subject, "[0-9]+"),
                           str_replace(session, "ses-", "" ), sep="_")) %>%
        relocate(label, .after="session") %>%
        relocate(source.file, .after=last_col()) %>%
        arrange(subject, session)
##            relocate(dPainRating, .before=source.file) %>%

    clusters.table.file=    file.path(derivative.data, sprintf("clusters.table.%s_%s.txt", infix, t.label.prefix))
    clusters.locations.file=file.path(derivative.data, sprintf("clusters.locations.%s_%s.csv", infix, t.label.prefix))
    rois.stats.file        =file.path(derivative.data, sprintf("roi.stats.%s_%s.txt", infix, t.label.prefix))
    rois.average.contrast.values.file=file.path(derivative.data, sprintf("roi.stats.%s_%s.averageContrastValue.txt", infix, t.label.prefix))
    rois.average.z.scores.file=file.path(derivative.data, sprintf("roi.stats.%s_%s.averageZscore.txt", infix, t.label.prefix))

    if ( ! file.exists(clusters.table.file)) {
        seed.count=seed.count+1
        warn.message("No clusters found for this seed. Skipping")
        next
    }
    
    clusters.table=read.clusters.table(clusters.table.file)
    clusters.locations=substitute.short.labels(read.clusters.locations(clusters.locations.file))
    ## clusters.locations=paste(seq.int(1, length(clusters.locations)), clusters.locations, sep=" ")
    ## useful to name the cluster locations by their corresponding
    ## Mean_? name form the roistats file. this then permits it to be used
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
    facetted.graph.filename=file.path(graph.dir, sprintf("facetted.%s.graph.pdf", infix))
    info.message(c("Saving", facetted.graph.filename))
    ggsave(facetted.graph.filename, facetted.graph, width=8, height=8, units="in")

    for (ii in seq.int(1, length(clusters.locations))) {
        roi=paste0("Mean_", ii)

        graph.dir=file.path(derivative.data, "graphs", infix)
        if ( ! dir.exists(graph.dir)) {
            info.message(c("Creating", graph.dir))
            dir.create(graph.dir)
        }
        graph.filename=file.path(graph.dir,
                                 sprintf("%02d_%s.%s", ii,
                                         paste0(str_replace_all(clusters.locations[roi], " ", "_")),
                                         "pdf"))
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
        
        ## dev.new(width=8, height=10, unit="in")
        ## print(cluster.graph)
        info.message(c("Saving", graph.filename))
        ggsave(graph.filename, cluster.graph, width=8, height=8, units="in")
    }

    publication.table=make.publication.table(clusters.locations,
                                             clusters.table,
                                             rois.average.contrast.values,
                                             rois.average.z.scores,
                                             rois.stats,
                                             in.volume.voxel.scale.factor=2.5*2.5*2.5)
    publication.table.filename=file.path(derivative.data, sprintf("publication.table.%s.tsv", infix))
    info.message("Saving publication table to", publication.table.filename)
    write_tsv(publication.table, publication.table.filename)
    info.message("Publication table")
    print(publication.table)

    correlations.table=run.correlations(clusters.locations, rois.stats, data.table)
    correlations.table.filename=file.path(derivative.data, sprintf("correlations.table.%s.tsv", infix))
    info.message("Saving correlations table to", correlations.table.filename)
    write_tsv(correlations.table, correlations.table.filename)
    info.message("Table of correlations")
    print(correlations.table, n=Inf)
    
    seed.count=seed.count+1
}
