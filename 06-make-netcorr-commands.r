#!/bin/env Rscript

rm(list=ls())

options(error = function() traceback())

AFNI_R_DIR=Sys.getenv("AFNI_R_DIR", unset=NA)

## use the functions for loading and saving briks from the AFNI
## distribution as they can cleanly handle floats/doubles
if ( ! is.na(AFNI_R_DIR) ) {
    source(file.path(AFNI_R_DIR, "AFNIio.R"))
} else {
    stop("Couldn't find AFNI_R_DIR in environment. This points to the location from which to load functions for reading and writing AFNI BRIKS. Stopping!")
}

library(getopt)

source("common.functions.r")

##########################################################################################################################################################################
### START OF FUNCTIONS ###################################################################################################################################################
##########################################################################################################################################################################


help <- function(){

}

check.command.line.arguments <- function (in.opt) {
    if (is.null(in.opt$quiet)) {
        in.opt$quiet=FALSE
    }

    if (is.null(in.opt$execute)) {
        in.opt$execute=FALSE
    }
    
    ## if help was asked for print a friendly message
    ## and exit with a non-zero error code
    if ( !is.null(in.opt$help) ) {
        cat(getopt(spec, usage=TRUE));
        if ( ! interactive()) 
            q(status=1)
    }

    if ( is.null(in.opt$window) ) {
        if (! in.opt$quiet)
            warn.message("No value provided for window. Assuming none\n")
        in.opt$window="none"
    }
    
    valid.window.types=c("consecutive", "overlap", "none")
    window.type=pmatch(in.opt$window, valid.window.types)
    if ( is.na(window.type) ) {
        error.message(sprintf("Valid options for window type are %s", paste(valid.window.types, collapse=", ")))
        if ( ! interactive()) 
            q(status=1)
    } else {
        in.opt$window=valid.window.types[window.type]
    }

    if (in.opt$window == "none" ) 
        in.opt$width=NA
    
    if ( is.null(in.opt$width) && in.opt$window != "none") {
        error.message("You must provide a window width when using a \"", in.opt$window,  "\" window", sep="")
        if ( ! interactive()) 
            q(status=1)
        
    }##  else {
        ## in.opt$width=NA
    ## }

    if ( is.null(in.opt$step) && in.opt$window == "overlap") {
        error.message("You must provide a step to leave between consecutive window starts when using a \"", in.opt$window,  "\" window", sep="")
        q(status=1)
    } else if ( in.opt$window %in% c("consecutive", "none") ) {
        if (! in.opt$quiet)
            info.message("Window type is set to \"", in.opt$window, "\", ignoring step", sep="")
        in.opt$step = NA
    }

    if ( is.null(in.opt$source.task)) {
        error.message("You must provide a source task name")
        if ( ! interactive()) 
            q(status=1)
    }

    if ( is.null(in.opt$source.task.afni_proc.dir)) {
        error.message("You must provide a source task afni_proc directory name")
        if ( ! interactive()) 
            q(status=1)
    }

    if ( is.null(in.opt$task)) {
        error.message("You must provide a task name")
        if ( ! interactive()) 
            q(status=1)
    }
    
    if ( is.null(in.opt$subject)) {
        error.message("You must provide a subject name")
        if ( ! interactive()) 
            q(status=1)
    }

    if (! grepl("sub-", in.opt$subject)) {
        warn.message("Subject name missing sub- prefix. Prepending it")
        in.opt$subject = paste("sub-", in.opt$subject, sep="")
    }
    
    if ( is.null(in.opt$session)) {
        error.message("You must provide a session name")
        if ( ! interactive()) 
            q(status=1)
    }

    if (! grepl("ses-", in.opt$session)) {
        warn.message("Session name missing ses- prefix. Prepending it")        
        in.opt$session = paste("ses-", in.opt$session, sep="")
    }

    if ( is.null(in.opt$source)) {
        error.message("You must provide a source file type")
        if ( ! interactive()) 
            q(status=1)
    }

    if ( ! grepl("^REML|[f]?anaticor$", opt$source)) {
        error.message(sprintf("Source file type (%s) must be one of REML, anaticor, fanaticor", opt$source))
        if ( ! interactive()) 
            q(status=1)
    }
    
    if (is.null(in.opt$derivative)) {
        error.message("You must provide a name for the derivative directory")
        if ( ! interactive()) 
            q(status=1)
    }

    if ( ! dir.exists(in.opt$derivative)) {
        cat(sprintf("*** The name of the derivative directory provided (%s) does not exist", in.opt$derivative))
        if ( ! interactive()) 
            q(status=1)
    }

    if (is.null(in.opt$atlas)) {
        error.message("You must provide a value for the atlas")
        if ( ! interactive()) 
            q(status=1)
    } else {
        in.opt$atlas = gsub("[-_]+", "", in.opt$atlas)
    }

   if (is.null(in.opt$rois)) {
       error.message("You must provide a value for the rois")
       if ( ! interactive()) 
           q(status=1)
   } else {
       in.opt$rois = substitute.variables(in.opt$rois, in.opt)
       if ( ! file.exists(in.opt$rois )) {
           error.message("You ROI(s) file does not exist")
           if ( ! interactive()) 
               q(status=1)
       }
   }

   if (is.null(in.opt$mask)) {
       error.message("You must provide a value for the mask")
       if ( ! interactive()) 
           q(status=1)
   } else {
       in.opt$mask = substitute.variables(in.opt$mask, in.opt)
       if ( ! file.exists(in.opt$mask )) {
           error.message("You ROI(s) file does not exist")
           if ( ! interactive()) 
               q(status=1)
       }
   }

    if (! is.null(in.opt$extra)) {
        in.opt$extra=gsub("[\\\'\"]", "", in.opt$extra)
    }

    return(in.opt)
}

print.command.line.arguments.summary <- function () {
    info.message("Subject name is set to          :", opt$subject)
    info.message("Session name is set to          :", opt$session)
    info.message("Source task name is set to      :", opt$source.task)
    info.message("Source task afni_proc directory name is set to      :", opt$source.task.afni_proc.dir)    
    info.message("Task name is set to             :", opt$task)        
    info.message("Source file type is set to      :", opt$source)
    info.message("Atlas name is set to            :", opt$atlas)
    info.message("Mask file is set to             :", opt$mask)        
    info.message("ROIs file is set to             :", opt$rois)    
    info.message("Derivative directory is set to  :", opt$derivative)
    info.message("Window type is set to           :", opt$window)
    info.message("Window width is set to          :", opt$width)
    info.message("Window step is set to           :", opt$step)
    if (opt$execute) 
        info.message("Executing 3dNetCorr commands")    
    if ( ! is.null(opt$extra)) 
        info.message("Extra agruments for 3dNetCorr   :", opt$extra)
}


make.windows <- function (in.opt, in.length) {
    if (in.opt$window == "none" ) {
        windows=("0..$")
    } else if (in.opt$window == "consecutive" ) {

        window.starts=seq.int(from=0, to=(in.length-in.opt$width), by=in.opt$width)
        window.ends=(window.starts + in.opt$width) -1
        if (tail(window.ends, 1) < in.length) {
            if (! in.opt$quiet)
                warn.message("The dataset is not a multiple of the window with and will be truncated")
        }
        
        indices=cbind(window.starts, window.ends)
        windows=unlist(apply(indices, 1, function (xx) {
            sprintf("%d..%d", xx[1], xx[2])
        }))

        return (windows)
        
        ## n.windows=ceiling(in.length/in.opt$width)
        ## windows=vector(mode="character", length=n.windows)
        ## cat("*** Preparing", n.windows, "windows: ")
        ## window.count=1
        ## done=FALSE
        ## window.start=0
        ## window.end=in.opt$width - 1
        ## while ( window.count <=  n.windows) {
        ##     if  (window.count ==  n.windows )
        ##         windows[window.count]=sprintf("%d..%d", window.start, in.length-1)
        ##     else 
        ##         windows[window.count]=sprintf("%d..%d", window.start, window.end)            
            
        ##     window.start=window.end + 1
        ##     window.end=window.end + in.opt$width

        ##     window.count=window.count+1
        ## }
        ## ## ww=seq.int(from=0, to=in.length, by=in.opt$width)
        ## cat(windows, sep=", ")
        ## cat("\n")
    } else { ## must be overlapping

        ## inspired by the following code
        ## http://stats.stackexchange.com/questions/3051/mean-of-a-sliding-window-in-r
        ## slideFunct <- function(data, window, step){
        ##     total <- length(data)
        ##     spots <- seq(from=1, to=(total-window), by=step)
        ##     result <- vector(length = length(spots))
        ##     for(i in 1:length(spots)){
        ##         result[i] <- mean(data[spots[i]:(spots[i]+window)])
        ##     }
        ##     return(result)
        ## }

        window.starts=seq.int(from=0, to=(in.length-in.opt$width), by=in.opt$step)
        window.ends=(window.starts + in.opt$width) -1
        if (tail(window.ends, 1) < in.length) {
            if (! in.opt$quiet)
                warn.message("The dataset is not a multiple of the window width and will be truncated")
        }
        indices=cbind(window.starts, window.ends)
        windows=unlist(apply(indices, 1, function (xx) {
            sprintf("%d..%d", xx[1], xx[2])
        }))
    }
    return(windows)
}

make.3dNetCorr.commands <- function (in.output.dir, in.source.file.name, in.opt, in.windows, in.sub.brik.selector) {
    yy=cbind(seq.int(1, length(in.windows)), in.windows)

    args=unlist(apply(yy, 1, function (xx) {
        if (in.sub.brik.selector) {
            if (in.opt$window == "none") {
                sprintf("%s-prefix %s/atlas-%s -mask %s -in_rois %s -inset %s\'[%s]\'",
                        ifelse(is.null(in.opt$extra), "", paste(in.opt$extra, " ", sep="")),
                        in.output.dir,
                        in.opt$atlas,
                        in.opt$mask,
                        in.opt$rois,
                        in.source.file.name,
                        xx[2])
            } else {
                sprintf("%s-prefix %s/atlas-%s_window-%02d -mask %s -in_rois %s -inset %s\'[%s]\'",
                        ifelse(is.null(in.opt$extra), "", paste(in.opt$extra, " ", sep="")),
                        in.output.dir,
                        in.opt$atlas,
                        as.integer(xx[1]),
                        in.opt$mask,                        
                        in.opt$rois,
                        in.source.file.name,
                        xx[2])
            }
        } else {
            if (in.opt$window == "none") {
                sprintf("%s-prefix %s/atlas-%s -mask %s -in_rois %s -inset %s",
                        ifelse(is.null(in.opt$extra), "", paste(in.opt$extra, " ", sep="")),
                        in.output.dir,
                        in.opt$atlas,
                        in.opt$mask,                        
                        in.opt$rois,
                        in.source.file.name)
            } else {
                sprintf("%s-prefix %s/atlas-%s_window-%02d -mask %s -in_rois %s -inset %s",
                        ifelse(is.null(in.opt$extra), "", paste(in.opt$extra, " ", sep="")),
                        in.output.dir,
                        in.opt$atlas,
                        as.integer(xx[1]),
                        in.opt$mask,                                                
                        in.opt$rois,
                        in.source.file.name)
            }
        }}))

    ## ll=unlist(sapply(in.windows, function (xx) {
    ##     sprintf("3dNetCorr -prefix %s/%s -rois %s -inset %s\'[%s]\'",
    ##             in.opt$destination, in.opt$prefix, in.opt$rois, in.opt$source, xx)
    ## }))
    
    ## ll=unlist(sapply(in.windows,
    ##     function (xx) {
    ##         sprintf("3dbucket -prefix %s.%s -session %s %s\'[%s]\'",
    ##                 in.opt$prefix, sub("..", ".to.", xx, fixed=TRUE), in.opt$destination, in.opt$source, xx)
    ##     }
    ##                  ))

    cmds=mapply(list, rep("3dNetCorr", length(args)), args, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    return(cmds)
    ## return(list("command" = "3dNetCorr", "arguments"=args))
}

get.mri.dims <- function(in.file) {
    if (file.exists(in.file)) {
        dd=scan(textConnection(system2("3dinfo", c("-n4", in.file),
                                       stdout=TRUE, stderr=NULL)),
                sep="\t", quiet=TRUE)
    } else {
        dd=c(NA)
    }

    return(dd)
}

get.mri.dset.max.value <- function(in.file) {
    if (file.exists(in.file)) {
        ## dd=scan(textConnection(system2("3dBrickStat", c("-max", in.file),
        ##                                stdout=TRUE, stderr=NULL)),
        ##         sep="\t", quiet=TRUE)

        tt=read.table(text=system2("3dclust", c("-isovalue", "3", "1", in.file),
                                   stdout=TRUE, stderr=NULL),
                      header=FALSE)
        colnames(tt) = c("Volume",
                         "CM RL",  "CM AP",  "CM IS",
                         "minRL",  "maxRL",  "minAP",
                         "maxAP",  "minIS",  "maxIS",
                         "Mean",   "SEM",    "Max Int",
                         "MI RL",  "MI AP",  "MI IS")
        dd=length(unique(sort(tt[, "Max Int"])))
    } else {
        dd=rep(NA, 4)
    }
     return(dd)
}
    
## read.mri.file <- function(in.filename) {
##     if ( file.exists(in.filename)) {
##         if (! opt$quiet)
##             cat("*** Reading", in.filename, "\n")
##         mri.dset=read.AFNI(in.filename, verb=TRUE)
##     } else {
##         mri.dset=NULL
##         err.msg=paste("*** ERROR: No such file", in.filename, ". Cannot continue. Stopping.\n", sep="")
##         if ( interactive()) {
##             cat(err.msg)
##         } else {
##             stop(err.msg)
##         }
##     }
##     return(mri.dset)
## }

substitute.variables <- function(in.line, in.opt) {
    ## aa=gregexec("\\$\\{([[:alnum:]]+)\\}", in.line)
    aa=gregexec("\\$\\{([a-zA-Z][._a-zA-Z0-9]*)\\}", in.line)    
    ## cat("### aa #################\n")
    
    ## print(ncol(aa[[1]]))
    ## print(aa)
    ## cat("### Got", ncol(aa[[1]]), "matches\n")
    ## cat("###### #################\n")
    
    newline=""

    for (ii in seq.int(ncol(aa[[1]]))) {
        ## cat ("### Match:", ii, "\n")
        match.length=attr(aa[[1]], "match.length")[1, ii]
        ## cat ("### Start: ", aa[[1]][1, ii], "\n")
        ## cat ("### End: ",   aa[[1]][1, ii] + match.length -1, "\n")    

        variable.reference=substr(in.line,
                                  aa[[1]][1, ii],
                                  aa[[1]][1, ii] + attr(aa[[1]], "match.length")[1, ii] - 1)
        variable.name=substr(in.line,
                             aa[[1]][2, ii],
                             aa[[1]][2, ii] + attr(aa[[1]], "match.length")[2, ii] - 1)
        ## print(variable.reference)
        ## print(variable.name)

        ## if the first variable match is after the start of the
        ## string, copy from the start of the string to the first
        ## character before the match to the newline variable
        if (ii == 1 && aa[[1]][1, 1] != 1) {
            newline=substring(in.line, 1, aa[[1]][1, ii] - 1)
        }
        
        if (variable.name %in% names(in.opt)) {
            ## cat(sprintf("### Substituting %s -> %s\n", variable.reference, variable.name))
            newline=paste0(newline, in.opt[[variable.name]])
        } else {
            ## cat("### No substitution possible for", variable.reference, "\n")
            newline=paste0(newline, substr(in.line,
                                           aa[[1]][1, ii],
                                           aa[[1]][1, ii] + attr(aa[[1]], "match.length")[1, ii] - 1))
        }

        if (ii == ncol(aa[[1]])) {
            ## this is the last match
            ## print("Last match")
            start=aa[[1]][1, ii] + attr(aa[[1]], "match.length")[1, ii]
            stop=nchar(in.line)
            ## cat("### Start=", start, " Stop=", stop, "\n")
            ## newline=paste0(newline, substr(in.line, aa[[1]][1, ii] + attr(aa[[1]], "match.length")[1, ii], length(in.line)))
            if (stop >= start) {
                newline=paste0(newline, substr(in.line, start, stop))
            }
        } else {
            ## print("Mid match")
            start=aa[[1]][1, ii] + attr(aa[[1]], "match.length")[1, ii]
            stop=aa[[1]][1, ii+1] - 1
            ## cat("### Start=", start, " Stop=", stop, "\n")
            ## newline=paste0(newline, substr(in.line, aa[[1]][1, ii] + attr(aa[[1]], "match.length")[1, ii], length(in.line)))
            newline=paste0(newline, substr(in.line, start, stop))
        } 
    }

    return(newline)
}

check.gridset <- function(in.source.file, in.opt) {
    if (file.exists(in.source.file)){
        dd.source=scan(textConnection(system2("3dinfo", c("-d3", in.source.file),
                                              stdout=TRUE, stderr=NULL)),
                       sep="\t", quiet=TRUE)
    } else {
        dd.source=rep(NA, 3)
    }

    if (file.exists(in.opt$mask)){
        dd.mask=scan(textConnection(system2("3dinfo", c("-d3", in.opt$mask),
                                            stdout=TRUE, stderr=NULL)),
                     sep="\t", quiet=TRUE)
    } else {
        dd.mask=rep(NA, 3)
    }

    if (file.exists(in.opt$rois)){
        dd.rois=scan(textConnection(system2("3dinfo", c("-d3", in.opt$rois),
                                            stdout=TRUE, stderr=NULL)),
                     sep="\t", quiet=TRUE)
    } else {
        dd.rois=rep(NA, 3)
    }

    dd.dims=matrix(abs(c(dd.source, dd.mask, dd.rois)), ncol=3, byrow=TRUE)
    ## the folowing line of code is useful for testing whether the
    ## dimension checking code below works or not
    ## dd.dims[1, 1]=dd.dims[1, 1]+1
    
    ## check that the ROIs, mask, and the EPI are at the same resolution
    if ( ! all(apply(dd.dims, 2, function (xx) { abs(max(xx) - min(xx)) < .Machine$double.eps }))) {
        rownames(dd.dims)=c("Source", "Mask", "ROIs")
        err.msg=c("The gridset of the source dataset and ROIs mask do not match. Cannot continue. Stopping",
                  capture.output(print(dd.dims)))

        if (interactive())
            error.message(err.msg)
        else
            stop(err.msg)
    } else {
        return(TRUE)
    }
}

check.template.space <- function(in.source.file, in.opt) {
    dd=system2("3dinfo", c("-space", in.source.file),
               stdout=TRUE, stderr=NULL)
    ss.source=scan(textConnection(dd), sep="\t", what=character(), quiet=TRUE)

    dd=system2("3dinfo", c("-space", in.opt$mask),
               stdout=TRUE, stderr=NULL)
    ss.mask=scan(textConnection(dd), sep="\t", what=character(),  quiet=TRUE)

    dd=system2("3dinfo", c("-space", in.opt$rois),
               stdout=TRUE, stderr=NULL)
    ss.rois=scan(textConnection(dd), sep="\t", what=character(),  quiet=TRUE)

    spaces=unique(c(ss.source, ss.mask, ss.rois))
    if (length(spaces) > 1) {
        ## check that the rois and the EPI in the same space
        err.msg=c("The template space of the source, mask, and ROIs datasets do not match. Cannot continue. Stopping",
                  "+-----------+-----------+",
                  "+ Dataset   + Space     +",
                  "+-----------+-----------+",    
                  sprintf("| %-10s| %-10s|", "Source", ss.source),
                  sprintf("| %-10s| %-10s|", "Mask",   ss.mask),
                  sprintf("| %-10s| %-10s|", "ROIs",   ss.rois),
                  "+-----------+-----------+")

        if (interactive())
            error.message(err.msg)
        else
            stop(err.msg)
    } else {
        return(TRUE)
    }
}

conditionally.make.directory <- function (dir) {
    if ( ! dir.exists(dir)) {
        if (! opt$quiet)
            info.message("Recursively creating", dir)
        dir.create(dir, recursive=TRUE)
    }
}


execute.commands <- function (in.commands) {
    for (ii in 1:length(in.commands)) {
        cat("*** Executing:", paste(net.corr.commands[[ii]][[1]],  net.corr.commands[[ii]][[2]]), "\n")
        ## the first element of each sublist in net.corr.commands
        ## contains the commands to be executed and the second the
        ## arguments to that command
        system2(net.corr.commands[[ii]][[1]], net.corr.commands[[ii]][[2]])
    }
}

print.commands <- function (in.commands) {
    for (ii in 1:length(in.commands)) {
        cat(paste(net.corr.commands[[ii]][[1]],  net.corr.commands[[ii]][[2]]), "\n")
    }
}

##########################################################################################################################################################################
### END OF FUNCTIONS #####################################################################################################################################################
##########################################################################################################################################################################

NO_ARGUMENT="0"
REQUIRED_ARGUMENT="1"
OPTIONAL_ARGUMENT="2"

## process command line arguments
spec = matrix(c(
    "source.task",   "1", REQUIRED_ARGUMENT, "character", "Name of the task directory containing the resting state analysis output",
    "source.task.afni_proc.dir", "2", REQUIRED_ARGUMENT, "character", "Name of the directory containing the output of the afni_proc resting state analysis",
    "task",          "a", REQUIRED_ARGUMENT, "character", "Name of the task directory to be created in the derivative directory",
    "derivative",    "d", REQUIRED_ARGUMENT, "character", "Path to the derivative directory",
    "step",          "e", REQUIRED_ARGUMENT, "integer",   "Step between start of operlapping windows",
    "help",          "h", NO_ARGUMENT,       "logical",   "Help for this program",
    "session",       "i", REQUIRED_ARGUMENT, "character", "Session name",
    "subject",       "j", REQUIRED_ARGUMENT, "character", "Subject name",
    "atlas",         "l", REQUIRED_ARGUMENT, "character", "Atlas name. Used as the prefix for the files produced by 3dNetCorr",
    "mask",          "m", REQUIRED_ARGUMENT, "character", "Mask within which tio restrict 3dNetCorr\'s work",
    "quiet",         "q", NO_ARGUMENT,       "logical",   "Print no informational message. Only print 3dNetCorr commands.",
    "rois",          "r", REQUIRED_ARGUMENT, "character", "Mask of ROIs",
    "source",        "s", REQUIRED_ARGUMENT, "character", "Type of source file: one and only one of REML, anaticor, fanaticor",
    "width",         "t", REQUIRED_ARGUMENT, "integer",   "Width of the window",
    "execute",       "u", NO_ARGUMENT,       "logical",   "Execute the 3dNetCorr commands instead of printing them out. The default is to print them out",
    "window",        "w", REQUIRED_ARGUMENT, "character", "Window type: consecutive, overlap, or none",
    "extra",         "x", REQUIRED_ARGUMENT, "character", "Extra arguments to provide to 3dNetCorr. These must be valid 3dNetCorr arguments"
), byrow=TRUE, ncol=5)

if (interactive()) {
    info.message("Setting interactive options")
    
    args=c(
        "--subject",     "test01",
        "--session",     "baseline",
        "--task",        "brain-graph",
        "--source.task", "afni-resting-state",
        "--source.task.afni_proc.dir", "resting-state-preprocessed-polortA-NL",        
        "--source",      "fanaticor",
        "--derivative",  "../derivative",
        "--atlas",       "destrieux.scgm",
        "--mask",        "${derivative}/${source.task}/${subject}/${session}/${source.task.afni_proc.dir}/mask_group+tlrc.HEAD",
        "--window",      "none",
        "--width",       "20",
        "--extra",       "\\-fish_z \\-ignore_LT",
        "--rois",        "${derivative}/${source.task}/${subject}/${session}/${source.task.afni_proc.dir}/destrieux.scgm/destrieux.scgm_aeseg+tlrc.HEAD",
        "-u"
    )
    #        "--rois",        "../standard/aal2_for_SPM12/aal2.3mm.nii.gz",

    opt = getopt(spec, opt=args)
} else {
    opt = getopt(spec)
}

opt=check.command.line.arguments(opt)
if (! opt$quiet)
    print.command.line.arguments.summary()

task.output.directory=file.path(opt$derivative, opt$task,  opt$subject, opt$session)
info.message("Setting task output directory to:", task.output.directory)
conditionally.make.directory(task.output.directory)

source.file.name=file.path(opt$derivative, opt$source.task, opt$subject, opt$session, opt$source.task.afni_proc.dir,
                           sprintf("errts.%s_%s.%s+tlrc.HEAD", opt$subject, opt$session, opt$source))
info.message("Setting source file name to:", source.file.name)
if ( ! file.exists(source.file.name)) {
    error.message(sprintf("Source file name (%s) does not exist. Cannot continue", source.file.name))
    if (! interactive())
        q(status=1)
}

source.file.dims=get.mri.dims(source.file.name)

check.gridset(source.file.name, opt)
check.template.space(source.file.name, opt)
number.of.rois=get.mri.dset.max.value(opt$rois)

if (! opt$quiet)
    info.message("There are", number.of.rois, "ROIs in the ROI mask dataset")

windows=make.windows(opt, source.file.dims[4])

if (source.file.dims[4] > 1 && opt$window != "none") {
    net.corr.commands=make.3dNetCorr.commands(task.output.directory, source.file.name, opt, windows, TRUE)
} else { 
    net.corr.commands=make.3dNetCorr.commands(task.output.directory, source.file.name, opt, windows, FALSE)
}

if (opt$execute) {
    execute.commands(net.corr.commands)
    ## print.commands(net.corr.commands)    
} else {
    print.commands(net.corr.commands)
}
