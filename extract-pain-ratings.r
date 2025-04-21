#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

library(getopt)
library(readr)
if (interactive()) {
    source("common.functions.r")
} else {
    source(file.path(dirname(get_Rscript_filename()), "common.functions.r"))
}

##########################################################################################################################################################################
### START OF FUNCTIONS ###################################################################################################################################################
##########################################################################################################################################################################

help <- function(){

}

check.command.line.arguments <- function (in.opt) {
    ## if help was asked for print a friendly message
    ## and exit with a non-zero error code
    if ( !is.null(in.opt$help) ) {
        help()
        error.message(getopt(command.line.argument.specification, usage=TRUE));
        q(status=1);
    }

    if (is.null(in.opt$logfiles)) {
        error.message("A Psychopy logfile is required")
        error.message(getopt(command.line.argument.specification, usage=TRUE));
        q(status=1);
    }
    ## else if (! file.exists(in.opt$logfile)) {
    ##     error.message("The specified Psychopy log file does not exist")
    ##     q(status=1);
    ## }

    if (is.null(in.opt$outputdir)) {
        error.message("A directory into which to save regressors and stats required")
        error.message(getopt(command.line.argument.specification, usage=TRUE));
        q(status=1);
    } else if (! dir.exists(in.opt$outputdir)) {
        error.message("The specified output directory into which to save regressors and stats does not exist")
        q(status=1);
    }

    if (is.null(in.opt$subject)) {
        error.message("A subject ID is required.")
        error.message(getopt(command.line.argument.specification, usage=TRUE));    
        q(status=1);
    }

    if (is.null(in.opt$session)) {
        error.message("A session name is required.")
        error.message(getopt(command.line.argument.specification, usage=TRUE));    
        q(status=1);
    }

     if (! grepl("sub-", in.opt$subject)) {
        warn.message("Subject name missing sub- prefix. Prepending it")
        in.opt$subject = paste("sub-", in.opt$subject, sep="")
    }

    if (! grepl("ses-", in.opt$session)) {
        warn.message("Session name missing ses- prefix. Prepending it")        
        in.opt$session = paste("ses-", in.opt$session, sep="")
    }
    
    if ( is.null(in.opt$verbose)) {
        in.opt$verbose=FALSE
    } else {
        in.opt$verbose=TRUE
    }
    
    
    return(in.opt)
}

print.command.line.arguments.summary <- function (in.opt) {
    info.message("Subject ID is:", in.opt$subject, "")
    info.message("Session ID is:", in.opt$session,  "")    
    info.message("Psychopy logfile glob pattern is:", in.opt$logfiles, "")
    info.message("Pain ratings will be placed in:", in.opt$outputdir, "")    
    if (opt$verbose) {
        info.message("Verbose messages enabled")
    }
}

get.task.from.filename <- function(in.filename) {
    task.pattern=".*/rawdata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/beh/((initialPainRating)|(kneeTapping))/.*"

    m = regexec(task.pattern, in.filename)
    if (m[[1]][1] != -1) {
        ## print("Got match!")
        ## print(m)
        ## print(regmatches(line, m))
        
        subject=regmatches(in.filename, m)[[1]][2]
        session=tolower(regmatches(in.filename, m)[[1]][3])
        task=regmatches(in.filename, m)[[1]][4]
        ## info.message(sprintf("Got subject     : %s", subject))
        ## info.message(sprintf("Got session     : %s", session))
        ## info.message(sprintf("Got task        : %s", task))

        if (task=="initialPainRating")
            task="initialpainrating"
        else if (task=="kneeTapping")
            task="kneeTapping"
        else
            task="UNKNOWN"
        
    } else {
        task="NO-MATCH"
    }

    return(task)
}

make.task.logfile.lists <- function(in.filenames) {
    task.pattern=".*/rawdata/(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/beh/((initialPainRating)|(kneeTapping))/(.*)"

    task.list=list()

    for (logfile in in.filenames) {
        m = regexec(task.pattern, logfile)
        if (m[[1]][1] != -1) {
            ## print("Got match!")
            ## print(m)
            ## print(regmatches(line, m))
            
            subject=regmatches(logfile, m)[[1]][2]
            session=tolower(regmatches(logfile, m)[[1]][3])
            task=regmatches(logfile, m)[[1]][4]
            file=regmatches(logfile, m)[[1]][1]
            ## info.message(sprintf("Got subject     : %s", subject))
            ## info.message(sprintf("Got session     : %s", session))
            ## info.message(sprintf("Got task        : %s", task))
            ## info.message(sprintf("Got file        : %s", file))            
            
            if (task=="initialPainRating")
                task="initialpainrating"
            else if (task=="kneeTapping")
                task="kneeTapping"
            else
                task="UNKNOWN"
            if (is.null(task.list[[task]]))
                task.list[[task]]=file
            else
                task.list[[task]]=c(task.list[[task]], file)
            
        } else {
            task="NO-MATCH"
        }
    }

    return(task.list)
}

process.log.file <- function(in.opt) {
    logfiles=Sys.glob(opt$logfiles)
    n.logfiles=length(logfiles)
    if (n.logfiles < 1) {
        error.message("No logfiles to process based on the glob pattern specified on the command line")
        stop()
    }

    logfiles=make.task.logfile.lists(logfiles)

    ## info.message(sprintf("Reading %d logfile%s", n.logfiles, ifelse(n.logfiles> 1, "s", "")))
    ## for (ii in seq.int(1, length(logfiles))) {
    ##     info.message(sprintf("[%02d] %s", ii, logfiles[ii]))
    ## }

    ## ##################################################
    ## Regexps to extract trial structure from log file
    ## ##################################################

    ##    103.4266        EXP     Pain rating for sub-105_ses-baseline_run-001_exp-initialpainrating is 001
    ## initial.pain.rating.pattern = "([0-9][0-9]*\\.[0-9][0-9]{3})[[:space:]]*EXP[[:space:]]*Pain rating for sub-([0-9]{3})_ses-([-z][a-z]*)_run-([0-9]{3}_exp-initialpainrating is ([0-9]{3})"

    ## 512.2662        EXP     Pain rating for sub-105_ses-baseline_run-001_exp-kneeTapping is 003
    ## 105.3026        EXP     Pain rating for sub-105_ses-Followup_run-001_exp-initialpainrating is 001
    ## pain.rating.pattern = "([0-9][0-9]*\\.[0-9][0-9]{3})[[:space:]]*EXP[[:space:]]*Pain[[:space:]]*rating[[:space:]]*for[[:space:]]*sub-([0-9]{3})_ses-([A-Za-z][A-Za-z]*)_run-([0-9]{3})_exp-((initialpainrating)|(kneeTapping))[[:space:]]*is[[:space:]]*([0-9]{3})"
    pain.rating.pattern = "([0-9][0-9]*\\.[0-9][0-9]{3})[[:space:]]*EXP[[:space:]]*Pain[[:space:]]*rating[[:space:]]*for[[:space:]]*sub-([0-9]{3,5})(_[01])?_ses-([A-Za-z0-9][ A-Za-z0-9]*)_run-([0-9]{3})_exp-((initialpainrating)|(kneeTapping))[[:space:]]*is[[:space:]]*([0-9]{3})"    
    ## ################################################################################
    ## Lists to keep track of event start, end, and duration times
    ## ################################################################################    

    regressor.list=list()

    for (task in names(logfiles)) {
        for (run in seq.int(1, length(logfiles[[task]]))) {

            regressor.key=paste0(task, run)
            lf=logfiles[[task]][run]
            info.message(sprintf("[%02d] Reading %s logfile %s", run, task, lf))
            
            logfile.con = file(lf, 'r')
            ## on.exit(close(logfile.con))
            if (is.null(logfile.con)) {
                error.message("Failed to successfully open logfile. Quiting")
                stop()
            }

            regressor.list[[regressor.key]] = list()
            
            prior.log.line=""
            line.count=1
            while (length(line <- readLines(logfile.con, n = 1, warn = FALSE)) > 0) {
                if (opt$verbose) {
                    info.message(sprintf("[%03d] %s", line.count, line))
                }
                line.count=line.count + 1
                if (line == prior.log.line) {
                    if (opt$verbose) {
                        info.message("Current log line is the same as the prior line. Skipping it")
                    }
                    prior.log.line=line
                    next
                }
                prior.log.line=line
                ## print(line)

                m = regexec(pain.rating.pattern, line)
                if (m[[1]][1] != -1) {
                    ## print("Got match!")
                    ## print(m)
                    ## print(regmatches(line, m))

                    subject=regmatches(line, m)[[1]][3]
                    session=tolower(regmatches(line, m)[[1]][5])
                    lf.run=regmatches(line, m)[[1]][6]
                    task=regmatches(line, m)[[1]][7]
                    rating=regmatches(line, m)[[1]][10]
                    info.message(sprintf("[%02d] Got subject     : %s", run, subject))
                    info.message(sprintf("[%02d] Got session     : %s", run, session))
                    info.message(sprintf("[%02d] Got run         : %s", run, lf.run))                
                    info.message(sprintf("[%02d] Got task        : %s", run, task))
                    info.message(sprintf("[%02d] Got pain rating : %s", run, rating))

                    regressor.list[[regressor.key]] =
                        c("subject"=subject,
                          "session"=gsub("ses-", "",
                                         ifelse(session != in.opt$session, in.opt$session, session),
                                         fixed=TRUE),
                          "task"=task,
                          "run"=lf.run,
                          "pain_rating"=rating,
                          "file.size"=file.size(lf),
                          "file"=R.utils::getAbsolutePath(lf))
                    next
                }
            } ## end of while (length(line <- readLines(logfile.con, n = 1, warn = FALSE)) > 0)
            
            close(logfile.con)

            ## print("regressor.list")
            ## print(regressor.list)
            ## print("length(regressor.list[[regressor.key]])")
            ## print(length(regressor.list[[regressor.key]]))            
            if (length(regressor.list[[regressor.key]]) < 1) {

                ## now let's try and see if ther is a CSV file for the log
                ## file. The pain rating might be in that.
                csv.logfile=gsub("log$", "csv", lf)
                info.message(sprintf("[%02d] Found no pain rating in the log file. Trying the CSV file", run))
                info.message(sprintf("[%02d] CSV file: %s", run, csv.logfile))
                if (file.exists(csv.logfile)) {
                    csv.df=suppressMessages(read_csv(csv.logfile))
                    ## print(colnames(csv.df))
                    ## print("PainRating" %in% colnames(csv.df))
                    if ("PainRating" %in% colnames(csv.df)) {
                        rating=csv.df$PainRating
                    } else {
                        info.message(sprintf("[%02d] Found no pain rating in the CSV file", run))
                        rating=c(NA)
                    }
                    
                    ## print(rating)
                    rating=rating[!is.na(rating)]
                    ## print(rating)                
                    if (length(rating) >= 1L ) {
                        info.message(sprintf("[%02d] Got rating from CSV file", run))
                        info.message(sprintf("[%02d] Got subject     : %s", run, opt$subject))                    
                        info.message(sprintf("[%02d] Got session     : %s", run, opt$session))
                        info.message(sprintf("[%02d] Got run         : %s", run, run))                
                        info.message(sprintf("[%02d] Got task        : %s", run, get.task.from.filename(lf)))
                        info.message(sprintf("[%02d] Got pain rating : %s", run, rating))
                        
                        regressor.list[[regressor.key]]=c(
                            "subject"=gsub("sub-", "", opt$subject, fixed=TRUE),
                            "session"=gsub("ses-", "", opt$session, fixed=TRUE),
                            "task"=get.task.from.filename(lf),
                            "run"=run,
                            "pain_rating"=rating,
                            "file.size"=file.size(csv.logfile),
                            "file"=R.utils::getAbsolutePath(csv.logfile))
                    }
                    else {
                        info.message(sprintf("[%02d] Got no rating from CSV file", run))
                        regressor.list[[regressor.key]]=c(
                            "subject"=gsub("sub-", "", opt$subject, fixed=TRUE),
                            "session"=gsub("ses-", "", opt$session, fixed=TRUE),
                            "task"=get.task.from.filename(lf),
                            "run"=run,
                            "pain_rating"=NA,
                            "file.size"=file.size(csv.logfile),
                            "file"=R.utils::getAbsolutePath(csv.logfile))
                    }
                } else {
                    info.message(sprintf("[%02d] CSV file does not exist", run))
                    regressor.list[[regressor.key]]=c(
                        "subject"=gsub("sub-", "", opt$subject, fixed=TRUE),
                        "session"=gsub("ses-", "", opt$session, fixed=TRUE),
                        "task"=get.task.from.filename(lf),
                        "run"=run,
                        "pain_rating"=NA,
                        "file.size"=NA,
                        "file"=NA)
                }
            } ## end of if (length(regressor.list[[regressor.key]]) < 1)
            
        } ## end of  for (run in seq.int(1, length(logfiles[[task]]))) {
    }  ## end of for (task in names(logfiles)) {
    ## print("All regressor.list")
    ## print(regressor.list)    
    
    regressor.df = as.matrix(do.call(rbind, regressor.list))

    if (opt$verbose) {
        info.message("Contents of the raw regressor data frame")
        info.message(capture.output(print(regressor.df)))
    }
    ## print(regressor.df)
    return(regressor.df)
}

##########################################################################################################################################################################
### END OF FUNCTIONS #####################################################################################################################################################
##########################################################################################################################################################################


################################################################################
NO_ARGUMENT="0"
REQUIRED_ARGUMENT="1"
OPTIONAL_ARGUMENT="2"

## process command line arguments
command.line.argument.specification = matrix(c(
    'logfiles',		'l',	REQUIRED_ARGUMENT,	'character',
    'outputdir',	'o',	REQUIRED_ARGUMENT,	'character',
    'subject',		's',	REQUIRED_ARGUMENT,	'character',
    'session',		'e',	REQUIRED_ARGUMENT,	'character',        
    'help',		'h',	NO_ARGUMENT,		'logical',    
    'verbose',		'v',	NO_ARGUMENT,		'logical'    
), byrow=TRUE, ncol=4)

## ######################################################
## Names of blocks for which  we will generate regressors
## ######################################################    

if (interactive()) {
    info.message("Setting interactive options")

    args=c("-l", "../rawdata/sub-10189/ses-baseline/beh/*/*.log",
           "-o", "../derivatives/r-pain-ratings",
           "-s", "10189",
           "-e", "baseline")#,
           #"-v")
    
    ##the name of this script
    script.name=parent.frame(2)$ofile
    ## the location (absolute path) to this script
    script.location=normalizePath(dirname(parent.frame(2)$ofile))
    
    command.line=paste(file.path(script.location, script.name), paste(args, collapse=" "))
    opt = getopt(command.line.argument.specification, opt=args)
} else {
    args.start.at=grep("--args", commandArgs(), fixed=TRUE)
    if (length(args.start.at) == 0){
        error.message("This program requires command line arguments. Cannot continue. Exiting.")
        error.message(getopt(command.line.argument.specification, usage=TRUE));
        q(status=1);
    }
    command.line=paste(get_Rscript_filename(), paste(commandArgs()[-c(1:args.start.at)], collapse=" "))
    opt = getopt(command.line.argument.specification)
}

opt=check.command.line.arguments(opt)
print.command.line.arguments.summary(opt)

## now process the log file
regressor.df=process.log.file(opt)
## make.event.regressors(regressor.list)

pain.ratings.filename=file.path(opt$outputdir, "pain_ratings.csv")
info.message("Writing pain ratings to", pain.ratings.filename)
write.table(regressor.df, file=pain.ratings.filename, append=TRUE,
            quote=FALSE, sep=",", row.names=FALSE,
            col.names= ! isTRUE(file.exists(pain.ratings.filename)))
