#!/usr/bin/Rscript

if (! require (rjson, quietly=TRUE) ) {
    stop("Could not load the required 'rjson' package. Install it and try running this program again\n")
}

if (! require (getopt, quietly=TRUE) ) {
    stop("Could not load the required 'getopt' package. Install it and try running this program again\n")
}

help <- function () {
    cat("")
}

process.command.line.options <- function(args=commandArgs(TRUE)) {
    if (length(args) == 0 || inherits(try(opt <- getopt(command.line.options.specification, opt=args)), "try-error")) {
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())
        q(status=1)
    }
    
    return(opt)
}

check.command.line.arguments <- function (opt) {
    ## if help was asked for print a friendly message
    ## and exit with a non-zero error code
    if ( !is.null(opt$help) ) {
        help()
        q(status=1)
    }

    if (is.null(opt$key)) {
        cat("A key name is required.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    }

    if (is.null(opt$json)) {
        cat("The JSON file name is required.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    }
}

print.options.summary <- function() {
    for (kk in names(opt) ) {
        cat(sprintf("%-5s -> %-10s\n", kk, as.character(opt[[kk]])))
    }
}

NO_ARGUMENT="0"
REQUIRED_ARGUMENT="1"
OPTIONAL_ARGUMENT="2"

## Setup the command line arguments
command.line.options.specification = matrix(c(
    'help',             'h', NO_ARGUMENT,       "logical",
    'key',  	        'k', REQUIRED_ARGUMENT, "character",
    "json",		'j', REQUIRED_ARGUMENT, "character"
), byrow=TRUE, ncol=4)

if (interactive()) {
    ## these are default arguments that are useful for testing
    ## purposes.
    args=c(
        "-k", "SliceTiming",
        "-j", "/data/colmconn/turmeric/derivative/ses-baseline/sub-002/unprocessed/func/sub-002_ses-baseline_task-rest_dir-ap_run-01_epi.json")
    opt=process.command.line.options(args)
    print.options.summary()
} else {
    opt=process.command.line.options()
}

if (file.exists(opt$json)) {
    json=fromJSON(file=opt$json)

    if (opt$key %in% names(json)) {
        cat(json[[opt$key]], file=stdout())
    } else {
        cat("No such key in JSON file:", opt$key, "\n", file=stderr())
        q(status=1)
    }
} else {
    cat("No such file:", opt$json, "\n", file=stderr())
    q(status=1)
}
