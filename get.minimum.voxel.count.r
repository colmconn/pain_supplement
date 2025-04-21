#!/usr/bin/Rscript

rm(list=ls())
graphics.off()

options(error=traceback)

if (! require (getopt, quietly=TRUE) ) {
    stop("Could not load the required 'getopt' package. Install it and try running this program again\n")
}

## number of significant decimal places to which the pthr, and alpha
## values supplied on the command line are standardized.
## 
## This standardization procedure applies to the values for pthr and
## alpha read from the command line and those read from the 3dClustSim
## table and used as the row and column names of the cluster volume
## table
n.digits=6

##########################################################################################################################################################################
### START OF FUNCTIONS ###################################################################################################################################################
##########################################################################################################################################################################

## sink.reset <- function(){
##     for(i in seq_len(sink.number())){
##         sink(NULL)
##     }
## }

## sink.reset()


help <- function () {
    cat(" NAME get.minimum.voxel.count.r - Get the minimum number of voxels to correct for mulitple comparisions across space according to 3dClustSim.
SYNOPSIS
        get.minimum.voxel.count.r [OPTIONS]

DESCRIPTION
	Reads 1D files created by an by an invocation of 3dClustSim
	and prints the minimum number of voxels required to correct for
	mulitple comparisions across space.

	With the exception of the -h , --help, -d, and --debug arguments,
	all other arguments are mandatory.

	-h, --help
		Print this help text and exit.

	-c, --csimfile
		The filename containing the output (in 1D format)
		from 3dClustSim to be read in.
	    
	-d, --debug
		Print verbose debugging output.
	    
	-n, --nn
		The neighborhood size. Must be one of 1, 2, or 3.

	-s, --side
		The sidedness of the tests. Must be one of 1, 2, or
	    	bi.

	-p, --pthr
		The voxelwise p value for which you want a
	    	corrected alpha value.

	-a, --alpha
		The corrected alpha value, at the cluster level, which you want.

EXAMPLE
	To get the minimum number of voxels required to correct a
	family of tests thresholded at a voxelwise p of 0.01 at a
	corrected alpha of 0.05 using files created by 3dClustSim that
	are stored in current directory and the begin with the prefix CC,
	the following command can be used:

	    get.minimum.voxel.count.r -n 1 -s 1 -p 0.01 -a 0.05 -c ./cc.NN1_1sided.1D 

AUTHOR
       Written by Colm G. Connolly

REPORTING BUGS
       Report bugs to <>

COPYRIGHT
       Copyright © 2025 Colm G. Connolly.  License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
       This is free software: you are free to change and redistribute it.  There is NO WARRANTY, to the extent permitted by law.
", file=stdout())
}


standardize.number.format <- function (numbers) {
    sapply(numbers, function(xx) {
        sprintf(sprintf("%%0.%df", n.digits), as.numeric(xx))
    })

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

    if ( is.null(opt$debug) ) {
        opt$debug=FALSE
    }

    if (opt$debug)
        cat("*** Debug output enabled\n", file=stderr())

    if (is.null(opt$nn)) {
        cat("A neighborhood size (1, 2, or 3) is required.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    }

    if ( ! opt$nn %in% c(1, 2, 3)) {
        cat("The value specified for neighborhood size be one of the numbers 1, 2, or 3.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    }

    if (is.null(opt$side)) {
        cat("A value for side is required. It must be one of 1, 2, or bi.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    }

    if ( ! opt$side %in% c("1", "2", "bi")) {
        cat("The value specified for side size be one of 1, 2, or bi.\n", file=stderr())
        cat(getopt(spec, usage=TRUE), file=stderr())    
        q(status=1)
    }

    if (is.null(opt$pthr)) {
        cat("A value for the voxelwise p value (pthr) is required.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    } else {
        ## standardize the format of the pthr
        opt$pthr=standardize.number.format(opt$pthr)
    }

    if (is.null(opt$alpha)) {
        cat("A value for the cluster-wise p value (alpha) is required.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    } else {
        ## standardize the format of the alpha
        opt$alpha=standardize.number.format(opt$alpha)
    }

    if (is.null(opt$csimfile)) {
        cat("The file produced by 3dClustSim is required.\n", file=stderr())
        cat(getopt(command.line.options.specification, usage=TRUE), file=stderr())    
        q(status=1)
    }

    if (isTRUE(grepl(".niml", opt$csimfile, fixed=TRUE))) {
        if (opt$debug)
            cat("*** 3dClustSim file is a NIML file\n")
        opt$is.niml=TRUE
    } else if (isTRUE(grepl(".1D", opt$csimfile, fixed=TRUE))) {
        if (opt$debug)
            cat("*** 3dClustSim file is a 1D file\n")
        opt$is.niml=FALSE
    } else {
        cat("Unknown 3dClustSim filename format\n")
        q(status=1)
    }
     
    return(opt)
}

print.options.summary <- function () {
    cat("*** Script name:", get_Rscript_filename(), "\n", file=stderr())
    cat("*** 3dClustSim output file:", opt$csimfile, "\n", file=stderr())
    cat("*** Neighbor hood size is set to:", opt$nn, "\n", file=stderr())
    cat("*** Side is set to:", opt$side, "\n", file=stderr())
    cat("*** Voxelwise p value (pthr) is set to:", opt$pthr, "\n", file=stderr())
    cat("*** Clusterwise p value (alpha) is set to:", opt$alpha, "\n", file=stderr())
}

read.1d.clustsim.file <- function(clustsim.file) {
    if (file.exists(clustsim.file)) {
        if(opt$debug)
            cat("*** Trying to read", clustsim.file, "\n", file=stderr())

        cstim.conn = file(clustsim.file, "r")

        header.lines=readLines(con=cstim.conn, n=8)
        
        if (opt$debug) {

            ## line 1: the 3dClustSim command line
            cat("*** The following 3dClustSim command was read from the 3dClustSim file on the command line: ", file=stderr())
            cat(header.lines[1], file=stderr())
            cat("\n", file=stderr())
            
            ## line 2: thresholding (1-, 2-, bi-sided)
            cat("*** Line 2:", header.lines[2], "\n", file=stderr())
            pattern="(#[[:space:]]*)(1|2|bi)(-sided[[:space:]]threshold)"
            m = regexec(pattern, header.lines[2])
            if (m[[1]][1] == -1) {
                ## no match: this should not happen in a properly formatted 3dClustSim file
                stop("The second line of the file output from 3dClustSim did not contain appropriate information about the sided-ness (1-, 2-, bi-sided) of the statistical tests.\n")
            } else {
                side = regmatches(header.lines[2], m)[[1]][3]
                if (side != opt$side) {
                    stop(sprintf("*** The second line of the file output from 3dClustSim file indicated that the tests were %s-sided but you asked for %s-sided tests on the command line.\n", side, opt$side))
                } else {
                    cat("*** Sidedness in 3dClustSim file (", side, ") matches sidedness requested on command line (", opt$side, ")\n", file=stderr())
                }
            }
            
            ## line 3: Grid information. E.g., # Grid: 50x63x51 3.00x3.00x3.00 mm^3 (60527 voxels in mask)
            cat("*** Line 3:", header.lines[3], "\n", file=stderr())
            pattern="(#[[:space:]]*)Grid:[[:space:]]*([0-9]+)x([0-9]+)x([0-9]+)[[:space:]]*([.0-9]*)x([.0-9]*)x([.0-9]*)[[:space:]]*mm\\^3[[:space:]]*\\(([0-9]+)[[:space:]]*voxels[[:space:]]*in[[:space:]]*mask\\)"
            m = regexec(pattern, header.lines[3])
            if (m[[1]][1] == -1) {
                ## no match: this should not happen in a properly formatted 3dClustSim file
                stop("The second line of the file output from 3dClustSim did not contain appropriate grid information\n")
            } else {
                cat("*** 3dClustSim was run on a grid of",
                    paste(regmatches(header.lines[3], m)[[1]][c(3, 4, 5)], collapse="x"),
                    "voxels each of",
                    paste(regmatches(header.lines[3], m)[[1]][c(6, 7, 8)], collapse="x"),
                    "mm^3 volume. There are",
                    regmatches(header.lines[3], m)[[1]][9],
                    "voxels were in the mask.\n", file=stderr())
            }

            ## line 4: empty
            cat("*** Line 4:", header.lines[4], "\n", file=stderr())
            
            ## line 5: CLUSTER SIZE THRESHOLD
            cat("*** Line 5:", header.lines[5], "\n", file=stderr())
            
            ## line 6: # -NN 1  | alpha = Prob(Cluster >= given size)
            cat("*** Line 6:", header.lines[6], "\n", file=stderr())
            pattern="(#[[:space:]]*)-NN[[:space:]]*([123]).*"
            m = regexec(pattern, header.lines[6])
            if (m[[1]][1] == -1) {
                ## no match: this should not happen in a properly formatted 3dClustSim file
                stop("The sixth line of the file output from 3dClustSim did not contain appropriate information about the neighborhood size (1, 2, 3).\n")
            } else {
                nn = regmatches(header.lines[6], m)[[1]][3]
                if (nn != opt$nn) {
                    stop(sprintf("*** The sixth line of the file output from 3dClustSim file indicated that the neighborhood size was %s but you asked for %s on the command line.\n", nn, opt$nn))
                } else {
                    cat("*** Neighborhood size in 3dClustSim file (", nn, ") matches neighborhood size requested on command line (", opt$nn, ")\n", file=stderr())
                }
            }
        } ## end of if (opt$debug) {

        trim <- function (x) gsub("^\\s+|\\s+$", "", x)

        ## line 7:#  pthr  | .10000 .09000 .08000 .07000 .06000 .05000 .04000 .03000 .02000 .01000 <-- list of thresholds can be specified on 3dClustSim command line
        if (opt$debug)
            cat("*** Line 7:", header.lines[7], "\n", file=stderr())
        ## for some reason this regexp captures the last group of digits twice. the reason for this eludes me
        pattern="#[[:space:]]*(pthr)[[:space:]]*\\|(([[:space:]]*[.0-9]+)+)"
        m = regexec(pattern, header.lines[7])
        if (m[[1]][1] == -1) {
            ## no match: this should not happen in a properly formatted 3dClustSim file
            stop("The seventh line of the file output from 3dClustSim did not contain an appropriate list of corrected alpha values\n")
        } else {
            table.header=c(regmatches(header.lines[7], m)[[1]][2],
                           standardize.number.format(strsplit(trim(regmatches(header.lines[7], m)[[1]][3]), " ")[[1]]))
            if (opt$debug)
                cat("*** Table header is:", table.header, "\n", file=stderr())
        }
            
        ## line 8: header comment
        if (opt$debug)
            cat("*** Line 8:", header.lines[8], "\n", file=stderr())
        
        ## remainder of file is the table
        table=read.table(file=cstim.conn, header=FALSE, colClasses = "character")
        ## set the table column names to be the list of corrected alpha values
        colnames(table) = table.header
        ## set the table row names to be the list of voxel-wise p values
        rownames(table) = standardize.number.format(table[, 1])
        ## now delete the first column as we no longer need it
        table[, 1] = NULL
        if (opt$debug) {
            cat("*** The final 3dClustSim table is as follows:\n", file=stderr())
            capture.output(print(table), file=stderr())
        }
        
        close(cstim.conn)
    } else {
        stop("*** No such file", clustsim.file, "\n")
    }

    return (table)
}


read.niml.clustsim.file <- function(clustsim.file) {
    if (file.exists(clustsim.file)) {
        if(opt$debug)
            cat("*** Trying to read", clustsim.file, "\n", file=stderr())
        
        cstim.conn = file(clustsim.file, "r")
        
        header.lines=readLines(con=cstim.conn, n=14)
        
        ## header processing stuff goes here
        pattern="<3dClustSim_NN([0-9])"
        m = regexec(pattern, header.lines[1])
        if (m[[1]][1] == -1) {
            ## no match: this should not happen in a properly formatted 3dClustSim file
            stop("The first line of the file output from 3dClustSim did not contain appropriate information about the neighborhood size.\n")
        } else {
            nn = regmatches(header.lines[1], m)[[1]][2]
            if (nn != opt$nn) {
                stop(sprintf("*** The first line of the file output from 3dClustSim file indicated that the neighborhood was %s but you asked for %s on the command line.\n", nn, opt$nn))
            } else {
                if (opt$debug)
                    cat("*** Neighborhood size in 3dClustSim file (", nn, ") matches size requested on command line (", opt$nn, ")\n", file=stderr())
            }
        }

        key.value.pattern='[[:space:]]*([_a-zA-Z0-9]*)="(.*)"[[:space:]]*'
        ## the first 14 lines of a NIML file are the header contained
        ## within <...> brackets, hence the hard coded 14 below. Lines
        ## 2 to 14 inclusive are in key=value format
        for (line.count in seq.int(2, 14, by=1)) {
            m = regexec(key.value.pattern, header.lines[line.count])
            if (m[[1]][1] == -1) {
                cat(header.lines[line.count], "\n")
                ## no match: this should not happen in a properly formatted 3dClustSim file
                stop("Line", line.count, "of the file output from 3dClustSim did not match the key.value.pattern\n")
            } else {
                if (opt$debug)
                    cat("KEY:", regmatches(header.lines[line.count], m)[[1]][2],
                        "= VALUE:", regmatches(header.lines[line.count], m)[[1]][3], "\n")
                if (regmatches(header.lines[line.count], m)[[1]][2] == "pthr" ) {
                    table.row.names=standardize.number.format(
                        unlist(
                            strsplit(
                                regmatches(header.lines[line.count], m)[[1]][3], ",", fixed=TRUE)))
                }
                if (regmatches(header.lines[line.count], m)[[1]][2] == "athr" ) {
                    table.header=standardize.number.format(
                        unlist(
                            strsplit(
                                regmatches(header.lines[line.count], m)[[1]][3], ",", fixed=TRUE)))
                }
            }
        } ## end of for (line.count in seq.int(2, 14, by=1)) {
        
        ## remainder of file is the table
        table=read.table(file=cstim.conn, header=FALSE, colClasses = "character", comment.char = "<")
        ## set the table column names to be the list of corrected alpha values
        colnames(table) = table.header
        ## set the table row names to be the list of voxel-wise p values
        rownames(table) = table.row.names
        if (opt$debug) {
            cat("*** The final 3dClustSim table is as follows:\n", file=stderr())
            capture.output(print(table), file=stderr())
        }
        
        close(cstim.conn)
    
    } else {
        stop("*** No such file", clustsim.file, "\n")
    }

    return (table)
}

get.number.of.voxels <- function () {

    row=pmatch(opt$pthr,  rownames(cluster.volume.table))
    col=pmatch(opt$alpha, colnames(cluster.volume.table)) 

    if (opt$debug) {
        cat("*** pthr of", opt$pthr, "and alpha of", opt$alpha,
            "maps to row", row, "column", col,
            "in the cluster volume table\n", file=stderr())
    }
    
    return(cluster.volume.table[row, col])
}

##########################################################################################################################################################################
### END OF FUNCTIONS #####################################################################################################################################################
##########################################################################################################################################################################

NO_ARGUMENT="0"
REQUIRED_ARGUMENT="1"
OPTIONAL_ARGUMENT="2"

## Setup the command line arguments
command.line.options.specification = matrix(c(
    'help',             'h', NO_ARGUMENT,       "logical",
    'debug',            'd', NO_ARGUMENT,       "logical",    
    'nn',          	'n', REQUIRED_ARGUMENT, "integer",
    'side', 	        's', REQUIRED_ARGUMENT, "character",
    "pthr",             'p', REQUIRED_ARGUMENT, "double",
    "alpha",            'a', REQUIRED_ARGUMENT, "double",
    "csimfile",		'c', REQUIRED_ARGUMENT, "character"  
), byrow=TRUE, ncol=4)

## set some command line args if in an interactive session and then
## process them. If not interactive read command line args from
## command line and process them
if (interactive()) {
    ## these are default arguments that are useful for testing
    ## purposes.
    args=c(
        "-d",
        "-n", "1",
        "-s", "bi",
        "-p", "0.01",
        "-a", "0.05",
        "-c", "../derivatives/afni-task-tapping-lmes-mt0.35-ex0.30/CStemp.NN1_bisided.niml")
    opt=process.command.line.options(args)
} else {
    opt=process.command.line.options()
}

## check that the command line arguments have appropriate values
opt=check.command.line.arguments(opt)
if (opt$debug)
    print.options.summary()

## csim.file.name=make.clustsim.file.name()
if (opt$is.niml) {
    cluster.volume.table=read.niml.clustsim.file(opt$csimfile)    
} else {
    cluster.volume.table=read.1d.clustsim.file(opt$csimfile)
}
min.cluster.size=get.number.of.voxels()

cat(min.cluster.size, "\n")
