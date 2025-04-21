## library(crayon)

## error.style <- combine_styles("bold", "red4")
## warn.style  <- combine_styles("bold", "yellow")
## info.style  <- combine_styles("bold", "green")

## error.message <- function(...) {
##     cat(error.style("*** ") %+% paste0(..., sep="") %+% "\n", sep="")
## }

## warn.message <- function(...) {
##     cat(warn.style("*** ") %+% paste(...) %+% "\n", sep="")
## }

## info.message <- function(...) {
##     cat(info.style("*** ") %+% paste(...) %+% "\n", sep="")
## }

library(cli)

error.message <- function(...) {
    cli_alert_danger(paste(..., collapse=" "))
}

warn.message <- function(...) {
    cli_alert_warning(paste(..., collapse=" "))
}

info.message <- function(...) {
    cli_alert_info(paste(..., collapse=" "))
}


pigz.save <- function (..., list=character(), file=stop("'file' must be specified"), verbose=FALSE, ncores=15) {    
    zipper=suppressWarnings(system("which pigz", intern=TRUE, ignore.stderr=TRUE))
    if (length(zipper) > 0) {
        ## found pigz!
        if (verbose)
            info.message("Saving using pigz for compression\n")
        con <- pipe(paste(zipper, "-p", ncores, ">", file), "wb")
        save(..., list=list, file = con)
        on.exit(close(con))
    } else {
        if (verbose)
            info.message("Saving using gzip for compression\n")
        save (..., list=list, file=file)
    }
}

pigz.load <- function (file, envir = parent.frame(), verbose=FALSE, ncores=15) {
    zipper=suppressWarnings(system("which pigz", intern=TRUE, ignore.stderr=TRUE))
    if (length(zipper) > 0) {
        ## found pigz!
        if (verbose)
            info.message("Loading using pigz for decompression\n")
        con <- pipe(paste(zipper, "-dc", "-p", ncores, "<", file), "rb")
        load(file = con, envir=envir, verbose=verbose)
        on.exit(close(con))
    } else {
        if (verbose)
            info.message("Loading using gzip for decompression\n")
        load (file=file, envir=envir, verobse=verobse)
    }
}


get.entities.from.directory <- function(ff, prefix="/sourcedata/", return.ff=FALSE, ff.name="ff") {

    mri.pattern=paste0(".*", prefix, "(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)?/?.*")

    m=regexec(mri.pattern, ff)
    matches=regmatches(ff, m)[[1]]
    ll=length(matches)
    
    ## this assumes that there are sessions within subject and
    ## that there is at least one BIDS entity to be matched by the
    ## entities group in the mri.pattern regex. The regex is
    ## coded such that subject directories are mandatory and
    ## session entities are optional
    if (isTRUE(return.ff)) {
        rr=c(matches[2],
             matches[3],
             ff
             )
        names(rr)=c("subject", "session", ff.name)
    } else {
        rr=c("subject"=matches[2],
             "session"=matches[3]
             )
    }
    rr
}


get.entities.from.filename <- function(ff, prefix="/sourcedata/") {

    mri.pattern=paste0(".*", prefix, "(sub-[[:alnum:]]+)/(ses-[[:alnum:]]+)/?([[:alnum:]]+)/(sub-[[:alnum:]]+)_(.*)_([[:alnum:]]+)\\.([.A-Za-z0-9]+)")

    m=regexec(mri.pattern, ff)
    matches=regmatches(ff, m)[[1]]
    ll=length(matches)
    
    ## this assumes that there are sessions within subject and
    ## that there is at least one BIDS entity to be matched by the
    ## entities group in the mri.pattern regex. The regex is
    ## coded such that subject directories are mandatory and
    ## session entities are optional
    rr=c("subject"=matches[2],
         "session"=matches[3],
         "data.type"=matches[4],
         "entities"=matches[ll-2],
         ## the following recursively splits into key-value pairs
         ## based on the _ separator, then further splits based on
         ## the - separator, names the value with it's key name
         ## and returns the named values all while unlisting to
         ## ultimately yield a vector of values named with their
         ## keys
         unlist(
             lapply(
                 unlist(str_split(matches[ll-2], "_")),
                 function(xx) {
                     pp=unlist(str_split(xx, "-"));
                     rr=pp[[2]]; names(rr)=pp[[1]];
                     rr
                 })),
         "suffix"=matches[ll-1],
         "ext"=matches[ll],
         "filename" = matches[1]
         )
    rr
}

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
