library(crayon)

error.style <- combine_styles("bold", "red4")
warn.style  <- combine_styles("bold", "yellow")
info.style  <- combine_styles("bold", "green")

error.message <- function(...) {
    cat(error.style("*** ") %+% paste0(..., sep="") %+% "\n", sep="")
}

warn.message <- function(...) {
    cat(warn.style("*** ") %+% paste(...) %+% "\n", sep="")
}

info.message <- function(...) {
    cat(info.style("*** ") %+% paste(...) %+% "\n", sep="")
}

pigz.save <- function (..., list=character(), file=stop("'file' must be specified"), ncores=15) {    
    zipper=suppressWarnings(system("which pigz", intern=TRUE, ignore.stderr=TRUE))
    if (length(zipper) > 0) {
        ## found pigz!
        cat("*** Saving using pigz for compression\n")
        con <- pipe(paste(zipper, "-p", ncores, ">", file), "wb")
        save(..., list=list, file = con)
        on.exit(close(con))
    } else {
        cat("*** Saving using gzip for compression\n")
        save (..., list=list, file=file)
    }
}

pigz.load <- function (file, envir = parent.frame(), verbose=FALSE, ncores=15) {    
    zipper=suppressWarnings(system("which pigz", intern=TRUE, ignore.stderr=TRUE))
    if (length(zipper) > 0) {
        ## found pigz!
        if (verbose) cat("*** Loading using pigz for decompression\n")
        con <- pipe(paste(zipper, "-dc", "-p", ncores, "<", file), "rb")
        load(file = con, envir=envir, verbose=verbose)
        on.exit(close(con))
    } else {
        if (verbose) cat("*** Loading using gzip for decompression\n")
        load (file=file, envir=envir, verobse=verobse)
    }
}
