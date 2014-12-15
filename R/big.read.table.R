#' @title Read in chunks from a large file with row/column filtering
#' to obtain a reasonable-sized data.frame.
#' @param file the name of the file, obviously
#' @param nrows the chunk size; consider reducing this if there are
#' lots of columns
#' @param sep by default we expect a CSV file
#' @param header is \code{TRUE} by default
#' @param row.names I really dislike row names
#' @param cols for filtering column by name or number (supporting negative indexing)
#' @param rowfilter a function that is assumed to take a chunk as a
#' data frame and return a smaller data frame (with fewer rows), separately
#' from the column filtering.
#' @param as.is \code{TRUE} by default
#' @param estimate do a preliminary estimation of the work to be done,
#' and then have a chance to bail out if it looks like a bad idea
#' @param location where do you want it?
#' @examples
#' data(CO2)
#' write.csv(CO2, "CO2.csv", row.names=FALSE)
#' x <- big.read.table("CO2.csv", nrows=10)
#' dim(x)
#' y <- big.read.table("CO2.csv", nrow=10,
#' rowfilter=function(a) a[a$conc!=1000,])
#' dim(y)
#' head(x)
#' @export
big.read.table <- function(file, nrows=100000, sep=",",
                          header=TRUE, row.names=NULL,
                          cols=NULL, rowfilter=NULL,
                          as.is=TRUE, estimate=FALSE,
                          location=NULL) { 
  # Modified by Baobao Zhang
  ###  What is cn? is it the column name?  Do you mind if I change it to col.name?
  if (!header) {
    cl <- length(read.table(file, sep=sep, nrows=1, header=FALSE))
    cn <- paste0("V",c(1:cl))
  } else {
    cn <- read.table(file, sep=sep, nrows=1)
  } # End of modification
  
  if (estimate) {
      warning("Estimate doesn't use rowfilter()")
      nlines <- getnrows(file)
      x <- read.table(file, sep=sep, row.names=row.names,
                      nrows=min(nlines, 1000), header=header)
      
      if (!is.null(cols)) x <- x[,cols,drop=FALSE]
      cat("Estimated read size without row filtering:",
          floor(object.size(x)*nlines/nrow(x)/1e6), "MB\n")
      
      # Checking whether the read size is acceptable and should continue:
      if (interactive()) {
        ANSWER <- readline("Continue with read (Y/n)? ")
        if (substring(ANSWER, 1, 1) != "Y") {
          warning("Terminated read.")
          return(NULL)
        }
      }
    }
  
    # change it so it's doing if/else (2 things)
  ###  I can't quite remember what the problem with this was
    if (is.null(rowfilter) & header) nlines <- getnrows(file)-1
    else if (is.null(rowfilter) & !header) nlines <- getnrows(file)
    else {
      myiter <- iread.table(file, header=header,
                            row.names=row.names, sep=sep, 
                            nrows=nrows, as.is=as.is)
      ### don't need foreach here
      nlines <- foreach(x=myiter, .combine=sum) %do%
      return(nrow(rowfilter(x)))
    }
  
    # Modification by Baobao Zhang
    print(paste0("Number of Rows: ", nlines))
    myiter <- iread.table(file, header=header,
                          row.names=row.names, sep=sep,
                          nrows=nrows, as.is=as.is)
    x <- nextElem(myiter)
  
  ### Accounting for edge cases???
    if (!header) names(x) <- cn
    if (!is.null(rowfilter)) x <- rowfilter(x)
    if (!is.null(cols)) x <- x[, cols, drop=FALSE]

    # Fixing the class types:
    theclasses <- sapply(x, class)
    theclasses[theclasses=="numeric"] <- "double"
    ans <- big.data.frame(nlines, location=location,
                          classes=theclasses, names=names(x))
  
    # for loop to input the data into big.data.frame by column
  ### don't need foreach here
    foreach(k=1:ncol(x), .combine = 'cbind') %do% {
      ans[, k] <- x[, k]
    } # End of Modification
  
  nextline <- nrow(x)+1
  ### probably don't need foreach here
  foo <- foreach(x=myiter, .combine=rbind) %do% {
    if (!is.null(rowfilter)) x <- rowfilter(x)
    if (!is.null(cols)) x <- x[, cols, drop=FALSE]
    gc() # do we need this?
    
    # Modification by Baobao Zhang
    rowindex <- as.integer(nextline:(nextline+nrow(x)-1))
    foreach(k=1:ncol(x), .combine = 'cbind') %do% {
      ans[rowindex,k] <- x[,k]
    } # End of Modification
    
    nextline <- as.integer(nextline+nrow(x))
    return(nrow(x))
  }
  return(ans)
}
