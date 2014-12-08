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
  if (!header){
       cl <- length(read.table(file,sep=sep,nrows=1,header=F))
      cn <- paste0("V",c(1:cl))
  } else {
    cn <- read.table(file,sep=sep,nrows=1)
  }
  #print(cn)
  if (estimate) {
      warning("Estimate doesn't use rowfilter()")
      nlines <- getnrows(file)
      x <- read.table(file, sep=sep, row.names=row.names,
      nrows=min(nlines, 1000), header=header)
      
      if (!is.null(cols)) x <- x[,cols,drop=FALSE]
      cat("Estimated read size without row filtering:",
      floor(object.size(x)*nlines/nrow(x)/1e6), "MB\n")
      
      if (interactive()) {
        ANSWER <- readline("Continue with read (Y/n)? ")
        if (substring(ANSWER, 1, 1) != "Y") {
          warning("Terminated read.")
          return(NULL)
        }
      }
    }
  
    if (is.null(rowfilter) & header) nlines <- getnrows(file)-1
  
    else if (is.null(rowfilter) & !header) nlines <- getnrows(file)
  
    else {
      myiter <- iread.table(file, header=header,
      row.names=row.names, sep=sep,
      nrows=nrows, as.is=as.is)
      nlines <- foreach(x=myiter, .combine=sum) %do%
      return( nrow(rowfilter(x)) )
    }
    print(paste0("Number of Lines: :",nlines))
    myiter <- iread.table(file, header=header,
      row.names=row.names, sep=sep,
      nrows=nrows, as.is=as.is)
    x <- nextElem(myiter)
    if (!header) {names(x) <- cn}
    if (!is.null(rowfilter)) {x <- rowfilter(x)}
    print(cols)
    if (!is.null(cols)) {x <- x[,cols,drop=FALSE]}
    print(dim(x))
    print(names(x))
    theclasses <- sapply(x, class)
    theclasses[theclasses=="numeric"] <- "double"
    #print(theclasses)
    #print(class(theclasses))
    ans <- big.data.frame(nlines, location=location,
                          classes=theclasses, names=names(x))
    #print(class(x))
    print(dim(x))
    print(typeof(x))
    print(ncol(x))
    for (i in 1:ncol(x)){
       ans[,i] <- x[,i]
     }
  print("Ans after")
  nextline <- nrow(x) + 1
  #print(nextline)
  #print(cols)
  #print(class(myiter)
  foo <- foreach(x=myiter, .combine=rbind) %do% {
    if (!is.null(rowfilter)) x <- rowfilter(x)
    if (!is.null(cols)) x <- x[,cols,drop=FALSE]
    gc()
    rowindex <- as.integer(nextline:(nextline+nrow(x)-1))
    #print(rowindex)
    foreach(k=1:ncol(x),.combine = 'cbind') %do%{
      print(paste0("column",k))
      print(paste0("Class of columns ",class(x[,k])))
      print(paste0("Type of columns ",typeof(x[,k])))
      print(paste0("Dimension of the big.data.frame ", length(ans[rowindex,k])))
      print(paste0("Class of the big.data.frame ", class(ans[rowindex,k])))
      ans[rowindex,k] <- x[rowindex,k]
      print("Successful insertion of a column.")
    }
#     for (k in 1:ncol(x)){
#       print(paste0("k=",k))
#       ans[as.integer(nextline:(nextline+nrow(x)-1)),k] <- x[,k]
#     }
    nextline <- as.integer(nextline + nrow(x))
    print(paste0("Nextline:",nextline))
    return(nrow(x))
  }
  return(ans)
}
