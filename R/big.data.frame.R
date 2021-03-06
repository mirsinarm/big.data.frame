#
# Development for package big.data.frame
#
# Jay Emerson
# October 2014
#
# - Could be in-RAM or filebacked
#
# - Fundamentally: a list of big* vectors of the same length
#
# - We can nicely support column addition/deletion, but
#   the number of rows must remain fixed.
#
# - Column names are simply the names of the list; row
#   names can be supported by big.strings, eventually?
#   Or perhaps this would be a future change for
#   bigmemory itself?  I have mixed feelings, frankly.
#
# - Not currently supporting factors.  Revisit.
#
###############################################################################
########################################################################## 80 #
###################################################### 60 #

#' S4 class big.data.frame is essentially a list of big vectors
#' provided by packages bigmemory and big.char.
#' @exportClass big.data.frame
setClass('big.data.frame', representation(desc='list',
                                          data='list'))

#' @title Create a big.data.frame
#'
#' @description
#' Create a \code{big.data.frame}
#'
#' @details
#' This is the full set of details for documentation.
#' 
#' @param nrow the number of rows
#' @param classes a vector of values from 'double', 'character', ... 
#' @param location folder to contain the object backingfiles, or NULL
#' if the object will be in-RAM.
#' @param names a vector of names for the columns (variables)
#' @param maxchar a vector with NA for numeric columns, but integers
#' specifying the maximum number of characters for string columns (see
#' \code{\link[big.char]{big.char}} for more information).
#' @param init a vector of values for initialization; note that for
#' large objects this will slow down the creation substantially.
#' @return Returns a \code{big.data.frame} object
#' @author Jay Emerson
#' @export
big.data.frame <- function(nrow, classes,
                           location=NULL,
                           names=NULL,
                           maxchar=NULL,
                           init=NULL) {

  if (!is.null(location)) {
    if (file.exists(location)) {
      warning(paste("Location", location, "exists; using it..."))
    } else {
      warning(paste("Creating", location))
      if (!dir.create(location)) stop("Directory creation failed")
    }
  }
  if (is.null(names)) names <- paste("V", 1:length(classes), sep=".")
  if (length(names) != length(unique(names))) {
    stop("names must be unique") 
  }
  if (any(!(classes %in%
              c("double", "integer", "short", "char", "character"))))
    stop("Invalid class")
  if (nrow < 1) stop("No rows?  Really?")
  if (is.null(maxchar)) {
    maxchar <- rep(NA, length(classes))
    if (any(classes=="character")) maxchar[classes=="character"] <- 8
  }
  # If maxchar is provided, we don't currently check sanity with classes
  
  x <- new('big.data.frame',
           desc=list(dim=c(nrow, length(classes)), 
                     classes=classes, maxchar=maxchar, names=names),
           data=list())
  
  if (!is.null(location)) {
    print("We will be creating filebackings in this location.")
    backingfile <- paste(names, ".bin", sep="")
    descriptorfile <- paste(names, ".desc", sep="")
    dput(list(dim=c(nrow, length(classes)),
              maxchar=maxchar, names=names, classes=classes),
         file.path(location, "info.txt"))
  } else {
    backingfile <- NULL
    descriptorfile <- NULL
  }
  
  i <- 0 # To shut up a warning with foreach...
  x@data <- foreach(i=1:length(classes)) %do% {    
    if (classes[i] == "character") {
      ans <- big.char::big.char(nrow, maxchar=maxchar[i],
                                init=init[[i]],
                                backingfile=backingfile[i],
                                descriptorfile=descriptorfile[i],
                                backingpath=location)
    } else {
      ans <- bigmemory::big.matrix(nrow, 1, type=classes[i],
                                   init=init[[i]],
                                   backingfile=backingfile[i],
                                   descriptorfile=descriptorfile[i],
                                   backingpath=location)
    }
    return(ans)
  }
  names(x@data) <- names
  
  return(x)
}

#
# attach functionality
#
#' @title Attach an existing big.data.frame from its backingfile
#' @description The expected usage is for shared-memory parallel computing
#' or for persistence via memory-mapped files of the (column) variables.
#' @return a \code{\link{big.data.frame}} object
#' @param location the folder containing the backingfiles.
#' @export
attach.big.data.frame <- function(location) {
  info <- dget(file.path(location, "info.txt"))
  x <- new('big.data.frame',
           desc=list(dim=info$dim, 
                     classes=info$classes,
                     maxchar=info$maxchar,
                     names=info$names),
           data=vector(mode="list", length=length(info$names)))
  names(x@data) <- info$names
  for (i in 1:length(info$names)) {
    if (info$classes[i] == "character") {
      x@data[[i]] <- big.char::attach.big.char(
        paste(info$names[i], ".desc", sep=""), path=location)
    } else {
      x@data[[i]] <- bigmemory::attach.big.matrix(
        paste(info$names[i], ".desc", sep=""), path=location)
    }
  }
  return(x)
}

#' @title ncol functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @exportMethod ncol
setMethod('ncol', signature(x="big.data.frame"),
  function(x) return(x@desc$dim[2]))

#' @title nrow functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @exportMethod nrow
setMethod('nrow', signature(x="big.data.frame"), 
  function(x) return(x@desc$dim[1]))

#' @title dim functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @exportMethod dim
setMethod('dim', signature(x="big.data.frame"),
  function(x) return(c(nrow(x), ncol(x))))

#' @title length functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @param x a big.data.frame
#' @exportMethod length
setMethod('length', signature(x="big.data.frame"),
  function(x) return(ncol(x)))

#####################################################
#  Added by Miranda, Baobao, Rose
#####################################################
#' @title head functionality for a big.data.frame
#' @author Miranda Sinnott-Armstrong
#' @rdname big.data.frame-methods
#' @param x a big.data.frame
#' @export head
setMethod('head', signature(x='big.data.frame'),
          function(x, n=6) {
            if(ncol(x) > 1000) {
              warning("Your big.data.frame has more than 1000 columns.  Only the first 999 were printed.")
              max.col <- 999
            }
            else max.col <- ncol(x)
            if (nrow(x) < n) {
              ans <- data.frame(x[,1:max.col])
              colnames(ans) <- names(x@data)
              return(ans)
            }
            else {
              ans <- data.frame(x[1:n,1:max.col])
              colnames(ans) <- names(x@data[1:max.col])
              return(ans)
            }
          }
)

#####################################################
#  Added by Miranda, Baobao, Rose
#####################################################
#' @title tail functionality for a big.data.frame
#' @author Miranda Sinnott-Armstrong
#' @rdname big.data.frame-methods
#' @param x a big.data.frame
#' @export tail
setMethod('tail', signature(x='big.data.frame'),
          function(x, n=6) {
            if(ncol(x) > 1000) {
              warning("Your big.data.frame has more than 1000 columns.
                      Only the first 999 were printed.")
              max.col <- 999
            } else max.col <- ncol(x)
            if (nrow(x) < n) {
              ans <- data.frame(x[,1:max.col])
              colnames(ans) <- names(x@data[1:max.col])
              return(ans)
            } else {
              end <- nrow(x)  
              ans <- data.frame(x[(end-n+1):end,1:max.col])
              rownames(ans) <- (end-n+1):end
              colnames(ans) <- names(x@data[1:max.col])
              return(ans)
            }
          })

#####################################################
#  Added by Rose, Baobao, Miranda
#####################################################
#' @title print summary of the big data frame
#' @rdname big.data.frame-methods
#' @author Rose Brewin
#' @exportMethod summary
setMethod("summary", signature=(object="big.data.frame"), function(object, ...) {
  cat("Are you sure you want to calculate summaries of big objects?")
  if (interactive()) {
    ANSWER <- readline("Continue with summary (Y/n)? ")
    if (substring(ANSWER, 1, 1) != "Y")
      stop("Terminated summary.")
  } 
  return(summary(object[1], ...))
})

#####################################################
#  Added by Rose, Miranda, Baobao
#####################################################
#' @title print structure of the big data frame
#' @rdname big.data.frame-methods
#' @author Rose Brewin
#' @exportMethod str
setMethod('str', signature=(object="big.data.frame"), function (object, ...) 
{
  if (!is.big.data.frame(object)) {
    stop("str.big.data.frame() called with non-big.data.frame")
    # object <- data.frame(object) Do we want to have coerce functionality?
  }
  cl <- oldClass(object)
  cl <- cl[cl != "big.data.frame"]
  if (0 < length(cl)) 
    cat("Classes", paste(sQuote(cl), collapse = ", "), "and ")
  cat("'big.data.frame':\t", nrow(object), " obs. of  ", (p <- length(object)), 
      " variable", if (p != 1) 
        "s", if (p > 0) 
          ":", "\n", sep = "")
  #    if (length(l <- list(...)) && any("give.length" == names(l))) 
  #      invisible(NextMethod("str",  ...))
  #    else 
  #      invisible(NextMethod("str", object=object@data, give.length = FALSE))
})


#####################################################
#  Added by Rose, Baobao, Miranda
#####################################################
#' @title names functionality for a big.data.frame
#' @rdname big.data.frame-methods
#' @author Rose Brewin
#' @exportMethod names
setMethod('names', signature(x="big.data.frame"),
          function(x) return(names(x@data)))

#####################################################
#  Added by Rose, Baobao, Miranda
#####################################################
#' @title set the names of a big.data.frame 
#' @rdname big.data.frame-methods
#' @author Rose Brewin
#' @exportMethod names<-
setMethod('names<-', signature(x="big.data.frame", value="character"),
          function(x, value) {
            if (!options()$bigmemory.allow.dimnames)
              warning("Descriptor file (if applicable) is not modified.\n")
            names(x@data) <- value
            x@desc$names <- value
            return(x)
          })

#####################################################
#  Added by Rose, Miranda, Baobao
#####################################################
#' @title Generic function is.big.data.frame()
#' @description Do we have a \code{\link{big.data.frame}}?
#' @details No further detail is needed.
#' @param x a \code{\link{big.data.frame}} combination of big.matrix 
#'  and big.char objects
#' @author Rose Brewin
#' @export
setGeneric('is.big.data.frame', function(x) standardGeneric('is.big.data.frame'))

#' @title Do we have a big.data.frame?
#' @rdname big.data.frame-methods
#' @author Rose Brewin
#' @exportMethod is.big.data.frame
setMethod('is.big.data.frame', signature(x='big.data.frame'),
          function(x) return(TRUE))

#####################################################
#  Added by Miranda, Rose, Baobao
#####################################################
#' @title Convert a \code{\link{big.matrix}} object to a \code{\link{big.data.frame}} object
#' @return a new \code{\link{big.data.frame}} object
#' @param x a big.matrix object
#' @author Miranda Sinnott-Armstrong
#' @export
as.big.data.frame <- function(x, names=NULL) {
  if(class(x) != "big.matrix") {
    stop ("Only converts big.matrix objects to a big.data.frame object.")
  }
  ans <- big.data.frame(nrow=nrow(x),
                        classes=rep(typeof(x), ncol(x)),
                        location=NULL,
                        names=names,
                        init=NULL)
  
  for(i in 1:ncol(x)) {
    ans[, i] <- x[, i]
  }
  
  if(!is.null(names)) {
    
    ###  This causes a warning: Descriptor file (if applicable) is not modified.
    ###  However, I modified the descriptor file by hand.
    ###  So the question is, either 1) suppress the warning, or 2) change the 
    ###      names() function to modify the descriptor file.
    
    names(ans) <- names
    ans@desc$names <- names
  }
  
  return(ans)
}


#
# Get/set signatures!
#

#' @rdname big.data.frame-methods
#' @exportMethod [
setMethod("[",
          signature(x = "big.data.frame", i="ANY", j="ANY", drop="missing"),
          function(x, i, j, ..., drop) {
            #cat("BDF get:(ANY,ANY,missing) row subset extraction.\n")
            # Could simplify this, but wait for now; factor issue.
            if (length(j)==1) return(as.data.frame(x@data[[j]][i],
                                                   stringsAsFactors=FALSE)[[1]])
            return(as.data.frame(lapply(x@data[j], function(a) a[i]),
                                 stringsAsFactors=FALSE))
          })


#####################################################
#  Added by Rose, Miranda, Baobao
#####################################################
#' @rdname big.data.frame-methods
#' @exportMethod [
setMethod("[<-",
          signature(x = "big.data.frame", i="ANY", j="ANY"),
          function(x, i, j, ..., value) {
            #cat("BDF get:(ANY,ANY,missing) row subset extraction.\n")
            # Could simplify this, but wait for now; factor issue.
            if(length(i)*length(j) < length(value)) {stop("Items to replace do not match replacement length.")}
            x@data[[j]][i] <- value
            return(x)
          })


#' @rdname big.data.frame-methods
#' @exportMethod [
setMethod("[",
          signature(x = "big.data.frame", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
            #stop("Not yet BDF get:(ANY, missing)")
            #cat("BDF get:(ANY,missing,missing) row subset extraction.\n")
            
            # Negative indices
            if(any(i < 0)) {
              # Need to check whether negative indices are mixed with positive
              if(any(i > 0)) stop("Negative indices can only mix with 0, not positive indices.")
              
              # If everything's kosher, extract the correct columns
              else {
                ii <- abs(i)
                ind <- 1:nrow(x)
                ind <- ind[-c(ii)]
                i <- ind
              }
            }
            
            # Here, current default is drop=TRUE
            if (ncol(x)==1) return(as.data.frame(x@data[[1]][i],
                                                 stringsAsFactors=FALSE)[[1]])
            # Otherwise, have multiple columns to extract:
            
### Jay's original code:  issue was that outputted rownames were incorrect
#             return(as.data.frame(lapply(x@data, function(a) a[i]),
#                                  stringsAsFactors=FALSE))
            
            ans <- as.data.frame(lapply(x@data, function(a) a[i]),
                          stringsAsFactors=FALSE)
            rownames(ans) <- i
            return(ans)
          })


#' @rdname big.data.frame-methods
#' @author Miranda Sinnott-Armstrong
#' @exportMethod [<-
setMethod("[<-",
          signature(x = "big.data.frame", i="ANY", j="missing"),
          function(x, i, j, ..., value) {
            # Edge cases:
            #  y[-2,]
            if(any(i < 0)) {
              stop("Warning: index is negative.  Not sure what you think this will do.")
            }

            # What if the classes of the columns don't match the classes
            # of the elements of the object you're inputting?
            
            # Currently, throws an error and produces an NA.
            # Not sure of desired behavior.

###  Jay's original code
#             #cat("BDF set:(ANY,missing,missing)\n")
#             for (jj in 1:ncol(x)) x@data[[jj]][i] <- value[,jj]
#             return(x)
            
            # repeat 'value' so that it is the same length as the number of cols
            val <- rep(value, length.out=ncol(x))
            for (jj in 1:ncol(x)) x@data[[jj]][i] <- val[jj]
            return(x)
          })


# We thought we modified this, but apparently not ... so I deleted the code that we wrote
# that turned out to be the same.
#' @rdname big.data.frame-methods
#' @exportMethod [
setMethod("[", signature(x = "big.data.frame", i="missing", j="ANY", drop="missing"),
    function(x, i, j, ..., drop) {
    #cat("BDF get:(missing,ANY,missing)\n")
      # Consider simplifying depending on factor issue, eventually:
      if (length(j)==1) return(as.data.frame(x@data[[j]][],
                                             stringsAsFactors=FALSE)[[1]])
      # Otherwise, multiple column extraction:
      return(as.data.frame(lapply(x@data[j], function(a) a[]),
                           stringsAsFactors=FALSE))
    })


#' @rdname big.data.frame-methods
#' @exportMethod [<-
setMethod("[<-",
          signature(x = "big.data.frame", i="missing", j="ANY"),
          function(x, i, j, ..., value) {
###  Jay's original code:
#             function(x, i, j, ..., value) {
#               #cat("BDF set:(missing,ANY,missing)\n")
#               return("Not done")
            
            
            # Check whether the class of the new data matches the class
            # of the old data
            # Must check whether the CONTENTS of the new data match the CONTENTS
            # of the old data

            if(sum(j < 0)) {
              stop("Warning: index is negative.  Not sure what this should do.")
            }
            
            #cat("BDF set:(missing,ANY,missing)\n")
            val <- rep(value, length.out=nrow(x))
            for (jj in 1:nrow(x)) x@data[[j]][jj] <- val[jj]
            return(x)
          })


#' @rdname big.data.frame-methods
#' @exportMethod [
setMethod("[",
signature(x = "big.data.frame", i="missing", j="ANY", drop="logical"),
    function(x, i, j, ..., drop) {
        #cat("BDF get:(missing,ANY,ANY)\n")
      
      if (length(j)==1) {
          if (!drop) {
            ans <- as.data.frame(x@data[[j]][], stringsAsFactors=FALSE)
            names(ans) <- names(x@data)[j]
            return(ans)
          } # else drop==TRUE next with one column:
          return(as.data.frame(x@data[[j]][], stringsAsFactors=FALSE)[[1]])
        } # and otherwise we have multiple columns to extract:
        
        return(as.data.frame(lapply(x@data[j], function(a) a[]),
                    stringsAsFactors=FALSE, drop=drop))
    })


###  Jay's original code:  unchanged by us
#' @rdname big.data.frame-methods
#' @exportMethod [
setMethod("[",
          signature(x = "big.data.frame",
                    i="missing", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
            #cat("BDF get:(missing,missing,missing)\n")
            #cat("Probably not right if a 1-column data frame\n")
            return(as.data.frame(lapply(x@data, function(a) a[]),
                                 stringsAsFactors=FALSE))
          })

#####################################################
#  Added by Miranda, Rose, Baobao
#####################################################
#' @rdname big.data.frame-methods
#' @author Miranda Sinnott-Armstrong
#' @exportMethod $
setMethod("$", "big.data.frame",
          function(x, name)
          {
            ## 'name' is a character(1)
            if(nrow(x) > 1000) {
              print("Warning:  printing too many rows.")
            }
            else return(x@data[[name]][])
          })

#####################################################
#  Added by Miranda, Rose, Baobao
#####################################################
#' @rdname big.data.frame-methods
#' @author Miranda Sinnott-Armstrong
#' @exportMethod $<-
setMethod("$<-", "big.data.frame",
          function(x, name, value)
          {
            
            # To code:  when the class changes for that column
            #             if(typeof(x[1, j]) != typeof(value)
            # maybe use match()?

            ## 'name' is a character(1)
            val <- rep(value, length.out=nrow(x))
            x@data[[name]][] <- val
            return(x)
          })

######################################################################
# Added by Rose, Miranda, and Baobao
######################################################################
#' @title Remove columns from an existing \code{\link{big.data.frame}}
#' @return a new \code{\link{big.data.frame}} object, with fewer columns
#' @param x a big.data.frame object
#' @param index a vector of indices or names of the columns to be removed
#' @author Rose Brewin
#' @export
big.drop.cols <- function(x, index, location=NULL) {
  if (is.character(index)) {
    index <- which(x@desc$names %in% index)
  }  
  if (any(index > ncol(x) | index <= 0)) {
    stop("Index out of bounds")
  } else if (length(unique(index)) >= ncol(x)) {
    stop("Index is entire big.data.frame")
  } 
  # a shorter version, modified 12/15/2014:
  # question: how to deal with the backing file?
    x@data <- x@data[-c(index)]
    x@desc$dim[2] <- length(x@data)
    x@desc$classes <- x@desc$classes[-c(index)]
    x@desc$maxchar <- x@desc$maxchar[-c(index)]
    x@desc$names <- x@desc$names[-c(index)]
    return(x)
  
# the old version:
#   ans <- big.data.frame(nrow=x@desc$dim[1],
#                         classes=x@desc$classes[-index],
#                         location=location,
#                         names=x@desc$names[-index],
#                         maxchar=x@desc$maxchar[-index],
#                         init=NULL)
#   ans@data <- x@data[-index]
#   return(ans)
}

######################################################################
# Added by Rose, Miranda, and Baobao
######################################################################
#' @title Add an extra column to an existing \code{\link{big.data.frame}}
#' @return a new \code{\link{big.data.frame}} object, with one extra column
#' @param x a big.data.frame object
#' @param new.col either a big.char object or a big.matrix object
#'  with a single column
#' @param after the position after which new.col will be appear
#' @export
big.add.col <- function(x, new.col, new.name=NULL, after=ncol(x), location=NULL) {
  if (class(new.col) == "big.matrix") new.class <- "double"
  else if (class(new.col) == "big.char") new.class <- "char"
  else stop ("new.col must be either a big.matrix or big.char object")
  
  # A problem: how to handle the backing files?
  
  # For now, stop if they don't pass a maxchar
  if (class(new.col) == "big.char" & maxchar == NA) {
    stop("Warning: must set a maxchar for your new big.char column.")
  }
  
  # If a new name wasn't provided:
  if (after > ncol(x)) after <- ncol(x)
  if(is.null(new.name)) new.name <- paste("V", after+1, sep=".")
  
  # Depending on the value of 'after':
  if (after == 0) {
    x@data <- c(new.col, x@data)
    x@desc$classes <- c(typeof(new.col), x@desc$classes)
    names(x) <- c(new.name, x@desc$names)
    x@desc$maxchar <- c(maxchar, x@desc$maxchar) # might change if don't terminate upon big.char without maxchar
  } else if (after >= ncol(x)) {
    x@data <- c(x@data, new.col)
    x@desc$classes <- c(x@desc$classes, typeof(new.col))
    names(x) <- c(x@desc$names, new.name)
    x@desc$maxchar <- c(x@desc$maxchar, maxchar) # might change if don't terminate upon big.char without maxchar
  } else {
    x@data <- c(x@data[1:after], new.col, x@data[(after+1):length(x@data)])
    x@desc$classes <- c(x@desc$classes[1:after], typeof(new.col), x@desc$classes[(after+1):length(x@desc$classes)])
    names(x) <- c(x@desc$names[1:after], new.name, x@desc$names[(after+1):length(x@desc$names)])
    x@desc$maxchar <- c(x@desc$maxchar[1:after], maxchar, x@desc$maxchar[(after+1):length(x@desc$maxchar)])
  }
  
  x@desc$dim <- c(nrow(x), length(x@data))
  return(x)
  
  # old version that worked by creating a new big.data.frame:
#   new.classes <- append(x@desc$classes, new.class, after=after)
#   new.names <- append(x@desc$names, new.name, after=after)
#   new.data <- append(x@data[], new.col, after=after)
#   names(new.data) <- new.names
#   
#   ans <- big.data.frame(x@desc$dim[1], 
#                         classes=new.classes,
#                         names=new.names,
#                         location=location)
#   ans@data <- new.data
#   return(ans)
}


#' @title Change the class of a column in an existing \code{\link{big.data.frame}}
#' @return a new \code{\link{big.data.frame}} object, with the updated column
#' @author Miranda Sinnott-Armstrong, Rose Brewin
#' @param x a big.data.frame object
#' @param new.col either a big.char object or a big.matrix object
#'  with a single column
#' @param index the position of the changed class column
#' @export
big.change.col.class <- function(x, new.col, index, new.name=NULL, location=NULL) {
  # fixing the class types in case the user inputs an alternative:
  if (class(index) == "character") index <- which(x@desc$names == index)
  if (class(new.col) == "big.matrix") new.class <- "double"
  else if (class(new.col) == "big.char") new.class <- "char"
  else stop ("new.col must be either a big.matrix or big.char object")

  if (index < 1 | index > ncol(x)) stop ("Index must exist already; use big.add.col to create a new column.")
  
  # If a new name wasn't provided:
  if(is.null(new.name)) new.name <- paste("V", index, sep=".")
  
  # Edge cases:  index=1, index=ncol(x)
  x@data[[index]] <- new.col
  x@desc$classes[[index]] <- new.class
  x@desc$names[[index]] <- new.name
  return(x)
}

##################################################################
###  Additions to big.char:  it has no head or tail function, which is necessary
###  for printing out x@data when one of the columns is a big.char


#' @title head functionality for a big.char object
#' @author Miranda Sinnott-Armstrong
#' @rdname big.char-methods
#' @param x a \code{\link{big.char}}
#' @export head
#' 
setMethod('head', signature(x='big.char'),
      function(x, n=6) {
        if (ncol(x) < n) return(x[])
        else return(x[1:n])
      }
)

#' @title tail functionality for a big.char object
#' @author Miranda Sinnott-Armstrong
#' @rdname big.char-methods
#' @param x a \code{\link{big.char}}
#' @export tail
setMethod('tail', signature(x='big.char'),
      function(x, n=6) {
        if (ncol(x) < n) return(x[])
        else return(x[(ncol(x)-n+1):ncol(x)])
      }
)
