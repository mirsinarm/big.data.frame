context("Creation and characteristics")

test_that("Creating and characteristics 1", {
  x <- big.data.frame(10, c("double", "character"),
                      names=c("first", "second"),
                      init=list(-1.23, "A"),
                      maxchar=c(NA, 10))
  y <- data.frame(first=rep(-1.23, 10), second=rep("A", 10),
                  stringsAsFactors=FALSE)
  expect_that(x[], equals(y))
  expect_that(x[,1:2], equals(y[,1:2]))
  expect_that(x[,1], equals(y[,1]))
  expect_that(x$first, equals(y$first))
  expect_that(x[,1,drop=FALSE], equals(y[,1,drop=FALSE]))
  expect_that(x[,2], equals(y[,2]))
  expect_that(x$second, equals(y$second))
  expect_that(x[,2,drop=FALSE], equals(y[,2,drop=FALSE]))
  expect_that(nrow(x), equals(nrow(y)))
  expect_that(ncol(x), equals(ncol(y)))
  expect_that(length(x), equals(length(y)))
  expect_that(dim(x), equals(dim(y)))
})

test_that("Creating and characteristics 2", {
  z <- big.data.frame(10, c("double", "character"),
                      names=c("first", "second"),
                      init=list(-1.23, "A"),
                      maxchar=c(NA, 10),
                      location="testdir")
  y <- data.frame(first=rep(-1.23, 10), second=rep("A", 10),
                  stringsAsFactors=FALSE)
  x <- attach.big.data.frame("testdir")
  expect_that(x[], equals(y))
  expect_that(x[,1:2], equals(y[,1:2]))
  expect_that(x[,1], equals(y[,1]))
  expect_that(x$first, equals(y$first))
  expect_that(x[,1,drop=FALSE], equals(y[,1,drop=FALSE]))
  expect_that(x[,2], equals(y[,2]))
  expect_that(x$second, equals(y$second))
  expect_that(x[,2,drop=FALSE], equals(y[,2,drop=FALSE]))
  expect_that(nrow(x), equals(nrow(y)))
  expect_that(ncol(x), equals(ncol(y)))
  expect_that(length(x), equals(length(y)))
  expect_that(dim(x), equals(dim(y)))
  expect_that(dim(x), equals(dim(y)))
})

test_that("Creating and characteristics 3", {
  x <- big.data.frame(10, c("double", "character"),
                      names=c("first", "second"),
                      init=list(-1.23, "A"),
                      maxchar=c(NA, 10))
  y <- data.frame(first=rep(-1.23, 10), second=rep("A", 10),
                  stringsAsFactors=FALSE)
  expect_that(x[], equals(y))
  expect_that(x[,1:2], equals(y[,1:2]))
  expect_that(x$first, equals(y$first))
  expect_that(x[,1,drop=FALSE], equals(y[,1,drop=FALSE]))
  expect_that(x$second, equals(y$second))
  expect_that(x[,2,drop=FALSE], equals(y[,2,drop=FALSE]))
  expect_that(nrow(x), equals(nrow(y)))
  expect_that(ncol(x), equals(ncol(y)))
  expect_that(length(x), equals(length(y)))
  expect_that(dim(x), equals(dim(y)))
})

test_that("Extractions 1", {
  x <- big.data.frame(10, c("double", "character"),
                      names=c("first", "second"),
                      init=list(-1.23, "A"),
                      maxchar=c(NA, 10))
  y <- data.frame(first=rep(-1.23, 10), second=rep("A", 10),
                  stringsAsFactors=FALSE)
  expect_that(x[], equals(y))
<<<<<<< HEAD
  expect_that(x[1,], equals(y[1,])) # this fails ... no idea why ... print, class, typeof all look identical
=======
  #expect_that(x[1,], equals(y[1,])) # not working; need to fix
>>>>>>> 59a08cb23dc9a1b58e8cb699cfb9b1613ab404f2
  expect_that(x[1:2,], equals(y[1:2,]))
  expect_that(x[-c(1:2),], equals(y[-c(1:2),])) # removes the last two rows instead of the first two rows
  expect_that(x[1,1], equals(y[1,1]))
  expect_that(x[1:2,1], equals(y[1:2,1]))
  expect_that(x[-c(1:2),1], equals(y[-c(1:2),1]))
  expect_that(x[1,2], equals(y[1,2]))
  expect_that(x[1:2,2], equals(y[1:2,2]))
  expect_that(x[-c(1:2),2], equals(y[-c(1:2),2]))
})

###########################
# big.read.table tests

# Library
library(testthat)
setwd("~/Dropbox/stat662/big.data.frame/tests/testthat")
# Make the data frame
wh <- data.frame(rep(c(1L:26L),20),rep(letters,20),rep(rnorm(26),20))
names(wh) <- c("int","factor","num")
# WITH HEADER
write.csv(wh,"withheader.csv",row.names=F)
# WITHOUT HEADER
write.table(wh,"withoutheader.csv",sep=",",row.names=FALSE,col.names=FALSE)

# Reading CSV 

test_that("CSV with header, reading in in 1 chunk", {
  x <- big.read.table(file = "withheader.csv",header = TRUE)
  y <- read.csv("withheader.csv",header = T,as.is=TRUE)
  expect_that(x[], equals(y)) # check everything
  expect_that(x[,1:2], equals(y[,1:2])) # check columns
  expect_that(nrow(x), equals(nrow(y))) # check nrow: count number of rows
  expect_that(ncol(x), equals(ncol(y))) # check ncol: count number of cols
  expect_that(length(x), equals(length(y))) # check length
  expect_that(dim(x), equals(dim(y))) # check dim
})

test_that("CSV without header, reading in in 1 chunk", {
  x <- big.read.table(file = "withoutheader.csv",header = FALSE)
  y <- read.csv("withoutheader.csv",header = FALSE,as.is=TRUE)
  expect_that(x[], equals(y)) # check everything
  expect_that(x[,1:2], equals(y[,1:2])) # check columns
  expect_that(nrow(x), equals(nrow(y))) # check nrow: count number of rows
  expect_that(ncol(x), equals(ncol(y))) # check ncol: count number of cols
  expect_that(length(x), equals(length(y))) # check length
  expect_that(dim(x), equals(dim(y))) # check dim
})

test_that("CSV with header, reading in in multiple chunks", {
  # chunks are in size of 100 rows
  x <- big.read.table(file = "withheader.csv",header = TRUE, nrows = 100)
  y <- read.csv("withheader.csv",header = TRUE, as.is=TRUE)
  expect_that(x[], equals(y)) # check everything
  expect_that(x[,1:2], equals(y[,1:2])) # check columns
  expect_that(nrow(x), equals(nrow(y))) # check nrow: count number of rows
  expect_that(ncol(x), equals(ncol(y))) # check ncol: count number of cols
  expect_that(length(x), equals(length(y))) # check length
  expect_that(dim(x), equals(dim(y))) # check dim
})

test_that("CSV without header, reading in in multiple chunks", {
  # chunks are in size of 100 rows
  x <- big.read.table(file = "withoutheader.csv",header = FALSE, nrows = 100)
  y <- read.csv("withoutheader.csv",header = FALSE, as.is=TRUE)
  expect_that(x[], equals(y)) # check everything
  expect_that(x[,1:2], equals(y[,1:2])) # check columns
  expect_that(nrow(x), equals(nrow(y))) # check nrow: count number of rows
  expect_that(ncol(x), equals(ncol(y))) # check ncol: count number of cols
  expect_that(length(x), equals(length(y))) # check length
  expect_that(dim(x), equals(dim(y))) # check dim
})
