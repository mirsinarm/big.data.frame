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

file <- "withheader.csv"
x <- big.read.table(file = "withheader.csv",header = TRUE)
x                    
                    ,nrows = 100)
y <- big.read.table(file = "withoutheader.csv",header = FALSE)
x.csv <- read.csv(file = "withheader.csv",header = TRUE)
y.csv <- read.csv(file = "withoutheader.csv",header = FALSE)

# WITHOUT HEADER
write.table(wh,"withoutheader.csv",sep=",",row.names=FALSE,col.names=FALSE)

# Reading CSV 

test_that("CSV with header", {
  x <- big.read.table(file = "withheader.csv",header = TRUE)
  y <- read.csv("withheader.csv",header = T,as.is=TRUE)
  expect_that(x[], equals(y)) # check everything
  expect_that(x[,1:2], equals(y[,1:2])) # check columns
  expect_that(nrow(x), equals(nrow(y))) # check nrow: count number of rows
  expect_that(ncol(x), equals(ncol(y))) # check ncol: count number of cols
  expect_that(length(x), equals(length(y))) # check length
  expect_that(dim(x), equals(dim(y))) # check dim
})

test_that("CSV with header", {
  x <- big.read.table(file = "withoutheader.csv",header = TRUE)
  y <- read.csv("withoutheader.csv",header = T,as.is=TRUE)
  expect_that(x[], equals(y)) # check everything
  expect_that(x[,1:2], equals(y[,1:2])) # check columns
  expect_that(nrow(x), equals(nrow(y))) # check nrow: count number of rows
  expect_that(ncol(x), equals(ncol(y))) # check ncol: count number of cols
  expect_that(length(x), equals(length(y))) # check length
  expect_that(dim(x), equals(dim(y))) # check dim
})

