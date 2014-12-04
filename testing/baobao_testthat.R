install.packages("testthat")

setwd("~/Dropbox/stat662/big.data.frame/testing")
# WITH HEADER
wh <- data.frame(rep(c(1L:26L),2000),rep(letters,2000),rep(rnorm(26),2000))
names(wh) <- c("int","factor","num")
write.csv(wh,"withheader.csv",row.names=F)
rwh <- read.csv("withheader.csv",header = T)
rwh1 <- big.read.table(file = "withheader.csv",as.is = T,header = TRUE)
rwh2 <- big.read.table(file = "withheader.csv",as.is = T,header = FALSE)
length(read.table("withheader.csv",sep=",",nrows=1,header=F))
