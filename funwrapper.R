#args <- commandArgs(trailingOnly=TRUE)

args <- c(1,2,"--x",1,"--y",2)
testinstance <- args[1]
id <- args[2]
i = 3
while(i< length(args)) {
  if(args[i]=="--x") x <- as.numeric(args[i+1])
  if(args[i]=="--y") y <- as.numeric(args[i+1])
  i<-i+2
}
source("testfun.R")
cat(testf(testinstance,x,y))