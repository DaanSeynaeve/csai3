args <- commandArgs(trailingOnly=TRUE)

instancefile <- args[1]
instance <- readRDS(instancefile)
#id <- args[2]
i = 3
while(i< length(args)) {
  if(args[i]=="--lfa") lfa <- as.numeric(args[i+1])
  if(args[i]=="--p1") p1 <- as.numeric(args[i+1])
  if(args[i]=="--p2") p2 <- as.numeric(args[i+1])
  if(args[i]=="--p3") p3 <- as.numeric(args[i+1])
  if(args[i]=="--p4") p4 <- as.numeric(args[i+1])
  if(args[i]=="--p5") p5 <- as.numeric(args[i+1])
  if(args[i]=="--p6") p6 <- as.numeric(args[i+1])
  if(args[i]=="--p7") p7 <- as.numeric(args[i+1])
  if(args[i]=="--p8") p8 <- as.numeric(args[i+1])
  i<-i+2
}
source("LAHC.R")
source("GSP.R")
maxit= 1000
p <- c(p1,p2,p3,p4,p5,p6,p7,p8)
cat(gsp_lahc(instance,lfa,maxit,p,FALSE)$res)