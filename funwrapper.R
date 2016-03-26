args <- commandArgs(trailingOnly=TRUE)

instancefile <- args[1]
instance <- readRDS(instancefile)
#id <- args[2]
i = 3
while(i< length(args)) {
  if(args[i]=="--lfa") lfa <- as.numeric(args[i+1])
  #if(args[i]=="--maxit") maxit <- as.numeric(args[i+1])
  i<-i+2
}
source("LAHC.R")
maxit= 500
p <- c(1,1,1,1,1,1,1,1)
cat(gsp_lahc(instance,lfa,maxit,p,FALSE)$res)