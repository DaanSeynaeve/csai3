readExperiments <- function(filename,label) {
  experiments <- read.table(file=paste("experiments",filename,sep="/"))

  colnames(experiments) <- c("id","lfa","replace","shrink","shift","and","or","xor","vertical swap","bitflip")
  parameters <- experiments[,-(1:2)]

  adj <- data.frame(t(apply(parameters,1,function(x) x/sum(x))))
  adj$label <- label
  return(adj)
}

#df <- readExperiments("experiment_general.txt","general")

#boxplot(df)

require(reshape2)
t10 = readExperiments("experimentb5.txt","b = [5,10]")
t100 = readExperiments("experimentb50.txt","b = [50,100]")

df <- rbind(t10,t100)
df.m <- melt(df, id.var = "label",variable.name = "neighbourhood", value.name = "weight")

require(ggplot2)
ggplot(data = df.m, aes(x=neighbourhood, y=weight)) + geom_boxplot(aes(fill=label))
