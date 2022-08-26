data=read.delim("pgx_dataset.txt", header = T, sep = '\t')


list=list()
for (i in 1:length(data)){
  l=print(table(data[,i]))
  list[[i]]=l
}

library(SNPassoc)
library(ggplot2)
library(ggrepel)

plotmy <- function(x, ...){
  if (!inherits(x, "WGassociation")) 
    stop("x must be an object of class 'WGassociation'")
  
  xx <- data.frame(SNP=rownames(x), data.frame(x)[,2:6])
  names(xx)[6] <- "additive"
  dat <- tidyr::gather(xx, key="model", value="p.value", -"SNP")
  dat$model <- factor(dat$model, 
                      levels=c("codominant", "dominant","recessive", "overdominant",
                               "additive"))
  
  plotmy <- ggplot(dat, aes(x=SNP, y=-log10(p.value))) + 
    geom_point() +
    xlab("SNPs") + ylab(expression(-log[10]("p-value"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_hline(aes(yintercept = -log10(0.05), 
                   linetype = "Nominal p-value < 0.05"), 
               colour="blue") +
    scale_linetype_manual(name = "Significance", values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue"))))+
    geom_text_repel(aes(label=ifelse(p.value<0.05,as.character(SNP),'')))
  
  plotmy + facet_wrap( ~ model, ncol=1) +
    theme(strip.text.x = element_text(face="bold")) +
    theme(legend.position="top", legend.direction="horizontal", axis.text.x = element_blank()) 
}

datSNP<-setupSNP(data,2:208,sep="")
summary=summary(datSNP)

ansAll<-WGassociation(clopi10~1,data=datSNP,model="all")
ansAll
plotmy=plotmy(ansAll)
plotmy

