library(DESeq2)
library(dplyr)

getwd()

cts = read.delim("nat_bw2-ReadCount.txt", sep="", header = T)
row.names(cts)=cts[,1]
cts=cts[,-1]
dim(cts)
head(cts)

cts2 <- select(cts, -c (ERR1315786,ERR1315787, ERR1315788
, ERR1315789, ERR1315790, ERR1315791, ERR1315792, ERR1315793))
dim(cts2)

log_cts<- log(cts2+1, 10) # This is log base 10 + 1 for "0"
table(rowSums(log_cts>log10(3))>=7)

keep.exprs<- rowSums(log_cts>log10(3))>=7
cts_filt<-cts2[keep.exprs,]
dim(cts_filt)
head(cts_filt,11)


Name=colnames(cts2)
Type=c(rep("wt_nomr", 4), rep("wt_cold", 4))
coldata=data.frame(Name,Type)
head(coldata)
dim(coldata)

dds <- DESeqDataSetFromMatrix(countData = round(cts_filt), colData = coldata, design = ~ Type)
dds$Type <- factor(dds$Type, levels = c("wt_nomr","wt_cold"))
head(dds)

dds <- DESeq(dds)
res <- results(dds)
res_order= res[order(res$padj),]
head(res_order)

write.csv(res_order,"wt_dataset_mm_nomr_cold.csv", row.names = T)
