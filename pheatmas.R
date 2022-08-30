getwd()

library(pheatmap)
library(grid)

cts_filt=read.table("cts.csv", sep = ",", header = T)
row.names(cts_filt)=cts_filt[,1]
cts_filt=cts_filt[,-1]
dim(cts_filt)

mm<- data.matrix((cts_filt), rownames = TRUE)

#####creat vector classification
cat_df = data.frame(colnames(cts_filt))
class <- data.frame(rep(c("Basal", "Tratamento"), 38))
rownames(class)=cat_df[,1]
colnames(class)=c("class")
head(class)
class$class <- factor(class$class, levels=c("Basal", "Tratamento"))

pheatmap(log(mm+1), cutree_cols = 2, cluster_rows = T, cluster_cols = T, show_colnames =T, main= "Analise 1", 
         annotation_col=class)

class
