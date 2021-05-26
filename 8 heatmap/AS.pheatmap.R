

#install.packages("pheatmap")

setwd("C:\\Users\\lexb4\\Desktop\\m6A\\07.sigPeatmap")      #设置工作目录
rt=read.table("geneSigExp.txt",sep="\t",header=T,row.names=1,check.names=F)
rt=log2(rt+1)

library(pheatmap)
Type=c(rep("N",32),rep("T",375))    #修改对照和处理组样品数目
names(Type)=colnames(rt)
Type=as.data.frame(Type)

pdf("heatmap.pdf",height=5,width=10)
pheatmap(rt, 
         annotation=Type, 
         color = colorRampPalette(c("green", "white", "red"))(50),
         cluster_cols =F,
         show_colnames = F,
         scale="row",
         fontsize = 10,
         fontsize_row=10,
         fontsize_col=3)
dev.off()

