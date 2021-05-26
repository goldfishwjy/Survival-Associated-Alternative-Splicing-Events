

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

library("limma")

setwd("C:\\Users\\lexb4\\Desktop\\m6A\\06.diff")                 #设置工作目录
inputFile="ASexp.txt"                                           #输入文件
conNum=32                                                        #normal组样品数目
treatNum=375                                                     #tumor组样品数目

#读取输入文件
outTab=data.frame()
grade=c(rep(1,conNum),rep(2,treatNum))
rt=read.table(inputFile,sep="\t",header=T,check.names=F)
rt=as.matrix(rt)
rownames(rt)=rt[,1]
exp=rt[,2:ncol(rt)]
dimnames=list(rownames(exp),colnames(exp))
data=matrix(as.numeric(as.matrix(exp)),nrow=nrow(exp),dimnames=dimnames)
data=avereps(data)

newGeneLists=c()
#差异分析
for(i in row.names(data)){
  geneName=unlist(strsplit(i,"\\|",))[1]
  geneName=gsub("\\/", "_", geneName)
  rt=rbind(expression=data[i,],grade=grade)
  rt=as.matrix(t(rt))
  wilcoxTest<-wilcox.test(expression ~ grade, data=rt)
  conGeneMeans=mean(data[i,1:conNum])
  treatGeneMeans=mean(data[i,(conNum+1):ncol(data)])
  logFC=log2(treatGeneMeans)-log2(conGeneMeans)  
  pvalue=wilcoxTest$p.value
  conMed=median(data[i,1:conNum])
  treatMed=median(data[i,(conNum+1):ncol(data)])
  diffMed=treatMed-conMed
	outTab=rbind(outTab,cbind(gene=i,conMean=conGeneMeans,treatMean=treatGeneMeans,logFC=logFC,pValue=pvalue))
	
	if(pvalue<0.001){
	  newGeneLists=c(newGeneLists,paste0(i,"***"))
	}else if(pvalue<0.01){
	  newGeneLists=c(newGeneLists,paste0(i,"**"))
	}else if(pvalue<0.05){
	  newGeneLists=c(newGeneLists,paste0(i,"*"))
	}else{
	  newGeneLists=c(newGeneLists,i)
	}
}

#输出所有基因的差异情况
write.table(outTab,file="diff.xls",sep="\t",row.names=F,quote=F)

#绘制热图需要的文件
heatmap=cbind(ID=newGeneLists,data)
write.table(heatmap,file="geneSigExp.txt",sep="\t",row.names=F,quote=F)


