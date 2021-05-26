

#install.packages("UpSetR")

library(UpSetR)
setwd("C:\\Users\\lexb4\\Desktop\\AS\\09.UpSetR")                                 #设置工作目录
rt=read.table("asMatrix.txt",sep="\t",header=T,check.names=F,row.names=1)         #读取文件
gene=sapply(strsplit(rownames(rt),"\\|"),"[",1)
asType=sapply(strsplit(rownames(rt),"\\|"),"[",3)
upsetList=list(AA=unique(gene[asType=="AA"]),
               AD=unique(gene[asType=="AD"]),
               AP=unique(gene[asType=="AP"]),
               AT=unique(gene[asType=="AT"]),
               ES=unique(gene[asType=="ES"]),
               ME=unique(gene[asType=="ME"]),
               RI=unique(gene[asType=="RI"]) )
upsetData=fromList(upsetList)

pdf(file="upset.pdf",onefile = FALSE,width=9,height=6)              #保存图片
upset(upsetData,
      nsets = 7,                                    #展示可变剪切类型个数
      nintersects = 50,                             #展示基因集数目
      order.by = "freq",                            #按照数目排序
      show.numbers = "yes",                         #柱状图上方是否显示数值
      number.angles = 20,                           #字体角度
      point.size = 1.5,                             #点的大小
      matrix.color="red",                           #交集点颜色
      line.size = 0.8,                                #线条粗线
      mainbar.y.label = "Gene Intersections", 
      sets.x.label = "Set Size")
dev.off()

