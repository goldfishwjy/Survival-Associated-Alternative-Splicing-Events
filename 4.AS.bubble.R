

#install.packages("ggplot2")

library(ggplot2)                                          #引用ggplot2这个包
setwd("C:\\Users\\lexb4\\Desktop\\AS\\14.bubble")                #设置工作目录
rt = read.table("uniCoxResult.Sig.txt",header=T,sep="\t")        #读取输入文件
row.names(rt)=rt[,1]
rt[,1]=gsub("\\|","\\-",rt[,1])

#绘制气泡图
for(asType in c("AA","AD","AP","AT","ES","RI","ME")){
		genes=rownames(rt)
		gene=grep(paste0("\\|",asType),genes,value=T)
		geneLength=ifelse(length(gene)>20,20,length(gene))
		data=rt[gene[1:geneLength],]
		
		data=data[order(as.numeric(as.vector(data$pvalue)),decreasing = T),]
		data$id = factor(data$id,levels=as.character(data[,1]))

		p = ggplot(data,aes(z,id))		
		# 四维数据的展示
		pbubble = p + geom_point(aes(color=pvalue,size=-1*log10(pvalue)) )
		# 绘制散点图，可以绘制各种颜色，只要将skyblue和red改成自己喜欢的颜色就行
		pr = pbubble + 
		     scale_colour_gradient(low="red",high="skyblue") + 
		     labs(color="pvalue",size="-log10(pvalue)",x="z-score",y="")+
		     guides(color = guide_colourbar(order = 1), size = guide_legend(order = 2))+
		     theme_bw()
		ggsave(paste0(asType,".bubble.pdf"),width=5.5,height=5)                  #保存图片
}

