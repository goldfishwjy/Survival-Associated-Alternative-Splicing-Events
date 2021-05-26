

#install.packages("pheatmap")

library(pheatmap)
setwd("C:\\Users\\lexb4\\Desktop\\AS\\19.riskPlot")             #设置工作目录
rt=read.table("risk.txt",sep="\t",header=T,row.names=1,check.names=F)       #读取输入文件
rt=rt[order(rt$riskScore),]                                     #按照riskScore对样品排序

#绘制风险曲线
riskClass=rt[,"risk"]
lowLength=length(riskClass[riskClass=="low"])
highLength=length(riskClass[riskClass=="high"])
line=rt[,"riskScore"]
line[line>10]=10
pdf(file="riskScore.pdf",width = 12,height = 5)
plot(line,
     type="p",
     pch=20,
     xlab="Patients (increasing risk socre)",
     ylab="Risk score",
     col=c(rep("green",lowLength),
     rep("red",highLength)))
abline(h=median(rt$riskScore),v=lowLength,lty=2)
dev.off()

#绘制生存状态图
color=as.vector(rt$fustat)
color[color==1]="red"
color[color==0]="green"
pdf(file="survStat.pdf",width = 12,height = 5)
plot(rt$futime,
     pch=19,
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
abline(v=lowLength,lty=2)
dev.off()

#绘制风险热图
rt1=rt[c(3:(ncol(rt)-2))]
rt1=t(rt1)
annotation=data.frame(type=rt[,ncol(rt)])
rownames(annotation)=rownames(rt)
pdf(file="heatmap.pdf",width = 12,height = 5)
pheatmap(rt1, 
         annotation=annotation, 
         cluster_cols = FALSE,
         fontsize_row=11,
         fontsize_col=3,
         color = colorRampPalette(c("green", "black", "red"))(50) )
dev.off()
