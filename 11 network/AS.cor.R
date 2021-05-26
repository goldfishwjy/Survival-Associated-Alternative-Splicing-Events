

corFilter=0.6              #相关系数过滤标准
pvalueFilter=0.001         #p值过滤标准

setwd("C:\\Users\\lexb4\\Desktop\\AS\\24.cor")                     #设置工作目录

#读取文件，并对样品取交集
SF = read.table("SFexp.txt", row.names=1 ,header=T,sep="\t",check.names=F)   #读取SF表达文件
AS = read.table("uniSigExp.txt", row.names=1 ,header=T,sep="\t",check.names=F)   #读取生存相关的AS文件
AS=t(AS[,3:ncol(AS)])
rownames(AS)=gsub("\\|","\\-",rownames(AS))
group=sapply(strsplit(colnames(SF),"\\-"),"[",4)
group=sapply(strsplit(group,""),"[",1)
group=gsub("2","1",group)
SF=SF[,group==0]
colnames(SF)=gsub("(.*?)\\-(.*?)\\-(.*?)\\-(.*?)\\-.*","\\1\\-\\2\\-\\3",colnames(SF))
sameSample=intersect(colnames(SF),colnames(AS))
SF1=SF[,sameSample]
AS1=AS[,sameSample]

#相关性检验
outTab=data.frame()
for(i in row.names(SF1)){
  if(sd(SF1[i,])>1){
	  for(j in row.names(AS1)){
	     x=as.numeric(SF1[i,])
	     y=as.numeric(AS1[j,])
	     corT=cor.test(x,y)
	     cor=corT$estimate
	     pvalue=corT$p.value
	     if((cor>corFilter) & (pvalue<pvalueFilter)){
	         outTab=rbind(outTab,cbind(SF=i,AS=j,cor,pvalue,Regulation="postive"))
	     }
	     if((cor< -corFilter) & (pvalue<pvalueFilter)){
	         outTab=rbind(outTab,cbind(SF=i,AS=j,cor,pvalue,Regulation="negative"))
	     }
	  }
	}
}
write.table(file="corResult.txt",outTab,sep="\t",quote=F,row.names=F)        #输出相关性结果

#输出调控网络节点属性
asSig = read.table("uniCoxResult.Sig.txt", row.names=1 ,header=T,sep="\t",check.names=F)
rownames(asSig)=gsub("\\|","\\-",rownames(asSig))
asUp=asSig[asSig$z>0,]
asDown=asSig[asSig$z<0,]
SFLabel=cbind(rownames(SF),"SF")
ASupLabel=cbind(rownames(asUp),"ASup")
ASdownLabel=cbind(rownames(asDown),"ASdown")
nodeLabel=rbind(c("ID","Classify"),SFLabel,ASupLabel,ASdownLabel)
write.table(nodeLabel,file="nodeType.txt",sep="\t",quote=F,col.names=F,row.names=F)

