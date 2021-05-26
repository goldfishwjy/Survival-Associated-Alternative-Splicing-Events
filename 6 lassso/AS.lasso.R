

#install.packages("glmnet")
#install.packages("survival")


library("glmnet")
library("survival")
asType="ME"           #(AA AD AP AT ES RI ME)中的一种,如果7种一起做，asType设置为空即可

setwd("C:\\Users\\lexb4\\Desktop\\AS\\15.lasso")                #设置工作目录
rt=read.table("uniSigExp.txt",header=T,sep="\t",row.names=1,check.names=F)       #读取文件
rt$futime[rt$futime<=0]=1
rt$futime=rt$futime/365
genes=colnames(rt)
gene=grep(paste0("\\|",asType),genes,value=T)
geneLength=ifelse(length(gene)>20,20,length(gene))
rt=rt[,c("futime","fustat",gene[1:geneLength])]

x=as.matrix(rt[,c(3:ncol(rt))])
y=data.matrix(Surv(rt$futime,rt$fustat))

fit <- glmnet(x, y, family = "cox", maxit = 1000)
pdf("lambda.pdf")
plot(fit, xvar = "lambda", label = TRUE)
dev.off()

cvfit <- cv.glmnet(x, y, family="cox", maxit = 1000)
pdf("cvfit.pdf")
plot(cvfit)
abline(v=log(c(cvfit$lambda.min,cvfit$lambda.1se)),lty="dashed")
dev.off()

coef <- coef(fit, s = cvfit$lambda.min)
index <- which(coef != 0)
actCoef <- coef[index]
lassoGene=row.names(coef)[index]
lassoGene=c("futime","fustat",lassoGene)
lassoSigExp=rt[,lassoGene]
lassoSigExp=cbind(id=row.names(lassoSigExp),lassoSigExp)
write.table(lassoSigExp,file="lassoSigExp.txt",sep="\t",row.names=F,quote=F)

