

#install.packages("survival")
#install.packages("survminer")

setwd("C:\\Users\\lexb4\\Desktop\\AS\\17.survival")              #设置工作目录
library(survival)
library("survminer")
rt=read.table("risk.txt",header=T,sep="\t")
diff=survdiff(Surv(futime, fustat) ~risk,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
pValue=signif(pValue,4)
pValue=format(pValue, scientific = TRUE)

fit <- survfit(Surv(futime, fustat) ~ risk, data = rt)

#绘制生存曲线
pdf(file="survival.pdf",onefile = FALSE,
       width = 5.5,             #图片的宽度
       height =5)             #图片的高度
ggsurvplot(fit, 
           data=rt,
           conf.int=TRUE,
           pval=paste0("p=",pValue),
           pval.size=4,
           risk.table=TRUE,
           legend.labs=c("High risk", "Low risk"),
           legend.title="Risk",
           xlab="Time(years)",
           break.time.by = 1,
           risk.table.title="",
           palette=c("red", "blue"),
           risk.table.height=.25)
dev.off()

summary(fit)    #查看五年生存率

