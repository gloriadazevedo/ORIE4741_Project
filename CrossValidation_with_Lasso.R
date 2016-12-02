library(gdata)
setwd("/Users/pihu_yadav/Downloads/ORIE4741_Project-master")
speeddating=read.csv("data_excel.csv")

sapply(speeddating, function(x) sum(is.na(x)))
dataimp<-subset(speeddating, select=c(dec,attr,sinc,intel,fun,amb,shar,like,prob,met,goal,date,go_out,exphappy))
library(leaps)
dataimp=dataimp[complete.cases(dataimp),]

attach(dataimp)
xdata<-subset(dataimp, select=c(attr,sinc,intel,fun,amb,shar,like,prob,met,goal,date,go_out,exphappy))
cv.fit<-cv.glmnet(x=as.matrix(xdata),y=dec,alpha=1,family='binomial', nfolds=20)
  > coef(cv.fit, s="lambda.min")
14 x 1 sparse Matrix of class "dgCMatrix"
                       1
(Intercept) -6.242358546
attr         0.425231191
sinc        -0.193552121
intel       -0.003051939
fun          0.126381962
amb         -0.183279102
shar         0.100765592
like         0.566926269
prob         0.137951350
met          0.006337728
goal        -0.039329749
date        -0.042999015
go_out       0.101862605
exphappy     0.049418567
> coef(cv.fit,s="lambda.1se")
14 x 1 sparse Matrix of class "dgCMatrix"
                      1
(Intercept) -5.96521699
attr         0.36994929
sinc        -0.09455284
intel        .         
fun          0.06343280
amb         -0.08870043
shar         0.07374769
like         0.48630445
prob         0.10616226
met          .         
goal         .         
date         .         
go_out       0.02017083
exphappy     0.02145154

