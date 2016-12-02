

sapply(speeddating, function(x) sum(is.na(x)))
dataimp<-subset(speeddating, select=c(dec,attr,sinc,intel,fun,amb,shar,like,prob,met,goal,date,go_out,exphappy))
library(leaps)
dataimp=dataimp[complete.cases(dataimp),]

attach(dataimp)
xdata<-subset(dataimp, select=c(attr,sinc,intel,fun,amb,shar,like,prob,met,goal,date,go_out,exphappy))
cv.fit<-cv.glmnet(x=as.matrix(xdata),y=dec,alpha=1,family='binomial', nfolds=20)
coef(cv.fit, s="lambda.min")
