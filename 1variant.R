summary(datasets1$Prisechol)
summary(datasets1$Mags)
summary(datasets1$Rasst)
summary(datasets1$Luds)
summary(datasets1$Averg)
pairs(datasets2,panel=panel.smooth)
cor(datasets1)
zet<-lm(Prisechol~Mags+Rasst+Luds+Averg+Bigset,data=datasets1)
vif(zet)
sqrt(vif(zet))>2
zeta<-lm(Prisechol~Mags+Rasst+Luds+Averg+Bigset,data=datasets1)
summary(zeta)
zeta<-lm(Prisechol~Mags+Rasst+Luds+Averg+Bigset,data=datasets1)
zeta2<-lm(Prisechol~Mags+Rasst+Luds+Bigset,data=datasets1)
AIC(zeta,zeta2)
stepAIC(zeta,direction="backward")
ncvTest(zeta)
gqtest(zeta)
durbinWatsonTest(zeta)
bgtest(zeta)
 zzeta<-as.data.frame(scale(datasets1))
zet<-lm(Prisechol~Mags+Rasst+Luds+Averg+Bigset,data=zzeta)
coef(zet)
> relweights<-function(zeta){
  + R<-cor(zeta$model)
  + nvar<-ncol(R)
  + rxx<-R[2:nvar,2:nvar]
  + rxy<-R[2:nvar,1]
  + svd<-eigen(rxx)
  + evec<-svd$vectors
  + ev<-svd$values
  + delta<-diag(sqrt(ev))
  + lambda<-evec %*% delta %*% t(evec)
  + lambdasq<-lambda^2
  + beta<-solve(lambda) %*% rxy
  + rsquare<-colSums(beta^2)
  + rawwgt<-lambdasq %*% beta^2
  + import<-(rawwgt/rsquare)*100
  + lbls<-names(zeta$model[2:nvar])
  + rownames(import)<-lbls
  + colnames(import)<-"Weights"
  + barplot(t(import),names.arg=lbls,ylab="% Предсказанной дисперсии",xlab="Независимые переменные",main="Относительные веса независимых переменных",sub=paste("Rsquare",round(rsquare,digits=3)))
  + return(import)
  + }
relweights(zeta)
relweights(zeta)
}
             


