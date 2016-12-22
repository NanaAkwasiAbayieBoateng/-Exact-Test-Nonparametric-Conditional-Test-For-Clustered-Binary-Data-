rm(list=ls())

t0=c(1,0,1,0,1,1,2,1,5,4,6,7,2,2,0,0,0,0,0,0,0)
t1=c(0,0,0,0,1,0,0,0,2,1,7,6,5,2,0,0,1,0,0,0,0)
t2=c(0,0,0,0,0,0,0,0,1,1,5,3,0,1,0,0,1,0,0,0,0)
t3=c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0)
t4=c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
t5=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t6=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t7=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t8=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t9=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t10=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t11=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t12=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t13=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t14=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t15=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t16=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t17=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t18=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#t19=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

d=rep(0,19)
d0=rbind(t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18)
d0=cbind(d,d0)
colnames(d0)=c("d","1","2","3","4","5","6","7","8","9","10","11","12","13",
                "14","15","16","17","18","19","20","21")
d0
dim(d0)

#d30
t0=c(0,1,1,2,0,2,2,1,2,4,8,2,3,2,1,0,0,0,0,0,0)
t1=c(0,0,0,0,0,0,1,1,2,8,4,5,5,2,2,0,0,0,0,0,0)
t2=c(0,0,0,0,0,0,2,2,0,1,1,2,1,2,1,0,0,0,0,0,0)
t3=c(0,0,0,0,0,0,0,0,0,0,1,3,2,0,1,0,0,0,0,0,0)
t4=c(0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0)
t5=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
t6=c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
t7=c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
t8=c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
t9=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t10=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t11=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t12=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t13=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t14=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t15=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
t16=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t17=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t18=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#t19=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

d=rep(30,19)
d30=rbind(t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18)
d30=cbind(d,d30)
colnames(d30)=c("d","1","2","3","4","5","6","7","8","9","10","11","12","13",
                "14","15","16","17","18","19","20","21")

dim(d30)


#d45
t0=c(1,0,1,1,0,0,1,1,0,1,8,3,0,0,1,0,0,0,0,0,0)
t1=c(0,0,1,0,1,0,1,1,3,2,4,3,1,1,1,0,0,0,0,0,0)
t2=c(0,0,0,0,1,1,0,0,1,0,5,4,3,2,1,0,0,0,0,0,0)
t3=c(0,0,0,0,0,0,0,0,0,0,1,2,3,1,1,1,0,0,0,0,0)
t4=c(0,0,0,0,0,0,0,0,1,0,0,1,0,2,1,0,0,0,0,0,0)
t5=c(0,0,0,0,0,0,0,0,0,1,1,1,3,1,1,0,0,0,0,0,0)
t6=c(0,0,0,0,0,0,0,0,0,1,0,2,0,3,1,1,0,0,0,0,0)
t7=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
t8=c(0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0)
t9=c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
t10=c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
t11=c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
t12=c(0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0)
t13=c(0,0,0,0,0,0,0,0,0,0,0,0,2,1,0,0,0,0,0,0,0)
t14=c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
t15=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t16=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t17=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t18=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
#t19=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

d=rep(45,19)
d45=rbind(t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18)
d45=cbind(d,d45)
colnames(d45)=c("d","1","2","3","4","5","6","7","8","9","10","11","12","13",
                "14","15","16","17","18","19","20","21")

d45
dim(d45)

#d60
t0=c(1,0,0,0,0,0,0,1,0,1,0,3,0,0,1,0,0,0,0,0,0)
t1=c(0,0,0,1,0,0,0,0,2,2,4,2,1,1,0,0,0,0,0,0,0)
t2=c(0,0,0,0,0,0,1,1,1,1,0,0,4,0,1,0,0,0,0,0,0)
t3=c(0,0,2,0,0,0,0,1,0,0,0,0,3,1,0,0,0,0,0,0,0)
t4=c(0,0,0,0,1,0,0,0,1,0,2,0,0,0,0,0,0,0,0,0,0)
t5=c(0,0,0,0,0,2,0,0,0,0,0,2,1,0,0,1,0,0,0,0,0)
t6=c(0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
t7=c(0,0,0,0,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0)
t8=c(0,0,0,0,0,0,0,2,1,0,0,0,2,0,0,0,0,0,0,0,0)
t9=c(0,0,0,0,0,0,0,0,1,0,2,1,0,0,0,0,0,0,0,0,0)
t10=c(0,0,0,0,0,0,0,0,0,5,1,0,0,0,0,0,0,0,0,0,0)
t11=c(0,0,0,0,0,0,0,0,0,0,2,0,1,0,0,0,0,0,0,0,0)
t12=c(0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0)
t13=c(0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0)
t14=c(0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0)
t15=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t16=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t17=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t18=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#t19=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

d=rep(60,19)
d60=rbind(t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18)
d60=cbind(d,d60)
colnames(d60)=c("d","1","2","3","4","5","6","7","8","9","10","11","12","13",
                "14","15","16","17","18","19","20","21")

d60
dim(d60)
#d75
t0=c(0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
t1=c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
t2=c(0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,0,0,0,0,0,0)
t3=c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
t4=c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
t5=c(0,0,0,0,2,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
t6=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t7=c(0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0)
t8=c(0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0)
t9=c(0,0,0,0,0,0,0,0,3,1,0,0,0,0,0,0,0,0,0,0,0)
t10=c(0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0)
t11=c(0,0,0,0,0,0,0,0,0,0,4,2,0,0,0,0,0,0,0,0,0)
t12=c(0,0,0,0,0,0,0,0,0,0,0,3,4,0,0,0,0,0,0,0,0)
t13=c(0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0)
t14=c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
t15=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
t16=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t17=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
t18=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#t19=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
d=rep(75,19)
d75=rbind(t0,t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18)
d75=cbind(d,d75)
colnames(d75)=c("d","1","2","3","4","5","6","7","8","9","10","11","12","13",
                "14","15","16","17","18","19","20","21")


dim(d75)

D=rbind(d0,d30,d45,d60,d75)

dim(D)
sum(D[,2:22])
sum(apply(D[,2:22],1,sum))
prodfact=function(x){
  y =prod(apply(apply(x,2,factorial),2,prod))
  
  return(y)  
}

p1=fisher.test(d0, simulate.p.value = TRUE, B = 1000)
p2=fisher.test(d30, simulate.p.value = TRUE, B = 1000)
p3=fisher.test(d45, simulate.p.value = TRUE, B = 1000)
p4=fisher.test(d60, simulate.p.value = TRUE, B = 1000)
p5=fisher.test(d75, simulate.p.value = TRUE, B = 1000)

#p=0.0009990*.000999*0.000999*0.5924*0.000999
p=p1[[1]]*p2[[1]]*p3[[1]]*p4[[1]]*p5[[1]]






probdist=function(D){
d0=D[1:19,]
d30=D[20:38,]
d45=D[39:57,]
d60=D[58:76,]
d75=D[77:95,]
m1=apply(d0,2,sum)
m2=apply(d30,2,sum)
m3=apply(d45,2,sum)
m4=apply(d60,2,sum)
m5=apply(d75,2,sum)
M=m1+m2+m3+m4+m5
d075=d0+d30+d45+d60+d75
p=sum(lfactorial(m1))+sum(lfactorial(m2))+sum(lfactorial(m3))+sum(lfactorial(m4))+sum(lfactorial(m5))+
  sum(apply(lfactorial(d075),2,sum))-sum(lfactorial(M))-
  sum(apply(lfactorial(d0),2,sum))-sum(apply(lfactorial(d30),2,sum))-sum(apply(lfactorial(d45),2,sum))-
  sum(apply(lfactorial(d60),2,sum))-sum(apply(lfactorial(d75),2,sum))


return(exp(p))
}

pobs=probdist(D[,-1])
rowTotals <- rowSums(D[,-1])
colTotals <- colSums(D[,-1])
Nsim=1000


psim=sapply(r2dtable(Nsim, rowTotals, colTotals), probdist)
hist(psim, col="red",prob = TRUE)
abline(v =pobs,col="blue")
pvalue=sum(((psim[pobs<=psim])))/Nsim


apply()

length(((psim[psim=pobs])))/Nsim

length(((psim[pobs>=psim])))/Nsim

variance=pvalue*(1-pvalue)/Nsim

t=unique(psim)
pval=sum(((t[pobs>=t])))




fisher.test(D,simulate.p.value = T,B = 200000,alternative ="greater")
chisq.test(D,D[,1], correct = TRUE,
           p = rep(1/length(x), length(x)), rescale.p = FALSE,
           simulate.p.value = FALSE, B = 200000)


library(CorrBin)

data(shelltox)
shelltox=shelltox
xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)


control=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
low=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2]
both=control+low
d=rbind(control,low)
dim(d)
dim(control)
m1=apply(low,2,sum)
m2=apply(control,2,sum)
M=m1+m2


probdist=function(d){
  control=d[1:7,]
  low=d[8:14,]
  m1=apply(low,2,sum)
  m2=apply(control,2,sum)
  M=m1+m2
  b=control+low
  p=sum(lfactorial(m1))+sum(lfactorial(m2))+
    sum(apply(lfactorial(both),2,sum))-sum(lfactorial(M))-
    sum(apply(lfactorial(low),2,sum))-sum(apply(lfactorial(control),2,sum))
    
  
  
  return(exp(p))
}



pobs=probdist(d)
rowTotals <- rowSums(d)
colTotals <- colSums(d)
Nsim=1000


psim=sapply(r2dtable(Nsim, rowTotals, colTotals), probdist)

pvalue=sum(((psim[pobs>=psim])))/Nsim

pl=length(((psim[pobs<=psim])))/Nsim
pg=length(((psim[pobs>=psim])))/Nsim


hist(psim, breaks="fd", col="red")

hist(psim, breaks="fd", col="red",xlim=c(0,0.05),prob = TRUE)
abline(v =pobs,col="blue")

hist(psim, breaks="sturges", col="red",xlim=c(0,0.05),prob = TRUE)
abline(v =pobs,col="blue")
hist(psim, breaks=20, col="red",xlim=c(0,0.05),prob = TRUE)
abline(v =pobs,col="blue")


fisher.test(d,simulate.p.value = T,B = 20000,alternative ="greater")
fisher.test(d,simulate.p.value = T,B = 20000,alternative ="less")
fisher.test(d,simulate.p.value = T,B = 20000,alternative ="two.sided")
chisq.test(d, correct = TRUE,
           p = rep(1/length(x), length(x)), rescale.p = FALSE,
           simulate.p.value = FALSE, B = 20000)


fisher.test(low,simulate.p.value = T,B = 20000,alternative ="greater")



D=rbind(c[,,1],c[,,2],c[,,3],c[,,4])
test=D
#test=matrix(c(5,2,2,0,0,1,0,1,0,2,3,4),byrow=T,nrow=3)
fisher.test(test,, B = 20000)
chisq.test(test)
chisq.test(test,simulate.p.value = T, B = 20000)
rowTotals <- rowSums( test)
colTotals <- colSums( test)
nOfCases <- sum( test)
expected <- outer(rowTotals, colTotals, "*") / nOfCases
#maxSqResid <- function(x) sum((x - expected) ^ 2 / expected)
#maxSqResid <- function(x) sum((sqrt(x)+sqrt(x+1)-sqrt(4*expected+1))^2)
maxSqResid <- function(x) 4*sum((sqrt(x)-sqrt(expected))^2)
#r2dtable(10000, rowTotals, colTotals)
simMaxSqResid <-sapply(r2dtable(20000, rowTotals, colTotals), maxSqResid)
simobs=maxSqResid(test)
mean(simobs<=simMaxSqResid)


