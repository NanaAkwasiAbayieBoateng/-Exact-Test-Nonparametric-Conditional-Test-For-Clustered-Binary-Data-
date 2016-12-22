rm(list=ls())
library(CorrBin)
library(vcd)
library(coin)
data(shelltox)
shelltox=shelltox
head(shelltox)
#input data
data(egde)
edge <- egde  # rename dataset because of typo in the code

shell=(xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox))

shell1=(xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox))[c(1,4),,]


c=shell
c=xtabs(Freq~Trt+NResp+ClusterSize, data=edge)
#c=c[c(1,2),,]
###########Cochran-Armitage  Trend Test w=j-1###

#### rank of columns ###

v=seq(1,dim(c)[2],1)-1
u=seq(1,dim(c)[1],1)-1


## observed test statistic
tobs=apply(u*c,3,function(x) t(x))*v
tobs=apply(tobs,1,sum)
sum(tobs)
tob=(apply((apply(u*c,3,function(x) t(x))*v),2,sum))
sum(tob)
multiply3=function(c,u,v) {
  ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  for (k in 1:dim(c)[3]){
    
    ts[,,k]=t(u[,k]*c[,,k])*v[,k]
    
  }
  
  tss=(ts)
  return(list(tss))
}





###########Asymptotic pvalue of Linear by Linear Association Test######
########### u=0,1,2,3 and v=0,1,2,3,4,5,6  ##########################
n=colSums(c) 
m=apply(c,3,rowSums)
n=apply(c,3,colSums) 
N=apply(c,3,sum)

v1=matrix(v,dim(c)[2],dim(c)[3])

u1=matrix(u,dim(c)[1],dim(c)[3])


tobs=apply(multiply3(c,u1,v1)[[1]],3,sum)
sum(tobs)
mean=colSums(u1*m)*colSums(v1*n)/N
sum(mean)
var=((colSums((u1^2)*m)-(colSums(u1*m)^2)/N)*(colSums(v1^2*n)-(colSums(v1*n)^2)/N))/N


tstar=(sum(tobs-mean)^2)/sum(var)
 
pchisq(tstar, df=1, ncp = 0, lower.tail = F, log.p = FALSE)
pchisq(tstar, df=1, ncp = 0, lower.tail = T, log.p = FALSE)

1-pchisq(tstar, df=1, ncp = 0, lower.tail = T, log.p = FALSE)


#################################################
##### wilcoxon scores##############

n=colSums(c) 
m=apply(c,3,rowSums)
n=apply(c,3,colSums) 
N=apply(c,3,sum)


midrankscores=function(c){
  
  d=matrix(0,dim(c)[1],dim(c)[2])
  
  d[1,] =0.5*(c[1,]+1 )
  
  d[2,]=c[1,]+0.5*(c[2,]+1)
  
  for (i in 3:dim(c)[1]){
    
    
    d[i,]=colSums(c[1:(i-1),])+0.5*(c[i,]+1)
    
    
  }
  return(d)
}


multiply3=function(c,u,v) {
  ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  for (k in 1:dim(c)[3]){
    
    ts[,,k]=t(u[,k]*c[,,k])*v[,k]
    
  }
  
  tss=(ts)
  return(list(tss))
}





v1=midrankscores(apply(colSums(c),2,rank))

u1=midrankscores(apply(apply(c,3,rowSums),2,rank))

tobs=apply(multiply3(c,u1,v1)[[1]],3,sum)

mean=colSums(u1*m)*colSums(v1*n)/N


var=((colSums((u1^2)*m)-(colSums(u1*m)^2)/N)*(colSums(v1^2*n)-(colSums(v1*n)^2)/N))/N




tstar=sum(tobs-mean)^2/sum(var)

pchisq(tstar, df=1, ncp = 0, lower.tail = F, log.p = FALSE)

pchisq(tstar, df=1, ncp = 0, lower.tail = T, log.p = FALSE)



