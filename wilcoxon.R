rm(list=ls())
library(CorrBin)
library(vcd)
library(coin)
data(shelltox)
#input data
data(egde)
edge <- egde  # rename dataset because of typo in the code

shell=(xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox))

c=shell

c=xtabs(Freq~Trt+NResp+ClusterSize, data=edge)
#c=c[c(1,2),,]


dim(c)


## rank each stratum####

#### Wiloxon ranks of nij,midranks for ties####

midrankscores=function(c){
  
  d=matrix(0,dim(c)[1],dim(c)[2])
  
  d[1,] =0.5*(c[1,]+1 )
  
  d[2,]=c[1,]+0.5*(c[2,]+1)
  
  for (i in 3:dim(c)[1]){
    
    
    d[i,]=colSums(c[1:(i-1),])+0.5*(c[i,]+1)
    
    
  }
  return(d)
}



#################
#####  mid rank weights of the column sums
######### in each stratum

v=midrankscores(apply(colSums(c),2,rank))

#################
#####  mid rank weights of the row sums
######### in each stratum

u=midrankscores(apply(apply(c,3,rowSums),2,rank))


multiply2=function(c,u,v) {
  ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  for (k in 1:dim(c)[3]){
    
    ts[,,k]=t(u[,k]*c[,,k])*v[,k]
    
  }
  
  tss=sum(ts)
  return(list(tss))
}



tobs=multiply2(c,u,v)[[1]]

ts=c()
nsim=100000
for (j in 1:nsim){
  
  t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  
  
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  v=midrankscores(apply(colSums(t),2,rank))
  
  u=midrankscores(apply(apply(t,3,rowSums),2,rank))
  
  ts[j]=multiply2(t,u,v)[[1]]
  
  
}


p.value1=(length(((ts[ts>=tobs])))+1)/(nsim+1)
p.value1


p.value1+p.value2
### mid p-value ##########
pval=0.5*(length(((ts[ts=tobs]))))/(nsim)+(length(((ts[ts>tobs]))))/(nsim)
pval

# tobss=abs(tobs-mean(ts))
# tnn=abs(ts-mean(ts))
# twosidedpvalue=(length(((tnn[tobss<=tnn])))+1)/(nsim+1)
# twosidedpvalue
# midpvalue=(length(((tnn[tobss<=tnn]))))/(nsim)+0.5*mean(tobss==tnn)
# midpvalue
# cbind(tobss,tnn)
# which(tobss==tnn)
#hist(ts, breaks="fd", col="gray",main="Distribution of test statistics using midranks ",xlab="",prob=TRUE,ylab="P(T=t)")
#abline(v =tobs,col="blue")
#box(lty = 'solid', col = 'black')

#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/w.png")
#hist(ts, breaks="fd", col="gray",main=" ",xlab="",prob=TRUE,ylab="P(T=t)")
#abline(v =tobs,col="blue")
#box(lty = 'solid', col = 'black')
#dev.off();










x=(apply(colSums(c),2,rank))
r=(apply(apply(c,3,rowSums),2,rank))
r[1,]=0.5*(r[1,]+1)
r[2,]=r[1,]+0.5*(r[2,]+1)

#### Wiloxon ranks of nij,midranks for ties####

midrankscores=function(x){
  
  d=matrix(0,dim(x)[1],dim(x)[2])
  
  d[1,] =0.5*(x[1,]+1 )
  
  d[2,]=x[1,]+0.5*(x[2,]+1)
  
  for (i in 3:dim(x)[1]){
    
    
    d[i,]=colSums(x[1:(i-1),])+0.5*(x[i,]+1)
    
    
  }
  return(d)
}


#################
#####  mid rank weights of the column sums
######### in each stratum

v=midrankscores(apply(colSums(c),2,rank))

#################
#####  mid rank weights of the row sums
######### in each stratus

r=(apply(apply(c,3,rowSums),2,rank))
u=matrix(0,dim(r)[1],dim(r)[2])
u[1,]=0.5*(r[1,]+1)
u[2,]=r[1,]+0.5*(r[2,]+1)





multiply2=function(c,u,v) {
  ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  for (k in 1:dim(c)[3]){
    
    ts[,,k]=t(u[,k]*c[,,k])*v[,k]
    
  }
  
  tss=sum(ts)
  return(list(tss))
}



tobs=multiply2(c,u,v)[[1]]

ts=c()
nsim=1000
for (j in 1:nsim){
  
  t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  
  
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  v=midrankscores(apply(colSums(t),2,rank))
  
 
  r=(apply(apply(t,3,rowSums),2,rank))
  u=matrix(0,dim(r)[1],dim(r)[2])
  u[1,]=0.5*(r[1,]+1)
  u[2,]=r[1,]+0.5*(r[2,]+1)
  
  
  ts[j]=multiply2(t,u,v)[[1]]
  
  
}


p.value1=(length(((ts[ts>=tobs])))+1)/(nsim+1)
p.value1



### mid p-value ##########
pval=0.5*(length(((ts[ts=tobs]))))/(nsim)+(length(((ts[ts>tobs]))))/(nsim)
pval


