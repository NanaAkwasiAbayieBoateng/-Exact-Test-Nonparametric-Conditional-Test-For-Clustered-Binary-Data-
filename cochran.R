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
#c=c[c(3,4),,]
#c
###########Cochran-Armitage  Trend Test w=j-1###

#### rank of columns ###
#v=matrix(seq(1,dim(c)[2],1),dim(c)[2],dim(c)[3])

v=seq(1,dim(c)[2],1)-1
u=seq(1,dim(c)[1],1)-1


multiply=function(c) {
  ts=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  for(i in 1:dim(c)[1]){
    for (j in 1:dim(c)[2]){
      
      for (k in 1:dim(c)[3]){
        
        ts[i,j,k]=u[i]*c[i,j,k]*v[j]
        
        
      }
      
    }
    
  }
  tss=sum(ts)
  return(tss)
}

tobs=multiply(c)


nsim=100000
tn=c()
for (l in 1:nsim){
  
  t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  
  
  for (z in 1:dim(c)[3]){
    
    t[,,z]=r2dtable(1, rowSums(c[,,z]), colSums(c[,,z]))[[1]]
    
  }
  
  
  tn[l]=multiply(t) 
}


p.value1=(length(((tn[tn>=tobs])))+1)/(nsim+1)
p.value1

### mid p-value ##########
pval=0.5*(length(((tn[tn=tobs]))))/(nsim)+(length(((tn[tn>tobs]))))/(nsim)
pval

p.value2=(length(((tn[tn>=tobs]))))/(nsim)
p.value2
# tobss=abs(tobs-mean(tn))
# tnn=abs(tn-mean(tn))
# twosidedpvalue=(length(((tnn[tobss<=tnn])))+1)/(nsim+1)
# twosidedpvalue
# midpvalue=(length(((tnn[tobss<tnn])))+1)/(nsim+1)+0.5*(length(which(tnn==tobss)))/nsim
# midpvalue

#hist(rchisq(n=10000, df=12, ncp = 0),prob=T,breaks="fd",col="gray")

#hist(tn,breaks="fd",col="gray",prob=TRUE,main=" Distribution of ts",ylab="P(T=t)")
#abline(v =tobs,col="blue")
#box(lty = 'solid', col = 'black')


#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/cochran.png")
#hist(tn, breaks="fd", col="gray",main="",xlab="",prob=TRUE,ylab="P(T=t)")
#abline(v =tobs,col="blue")
#box(lty = 'solid', col = 'black')
#dev.off();



###########Cochran-Armitage  Trend Test w=j-1###

#### rank of columns ###
#v=matrix(seq(1,dim(c)[2],1),dim(c)[2],dim(c)[3])


v=seq(1,dim(c)[2],1)-1
u=seq(1,dim(c)[1],1)-1


## observed test statistic
tobs=sum(apply(u*c,3,function(x) t(x))*v)
tobs
######################
#### simulating tables with marginals
#### same as observed
ts=c()
nsim=1000
for (j in 1:nsim){
  
  t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
  
  
  
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  
  t
  
  ###rank of columns###
  v=seq(1,dim(c)[2],1)-1
  
  ###rank of rows###
  
  u=seq(1,dim(c)[1],1)-1
  
  
  
  
  ts[j]=sum(apply(u*t,3,function(x) t(x))*v)
}
ts

p.value=(length(((ts[ts>=tobs])))+1)/(nsim+1)
p.value
### mid p-value ##########
pval=0.5*(length(((ts[ts=tobs]))))/(nsim)+(length(((ts[ts>tobs]))))/(nsim)
pval
