rm(list=ls())
library(CorrBin)
data(shelltox)
shelltox=shelltox


#####################################################################################
####### Dose group  and High #####################################
xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)
d1=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
d4=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]
datan=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,c(1,4)]

both=d1+d4

#d=rbind(d1,d4)
d=datan

probdist=function(d){
  d1=d[,,1]
  d4=d[,,2]
  m1=apply(d4,2,sum)
  m2=apply(d1,2,sum)
  M=m1+m2
  b=d1+d4
  p=sum(lfactorial(m1))+sum(lfactorial(m2))+
    sum(apply(lfactorial(b),2,sum))-sum(lfactorial(M))-
    sum(apply(lfactorial(d4),2,sum))-sum(apply(lfactorial(d1),2,sum))
  return((p))
}
pobs=probdist(d)
pobs






c=datan
t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))

ps=c()
nsim=100000
for (j in 1:nsim){
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  
  
  ps[j]=probdist(t)
  
} 


###### two sided ##
##0.103
p.value=(length(((ps[pobs<=ps])))+1)/(nsim+1)
p.value   #####0.6914231
se=sqrt((p.value*(1-p.value))/(nsim-1))
up=p.value+2.575*se
lo=p.value-2.575*se
c(lo,up)   ####0.6876618 0.6951843
### mid p-value ##########
pval=0.5*(length(((ps[ps==pobs]))))/(nsim)+(length(((ps[ps>pobs]))))/(nsim)
pval
###0.597  new 0.68694

#par(mfrow=c(1,1))
#hist(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values ",xlab="",prob=TRUE)
#abline(v =pobs,col="blue")
#box(lty = 'solid', col = 'black')
#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/gfch.png")
#hist(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values ",xlab="",prob=TRUE,ylab="P(T=t)")
#abline(v =pobs,col="blue")
#box(lty = 'solid', col = 'black')
#dev.off();


#####################################################################################
####### Dose group control,low,medium and High #####################################
c=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)
control=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
low=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2]
medium=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3]
high=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]
both=control+high+medium+low


d=c
probdist=function(d){
  control=d[,,1]
  low=d[,,2]
  medium=d[,,3]
  high=d[,,4]
  m1=apply(control,2,sum)
  m2=apply(low,2,sum)
  m3=apply(medium,2,sum)
  m4=apply(high,2,sum)
  M=m1+m2+m3+m4
  b=control+high+low+medium
  p=sum(lfactorial(m1))+sum(lfactorial(m2))+sum(lfactorial(m3))+sum(lfactorial(m4))+
    sum(apply(lfactorial(b),2,sum))-sum(lfactorial(M))-
    sum(apply(lfactorial(high),2,sum))-sum(apply(lfactorial(control),2,sum))-
    sum(apply(lfactorial(low),2,sum))-sum(apply(lfactorial(medium),2,sum))
  return((p))
}
pobs=probdist(d)
pobs




t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
ps=c()
nsim=100000
for (j in 1:nsim){
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  
  
  ps[j]=probdist(t)
  
} 

###0.182  new 0.3479965
p.value=(length(((ps[pobs<=ps])))+1)/(nsim+1)
p.value
se=sqrt((p.value*(1-p.value))/(nsim-1))
up=p.value+2.575*se
lo=p.value-2.575*se
c(lo,up)
### 0.3424222 0.3501708
### mid p-value ##########
pval=0.5*(length(((ps[ps==pobs]))))/(nsim)+(length(((ps[ps>pobs]))))/(nsim)
pval
###0.67  ##new  0.346165
#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/gfclmh.png")
#hist(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values ",xlab="",prob=TRUE,ylab="P(T=t)")
#abline(v =pobs,col="blue")
#box(lty = 'solid', col = 'black')
dev.off();

