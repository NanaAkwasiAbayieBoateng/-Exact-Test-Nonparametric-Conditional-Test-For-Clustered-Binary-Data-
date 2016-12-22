rm(list=ls())
library(CorrBin)
data(shelltox)
shelltox=shelltox
#input data
data(egde)
edge <- egde  # rename dataset because of typo in the code




#####################################################################################
####### Dose group control,low,medium and High #####################################
c=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)
control=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
low=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2]
medium=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3]
high=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]


expectedobs=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))

for (i in 1:dim(c)[3]){
  
  rowTotals <- rowSums(c[,,i])
  colTotals <- colSums(c[,,i])
  nOfCases <- sum(rowTotals)
  expectedobs[,,i] <- outer(rowTotals, colTotals, "*") / nOfCases  
  
  
}

pobs=sum(((c-expectedobs-0.5)^2)/(expectedobs-0.5))

#input data
data(egde)
edge <- egde  # rename dataset because of typo in the code

c=xtabs(Freq~Trt+NResp+ClusterSize, data=edge)







expectedobs=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))

for (i in 1:dim(c)[3]){
  
  rowTotals <- rowSums(c[,,i])
  colTotals <- colSums(c[,,i])
  nOfCases <- sum(rowTotals)
  expectedobs[,,i] <- outer(rowTotals, colTotals, "*") / nOfCases  
  
  
}
#pobs=4*sum((sqrt(c)-sqrt(expectedobs))^2)

pobs=sum((sqrt(c)+sqrt(c+1)-sqrt(4*expectedobs+1))^2)


ps=c()
nsim=1000
for (j in 1:nsim){
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
    rowTotals <- rowSums(t[,,i])
    colTotals <- colSums(t[,,i])
    nOfCases <- sum(rowTotals)
    expected[,,i] <- outer(rowTotals, colTotals, "*") / nOfCases  
    
    
  }
  
  
  
  #ps[j]=4*sum((sqrt(t)-sqrt(expected))^2)
  ps[j]=sum((sqrt(t)+sqrt(t+1)-sqrt(4*expected+1))^2)
  
  
}


p.value1=(length(ps[ps>=pobs])+1)/(nsim+1)
p.value1
length(ps[ps<=pobs])/nsim

length(ps[ps>=pobs])/nsim

 # png("~/Documents/memphisclassesbooks/RESEARCH/Trend/gfch.png")
 # hist(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values ",xlab="",prob=TRUE,ylab="P(T=t)")
 # abline(v =pobs,col="blue")
# box(lty = 'solid', col = 'black')
# dev.off();
### mid p-value ##########
pval=0.5*(length(((ps[ps=pobs]))))/(nsim)+(length(((ps[ps>pobs]))))/(nsim)
pval
#cbind(pobs,ps)
se1=sqrt((p.value1*(1-p.value1))/(nsim-1))
up1=p.value1+2.575*se1
lo1=p.value1-2.575*se1
c1=c(lo1,up1)####0.6876618 0.6951843
c1
#sqrt(matrix(c(4,9,16,25),2,2))



kruskal=function(x){
  
  columns=function(x){
    
    d=matrix(0,1,dim(x)[2])
    
    d[1] =0.5*(colSums(x)[1]+1)
    
    d[2]=colSums(x)[1]+0.5*(colSums(x)[2]+1)
    
    for (i in 3:dim(x)[2]){
      
      d[i]=sum(colSums(x)[1:(i-1)])+0.5*(colSums(x)[i]+1)
      
    }
    return(d)
  } 
  
  
  #columns(x)
  
  R=x%*%t(columns(x))
  
  
  lambda=sum(sapply(colSums(x),function(x) x^3-x))
  
  m=rowSums(x)
  
  N=sum(x)
  
  D=(12/(N*(N+1)*(1-(lambda/(N^3-N)))))*sum(((R-m*(0.5*(N+1)))^2)/m)
  
  return(D)
}

kruskal(x)

kruskal(c[,,1])
apply(c,3,sum)
h=(apply(c,3,kruskal))
h[is.na(h)]=0
pobs=sum(h)

colSums(c)
colSums(t)
apply(c,3,rowSums)
apply(t,3,rowSums)


t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
nsim=1000
p=matrix(0,nsim,dim(c)[3])
for (j in 1:nsim){
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  
  p[j,]=(apply(t,3,kruskal))
  
  
  #ps[j]=sum(h)
  
  
  
  
} 


p[is.na(p)]=0

psim=apply(p,1,sum)

(length(psim[psim>=pobs])+1)/(nsim+1)

length(psim[psim<=pobs])/nsim

pchisq(pobs, df=12, ncp = 0, lower.tail = F, log.p = FALSE)
