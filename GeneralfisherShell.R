rm(list=ls())
library(CorrBin)
data(shelltox)
shelltox=shelltox
#input data
data(egde)
edge <- egde  # rename dataset because of typo in the code
#install.packages("CorrBin")

#####################################################################################
####### Dose group  and High #####################################
# sum(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))
# d1=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1]
# d4=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4]
# datan=xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,c(1,4)]
# 
# both=d1+d4
# 
# d=c


# probdist=function(d){
#   d1=d[,,1]
#   d4=d[,,2]
#   m1=apply(d4,2,sum)
#   m2=apply(d1,2,sum)
#   M=m1+m2
#   b=d1+d4
#   p=sum(lfactorial(m1))+sum(lfactorial(m2))+
#     sum(apply(lfactorial(b),2,sum))-sum(lfactorial(M))-
#     sum(apply(lfactorial(d4),2,sum))-sum(apply(lfactorial(d1),2,sum))
#   return((p))
# }
# pobs=probdist(d)
# pobs






c=datan
t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))

ps=c()
nsim=1000
for (j in 1:nsim){
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  
  
  ps[j]=probdist(t)
  
} 



p.value1=(length(((ps[ps>=pobs])))+1)/(nsim+1)
p.value2=(length(((ps[ps<pobs])))+1)/(nsim+1)
p.value1
p.value2
#2*min(p.value1,p.value2)

se1=sqrt((p.value1*(1-p.value1))/(nsim-1))
up1=p.value1+2.575*se1
lo1=p.value1-2.575*se1
c1=c(lo1,up1)
c1
se2=sqrt((p.value2*(1-p.value2))/(nsim-1))
up2=p.value2+2.575*se2
lo2=p.value2-2.575*se2
c2=c(lo2,up2) 
c2
### mid p-value ##########
pval1=0.5*(length(((ps[ps==pobs]))))/(nsim)+(length(((ps[ps>pobs]))))/(nsim)
pval1
pval2=0.5*(length(((ps[ps==pobs]))))/(nsim)+(length(((ps[ps<pobs]))))/(nsim)
pval2

(length(((pobs[pobs>=ps])))+1)/(nsim+1)
#par(mfrow=c(1,1))
#hist(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values ",xlab="",prob=TRUE)
#abline(v =pobs,col="blue")
#box(lty = 'solid', col = 'black')
#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/gfch.png")
#hist(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values ",xlab="log P(Y)",prob=TRUE,ylab="P(T=t)")
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

#c=xtabs(Freq~Trt+NResp+ClusterSize,data=shelltox)

#c=xtabs(Freq~NResp+ClusterSize+Trt, data=edge)
d=c

# probdist=function(d){
#   control=d[,,1]
#   low=d[,,2]
#   medium=d[,,3]
#   high=d[,,4]
#   m1=apply(control,2,sum)
#   m2=apply(low,2,sum)
#   m3=apply(medium,2,sum)
#   m4=apply(high,2,sum)
#   M=m1+m2+m3+m4
#   b=control+high+low+medium
#   p=sum(lfactorial(m1))+sum(lfactorial(m2))+sum(lfactorial(m3))+sum(lfactorial(m4))+
#     sum(apply(lfactorial(b),2,sum))-sum(lfactorial(M))-
#     sum(apply(lfactorial(high),2,sum))-sum(apply(lfactorial(control),2,sum))-
#     sum(apply(lfactorial(low),2,sum))-sum(apply(lfactorial(medium),2,sum))
#   return((p))
# }
# pobs=probdist(d)
# pobs

t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))

ps=c()
nsim=1000
for (j in 1:nsim){
  for (i in 1:dim(c)[3]){
    
    t[,,i]=r2dtable(1, rowSums(c[,,i]), colSums(c[,,i]))[[1]]
    
  }
  
  
  ps[j]=probdist(t)
  
}

c=xtabs(Freq~Trt+NResp+ClusterSize, data=edge)
#c=xtabs(Freq~Trt+NResp+ClusterSize, data=shelltox)[-c(2,3),,]
dim(c)


Homogeneity=function(c,nsim){
  
expectedobs=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))

for (i in 1:dim(c)[3]){
  
  rowTotals <- rowSums(c[,,i])
  colTotals <- colSums(c[,,i])
  nOfCases <- sum(rowTotals)
  expectedobs[,,i] <- outer(rowTotals, colTotals, "*") / nOfCases  
  
  
}
#pobs=4*sum((sqrt(c)-sqrt(expectedobs))^2)

pobs=sum((sqrt(c)+sqrt(c+1)-sqrt(4*expectedobs+1))^2)




t=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
expected=array(0,c(dim(c)[1],dim(c)[2],dim(c)[3]))
ps=c()
#nsim=1000
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

return(list(p.value=p.value1,ps=ps,pobs=pobs))

}


rs=Homogeneity(c,nsim=10000)





library(Rmisc)
library(calibrate)
library(ggplot2)
# png("~/Documents/memphisclassesbooks/RESEARCH/Trend/gfch.png")
qplot(rs$ps, geom="histogram",xlab="",col=I("black"),fill=I("gray")
      ,main=expression(paste("Distribution of of Simulated Values of ",D[y]),binwidth = 0.5 ))
lines( c(rs$pobs,rs$pobs), c(0,300), col = "red", lwd = 2)
text(pobs, 300 , expression(paste("Observed ",D[x])), cex = 1)

abline(v =pobs,col="blue")

qplot(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values of P(y) 
      ",xlab="",prob=TRUE,ylab="P(T=t)")

#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/gfch.png")
setwd("C:/Users/Gucci148/Desktop/Trend")
png("C:/Users/Gucci148/Desktop/Trend/gfch.png")
 hist(rs$ps, breaks="fd", col="gray",main=expression(paste("Distribution of of Simulated Values of " , D[y])) 
      ,xlab="",ylab="P(T=t)",prob=T,sub=expression(paste("red line is the observed value of ", D[x])))
 lines( c(rs$pobs,rs$pobs), c(0,300), col = "red", lwd = 2)
 lines(density(rs$ps),col="black")
 text(rs$pobs, 300 , "Observed D(x)", cex = 1)
 box(lty = 'solid', col = 'black')
 dev.off();
 
 
hist(rchisq(10000,df=10))
 

 
 hist(ps,main="Distribution of of Simulated Values of D(y)")
 lines( c(pobs,pobs), c(0,300), col = "red", lwd = 2)
 text(pobs, 300 , "Observed D(x)", cex = 1)
 box(lty = 'solid', col = 'black')
 dev.off();
 
 
 hist(ps,main="Distribution of of Simulated Values of D(y)",ylim=c(0,315))
 lines( c(pobs,pobs), c(0,300), col = "red", lwd = 2)
 text(x=pobs, y = 310, labels = "obs", adj = NULL,
      pos = NULL, offset = 0.5, vfont = NULL,
      cex = 1, col = "red", font = NULL)
 box(lty = 'solid', col = 'black')

 
 da=data.frame(ps=rs$ps)
 
 ggplot(data=da, aes(da$ps)) + 
   geom_histogram(breaks=seq(min(ps), max(ps), by = 0.5), 
                  col="black", 
                  fill="gray", 
                  alpha = .2) + 
   labs(title="Simulated Distribution of D(y)") +
   labs(x="", y="Frequency") + 
   xlim(c(min(da$ps),max(da$ps))) + 
   ylim(c(0,0.09))
 
 
 
 
 
 
 
 
 
 
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

h
p
p[is.na(p)]=0
p
psim=apply(p,1,sum)

(length(psim[psim>=pobs])+1)/(nsim+1)

length(psim[psim<=pobs])/nsim

pchisq(pobs, df=12, ncp = 0, lower.tail = F, log.p = FALSE)
