# Exact-Test
set.seed(148)
N<-30
meanlambda<-5
p=.8
q=.1


  ############################################
  ##generating observed variable in a cluster
  
  n<-rep(0,N) #size of clusters
  r<-rep(0,N) #response type one
  
  for (i in 1:N){
    r[i]<-0
    
    n[i]<-rpois(1,meanlambda)
    Z1<-rbinom(1,1,p)
    
    for (j in 1:n[i]){
      Y1<-rbinom(1,1,p)
      
      
      U<-rbinom(1,1,q)
      x<-(1-U)*Y1+U*Z1
      if (x==0) r[i]<-r[i]+1
      
    }
    obs1<-cbind(r,n)
  }
  
  
  
  obs1
  

  

  
  
  ############################################
  ##generating observed variable in a cluster
  #set.seed(148)
  N=100
  n<-rep(0,N) #size of clusters
  r<-rep(0,N) #response type one
  treatment=rep(0,N)
 
  for (i in 1:N){
    
    #n[i]<-rpois(1,meanlambda)
    n[i]=sample(seq(1,3))
    Z1<-rbinom(1,1,p)
    
    p= runif(n=1, min = 0, max = 0.5) 
    q=2*p 
    
    if(q <=0.25){
      treatment[i] <- 0
    } else if(q >0.25& q <=0.6){
      treatment[i] <- 50
    
    
    }else {
      treatment[i] <- 100
    
    } 
   
       
    
    
    
    x=c()
    for (j in 1:n[i]){
      Y1<-rbinom(1,1,p)
      
      
      U<-rbinom(1,1,q)
      x[j]<-(1-U)*Y1+U*Z1
      
      
    }
    r[i]<-sum(x)
    obs<-data.frame(treatment,r,n)
  }
  
  
  
  obs
  
 d= xtabs(~r+n+treatment,data=obs) 
 d
apply(d,3,sum)


c=xtabs(~r+treatment+n,data=obs)
c=array(d[,,1],c(4,3,2))





############################################
##generating observed variable in a cluster
#set.seed(148)
N=100
n<-rep(0,N) #size of clusters
r<-rep(0,N) #response type one
treatment=rep(0,N)
meanlambda=3

for (i in 1:N){
  
  n[i]<-rpois(1,meanlambda)
  #n[i]=sample(seq(1,3))
  Z1<-rbinom(1,1,p)
  
  p= runif(n=1, min = 0, max = 0.5) 
  q=2*p 
  
  if(q <=0.25){
    treatment[i] <- 0
  } else if(q >0.25& q <=0.6){
    treatment[i] <- 50
    
    
  }else {
    treatment[i] <- 100
    
  } 
  
  
  
  
  
  x=c()
  for (j in 1:n[i]){
    Y1<-rbinom(1,1,p)
    
    
    U<-rbinom(1,1,q)
    x[j]<-(1-U)*Y1+U*Z1
    
    
  }
  r[i]<-sum(x)
  obs<-data.frame(treatment,r,n)
}



obs

d= xtabs(~r+n+treatment,data=obs) 
d
apply(d,3,sum)


c=xtabs(~r+treatment+n,data=obs)
c

N<-30
meanlambda<-5
p=.8
q=.1

niter<-100
LAMBDA=matrix(0,niter,6)


  
#   ############################################
#   ##generating observed variable in a cluster
#   N=100
#   n<-rep(0,N) #size of clusters
#   r<-rep(0,N) #response type one
#   Treatment=rep(0,N)
#   Frequency=rep(0,N)
#   meanlambda=1
#   p=0.2
#   q=0.5
#   for (i in 1:N){
#     r[i]<-0
#     
#     #n[i]<-rpois(1,meanlambda)
#     n[i]<-sample(1:5,1,replace=T)
#     Z1<-rbinom(1,1,p)
#     
#     for (j in 1:n[i]){
#       Y1<-rbinom(1,1,p)
#       
#       
#       U<-rbinom(1,1,q)
#       x<-(1-U)*Y1+U*Z1
#       if (x==0) r[i]<-r[i]+1
#       
#     }
#     Frequency= sample(0:5,N,replace=T)
#     obs1<-cbind(r,n,Frequency)
#   }
#   
# 
#   
#   c1=xtabs(Frequency~r+n, data=obs2)
#   #c1=xtabs(~r+n,data=obs1) 
#   c1
#   
#   
#   ############################################
#   ##generating observed variable in a cluster
#   N=100
#   n<-rep(0,N) #size of clusters
#   r<-rep(0,N) #response type one
#   Treatment=rep(0,N)
#   Frequency=rep(0,N)
#   sample(0:1,N,replace=T)
#   meanlambda=1
#   p=0.2
#   q=0.5
#   x=c()
#   for (i in 1:N){
#     #r[i]<-0
#     r=c()
#    # n[i]<-rpois(1,meanlambda)
#     n[i]<-sample(1:5,1,replace=T)
#     
#     Z1<-rbinom(1,1,p)
#     
#     for (j in 1:n[i]){
#       Y1<-rbinom(1,1,p)
#       
#       
#       U<-rbinom(1,1,q)
#       
#       x[j]<-(1-U)*Y1+U*Z1
#       #if (x==0) r[i]<-r[i]+1
#     
#      
#     }
#     r[i]=sum(x)
#     Frequency= sample(0:5,N,replace=T)
#     obs2<-cbind(r,n, Frequency)
#   }
#   
#   
#   
#  c2= xtabs(Frequency~r+n, data=obs2)
#   #c2=xtabs(~r+n,data=obs2) 
#   c2  
#   
# c= array(c(c1,c2),c(dim(c2)[1],dim(c2)[2],2))
# c
apply(c,3,fisher.test(c,B=2000,workspace=2e+10))

 a=matrix(0,2,2)
 b=matrix(c(1,2,3,4),2,2)
  array(c(a,b),c(2,2,2))
  
  set.seed(3486)
  library(CorrBin)
  library(lattice)
  ss <- expand.grid(Trt=0:1, ClusterSize=1:10, Freq=5)
  #Trt is converted to a factor
  rd <- ran.CBData(ss, p.gen.fun=function(g) 0.2+0.1*g)
  rd <- ran.CBData(ss, p.gen.fun=function(g) 0.1*g)
  rd
  c=xtabs(Freq~Trt+NResp+ClusterSize, data=rd)
  
  ss$ClusterSize[1]
  cs <- ss$ClusterSize[1]
  trt <- unclass(ss$Trt)[1]
  n <- ss$Freq[1]
  p <- p.gen.fun(trt)
  rho <- rho.gen.fun(trt)
  probs <- pdf.fun(p, rho, cs)
  
 r1= rmultinom(1,size = 17, prob = c(0.7,0.7,0.7,0.7,0.7))
  r2=rmultinom(1, size = 10, prob = c(0.5,0.5,0.5,0.5,0.5))
  r3=rmultinom(1, size = 10, prob = c(0.1,0.1,0.1,0.1,0.1))
  apply(cbind(r1,r2,r3),2,sum)
  pr <- c(1,3,6,10) # normalization not necessary for generation
  rmultinom(10, 20, prob = pr)
  
  
  rm(list=ls())
  #set.seed(148)
  N<-50
  meanlambda<-10
  p=.1
  q=.3
  n=c()
  z=c()
  x=c()
  y=c()
  u=c()
  r=c()
  s=c()
  c=c()
  for (j in 1:N){
    y<-rbinom(1,1,p) 
    u<-rbinom(1,1,p) 
    n[j]=rpois(1,meanlambda)
   #n[j]= sample(1:10,1,replace=T)
    #g=NULL
    g=c()
    
    for(i in 1:n[j]){
      z<-rbinom(1,1,p) 
      x =(1-u) *y+u*z
      
      g=c(g,x)
      r[j] =sum(g)
      c[j]=length(g)
    }
    
  }
  
  obs1=cbind(r=r,n=c,t=rep(1,length(r)))
  
  #xtabs(~r+n, data=obs1)
  p=.5
  q=.3
  n=c()
  z=c()
  x=c()
  y=c()
  u=c()
  r=c()
  s=c()
  c=c()
  for (j in 1:N){
    y<-rbinom(1,1,p) 
    u<-rbinom(1,1,p) 
    n[j]=rpois(1,meanlambda)
    #n[j]= sample(1:10,1,replace=T)
    #g=NULL
    g=c()
    
    for(i in 1:n[j]){
      z<-rbinom(1,1,p) 
      x =(1-u) *y+u*z
      
      g=c(g,x)
      r[j] =sum(g)
      c[j]=length(g)
    }
    
  }
  
  obs2=cbind(r=r,n=c,t=rep(2,length(r)))
 
  obs= rbind(obs1,obs2)
  
 c= xtabs(~t+r+n, data=obs)
 c
