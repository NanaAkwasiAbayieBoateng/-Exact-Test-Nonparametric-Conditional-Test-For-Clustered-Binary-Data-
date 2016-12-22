rm(list=ls())
library(CorrBin)
data(egde)
edge <- egde  
data(shelltox)



##########################################################################################################
####### Generate random variaavles R  with the  ##########################################################
####### Inverse transform method from the logistic  distribution  #####################################

beta1=0.4553394;
beta2=0.0042140;
dose=c(0,25)
#beta1=4.9568
#beta2=-5.54101
#beta=beta1+beta2*(dose/100);
beta=beta1+beta2*(dose);

logistic.pdf<-function(n,beta){
  v=c()
  for(r in 0:n){
    
     k =0:(n-r)
     
      v[r+1]=choose(n,r)*sum((-1)^k*choose(n-r,k)*(2/(1+exp((beta)*log(r+k+1)))))
  
  }
  v = pmax(pmin(v,1),0)
  return(v)
}


cdf=function(n,beta){c=c();c=cumsum(logistic.pdf(n,beta)); return(c)  }

cdf(50,2)



generate.r=function(beta,n){
  
  c=c(0,cdf(n,beta));
  u=runif(1);
  
  for (i in 2:length(c)){
    if( (u>=c[i-1])&(u<c[i]) ){
      r=(i-2)
      
      
    }
  }
  return(list(n=n,r=r,u=u))
  
}
generate.r(beta=beta[1],n=5)
generate.r(beta=beta[2],n=5)

s.pvalue=c()

for (i in 1:100){
  
  
  n=50
  s1=sapply(rpois(n,lambda = 5),generate.rq,p=0.1,rho=0.3)
  s2=sapply(rpois(n,lambda = 5),generate.rq,p=0.1,rho=0.3)
  
  
  obs1=cbind(r=unlist(s1[2,]),n=unlist(s1[1,]),Trt=rep(1,n))
  obs2=cbind(r=unlist(s2[2,]),n=unlist(s2[1,]),Trt=rep(2,n))
  
  
  obs= data.frame(rbind(obs1,obs2))
  
  c= xtabs(~Trt+r+n, data=obs)
  
  
  
  
  s.pvalue[i]=Homogeneity(c,nsim=1000)
  
  avg.pvalue=mean(s.pvalue)
}
avg.pvalue


##########################################################################################################
####### Generate random variaavles R  with the  ##########################################################
####### Inverse transform method from q power distribution  ##############################################

qpower_pdf <- function(p, rho, n){
  q <- 1-p
  gamma <- log2(log(q^2+rho*q*(1-q))/log(q))
  pr <- numeric(n+1)
  for (r in 0:n){
    k <- 0:r
    pr[r+1] <- choose(n,r) * sum( (-1)^k * choose(r,k) * q^((n-r+k)^gamma))
  }
  pr <- pmax(pmin(pr,1),0)  #to account for numerical imprecision
  pr
}



cdfq=function(p, rho, n){c=c();c=cumsum(qpower_pdf(p,rho,n)); return(c)  }

generate.rq=function(p, rho, n){
  
  c=c(0,cdfq(p, rho, n));
  u=runif(1);
  
  for (i in 2:length(c)){
    if( (u>=c[i-1])&(u<c[i]) ){
      r=(i-2)
      
      
    }
  }
  return(list(n=n,r=r,u=u))
  
}


# generate.rq(p=0.9,rho=0.9,n=5)
# cdfq(p=0.9,rho=0.9,n=8)
# sapply(rpois(n=5,lambda = 5),generate.rq,p=0.1,rho=-0.9)
# p=0.9
# q=0.1
# rho=0.9
# log2(log(q^2+rho*q*(1-q))/log(q))



s.pvalue=c()

for (i in 1:1000){

   
n=50
s1=sapply(rpois(n,lambda = 5),generate.rq,p=0.1,rho=0.9)
s2=sapply(rpois(n,lambda = 5),generate.rq,p=0.9,rho=0.9)


obs1=cbind(r=unlist(s1[2,]),n=unlist(s1[1,]),Trt=rep(1,n))
obs2=cbind(r=unlist(s2[2,]),n=unlist(s2[1,]),Trt=rep(2,n))


obs= data.frame(rbind(obs1,obs2))

c= xtabs(~Trt+r+n, data=obs)




s.pvalue[i]=Homogeneity(c,nsim=1000)

avg.pvalue=mean(s.pvalue)
}
avg.pvalue






##########################################################################################################
####### Generate random variaavles R  with the  ##########################################################
####### Inverse transform method from the Beta-Binomial distribution  #####################################


betabin_pdf <- function(p, rho, n){
  a <- p*(1/rho-1)
  b <- (1-p)*(1/rho-1)
  k <- 0:n
  pr <- choose(n, k)*beta(a+k, b+n-k)/beta(a,b)
  
  return(pr)
} 

betabin_pdf(0.2,.3,5)

cdfbb=function(p, rho, n){c=c();c=cumsum(betabin_pdf(p,rho,n)); return(c)  }
cdfbb(p=0.2,rho=0.3,n=8)

generate.rbb=function(p, rho, n){
  
  c=c(0,cdfq(p, rho, n));
  u=runif(1);
  
  for (i in 2:length(c)){
    if( (u>=c[i-1])&(u<c[i]) ){
      r=(i-2)
      
      
    }
  }
  return(list(n=n,r=r,u=u))
  
}


generate.rbb(p=0.2,rho=0.3,n=8)


s.pvalue=c()

for (i in 1:100){
  
  
  n=50
  s1=sapply(rpois(n,lambda = 5),generate.rbb,p=0.1,rho=0.3)
  s2=sapply(rpois(n,lambda = 5),generate.rbb,p=0.1,rho=0.3)
  
  
  obs1=cbind(r=unlist(s1[2,]),n=unlist(s1[1,]),Trt=rep(1,n))
  obs2=cbind(r=unlist(s2[2,]),n=unlist(s2[1,]),Trt=rep(2,n))
  
  
  obs= data.frame(rbind(obs1,obs2))
  
  c= xtabs(~Trt+r+n, data=obs)
  
  
  
  
  s.pvalue[i]=Homogeneity(c,nsim=1000)
  
  avg.pvalue=mean(s.pvalue)
}
avg.pvalue


##########################################################################################################
####### plots of intracluster correlation  ##########################################################
####### ##########################################################  #####################################


p=c(0.4948332,0.5035944,0.5005315)
rho=c(0.3,0.6,0.9)
plot(rho,p)
