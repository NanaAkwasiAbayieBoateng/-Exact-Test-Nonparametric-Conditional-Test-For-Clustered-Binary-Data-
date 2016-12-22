rm(list=ls())
library(CorrBin)
library(lattice)
#input data
data(egde)
edge <- egde  # rename dataset because of typo in the code
data(shelltox)
shelltox=shelltox
data(dehp)
dehp <- CMData(dehp, trt="Trt", nresp=c("NResp.1","NResp.2","NResp.3"))
str(dehp)
DEHP=data.frame(Trt=dehp[,1],ClusterSize=dehp[,2],NResp=dehp[,3]+dehp[,4],Freq=dehp[,6])
xtabs(Freq~Trt+NResp+ClusterSize, data=edge)
xtabs(Freq~NResp+ClusterSize+Trt, data=edge)
edge=shelltox

# mean cluster size
by(edge, edge["Trt"], function(x)sum(x$ClusterSize*x$Freq)/sum(x$Freq))


by(edge, function(x)sum(x$ClusterSize*x$Freq)/sum(x$Freq))

####total mean cluster size

sum(edge$ClusterSize*edge$Freq)/sum(edge$Freq)

fisher.test(xtabs(Freq~ClusterSize+Trt, data=edge),workspace = 2000000,B=1000)

#fisher.test(xtabs(Freq~ClusterSize+NResp, data=edge),workspace = 200000,B=1000)

chisq.test(xtabs(Freq~ClusterSize+NResp, data=edge),B=1000,simulate.p.value = TRUE)

# prob(affected)
by(edge, edge["Trt"], function(x)sum(x$NResp*x$Freq)/sum(x$ClusterSize*x$Freq))

color=rainbow(20)
y=as.vector(by(edge, edge["Trt"], function(x)sum(x$NResp*x$Freq)/sum(x$ClusterSize*x$Freq)))
x=c(0,25,50,100)
plot(x,y,ylab="Marginal Probability ",xlab="Dose Groups",axes=T,col=color[14])
lines(x,y,type="l")

wilcox.test(c(0.1348837,0.1353383, 0.3377483,0.2277228))

wilcox.test(c(0.1348837,0.1353383, 0.3377483,0.2277228))
t.test(c(0.1348837,0.1353383, 0.3377483,0.2277228))
wilcox.test(c(0.1834862,0.1773585,0.25, 0.7154812))

t.test(c(0.1834862,0.1773585,0.25, 0.7154812))
x=(c(0.1834862,0.1773585,0.25, 0.7154812))
x=c(0.1348837,0.1353383, 0.3377483,0.2277228)
g=c(1,2,3,4)


kruskal.test(x=c(0.1834862,0.1773585,0.25, 0.7154812),g)
jonckheere.test(x+0.3*g,alternative = 'increasing', g,nperm=1000)
jonckheere.test(x+0.3*g,alternative = 'increasing', g)
jonckheere.test(x+0.3*g,alternative = 'two.sided', g,nperm=1000)
cor.test(x, g, method="k")

####Joncheere Terpstra test of sample proportions
jonckheere.test(x+0.3*g,alternative = 'two.sided', g)
jonckheere.test(x+0.3*g,alternative = 'increasing', g)


t.test(c(0.1348837,0.1353383, 0.3377483,0.2277228), alternative="greater", mu=0.0)
t.test(c(0.1348837,0.1353383, 0.3377483,0.2277228))
binom.test(c(0.1348837,0.1353383, 0.3377483,0.2277228))

# probability of at least one fetus affected
# prob(>0 affected in cluster)
by(edge, edge["Trt"], function(x)sum((x$NResp>0)*x$Freq)/sum(x$Freq))[[1]]

y=as.vector(by(edge, edge["Trt"], function(x)sum((x$NResp>0)*x$Freq)/sum(x$Freq)))
y1=as.vector(by(edge, edge["Trt"], function(x)sum((x$NResp>1)*x$Freq)/sum(x$Freq)))
y2=as.vector(by(edge, edge["Trt"], function(x)sum((x$NResp>2)*x$Freq)/sum(x$Freq)))
y3=as.vector(by(edge, edge["Trt"], function(x)sum((x$NResp>3)*x$Freq)/sum(x$Freq)))

x=c(0,25,50,100)
colors <- rainbow(4)
plot(x,y,ylab="Probability of at least one Malformation",xlab="dose",axes=T)
lines(x, y, type="l", lwd=2, col=colors[1])
lines(x, y1, type="l", lwd=2, col=colors[2])
lines(x, y2, type="l", lwd=2, col=colors[3])
lines(x, y3, type="l", lwd=2, col=colors[4])
p=cbind(y,y1,y2,y3)
xrange <- range(x)
yrange <- round(range(p))
png("~/Documents/memphisclassesbooks/RESEARCH/Trend/edge1.png")
matplot(x,p, type="l",col = rainbow(4),ylab="Marginal Probaility",xlab="dose", lwd=2,main="Marginal Probability of EDGE Data")
legend("topright", 8, c("p1","p2","p3","p4"), fill = colors)
# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.10), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")
#axis(1,at=c(0,20,50,100),labels=paste(c(0,20,50,100)))
#axis(2,at=c(0,0.2,0.4,0.6,0.8,1),labels=paste(c(0,0.2,0.4,0.6,0.8,1)))
box(lty = 'solid', col = 'black')
dev.off();

#hist(ps, breaks="fd", col="gray",main="Distribution of of Simulated Values ",xlab="log P(Y)",prob=TRUE,ylab="P(T=t)")
#abline(v =pobs,col="blue")
#box(lty = 'solid', col = 'black')
#dev.off();





colors=rainbow(4)
png("~/Documents/memphisclassesbooks/RESEARCH/Trend/stripedge.png")
stripchart(I(NResp/ClusterSize)~Trt, cex=sqrt(egde$Freq), data=egde,col=colors, pch=1,
           method="jitter",xlab="Dose Groups", vertical=TRUE, ylab="Proportion affected",main="Stripchart Representation of EDGE Data")

box(lty = 'solid', col = 'black')
dev.off();

fit=lm(y~x)
summary(fit)
fitted(fit)
plot(x,y)
abline(fit)

reg1 <- lm(y~x)
par(cex=.8)
color=rainbow(2)
png("~/Documents/memphisclassesbooks/RESEARCH/Trend/fitedge.png")
plot(x,y,col=color[1],xlab="Dose Groups",ylab="Marginal Proportion",main="Marginal Proportion and Estimated Marginal Proportions")
abline(reg1,col=color[1])
lines(x,y,type="o",col=color[2],lw=2)
legend("topright", 8, c("fit","MP"), fill = color,lw=2)
box(lty = 'solid', col = 'black')
dev.off();

# Total probability of at least one fetus affected
#prob(>0 affected in cluster)
sum((edge$NResp>0)*edge$Freq)/sum(edge$Freq)


# prob(>0 affected) by clustersize
paff <- by(edge, edge[c("Trt","ClusterSize")], function(x)sum((x$NResp>0)*x$Freq)/sum(x$Freq))[,]
sstab <- xtabs(Freq~ClusterSize, data=edge, subset=ClusterSize>4 & ClusterSize<14)
ssprob <- sstab/sum(sstab)
apply(paff[,as.character(5:13)], 1, function(x) x %*% ssprob) 

dim(paff)
by(edge, edge["Trt"], function(x)sum(x$ClusterSize*x$NRes))


###number of clusters in each treatment group
apply(xtabs(~Trt+ClusterSize,data=edge),1,sum)
sum(apply(xtabs(~Trt+ClusterSize,data=edge),1,sum))

xtabs(~Trt+ClusterSize,data=edge)
### sample size

by(edge, edge["Trt"], function(x)sum(x$ClusterSizex*x$NRes))

by(edge, edge["Trt"], function(x)length(x$ClusterSize))

by(edge, edge["Trt"], function(x)(x$ClusterSize*x$NRes))

by(edge, edge["Trt"], function(x)sum(x$NResp*x$Freq)/sum(x$ClusterSize*x$Freq))

### number of responses

by(edge, edge["Trt"], function(x)sum(x$NRes))


#Total number of live foetuses in a litter.
b1=by(edge, edge["Trt"], function(x)sum(x$ClusterSize*x$Freq))
sum(b1)
edge$ClusterSize*edge$Freq)
length(edge$ClusterSize)
table(edge$ClusterSize)

# of responses in each treatment group
b2=by(edge, edge["Trt"], function(x)sum(x$NResp*x$Freq))
sum(b2)
(b2/b1)*100
(sum(b2)/sum(b1))*100
sh <-edge
levels(sh$Trt)<-c("Control","Low","Medium", "High")
str(sh)

mc.test.chisq(sh)

sh.mc <- mc.est(sh)

print(xyplot(Prob~NResp|factor(ClusterSize), groups=Trt, data=sh.mc, subset=ClusterSize>0 & ClusterSize<13,
              type="l", as.table=TRUE, auto.key=list(columns=4, lines=TRUE, points=FALSE),
             xlab="Number of responses", ylab="P(R=r|N=n)"))




panel.cumsum <- function(x,y,...){
  x.ord <- order(x)
   panel.xyplot(x[x.ord], cumsum(y[x.ord]), ...)}
print(xyplot(Prob~NResp|factor(ClusterSize), groups=Trt, data=sh.mc,
               subset=ClusterSize>0&ClusterSize<13, type="s",
                panel=panel.superpose, panel.groups=panel.cumsum,
                as.table=T, auto.key=list(columns=4, lines=T, points=F),
                xlab="Number of responses", ylab="Cumulative Probability R(R>=r|N=n)",
               ylim=c(0,1.1)))



set.seed(4461)
(so.res <- trend.test(sh, test="SO", R=50, control=soControl(eps=0.1, max.directions=40)))


hist(attr(so.res, "boot")$t[,1], freq=FALSE, xlab="Statistic", ylab="Density", main="")
 points(so.res$statistic, 0, pch="*",col="red", cex=3)
 
 
 trend.test(sh, test="RS")
 
 
 sh.SO.est <-SO.mc.est(sh, control=soControl(eps=0.1, max.directions=40))
 str(sh.SO.est)
 
 
 
 
 attr(*, "loglik")= num -124
 attr(*, "converge")= Named num 0.0752 18
attr(*, "names")= chr "rel.error" "n.iter"
print(xyplot(Prob~NResp|factor(ClusterSize), groups=Trt, data=sh.SO.est,
                 subset=ClusterSize<13, type="s",
                 panel=panel.superpose, panel.groups=panel.cumsum,
                 as.table=T, auto.key=list(columns=4, lines=T, points=F),
                 xlab="Number of responses", ylab="Cumulative Probability R(R>=r|N=n)",
                 ylim=c(0,1.1), main=""))


NOSTASOT(sh, test="RS")
NOSTASOT(sh, test="SO")
NOSTASOT(sh, test="GEE")
GEE.trend.test(cbdata, scale.method = c("fixed", "trend", "all"))
GEE.trend.test(edge, scale.method ="trend")
GEE.trend.test(edge, scale.method ="fixed")
GEE.trend.test(edge, scale.method ="all")
GEE.trend.test(edge, scale.method ="all")