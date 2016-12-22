alpha=c(0.1,0.05,0.01,0.001)
n=100
s=seq(1,n,1)
p1=1-(1-0.1)^s
p2=1-(1-0.05)^s
p3=1-(1-0.01)^s
p4=1-(1-0.001)^s

plot(s, p1, lwd=2, col="blue",pch=2)
lines(s, p2, type="l", lwd=2, col="red")
lines(s, p3, type="l", lwd=2, col="green")
lines(s, p4, type="l", lwd=2, col="pink")
# set up graph
xrange <- range(s)
yrange <- round(range(p))
colors <- rainbow(length(alpha))
p=cbind(p1,p2,p3,p4)

plot(xrange, yrange, type="n",
     xlab="s",
     ylab="P(at least one Type I Error)")
#panel.first=grid() 
# add power curves
opts = c("p","l","o","b")


for (i in 1:length(alpha)){
  lines(s, p[,i], type=opts[i], lwd=1, col=colors[i],cex=0.5)
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.10), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")
title("Graph of different type-I error rates levels\n")
#legend("topright",1,alpha, title="alpha", opts)
legend(xrange[1], yrange[2], alpha, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="alpha")

#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/mult.png")
#matplot(s,p, pch=c(15,16,17,20),col = rainbow(4),ylab="P(at least one Type I Error)",xlab="n", lwd=1,cex=0.5,lty=1"4")
linetype <- c(1:length(alpha)) 
plotchar <- seq(17,17+length(alpha)-1,1)
opts = c("p","l","o","b")
matplot(s,p,lwd=1,lty=linetype,col = colors,pch=plotchar,type=opts,cex=0.6,xlab="s",ylab="P(at least one Type I Error)")
# add a legend 
title("Graph of different type-I error rates levels", "")
# add a legend 
legend(xrange[1], yrange[2], alpha, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="alpha")

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.10), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")

box(lty = 'solid', col = 'black')




#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/mult.png")
#x11()
getwd()
png("C:/Users/Gucci148/Desktop/Trend/mult.png")
colors <- rainbow(length(alpha))
linetype <- c(1:length(alpha)) 
plotchar <- seq(17:20)
opts = c("b","p","l","o")
plot(xrange, yrange,
     xlab="s",
     ylab="P(at least one Type I Error)")

# Plot solid circles with solid lines
points(s, p1, type=opts[1], pch=plotchar[1],lwd=1,cex=0.5,col=colors[1])
# Add open squares with dashed line, with heavier line width
points(s, p2, type=opts[2], pch=plotchar[2],  lty=2, lwd=1,cex=0.5,col=colors[2])

points(s, p3, type=opts[3], pch=plotchar[3],   # Diamond shape
       lty="dotted", cex=0.5, col=colors[3] ,             # Dotted line, double-size shapes
       lwd=1) 

points(s, p4, type=opts[4], pch=plotchar[4],  lty=2, lwd=1,cex=0.5,col=colors[4])

#legend("topright", 8, alpha, fill = rainbow(4))
# add annotation (grid lines, title, legend)
# add a title and subtitle 
title("Graph of various  type-I error rates ", "Number of Test")
# add a legend 
legend(xrange[1]-0.5, yrange[2], alpha, cex=0.8,
       pch=plotchar, lty=linetype, title=expression(alpha),col=colors)

abline(v=0, h=seq(0,yrange[2],0.10), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")

# add a legend 

box(lty = 'solid', col = 'black')
dev.off()


###########makes you see the plot in r and saves the graph in the 
#######################  same folder as the r program
#dev.copy(jpeg,filename="mult.jpg");
#dev.off ();



# Create Line Chart

#png("~/Documents/memphisclassesbooks/RESEARCH/Trend/mult.png")
# get the range for the x and y axis 
xrange <- range(s)
yrange <- round(range(p))
# set up the plot 
plot(xrange, yrange, type="n", xlab="s",
     ylab="P(at least one Type I Error)" ) 
colors <- rainbow(length(alpha))
linetype <- c(1:length(alpha)) 
plotchar <- seq(18,18+length(alpha),1)
opts = c("p","l","o","b")
# add lines 
for (i in 1:length(alpha)) { 
  
  lines(s, p[,i], type=opts[i], lwd=1.0,
        lty=linetype[i], col=colors[i], pch=plotchar[i],cex=0.5) 
  
} 

# add a title and subtitle 
title("Graph of different type-I error rates levels", "")

# add a legend 
legend(xrange[1], yrange[2], alpha, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="alpha")


# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.10), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")

box(lty = 'solid', col = 'black')







# Create Line Chart

# get the range for the x and y axis 
xrange <- range(s)
yrange <- round(range(p))
# set up the plot 
plot(xrange, yrange, type="n", xlab="s",
     ylab="P(at least one Type I Error)" ) 
colors <- rainbow(length(alpha))
linetype <- c(1:length(alpha)) 
plotchar <- seq(18,18+length(alpha),1)
opts = c("p","l","o","b")
# add lines 
for (i in 1:length(alpha)) { 
  
  points(s, p[,i], type=opts[i], lwd=1.0,
        lty=linetype[i], col=colors[i], pch=plotchar[i],cex=0.5) 
  
} 

# add a title and subtitle 
title("Graph of different type-I error rates levels", "")

# add a legend 
legend(xrange[1], yrange[2], alpha, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="alpha")


# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.10), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")

box(lty = 'solid', col = 'black')





#dev.off();

matplot(s,p, type="l",col = rainbow(4),ylab="P(at least one Type I Error)",xlab="n", lwd=2,pch=1:4)
legend(80, 0.95, as.character(alpha), fill = rainbow(4))
# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.10), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")

dev.off();



set.seed(123)
x <- rnorm(50, mean = c(rep(0, 25), rep(3, 25)))
p <- 2*pnorm(sort(-abs(x)))
#p=runif(6)
p1=p.adjust(p, method = 'fdr', n = length(p))
p2=p.adjust(p, method = 'holm', n = length(p))
p3=p.adjust(p, method = 'hochberg', n = length(p))
p4=p.adjust(p, method = 'hommel', n = length(p))
p5=p.adjust(p, method = 'bonferroni', n = length(p))
p6=p.adjust(p, method = 'BH', n = length(p))
p7=p.adjust(p, method = 'BY', n = length(p))
p8=p.adjust(p, method = 'none', n = length(p))
pp=cbind(p1,p2,p3,p4,p5,p6,p7,p7)
pp

matplot(p, pp, ylab="p.adjust", type = "l", asp = 1, lty = 1:8,
        main = "P-value adjustments")
legend(0.7, 0.6, c("fdr","holm" ,"hochberg", "hommel", "bonferroni", "BH", "BY", "none"),
       col = rainbow(8), lty = 1:8,fill = rainbow(8))


# set up graph
xrange <- range(p)
yrange <- round(range(pp))
# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],0.1), lty=2, col="grey89")
abline(h=0, v=seq(0,xrange[2],10), lty=2,
       col="grey89")

plot(rank(p),p,lty=2,pch=20,type="l")



p.adjust(p, n = 70)



#######################
### Q-VALUE ################
install.packages("qvalue")
library(qvalue)
data(hedenfalk)
pvalues <- hedenfalk$p
qobj <- qvalue(p = pvalues)

Qvalue(pValues=p, lambda=seq(0, 0.9, 0.05), pi0.method="smoother",
       fdr.level=NULL, robust=FALSE, smooth.df=3, smooth.log.pi0=FALSE,
       silent=FALSE)



p=c(0.49135,0.26335,0.000005)
p.adjust(p, method = p.adjust.methods, n = length(p))
p.adj
p.adjust.M <- p.adjust.methods[p.adjust.methods != "none"]
p.adj <- sapply(p.adjust.M, function(meth) p.adjust(p, meth))
p.adj

plot(rank(p),p,lty=2,pch=20,type="l")


library(BiocInstaller)
biocLite()
library(qvalue)
data(hedenfalk)
pvalues <- hedenfalk$p
pvalues=c(0.49135,0.26335,0.000005)
qobj <- qvalue(p = pvalues)



## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite()
install.packages("mutoss")
install.packages("multtest")
library(mutoss)
library(multtest)
install.packages("devtools")
library("devtools")
install_github("jdstorey/qvalue")
p=c(0.49135,0.26335,0.000005)
qvalue(p)

#p=runif(10)
p=c(0.49135,0.26335,0.000005)
qvalue(p, lambda=seq(0,0.99,0.05), pi0.method="smoother", fdr.level=0.05, robust=T, gui=FALSE, 
       smooth.df=3, smooth.log.pi0=T)


###################################################################
###### Adaptive Benjamini Hochberg2001####################################
p=c(0.49135,0.26335,0.000005)
adaptiveBH(pValues=p, alpha=0.05, silent=FALSE)




######################################################################################################
###### Storey-Taylor-Siegmund's (2004) adaptive step-up procedure####################################

ABH=adaptiveSTS(p, alpha=0.05, lambda=0.5, silent=TRUE)
ABH

adjPValuesPlot(adjPValues=ABH$adjPValues, alpha=0.05)

######################################################################################################
###### Benjamini-Hochbergs Linear Step-Up Procedure.####################################
BH(pValues=p, alpha=0.05, silent=FALSE)

###################################################################
######  Benjamini Yekutieli 2001####################################
p=c(0.49135,0.26335,0.000005)
BY(pValues=p, alpha=0.05, silent=FALSE)


hommel(p,alpha=0.05)
bonferroni(p,alpha=0.05)
holm(p,alpha=0.05)
hochberg(p,alpha=0.05)
sidak(pValues=p, alpha=0.05, silent=FALSE)
SidakSD(pValues=p, alpha=0.05, silent=FALSE)


install.packages("fdrtool")
library(fdrtool)
