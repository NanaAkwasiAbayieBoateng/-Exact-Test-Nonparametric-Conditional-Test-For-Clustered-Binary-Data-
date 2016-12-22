######################################## 
########################################
############### fixed row and column sums             #########################
set.seed(144)
observed <- rbind(c(3,0,1,1),c(2,1,0,1),c(0,1,3,1),c(0,1,4,1))
x <- rep(1:nrow(observed), rowSums(observed))
y <- rep(1:ncol(observed), colSums(observed))
samples <- lapply(1:100, function(a) table(x, sample(y)))

x=c(0:9,0:9)
x=rep(0:9,2)
x=rep(0:1,10)
y <- rep(1:ncol(observed), colSums(observed))
samples <- lapply(1:10, function(a) table(x, sample(y)))

colSums(samples[[1]])
rowSums(samples[[1]])