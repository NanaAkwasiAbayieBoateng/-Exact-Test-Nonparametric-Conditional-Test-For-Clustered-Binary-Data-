#######################################################################
####### Dose group control and High ####################################
rm(list=ls())
library(CorrBin)
library(vcd)
data(shelltox)
shelltox=shelltox
head(shelltox)
#install.packages("CorrBin")
summary(shelltox)

ftable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))


#####################################
##### 0 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][1,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][1,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][1,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][1,])

#####################################
##### 1 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][2,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][2,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][2,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][2,])

#####################################
##### 2 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][3,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][3,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][3,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][3,])


#####################################
##### 3 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][4,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][4,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][4,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][4,])
#####################################
##### 4 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][5,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][5,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][5,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][5,])

#####################################
##### 5 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][6,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][6,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][6,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][6,])


#####################################
##### 6 response for c,l,m,h#########
rbind(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,1][7,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,2][7,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,3][7,],
      xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,4][7,])


#####################################
##### structable performs the task above for c,l,m,h#########
structable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))

#####################################
##### selects a 0 response for c,l,m,h#########
structable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox))[1,]



#####################################
##### selecting response for c,h #########
data=structable(xtabs(Freq~NResp+ClusterSize+Trt,data=shelltox)[,,c(1,4)])

dim(data)[1]
dim(data)[2]
dim(data)[3]

