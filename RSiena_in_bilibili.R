suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(progress)
  library(tidyverse)
  library(ggthemes)
  library(RColorBrewer)
  library(RSiena)
}))

setwd("e:/wu/code/RSiena")




#-------------------------------------------------#
#----------------B站视频教程----------------------#
#-------------------------------------------------#
#链接地址
#https://www.bilibili.com/video/av370047525/

#1.所用数据集
#50-actor excerpt from the Teenage Friends and Lifestyle Study data set of West et al.

friend.data.w1 <- read.table("s50_data/s50-network1.dat") %>% 
  as.matrix() 
friend.data.w2 <- read.table("s50_data/s50-network2.dat") %>% 
  as.matrix()
friend.data.w3 <- read.table("s50_data/s50-network3.dat") %>% 
  as.matrix()

# 行为数据
drink <- read.table("s50_data/s50-alcohol.dat") %>% 
  as.matrix()
# 协变量数据
smoke <- read.table("s50_data/s50-smoke.dat") %>% 
  as.matrix()

Friends<- sienaNet(array(c(friend.data.w1,friend.data.w2,friend.data.w3),dim = c(50,50,3)))
Depressionbeh<-sienaDependent(drink,type="behavior")
#变量缩写：constant dyadic二元的
sienaData<-sienaDataCreate(Friends,Depressionbeh)

groupEffects<-getEffects(sienaData)
#网络结构效应
groupEffects<-includeEffects(groupEffects,density,recip,transTrip,test=F,fix=F)
#选择效应
groupEffects<-includeEffects(groupEffects,sameX,interaction1 = "Depressionbeh",test=F,fix=F)
#影响效应
groupEffects<-includeEffects(groupEffects,name = "Depressionbeh",avSim,interaction1 = "Friends")

groupModel<-sienaModelCreate(useStdInits = F,projname ="practice",seed=1243 )
Results<-siena07(data=sienaData,effects=groupEffects,groupModel,batch=F,
                 verbose = F,nbrNodes = 2,useCluster = T,initC = T,returnDeps=T)


#goodness of fit on indegree
gof.id<-sienaGOF(Results,verbose = T,varName = "Friends",IndegreeDistribution,join = T,cumulative=F)
plot(gof.id)

#goodness of fit on behaviour
gof.bd<-sienaGOF(Results,verbose = T,varName = "Depressionbeh",BehaviorDistribution,join = T,cumulative=F)
plot(gof.bd)


