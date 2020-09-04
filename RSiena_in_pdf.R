suppressPackageStartupMessages(suppressWarnings({
  library(igraph)
  library(progress)
  library(tidyverse)
  library(ggthemes)
  library(RColorBrewer)
  library(RSiena)
}))

setwd("e:/wu/code/RSiena")



#---------------------------------------------------------------------#
#----------------10.3.2 无向网络结构演化分析实例----------------------#
#---------------------------------------------------------------------#
#1.所用数据集
#50-actor excerpt from the Teenage Friends and Lifestyle Study data set of West et al.

sym.min <- function(x){ 
  tx <- t(x) 
  return(pmin(x[],tx[])) #pmin（）实现在相同位置上取最小值 
}

friend.data.w1 <- read.table("s50_data/s50-network1.dat") %>% 
  as.matrix() %>% 
  sym.min()
friend.data.w2 <- read.table("s50_data/s50-network2.dat") %>% 
  as.matrix()%>% 
  sym.min()
friend.data.w3 <- read.table("s50_data/s50-network3.dat") %>% 
  as.matrix()%>% 
  sym.min()

# 行为数据
drink <- read.table("s50_data/s50-alcohol.dat") %>% 
  as.matrix()
# 协变量数据
smoke <- read.table("s50_data/s50-smoke.dat") %>% 
  as.matrix()



#2. 数据格式转换
# 定义网络因变量  将多次的网络观测数据合并为一个对象。
friendship<- sienaNet(array(c(friend.data.w1,friend.data.w2,friend.data.w3),
                            dim = c(50,50,3)))
#协变量有两种，一种是不随时间变化的，例如性别；一种是随时间变换的，例如情绪状态。
#第一种协变量用函数coCovar()来标定，第二种协变量用varCovar()来标定。
# 将吸烟行为定义为不随时间变化的协变量
smoke1 <- coCovar(smoke[,1])
# 将喝酒行为定义为随时间变化的协变量
alcohol <- varCovar(drink)



#3. 模型运行
#定义数据集
mydata <- sienaDataCreate(friendship,smoke1,alcohol)
#定义目标函数
myeff<- getEffects(mydata)
#myeff<-includeEffects(myeff,Rate,outRate)
effectsDocumentation(myeff)
fix(myeff)
#构建模型
#print01Report(mydata,myeff,modelname ='s50_sym')
mymodel <- sienaModelCreate(useStdInits=FALSE,projname = "s50_sym")
#模型参数估计
myresult <- siena07( mymodel, data = mydata, effects = myeff)
#估计结果，如图 10-2 所示
myresult




#--------------------------------------------------------------------------#
#----------------10.4.6 网络与行为协同演化分析的 实例----------------------#
#--------------------------------------------------------------------------#

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



# 定义网络因变量
friendship<- sienaNet(array(c(friend.data.w1,friend.data.w2,friend.data.w3),dim = c(50,50,3)))
# 定义行为因变量，type 为"behavior"
drinkingbeh<- sienaNet( drink, type = "behavior" )
#定义协变量
smoke1 <- coCovar( smoke[ , 1 ] )
# 定义数据集
myCoEvolutionData<- sienaDataCreate( friendship, smoke1,
                                       drinkingbeh )
# 定义目标函数
myCoEvolutionEff<- getEffects( myCoEvolutionData )
# 构建模型
myCoEvolutionEff<- includeEffects( myCoEvolutionEff, transTrip,
                                     cycle3)
# 在模型中增加抽烟的同质性影响
myCoEvolutionEff<- includeEffects( myCoEvolutionEff, simX,
                                     interaction1 = "smoke1" )
# 在模型中增加喝酒行为对自我、他我、相似性的交互影响
myCoEvolutionEff<- includeEffects(myCoEvolutionEff, egoX, altX,
                                    simX,
                                    interaction1 = "drinkingbeh" )
# 在模型中增加 友谊网络中的出度，入度等指标对喝酒行为的交互影响
myCoEvolutionEff<- includeEffects( myCoEvolutionEff,
                                     name ="drinkingbeh",indeg, outdeg,interaction1 = "friendship" )
# 列出你所选定的效应函数以便检查有无错误、遗漏
myCoEvolutionEff

# 定义算法集合
myCoEvAlgorithm<- sienaModelCreate( projname = 's50CoEv_3' )
# 模型参数估计
myresult<- siena07( myCoEvAlgorithm, data = myCoEvolutionData,
                       effects = myCoEvolutionEff )
# 估计结果
myresult





