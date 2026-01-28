# julia sveen

library(cluster)
library(factoextra)
library(caret)
library(dplyr)
library(reshape2)
library(ggplot2)
library(readr)
getwd()
setwd("/Users/juliasveen/Downloads")

###IMPORT NCAA_Passing DATASET###
WR_Clustering=read_csv("WR_Clustering.csv")


###SELECT ONLY THOSE VARIABLES THAT WE WANT TO CLUSTER###
NCAA_WR=WR_Clustering%>%select(8,11,12,13,15,16,17,19)
head(NCAA_WR)

###STANDARDIZE ALL VARIABLES TO THEIR Z-SCORES USING SCALE###
NCAA_WR=scale(NCAA_WR)
head(NCAA_WR)

###CREATE A WITHIN SUM OF SQUARES PLOT###
###WE CHOOSE SEED NUMBER BASED ON THE ELBOW OF THE PLOT (WHERE IT STARTS TO LEVEL OUT)###
wssplot <- function(NCAA_WR, nc=15, seed=5500){ # dr pifer did 4400
  wss <- (nrow(NCAA_WR)-1)*sum(apply(NCAA_WR,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(NCAA_WR, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
###VIEW PLOT###
wssplot(NCAA_WR,nc=15)
###SELECTION OF CLUSTERS USING THIS METHOD IS SOMEWHAT ARBITRARY###

###ELBOW WAS AT 4 SO WE GO WITH A 4-MEANS CLUSTER###
set.seed(5500) # dr pifer did 4400
K5=kmeans(NCAA_WR,centers=5,nstart=50)
K5

###CREATE A PLOT###
clusplot(NCAA_WR,K5$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
###CREATE A PRETTIER PLOT###
fviz_cluster(K5,data=NCAA_WR)

###SEE AVERAGE Z-SCORES FOR EACH CLUSTER###
K5$centers
K5T=data.frame(K5$centers,cluster=seq.int(nrow(K5$centers)))
K5T

###ADD THE CLUSTERS BACK INTO THE ORIGINAL DATASET###
WR_Clusters=data.frame(WR_Clustering,K5$cluster)
head(WR_Clusters)
Upcoming_Draft=WR_Clusters%>%filter(To==2023)

###CREATE BAR CHART FOR RESULTS###
###WE HAVE TO SWITCH THIS FROM WIDE DATA TO LONG DATA###
#library(reshape2)
centers.long=melt(K5T,id.vars="cluster")
centers.long
###NOW WE CREATE THE GROUPED BAR CHART###
#library(ggplot2)
ggplot(centers.long,aes(x=variable,y=value,fill=factor(cluster)))+
  geom_bar(stat="identity",position="dodge",width=0.75)+ylab("z-score")+
  labs(fill="Cluster #")
###WE HAVE TO USE FACTOR TO LET IT KNOW THAT CLUSTER IS A FACTOR LABEL, NOT A NUMERICAL VALUE###
###THE DODGE CODE PUTS THE BARS BESIDE EACH OTHER AND THE WIDTH ADJUSTS THE WIDTH OF THE COLUMNS TO ADD MORE SPACE B/W VARIABLE GROUPS###

Draft_WR=WR_Clusters %>% 
  group_by(K5.cluster) %>% summarize(Round=mean(Round,na.rm=TRUE),
                                     Overall=mean(Overall,na.rm=TRUE))
