# clear working environment
remove(list = ls())  

##load required packages
library(ggpubr)    ##
library(cluster)  ##
library(dendextend)

#get data
data=read.csv("SH_Analysis.csv", header=T)
attach(data)

##create groups
number=data[,"No"]
gov=as.numeric(data[,7])
time=as.numeric(data[,8])
potential=as.numeric(data[,9])
#gov2=as.factor(data[,4])

####################dendogram ################ 
### https://www.datacamp.com/community/tutordatals/hierarchical-clustering-R
matrix = as.matrix(data[,c(7:9)])
rownames(matrix)=data[,1]
s.matrix = as.data.frame(scale(matrix))                                           #s.matrix is scaled matrix
s.matrix = as.matrix(s.matrix)
dist_mat = dist(matrix, method = 'euclidean')                                     #going forward with m`matrix` to put greater emphasis on transf.p. and gov.arrang

#####assessing the optimal number of cluster (k)
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(matrix, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")


####dendrogram with complete method
clust = hclust(dist_mat, method = 'complete')
par(cex.axis = 1, cex.lab= 1, cex.main=1, cex.sub=0.7, mar = c(5.1, 4.1, 4.1, 2.1), mgp=c(2,1,0))
plot(clust)

###adapt dendrogram to optimal number of k=5
library(wesanderson)
col=wes_palette("Darjeeling1", 5, type = c("discrete"))
as.vector(col)
cut_clust = cutree(clust, k=5)                                                  
rect.hclust(clust, k=5, border=col)
abline(h=2.2)
coef.hclust(clust)
data$Cluster=cut_clust