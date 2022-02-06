# Read data (size.dat) from your directory and attach in R for use.

size = read.table('C:/Users/user/Desktop/고려대/3-1/다변량통계분석/mva/size.dat',header=T)

# Scatterplot matrix 
pairs(size[,-1])


par(mfrow=c(3,1))

# Single Linkage
plot(hclust(dist(size[,-1]),method="single"),labels=size[,1],xlab=NA,ylab="Distance",main="Single linkage dendrogram")

# Complete Linkage
plot(hclust(dist(size[,-1]),method="complete"),labels=size[,1],xlab=NA,ylab="Distance",main="Complete linkage dendrogram")

# Group Average
plot(hclust(dist(size[,-1]),method="average"),labels=size[,1],xlab=NA,ylab="Distance",main="Group Average dendrogram")





##############################################################################################

# Read data (tibetan.dat) from your directory and attach in R for use.
skull = read.table('C:/Users/user/Desktop/KU/3-1/다변량통계분석/mva/tibetan.dat')

par(mfrow=c(2,1))

# Complete Linkage
plot(hclust(dist(skull),method="complete"),xlab=NA,ylab="Distance",main="Complete linkage dendrogram ")

# Group Average
plot(hclust(dist(skull),method="average"),xlab=NA,ylab="Distance",main="Group Average dendrogram ")



# K-means method (k=2)
skull.kmeans = kmeans(skull,2)
skull.kmeans

skull.kmeans2 = kmeans(skull,2,nstart=10)
skull.kmeans2

# Principal component analysis (used for making the following figures);
skull.pca=princomp(skull,cor=T)
summary(skull.pca) # 2개의 주성분만 사용해도 됨.
skull.pca2=prcomp(skull,center=T,scale=T) # with real data

#Plot of first two principal component scores by clutering
par(mfrow=c(1,2))
plot(skull.pca$scores[,1], skull.pca$scores[,2], xlab="PC1",ylab="PC2",type="n",lwd=2)
text(skull.pca$scores[,1], skull.pca$scores[,2],labels=skull.kmeans$cluster,col="blue",cex=.8,lwd=2)

plot(skull.pca2$x[,1], skull.pca2$x[,2], xlab="PC1",ylab="PC2",type="n",lwd=2)
text(skull.pca2$x[,1], skull.pca2$x[,2],labels=skull.kmeans$cluster,col="blue",cex=.8,lwd=2)



#Plot of first two principal component scores by original grouping(초기 grouping)
group=c(rep(1,17), rep(2,15))
plot(skull.pca$scores[,1], skull.pca$scores[,2], xlab="PC1",ylab="PC2",type="n",lwd=2)
text(skull.pca$scores[,1], skull.pca$scores[,2],labels=group,col="blue",cex=.8,lwd=2)


