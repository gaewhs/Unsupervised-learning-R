myseeds <-read.table(file.choose(), header = TRUE)

means <- apply(myseeds,2,mean) 
standarddeviations <- apply(myseeds,2,sd)
myseedsSTAN <- scale(myseeds,center=means,scale=standarddeviations)
mydistmatrixSTAN <- dist(myseedsSTAN)
print(mydistmatrixSTAN)
mydistsubmatrixSTAN <- as.matrix(mydistmatrixSTAN)
mydistsubmatrixSTAN <- mydistsubmatrixSTAN[1:10,1:10]
print(mydistsubmatrixSTAN)

myahclustSTAN <-hclust(dist(myseedsSTAN))
plot(myahclustSTAN, main= "Euclidean Distance Dendrogram", hang=-1, cex=0.5)
rect.hclust(myahclustSTAN, k=5)
summary(myahclustSTAN)


myahclustManhattanSTAN <-hclust(dist(myseedsSTAN,method="manhattan"))
plot(myahclustManhattanSTAN, main= "Manhattan Distance Dendrogram", hang=-1, cex=0.5) 
rect.hclust(myahclustManhattanSTAN, k=5)
summary(myahclustManhattanSTAN)

myahclustMaximumSTAN <-hclust(dist(myseedsSTAN,method="maximum"))
plot(myahclustMaximumSTAN, main= "Maximum Distance Dendrogram", hang=-1, cex=0.5) 
rect.hclust(myahclustMaximumSTAN, k=5)
summary(myahclustMaximumSTAN)


#labels2 <-cutree(myahclustSTAN,k=2)
#labels3 <-cutree(myahclustSTAN,k=3)
#labels4 <-cutree(myahclustSTAN,k=4)
#labels5 <-cutree(myahclustSTAN,k=5)

myahclustSTANsingle <-hclust(dist(myseedsSTAN),method="single")
myahclustSTANward <-hclust(dist(myseedsSTAN),method="ward.D2")
par(mfrow=c(3,1))
plot(myahclustSTAN,hang=-1, main = "Complete linkage HC")
plot(myahclustSTANsingle,hang=-1, main = "Single linkage HC")
plot(myahclustSTANward,hang=-1, main = "Ward linkage HC")



