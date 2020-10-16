myseeds <-read.table(file.choose(), header = TRUE)
summary(myseeds)
ls(myseeds)
dim(myseeds)

mydistmatrix<-dist(myseeds) #dist returns Euclidean distance by default
mydistmatrix
mydistsubmatrix<-as.matrix(mydistmatrix)
mydistsubmatrix<-mydistsubmatrix[1:10,1:10]  #prints a 10x10
mydistsubmatrix

myahclust <- hclust(dist(myseeds)) #Step 3.	Derive an agglomerative clustering using the distance matrix and the "complete" linkage
plot(myahclust,main= "Euclidean Distance Dendrogram", hang=-1, cex=0.5)

myahclustManhattan <- hclust(dist(myseeds,method = "manhattan"))
plot(myahclustManhattan, main= "Manhattan Distance Dendrogram", hang=-1, cex=0.5) 

myahclustMaximum <-hclust(dist(myseeds,method="maximum"))
plot(myahclustMaximum, main= "Maximum Distance Dendrogram", hang=-1, cex=0.5) 


