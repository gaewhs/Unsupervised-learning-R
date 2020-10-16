myseeds <-read.table(file.choose(), header = TRUE)
dim(myseeds)
summary(myseeds)
#to normalize:
minimum <- apply(myseeds,2,min) 
maximum <- apply(myseeds,2,max) 
myseedsNORM <- scale(myseeds,center=minimum,scale=(maximum-minimum))
summary(myseedsNORM)
names(myseeds)

set.seed(1)
myKmeans1 <- kmeans(myseedsNORM, 1, nstart=50)

#plot the clustering
with(as.data.frame(myseedsNORM), pairs(as.data.frame(myseedsNORM), col=c(1:11)[myKmeans1$cluster]))

#size of the clusters (number of points in each cluster), sum of squares vector (withinss -vector of within-cluster sum of squares, one component per cluster), total sum of squares
str(myKmeans1)

set.seed(42)
myKmeans3multistart <- kmeans(myseedsNORM, 3, nstart=50)
str(myKmeans3multistart)
with(as.data.frame(myseedsNORM), pairs(as.data.frame(myseedsNORM), col=c(1:11)[myKmeans3multistart$cluster]))
myKmeans3multistart



#Finding the best k
bestk=0 
besttotwithinss=999999999
mytotwithinss <- NULL
for(auxk in  1:15){
  set.seed(13)
  myKmeansauxkmultistart <- kmeans(myseedsNORM, auxk, nstart=50)
  mytotwithinss[auxk] = myKmeansauxkmultistart$tot.withinss
  if(besttotwithinss > mytotwithinss[auxk]) bestk=auxk
  if(besttotwithinss > mytotwithinss[auxk]) besttotwithinss = mytotwithinss[auxk]}
plot(mytotwithinss, xlab='k')


