#####Task 1#####
load.data.stock <- function() {
  data.url = 'http://www.yurulin.com/class/spring2016_datamining/data/stock_price.csv' 
  dataset <- read.csv(sprintf(data.url)) 
  print(head(dataset))
  print(dim(dataset))
  n = nrow(dataset)
  print(dim(dataset))
  t(dataset)
}
dataset = load.data.stock()
##normalize the data
dataset = t(scale(t(dataset)))
dim(dataset)
##show first three rows
dataset[1:3, ]
##define the labels
labels = rownames(dataset)
labels

##1 & 2##
do.pca <- function(dataset,lbls, do.screeplot=F,do.scatter=F,do.biplot=F,do.loadingplot=F) {
  data.pca = prcomp(dataset, scale=TRUE)
  data.pc = predict(data.pca)
  if (do.screeplot) plot(data.pca, main='screeplot for PCA') 
  if (do.scatter) {
    plot(data.pc[,1:2], type="n")
    text(x=data.pc[,1], y=data.pc[,2], labels=lbls)
  }
  if (do.biplot) biplot(data.pca) 
  if (do.loadingplot) {
    plot(data.pca$rotation[,1],type='l')
    #plot(data.pc[,1],type='l')
  }
  data.pc 
}
data.pc=do.pca(dataset,labels, do.screeplot=T, do.scatter=T, do.loadingplot=T)

##3##
do.mds <- function(dataset,lbls,do.scatter=T) { 
  data.dist = dist(dataset)
  data.mds = cmdscale(data.dist)
  if (do.scatter) {
    plot(data.mds, type = 'n')
    text(data.mds,labels=lbls)
  }
  data.mds 
}
data.mds=do.mds(dataset, labels, do.scatter=T)

##4##
##k-means, cluster=3
do.kmeans <- function(dataset,lbls,k=3,do.scatter=F) {
  set.seed(12345)
  data.clu = kmeans(dataset, centers=k, nstart=10) 
  if (do.scatter) {
    plot(dataset,type='n')
    text(dataset,labels=lbls,col=rainbow(k)[data.clu$cluster])
  }
  data.clu 
}
cluster1=do.kmeans(dataset, labels, k=3)$cluster
cluster1
##generate MDS map
plot(data.mds, type="n", main="k-means, cluster = 3")
text(data.mds, labels, col = rainbow(3)[cluster1])

##k-means, cluster=6
do.kmeans <- function(dataset,lbls,k=6,do.scatter=F) {
  set.seed(12345)
  data.clu = kmeans(dataset, centers=k, nstart=10) 
  if (do.scatter) {
    plot(dataset,type='n')
    text(dataset,labels=lbls,col=rainbow(k)[data.clu$cluster])
  }
  data.clu 
}
cluster2=do.kmeans(dataset, labels, k=6)$cluster
cluster2
##generate MDS map
plot(data.mds, type="n", main="k-means, cluster = 6")
text(data.mds, labels, col = rainbow(6)[cluster2])

##h-clustering with single-linke, cluster=3
do.hclust <- function(dataset,lbls,k=3,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method='single') ## change method to be single 
  if (do.dendrogram) 
    plot(hc, main="h-clustering with single-linke, cluster=3") 
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
cluster3=do.hclust(dataset, labels, k=3, do.dendrogram=T)
##generate MDS map
plot(data.mds, type="n", main="h-clustering with single-link, cluster = 3")
text(data.mds, labels, col = rainbow(3)[cluster3])

##h-clustering with single-linke, cluster=6
do.hclust <- function(dataset,lbls,k=6,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method='single') ## change method to be single 
  if (do.dendrogram) 
    plot(hc, main="h-clustering with single-linke, cluster=6") 
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
cluster4=do.hclust(dataset, labels, k=6, do.dendrogram=T)
##generate MDS map
plot(data.mds, type="n", main="h-clustering with single-link, cluster = 6")
text(data.mds, labels, col = rainbow(6)[cluster4])

##h-clustering with complete-linke, cluster=3
do.hclust <- function(dataset,lbls,k=3,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method='complete') ## change method to be complete 
  if (do.dendrogram) 
    plot(hc, main="h-clustering with complete-link, cluster=3") 
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
cluster5=do.hclust(dataset, labels, k=3, do.dendrogram=T)
##generate MDS map
plot(data.mds, type="n", main="h-clustering with complete-link, cluster = 3")
text(data.mds, labels, col = rainbow(3)[cluster5])

##h-clustering with complete-linke, cluster=6
do.hclust <- function(dataset,lbls,k=6,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method='complete') ## change method to be complete
  if (do.dendrogram) 
    plot(hc, main="h-clustering with complete-link, cluster=6") 
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
cluster6=do.hclust(dataset, labels, k=6, do.dendrogram=T)
##generate MDS map
plot(data.mds, type="n", main="h-clustering with complete-link, cluster = 6")
text(data.mds, labels, col = rainbow(6)[cluster6])

##h-clustering with average-linke, cluster=3
do.hclust <- function(dataset,lbls,k=3,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method='average') ## change method to be average 
  if (do.dendrogram) 
    plot(hc, main="h-clustering with average-link, cluster=3") 
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
cluster7=do.hclust(dataset, labels, k=3, do.dendrogram=T)
##generate MDS map
plot(data.mds, type="n", main="h-clustering with average-link, cluster = 3")
text(data.mds, labels, col = rainbow(3)[cluster7])

##h-clustering with average-linke, cluster=6
do.hclust <- function(dataset,lbls,k=6,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method='average') ## change method to be average 
  if (do.dendrogram) 
    plot(hc, main="h-clustering with average-link, cluster=6") 
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
cluster8=do.hclust(dataset, labels, k=6, do.dendrogram=T)
##generate MDS map
plot(data.mds, type="n", main="h-clustering with average-link, cluster = 6")
text(data.mds, labels, col = rainbow(6)[cluster8])

#####Task 2#####
rollcall.simplified <- function(df) { 
  no.pres <- subset(df, state < 99)
  ## to group all Yea and Nay types together 
  for(i in 10:ncol(no.pres)) {
    no.pres[,i] = ifelse(no.pres[,i] > 6, 0, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 0 & no.pres[,i] < 4, 1, no.pres[,i])
    no.pres[,i] = ifelse(no.pres[,i] > 1, -1, no.pres[,i])
  }
  return(as.matrix(no.pres[,10:ncol(no.pres)])) 
}
library('foreign') ## for loading dta files using read.dta 
library('ggplot2')
library('plyr')#for recording data
theme_set(theme_bw( base_family="Helvetica")) # the font changign doesn't work for Mac 
theme_update(plot.title = element_text(size=11,vjust=1,face='bold'),
             axis.title.x = element_text( size=12),
             axis.title.y = element_text( size=12,angle=90 ),
             axis.text.x = element_text( size=10),
             axis.text.y = element_text( size=10,hjust=1 ))
load.roll.call <- function(congr=11) { ## extract the 111th congress data by default 
  data.url = 'http://www.yurulin.com/class/spring2016_datamining/data/roll_call/'
  data.files = c("sen101kh.dta", "sen102kh.dta",
                 "sen103kh.dta", "sen104kh.dta",
                 "sen105kh.dta", "sen106kh.dta",
                 "sen107kh.dta", "sen108kh_7.dta",
                 "sen109kh.dta", "sen110kh_2008.dta",
                 "sen111kh.dta")
  dataset = read.dta(file.path(data.url, data.files[congr]), convert.factors = FALSE)
  dataset = subset(dataset, state < 99)
  print(dim(dataset))
  print(head(dataset[,1:12]))
  dataset 
}
dataset = load.roll.call(congr = 11) ## load the 111th congress data

roll.call.mds <- function(dataset,do.scatter=F,do.scatter.ggplot=F,do.clust='kmeans') {
  get.dist <- function(m) {
    dist(m %*% t(m))
  }
  data1 = rollcall.simplified(dataset)
  ## use either kmeans or hclust
  if (do.clust=='kmeans') { 
    clu = do.kmeans(data1,NULL,k=2)$cluster 
  } else if (do.clust=='hclust') { 
    clu = do.hclust(data1,NULL,k=2) 
  } else {}
  print(dim(data1))
  print(head(data1[,1:12]))
  data.dist = get.dist(data1)
  lbls = dataset$name
  party = mapvalues(dataset$party,from=c(100, 200, 328),to=c("Dem", "Rep", "Ind") ) 
  data.mds = cmdscale(data.dist)
  if (do.scatter) {
    colpt = mapvalues(dataset$party,from=c(100,200,328),to=c("1","2","3"))
    plot(data.mds, type = 'n')
    text(data.mds,labels=lbls, col=colpt)
  }
  data2 = data.frame(x=data.mds[,1],y=data.mds[,2],name=lbls,party=party,clu=factor(clu))
  if (do.scatter.ggplot) {
    p = ggplot(aes(x=x,y=y,shape=party,color=clu), data=data2) +
      geom_point(size=6,alpha=0.5) +
      geom_text(aes(x=x,y=y,shape=party,color=clu,label=name), size=4)
    print(p)
  } 
  print(cluster.purity(clu,party)) 
  print(cluster.entropy(clu,party)) 
  #data.mds
  #get.dist(data1)
}
##purity 
cluster.purity <- function(clusters, classes) { sum(apply(table(classes, clusters), 2, max)) / length(clusters)}
##entropy
cluster.entropy <- function(clusters,classes) {
  en <- function(x) {
    s = sum(x)
    sum(sapply(x/s, function(p) {
      if (p) {
        -p*log2(p)
      } else 0
    } ) )
  }
  M = table(classes, clusters)
  m = apply(M, 2, en)
  c = colSums(M) / sum(M)
  sum(m*c)
}

##1##
roll.call.mds(dataset, do.scatter = F, do.scatter.ggplot =T)

##2##
#k-means, cluster=2
do.kmeans <- function(dataset,lbls,k=2,do.scatter=F) { 
  set.seed(12345)
  data.clu = kmeans(dataset, centers=k, nstart=10)
  if (do.scatter) {
    plot(dataset,type='n')
    text(dataset,labels=lbls,col=rainbow(k)[data.clu$cluster])
  }
  data.clu 
}
roll.call.mds(dataset,do.scatter=F,do.scatter.ggplot=T,do.clust='kmeans')

#h-clustering with single-link, cluster=2
do.hclust <- function(dataset,lbls,k=2,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method="single") ## change method to be single 
  if (do.dendrogram) 
    plot(hc)
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
roll.call.mds(dataset,do.scatter=F,do.scatter.ggplot=T,do.clust='hclust')

#h-clustering with complete-link, cluster=2
do.hclust <- function(dataset,lbls,k=2,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method="complete") ## change method to be complete 
  if (do.dendrogram) 
    plot(hc)
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
roll.call.mds(dataset,do.scatter=F,do.scatter.ggplot=T,do.clust='hclust')

#h-clustering with average-link, cluster=2
do.hclust <- function(dataset,lbls,k=2,do.dendrogram=F) {
  data.dist = dist(dataset)
  hc = hclust(data.dist,method="average") ## change method to be average 
  if (do.dendrogram) 
    plot(hc)
  hc1 = cutree(hc,k)
  print(hc1)
  hc1
}
roll.call.mds(dataset,do.scatter=F,do.scatter.ggplot=T,do.clust='hclust')


##3##
##4##
##5##