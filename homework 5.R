##Task1: Text Mining
##1
##load libraries
Sys.setenv(NOAWT= "true") ## work around for the stemmer issue
library(plyr)
library(ggplot2)
library(tm)
library(lsa)
##load and check the data
ifilename = "http://www.yurulin.com/class/spring2016_datamining/data/hw5_1_dataset/r52.csv"
reuters = read.csv(ifilename)
dim(reuters)
reuters[1:3,]
##plot
ggplot(reuters, aes(Topic)) + geom_bar() + theme(axis.text.x=element_text(angle = 90,colour = "black"))
##select the top 4 popular topics
select.topics = sort(table(reuters$Topic),decreasing = T)[1:4]
select.topics = names(select.topics)
select.topics

##2
##give the topic labels
doc.idx = which(reuters$Topic %in% select.topics)
dataset = reuters[doc.idx,]
##create a corpus
corpus = Corpus(VectorSource(dataset$Content))
corpus
##pre-processing
corpus=tm_map(corpus,content_transformer(tolower))
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus,function(x) removeWords(x, stopwords("english")))
corpus=tm_map(corpus,stemDocument,language="english")
corpus=tm_map(corpus,stripWhitespace)
inspect(corpus[1:3]) # check first three rows
corpus  # check corpus
td.mat = TermDocumentMatrix(corpus)
td.mat
dim(td.mat) ##dimention of term-doc matrix
td.mat4 = findFreqTerms(td.mat, 4)##Use terms that appear at at least four time
term.idx = which(row.names(td.mat) %in% td.mat4)
td.mat = td.mat[term.idx,]
dist.mat = dist(t(as.matrix(td.mat)))  ## compute distance matrix
doc.mds = cmdscale(dist.mat, k = 2)
data = data.frame(x = doc.mds[, 1], y = doc.mds[, 2], topic = dataset$Topic, id = row.names(dataset))
ggplot(data, aes(x = x, y = y, color = topic)) + geom_point()

##3
##TFIDF
td.mat=as.matrix(td.mat)
td.mat.w=lw_tf(td.mat)*gw_idf(td.mat)
dist.td.mat.w=dist(t(as.matrix(td.mat.w)))
doc.td.mat.w=cmdscale(dist.td.mat.w,k=2)
data2=data.frame(x=doc.td.mat.w[,1],y=doc.td.mat.w[,2],topic=dataset$Topic,id=row.names(dataset))
ggplot(data2,aes(x=x,y=y,color=topic))+geom_point()
##LSA
lsa=lsa(td.mat.w,dims=4) ## create LSA space
dist.lsa=dist(t(as.textmatrix(lsa)))## compute distance matrix
doc.lsa=cmdscale(dist.lsa,k=2)
data3=data.frame(x=doc.lsa[,1],y=doc.lsa[,2],topic=dataset$Topic,id=row.names(dataset))
ggplot(data3,aes(x=x,y=y,color=topic))+geom_point()
##NMF
library(NMF)
set.seed(12345)
res = nmf(td.mat, 3,"lee") # lee & seung method 
V.hat = fitted(res)
w = basis(res) 
h = coef(res) 
dist.mat.nmf = dist(t(h)) 
doc.mds.nmf = cmdscale(dist.mat.nmf, k = 2)
data4 = data.frame(x = doc.mds.nmf[, 1], y = doc.mds.nmf[, 2], topic = dataset$Topic, id = row.names(dataset)) 
ggplot(data4, aes(x = x, y = y, color = topic)) + geom_point()

##Task2: Network Analysis
##1
##Load user_artists.csv dataset
user = read.csv("http://www.yurulin.com/class/spring2016_datamining/data/hw5_2_dataset/user_artists.csv")
View(user)
##extract data with artists who were listen equal or above 1500 times (weight >= 1500).
user1 = user[which(user$weight>=1500),]
View(user1)
##extract the top 30 most frequently listened artists as the nodes
artsumWeight <- aggregate(weight ~ artistID, data = user1, sum)
artsumWeight <- artsumWeight[order(artsumWeight$weight, decreasing = TRUE), ]
artist30 = artsumWeight[1:30,]
View(artist30)
artist30ID = as.character(artist30$artistID)
artist30ID
##Generate a network with edge weights >= 5,
library(igraph)
df = subset (user1, artistID %in% artist30ID)
g = graph.data.frame(df, directed = T)
mat = get.adjacency(g)
mat = as.matrix(mat)
m2 = t(mat) %*% mat
artist.idx = which(colSums(m2) > 0)
artist.mat = m2[artist.idx, artist.idx]
diag(artist.mat) = 0  ## co-artist with self does not count
artist.idx = which(colSums(artist.mat) > 0)
artist.mat = artist.mat[artist.idx, artist.idx]
dim(artist.mat)
artist.mat[which(artist.mat<5)] = 0
g = graph.adjacency(artist.mat, weighted = T, mode = "undirected", diag = F)
set.seed(1)
plot(g, layout = layout.fruchterman.reingold, vertex.label = V(g)$name)
##Load artists.csv to replace artistsID with artist names
artistscsv = read.csv("http://www.yurulin.com/class/spring2016_datamining/data/hw5_2_dataset/artists.csv")
View(artistscsv)
id <- rownames(artist.mat)
id <- as.array(id)
name <- artistscsv$name[which(artistscsv$id %in% id)]
name <- as.character(name)
artist.mat<- artist.mat[, order(as.integer(colnames(artist.mat)))]
artist.mat<- artist.mat[order(as.integer(rownames(artist.mat))), ]
rownames(artist.mat) <- c(name)
colnames(artist.mat) <- c(name)
artist.mat
g = graph.adjacency(artist.mat, weighted = T, mode = "undirected", diag = F)
set.seed(1)
plot(g, layout = layout.fruchterman.reingold, vertex.label = V(g)$name)
##List the names of the top 10 artists and their number of listening
#use original user data
sumWeight <- aggregate(weight ~ artistID, data = user, sum)
View(sumWeight)
sortSumWeight <- sumWeight[order(sumWeight$weight, decreasing = TRUE), ]
View(sortSumWeight)
top10artist <- sortSumWeight[1:10,]
View(top10artist)
top10name <- artistscsv[which(artistscsv$id %in% top10artist$artistID), ]
View(top10name)
top10 <- merge(top10artist,top10name, by.x = "artistID", by.y = "id")
View(top10)
#use the data which weight >= 1500
sumWeight1500 <- aggregate(weight ~ artistID, data = user1, sum)
sortSumWeight1500 <- sumWeight1500[order(sumWeight1500$weight, decreasing = TRUE), ]
top10artist1500 <- sortSumWeight1500[1:10,]
top10name1500 <- artistscsv[which(artistscsv$id %in% top10artist1500$artistID), ]
top101500 <- merge(top10artist1500,top10name1500, by.x = "artistID", by.y = "id")
View(top101500)

##2
##get modularity-based community
fc = fastgreedy.community(g)
modularity(fc)
membership(fc)
set.seed(1)
plot(fc, g, main = "modularity community", layout = layout.fruchterman.reingold, vertex.size = 4, vertex.label.cex = 0.5)
dendPlot(fc)

##3
##Degree centrality
deg = degree(g)
deg
top = order(deg, decreasing = T)[1:5]##the top-5 nodes with highest degrees
top
##size node by degree
V(g)$size = abs(deg) * 2
V(g)$color = "gray"
V(g)$label.color = "gray"
V(g)$label.cex = 0.1
E(g)$color = "black"
V(g)[top]$label.color = "black"  ## highlight the top-5 nodes
V(g)[top]$label.cex = 1
V(g)[top]$color = "Skyblue"
set.seed(1)
plot(g, layout = layout.circle)
title("degree centrality")
##Closeness centrality
clo = closeness(g)
clo
top = order(clo, decreasing = T)[1:5]
## size node by closeness
V(g)$size = (abs(clo)^2 * 1e+05)*50
V(g)$color = "gray"
V(g)$label.color = "black"
V(g)$label.cex = 0.2
V(g)[top]$label.color = "red"  ## highlight the top-5 nodes
V(g)[top]$label.cex = 1
set.seed(1)
plot(g, layout = layout.circle)
title("closeness")
##Betweenness centrality
bet = betweenness(g)
bet
top = order(bet, decreasing = T)[1:5]
## size node by betweenness
V(g)$size = abs(bet) * 0.5
V(g)$color = "gray"
V(g)$label.color = "black"
V(g)$label.cex = 0.2
V(g)[top]$label.color = "red"  ## highlight the top-5 nodes
V(g)[top]$label.cex = 1
set.seed(1)
plot(g, layout = layout.circle)
title("betweenness")
##PageRank
pg = page.rank(g)$vector
pg
top = order(pg,decreasing=T)[1:5]
## size node by pagerank
V(g)$size = abs(pg) * 250
V(g)$label.color = "black"
V(g)[ top ]$label.color = "red" ## highlight the top-5 nodes
set.seed(1)
plot(g)
title("PageRank")

##Task3
##load the user_ratedmovies.csv & movies.csv data
dataset = read.csv("http://www.yurulin.com/class/spring2016_datamining/data/hw5_3_dataset/user_ratedmovies.csv")
movies = read.csv("http://www.yurulin.com/class/spring2016_datamining/data/hw5_3_dataset/movies.csv")
View(dataset)
View(movies)
##Create a moive rating matrix,consider movies that were rated by at least 50 users,and users that rated at least 10 movies.
movies = unique(movies$id)
d4 = subset(dataset, movieID %in% movies)
d4 = data.frame(from = d4$userID, to = d4$movieID, weight = d4$rating)
g = graph.data.frame(d4)
mat = get.adjacency(g)
mat.w = get.adjacency(g, attr = "weight")
mat = as.matrix(mat)
movie.idx = which(colSums(mat) >= 50)
user.idx = which(rowSums(mat) >= 10)
rmat = mat.w[user.idx, movie.idx]
dim(rmat)

##1
library(recommenderlab)
library(mlegp)
m = as.matrix(rmat)
m = as(m,"realRatingMatrix")
e = evaluationScheme(m,method="cross-validation",train = 0.9,k = 5,given = 5,goodRating = 3)
r1 = Recommender(getData(e,"train"),"Random")
r2 = Recommender(getData(e,"train"),"Popular")
r3 = Recommender(getData(e,"train"),"UBCF")
r4 = Recommender(getData(e,"train"),"IBCF")
p1 = predict(r1,getData(e,"known"),type="ratings")
p2 = predict(r2,getData(e,"known"),type="ratings")
p3 = predict(r3,getData(e,"known"),type="ratings")
p4 = predict(r4,getData(e,"known"),type="ratings")
error = rbind(calcPredictionAccuracy(p1, getData(e, "unknown")),
              calcPredictionAccuracy(p2, getData(e, "unknown")),
              calcPredictionAccuracy(p3, getData(e, "unknown")),
              calcPredictionAccuracy(p4, getData(e, "unknown"))
)
rownames(error) = c("Random","Popular","UBCF","IBCF")
error