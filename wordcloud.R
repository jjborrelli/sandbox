#Script to take in vector of names to create word clouds and network of abstract word interactions
#ie to show how the department is connected

#load libraries

library("XML")
library("stringr")
library("RCurl")
library("wordcloud")
library("tm")
require(reshape)
require(sna)
require(bipartite)
require(igraph)
require(ggplot2)
require(grid)
require(reshape2)

#source functions
getAbstracts <- function(author,university, dFrom, dTill, nRecs)
{
  #For more details about Pubmed queries see: http://www.ncbi.nlm.nih.gov/books/NBK25500/
  
  #Text search - basic URL
  eSearch <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term="
  #Data record download - basic URL
  eDDownload <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id="
  
  #In case of multiple words (e.g., first and the last name), add "+" sign in between them 
  aL <- str_replace_all(author, " ", "+")
  #Add the search keyword - author
  aQ <- paste(aL, "[author]", sep = "")
  
  #add institution?
  
  if(exists("university")){
    aU <- str_replace_all(university, " ", "+")
    #Add the search keyword - affiliation
    aUU <- paste(aL, "[ad]", sep = "")
  }
  
  #Format the publication date and add the search keyword - pdat
  #If only one year is provided, use that year, otherwise use year_1:year_2
  dQ <- ""
  
  if ((str_length(dFrom) > 0) & (str_length(dTill) > 0))
  {
    d1 <- paste(dFrom, dTill, sep = ":")
    dQ <- paste(d1, "[pdat]", sep = "")
  }
  
  if ((str_length(dFrom) > 0) & (str_length(dTill) == 0))
    dQ <- paste(dFrom, "[pdat]", sep = "")
  
  if ((str_length(dTill) > 0) & (str_length(dFrom) == 0))
    dQ <- paste(dTill, "[pdat]", sep = "")
  
  #Add two seqrch queries together
  hlpQ1 <- aQ  
  
  if (str_length(dQ) > 0)    
    hlpQ1 <- paste(aQ, dQ, sep = "+")
  
  #Add the max number of retrieved articles at the end of the query
  rmQ <- paste("&retmax=", nRecs, sep="")
  hlpQ2 <- paste(hlpQ1, rmQ, sep="")
  
  if(exists("university")){
    hlpQ3 <- paste(hlpQ2, aUU, sep = "+")
  }
  
  #Finalize the query and serch Pubmed
  searchUrl <- paste(eSearch, hlpQ2, sep = "" )
  #Wait - to ensure that all requests will be processed
  Sys.sleep(3)    
  hlpURL <- getURL(searchUrl)
  #The result is in form of XML document - you can paste the searchUrl in the browser to see/download it
  doc <- xmlTreeParse(hlpURL, asText = TRUE)     
  IdlistHlp = xmlValue(doc[["doc"]][["eSearchResult"]][["IdList"]])
  
  #I am sure there is more elegant way (i.e., a function) to proccess this, but I was lazy to search for it
  if (length(IdlistHlp) > 0)
  {
    Idlist <- c()
    
    #Each ID is 8 digits long
    for(k in 1:(str_length(IdlistHlp)/8))
      Idlist <- c(Idlist, str_sub(IdlistHlp, start = 8*(k-1) + 1, end = k*8))
    
    #Once we retrieved articles' IDs for the author/dates, we can process them and get abstracts             
    Sys.sleep(2)
    hlp1 <- paste(eDDownload, paste(Idlist, collapse = ",", sep = ""), sep = "")
    hlp2 <- paste(hlp1, "&rettype=abstract", sep = "")
    testDoc <- xmlTreeParse(hlp2, useInternalNodes = TRUE)
    topFetch <-xmlRoot(testDoc)
    abst <- xpathSApply(topFetch, "//Abstract", xmlValue)
  }
  
  #In case that nothing was found
  if (length(IdlistHlp) == 0)
    abst = c("Zero", "Articles", "Found")
  
  abst
}

plotWC <- function(abstracts, nc, cs)
{
  #Once we have abstracts, we can create a document corpus
  abstTxt <- Corpus(VectorSource(abstracts))
  
  text2.corpus = tm_map(abstTxt, removePunctuation)
  text2.corpus = tm_map(text2.corpus, tolower)
  text2.corpus = tm_map(text2.corpus, removeWords, stopwords("english"))
  
  #Transform it into a matrix and sort based on the total word occurence
  tdm <- TermDocumentMatrix(text2.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  #Select the color scheme
  pal2 <- brewer.pal(nc, cs)
  
  #And plot the cloud
  wordcloud(d$word,d$freq, scale=c(8,.2), min.freq = 2, max.words=50, random.order = FALSE, rot.per=.15, color = pal2, vfont=c("sans serif","plain"))
}


# Get inputs, download abstracts, and create a corresponding wordcloud 

#Run test
abs<-getAbstracts(author="Catherine H. Graham", "Stony Brook",2010,2014,10)

#plot the abstracts, the 2nd and third argument are the color brewer ?brewer.pal, number of colors and palette
plotWC(abs,8,"Accent")

#Step two make a network of the participants

profs<-c("Heather J. Lynch","Catherine H. Graham","Lev Ginzburg","H. Resit Akcakaya","Diana Padilla","John R. True","Walt Eanes","Mike Bell","Jeff S. Levinton","Brenna Henn", "Liliana M. Davalos","Joshua S. Rest","Jessica Gurevitch","Stephen B. Baines")
abs_all<-lapply(profs,function(x){
  abs<-getAbstracts(x,"Stony Brook",2000,2014,20)  
}
)

#plot all as one word cloud
plotWC(abs_all,8,"Accent")

#seperate into individual matrices
abstTxt <- Corpus(VectorSource(abs))

me<-lapply(abs_all,function(x){
  abstTxt <- Corpus(VectorSource(x))
  
  text2.corpus = tm_map(abstTxt, removePunctuation)
  text2.corpus = tm_map(text2.corpus, tolower)
  text2.corpus = tm_map(text2.corpus, removeWords, stopwords("english"))
  
  #Transform it into a matrix and sort based on the total word occurence
  tdm <- TermDocumentMatrix(text2.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
})

names(me)<-profs
mem<-melt(me,id.var=c("word","freq"))

#what are the strongest interacting words
word_matrix<-acast(mem,L1~word,value.var="freq",fill=0)

#as distance matrix
dist_matrix<-dist(word_matrix)

#To do only keep shared words?

g<-graph.adjacency(as.matrix(dist_matrix),diag=FALSE,mode="lower",weighted=TRUE)

#names of the vertices you just imported:
V(g)$name
E(g)$size

plot.igraph(g,layout=layout.fruchterman.reingold, edge.color="black",edge.width=E(g)$weight/50) 

#color by weight
cols<-gray(E(g)$weight/max(E(g)$weight))

plot.igraph(g,layout=layout.fruchterman.reingold, edge.color=cols,edge.width=E(g)$weight/50) 

#that was grey, try color

colramp<-colorRampPalette(c("blue","red"))(length(E(g)$weight))

#original order
orig<-E(g)$weight/max(E(g)$weight)

orig.order<-data.frame(orig,1:length(orig))

weight.order<-orig.order[order(E(g)$weight/max(E(g)$weight)),]

#merge with col
colramp.w<-data.frame(weight.order,colramp)

#get original order
colsRB<-colramp.w[order(colramp.w$X1.length.orig.),]

plot.igraph(g,layout=layout.fruchterman.reingold, edge.color=as.character(colsRB$colramp),edge.width=(E(g)$weight/100)) 

mem$word<-as.character(mem$word)


#make another prettier graph

## Test
plot(g,edge.arrow.size=E(g)$size/10)

mem[order(mem$freq,decreasing=TRUE),][1:20,]

pos <- data.frame(x = sample(1:20, 14), y = sample(1:20, 14), name = V(g)$name)
edges <- get.edgelist(g)
x1 <- c()
y1 <- c()
x2 <- c()
y2 <- c()
for(i in 1:nrow(edges)){
  x1[i] <- pos$x[which(edges[i,1] == pos$name)]
  y1[i] <- pos$y[which(edges[i,1] == pos$name)]
  x2[i] <- pos$x[which(edges[i,2] == pos$name)]
  y2[i] <- pos$y[which(edges[i,2] == pos$name)]
}
e <- data.frame(x1, y1, x2, y2)
w <- E(g)$weight/100


p <- ggplot(pos, aes(x = x, y = y)) 
p <- p + geom_segment(data = e, aes(x = x1, y = y1, xend = x2, yend = y2), size = w, alpha = .75)
p <- p + geom_point(size = 5, col = "red") 
# the rest is just eliminating the background
p <- p + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) 
p <- p + theme(panel.background = element_blank()) + theme(legend.position="none")
p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
p <- p + theme( legend.background = element_rect(colour = NA)) 
p <- p + theme(panel.background = element_rect(fill = "white", colour = NA)) 
p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
p
