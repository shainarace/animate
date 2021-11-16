install.packages("tm")
install.packages("Matrix")
install.packages("igraph")
install.packages("rARPACK")
install.packages("NLP")
library(rARPACK)
library(tm)
library(Matrix)
library(NLP)
library(igraph)
########################################################################
########################################################################
# Put the path where you store the .txt files in the quotes below
########################################################################
########################################################################
setwd('')
t = "SAarticle.txt"
t = readChar(t, file.info(t)$size)
# remove newline, linebreak characters, substitute "." for other things that
# end sentences, like "!" and "?" 
t = gsub("\r?\n|\r", " ", t)
t = gsub("\\?", ".", t)
t = gsub("\\!", ".", t)
#t = gsub("\\:", ".", t)
t2 = strsplit(t, ".",fixed=TRUE)
t3 = as.data.frame(t2)
corpus <- Corpus(VectorSource(t3[,1]))
# Remove stop words
corpus = tm_map(corpus, removeWords, stopwords("english"))
# Remove numbers
corpus = tm_map(corpus, removeNumbers)

########################################################################
# Remove empty documents that appear after removal of stopwords and numbers
td1 = TermDocumentMatrix(corpus)
colTotals = apply(td1 , 2, sum) #Find the sum of words in each Document
empty.cols = td1[,colTotals == 0]$dimnames[2][[1]]
if(length(empty.cols)>0){corpus = corpus[-as.numeric(empty.cols)]}
########################################################################
# Create TF-IDF weighted term-document matrix:
td = TermDocumentMatrix(corpus,control = list(weighting = weightTfIdf))
m = td$nrow
n = td$ncol
# Create sparse matrix, normalize columns to efficiently compute cosine matrix:
tdm = sparseMatrix(i = td$i, j=td$j, x=td$v,dims = c(td$nrow,td$ncol), dimnames = td$dimnames, giveCsparse=F)
colnorms = apply(tdm, 2, function(x){sqrt(t(x)%*%x)})
tdm_norm = tdm%*%diag(1/colnorms)

# Cosine similarity is just inner product of the normalized matrix with itself
C=t(tdm_norm)%*%tdm_norm
# Remove self loops
C[C>0.9999999] = 0
# Check distribution of cosine values
hist(C@x,100)
# Convert to 0 values below a chosen threshold
C[C<0.01] = 0

# R base plots for networks are terrible. Write the graph as .gml and open it in Gephi (Free, download it)
# We'll export a weighted graph for visualization purposes, although gephi will not use the weights
# when computing eigenvector centrality, it will use the binary adjacency matrix.
g = graph_from_adjacency_matrix(as.matrix(C),weighted=TRUE,mode="undirected")
# Check for more than 1 connected component
components(g)
# First add the sentence text as an attribute
vertex_attr(g)=list(text = corpus$content)
# The following file can be opened in Gephi
write_graph(g, "g.gml", format = c("gml"))


# Calculate first eigenvector
f = eigs_sym(C,1)
# Indices of sentence ordered by centrality
j = order(f$vectors, decreasing = TRUE)
# Top 3 sentences
corpus$content[j[1:3]]
# Indices of sentence ordered by centrality
order(f$vectors, decreasing = TRUE)


########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
