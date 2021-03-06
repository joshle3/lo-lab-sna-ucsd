# load the library
library(igraph)

# import EdgeList and prompt; run as Source with Echo to get user input
importedData <- read.csv(file.choose(), header = TRUE)
userInput <- readline("Is your data directed? Please enter 1 for TRUE or 0 for FALSE\t")
ifelse(userInput == 1, outcomeBool <- "TRUE", outcomeBool <- "FALSE")

g <- graph.data.frame(importedData, directed = outcomeBool) # Gets edgelist

# Creates and graphs weighted adjacency matrix from edgelist
ifelse(userInput == 1, outcome <- "directed", outcome <- "undirected")
gadj <- get.adjacency(g, sparse=FALSE) # Gets weighted adjacency matrix
graphedadj <- graph.adjacency(gadj, mode = outcome, weighted = TRUE)
plot(graphedadj) # WORK IN PROGRESS

# plot the data using straight lines (curved algorithm causes warnings)
#plot(g, edge.curved = FALSE)

# basic analysis
nedge <- ecount(g) # Edge count
nnode <- gorder(g) # Node count
den <- edge_density(g, loops=TRUE) # Density with self interactions allowed
inoutdeg <- degree(g) # Degrees of all nodes
degavg <- mean(degree(g)) # Average degree
diam <- diameter(g) # Diameter of graph

strwkplotchk <- clusters(g, mode = "strong")$membership # Finding and plotting strong/weak clusters
plot(g, vertex.color = strwkplotchk, edge.curved = FALSE)

membershipvec <- cluster_walktrap(g) # Creates a vector based on short walkthroughs to find communities
submod <- modularity(membershipvec) # Finds unrestricted modularity of graph
relsubmod <- modularity(g, membership(membershipvec), weights = NULL) # Finds modularity relative to membershipvec, a vector of communities
