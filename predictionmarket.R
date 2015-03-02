setwd("Functions")

#Options

visu <- 0   # if 1, plots the network

#Functions

source("populatenetwork.R")
source("shapenet.R")

if (visu == 1){
source("colored.R")
}

# Libaries

library(igraph)

# Populate network

net <- populatenet(100)

# Create links in the network according to approximate model homophily

net <- shapenet (g = net, seg = 0.9, nedg = 100)

# Visualize network (optional)

if (visu == 1){
net <- colored(net)
plot.igraph(net,vertex.label=NA,layout=layout.fruchterman.reingold, vertex.size = 7)
}

# Construct true time series (to be eventually mapped with actual data)






