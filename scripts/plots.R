list.of.packages = c('ape','factoextra','RColorBrewer','zoom','ggplot2','gridExtra')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Loads ape library used for dendogram plots
library(ape) # Dendogram plotting
library(RColorBrewer) # Color printing
library(factoextra) # Plotting clusters
library(ggplot2)
require(gridExtra)

# Loads library to zoom over plots
library(zoom)

debugSource('scripts/util.R')

# Plots a dendogram based on a hclust input, a color set and a cut level
plotDendogram = function(clus,tree,cutLevel, title){
  # color = grDevices::colors()[grep('(gr(a|e)y)|light', grDevices::colors(), invert = T)]
  # selectedColors = sample(color, cutLevel)
  if (cutLevel <= 8){
    selectedColors = brewer.pal(cutLevel,name = 'Dark2')
  }
  else if (cutLevel <= 17){
    selectedColors = c(brewer.pal(8,name = 'Dark2'),brewer.pal(9,name = 'Set1'))[1:cutLevel]
  }
  else{
    selectedColors = sample(c(brewer.pal(8,name = 'Dark2'),brewer.pal(9,name = 'Set1')),cutLevel)
  }
  
  # print(selectedColors)
  phy = ladderize(as.phylo(clus),right = TRUE)
  plot.phylo(phy
             ,tip.color = selectedColors[tree]
             ,edge.lty="dotted"
             ,cex = 0.5
             ,main = title
             ,font = 2
             ,show.node.label = FALSE)
  axisPhylo(backward = FALSE)
  
  plot(clus,main=title)
}

#   Applies multiple dimensional Scaling over distance matrix and
# uses the first two principal components to plot distance between 
# points.
plotMultipleDimensionalScaling = function(distanceMatrix,title){
  
  fit = cmdscale(distanceMatrix,eig=TRUE, k=2) # k is the number of dim
  # plot solution 
  x = fit$points[,1]
  y = fit$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main=title,	type="n")
  text(x, y, labels = rownames(distanceMatrix), cex=.7)
}

prePlot <- function(distances){
  contentDistance               = distances$contentDistance
  behaviourDistance             = distances$behaviourDistance
  mixedDistance                 = distances$mixedDistance
  contentData                   = distances$data$usersTagsMatrix
  forgCurveLambdaIndex          = distances$lambdaIndex
  userUserTimeDiffAlphaIndex    = distances$alphaIndex
  articlesPopularityIndexVector = distances$data$articlesPopularityIndexVector

  # plotMultipleDimensionalScaling(contentDistance,'MDS Content Distance')
  # plotMultipleDimensionalScaling(behaviourDistance,'MDS Behaviour Distance')
  # plotMultipleDimensionalScaling(mixedDistance,'MDS Mixed Distance')
  
  # Evaluate the best number of clusters based on silhouette
  print(fviz_nbclust(x = contentData,diss = contentDistance, method = "silhouette",FUNcluster = pam) + theme_classic(), title = 'Silhouette over Content Distance')
  
  # Evaluate the best number of clusters based on kmeans (within sum of squares)
  # print(fviz_nbclust(x = contentData, FUNcluster = kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2), title = 'WSS for K-means over Content Distance')
  
}

# Plots a cluster obtained by PAM method
plotPam = function(data, clusters, title){
  # if (is.null(data)){
  # print(fviz_cluster(object = clusters, repel = TRUE, ggtheme = theme_minimal(), main = title))
  # }
  # else{
  print(fviz_cluster(object = clusters, repel = TRUE, ggtheme = theme_minimal(), main = title))  
  # }
}


plotClusters <- function(clusters,k){
  emails = rownames(as.matrix(contentDistance))
  clusterSizes = rep(k,6)[1:6] #c(7,13,11,16,16,5)
  # clusterMethods = c('complete', 'single','average', 'centroid','media','ward.D')
  clusterMethods = c('average','ward.D')
  clusterTitles = c('User x User Content Cluster\n with average distance between users of all clusters',
                    'User x User Content Cluster\n with ward.D distance between clusters')
  for (i in 1:length(clusters)){
    if (length(which(clusters[[i]]$method %in% clusterMethods)) == 1){
      plotDendogram(clus = clusters[[i]]$cluster,tree = cutree(tree = clusters[[i]]$cluster,k = clusters[[i]]$k),cutLevel = clusters[[i]]$k, title = clusterTitles[i])
    }
    else if (clusters[[i]]$method == 'K-means' && clusters[[i]]$distance == 'content'){
      # plot(x = clusters[[i]]$clustering,main = paste0(clusters[[i]]$method,', ',clusters[[i]]$k,' Clusters',collapse = ''))      
      # plot(clusters = clusters[[i]]$clusters,main = paste0(clusters[[i]]$method,', ',clusters[[i]]$k,' Clusters',collapse = ''))
    }
    else if (clusters[[i]]$method == 'K-means'){
      # plot(x = clusters[[i]]$clusters,main = paste0(clusters[[i]]$method,', ',clusters[[i]]$k,' Clusters',collapse = ''))
      # plotPam(clusters = clusters[[i]]$clusters,title = paste0(clusters[[i]]$method,', ',clusters[[i]]$k,' Clusters',collapse = ''))
    }
    else{
      plot(x = clusters[[i]]$clusters,main = paste0(clusters[[i]]$method,', ',clusters[[i]]$k,' Clusters',collapse = ''))
      # plotPam(clusters = clusters[[i]]$clusters,title = paste0(clusters[[i]]$method,', ',clusters[[i]]$k,' Clusters',collapse = ''))
    }
  }
}