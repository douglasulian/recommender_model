list.of.packages = c('dplyr','ClusterR','cluster','factoextra','NbClust','foreach','doParallel','parallel','xtable')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

library(cluster)
library(ClusterR)
library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)
library(foreach)
library(doParallel)
library(parallel)
library(tibble)
library(xtable)

debugSource(fileName = 'scripts/cache.R')
debugSource(fileName = 'scripts/cluster.R')


funPam = function(k, dissDistance){
  cluster = pam(x = dissDistance, diss = TRUE, k = k, cluster.only = TRUE, do.swap = TRUE, pamonce = 2)
  return(as.vector(cluster))
}

funKMedoid = function(k, matrixDistance){
  resultado = Cluster_Medoids(data = matrixDistance,clusters = k,verbose = FALSE,threads = 3,swap_phase = TRUE)
  return(resultado$clusters)
}

funHierarch = function(k, dissMatrix, method){
  hierarcCluster = hclust(d = dissMatrix,method = method)
  cluster = cutree(hierarcCluster,k = k) 
  return(cluster)
}

# load(file = 'distances.RData')
cacheDir = 'cache//6000_all_clusters//'
plotsDir = 'document//plots//'
tablesDir = 'document//tables//'
load(file = paste0(cacheDir,'distances-01767-0.00-0.66-0.88.RData'))

# save(list = c('distances'),file = 'distances.RData', compress = FALSE)

matrixDistance = distances$mixedDistance
usersTags = distances$data$usersTagsMatrix
dissDistance = as.dist(matrixDistance)
rm('distances')
gc()
test = list()
for (i in 2:20){
  name = 'ward.D'
  time = system.time({ cluster = funHierarch(k = i,dissMatrix = dissDistance, method = name) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'hierarchical')
  
  name = 'ward.D2'
  time = system.time({ cluster = funHierarch(k = i,dissMatrix = dissDistance, method = name) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'hierarchical')
  
  name = 'average'
  time = system.time({ cluster = funHierarch(k = i,dissMatrix = dissDistance, method = name) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'hierarchical')
  
  name = 'single'
  time = system.time({ cluster = funHierarch(k = i,dissMatrix = dissDistance, method = name) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'hierarchical')
  
  name = 'complete'
  time = system.time({ cluster = funHierarch(k = i,dissMatrix = dissDistance, method = name) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'hierarchical')
  
  name = 'mcquitty'
  time = system.time({ cluster = funHierarch(k = i,dissMatrix = dissDistance, method = name) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'hierarchical')
  
  name = 'pam'  
  time = system.time({ cluster = funPam(k = i,dissDistance = dissDistance) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'optimization')
  
  name = 'kmedoid'
  time = system.time({ cluster = (funKMedoid(k = i,matrixDistance = matrixDistance)) })
  test$k = c(test$k,i)
  test$name = c(test$name,name)
  test$userTime = c(test$userTime,time[1])
  test$systemTime = c(test$systemTime,time[2])
  test$elapsedTime = c(test$elapsedTime,time[3])
  test$silhouette = c(test$silhouette,(summary(silhouette(x = cluster,dist = dissDistance)))$si.summary[4])
  test$type = c(test$type,'optimization')
  
}
gc()
gg1 = ggplot2::ggplot(data = as.tibble(test) %>% filter(type == 'hierarchical'), mapping = aes(x = k,y = silhouette,colour = name)) +
  geom_line() +
  ggtitle('Average Silhouette Width x Cluster Method') +
  labs(x = 'Number of Clusters', y = 'Average. Silhouette Width', colour = 'Method') +
  theme(plot.title = element_text(hjust = 0.5))
# print(gg1)
ggsave(filename = paste0(plotsDir,"hierarchical-cluster-avg-silhouette-plot.png"), plot = gg1, width = 10, height = 8, dpi = 600)

gg2 = ggplot2::ggplot(data = as.tibble(test) %>% filter(type == 'hierarchical'), mapping = aes(x = k,y = elapsedTime,colour = name)) +
  geom_line() +
  ggtitle('Cluster Method x Elapsed Time') +
  labs(x = 'Number of Clusters', y = 'Elapsed Time', colour = 'Method') +
  theme(plot.title = element_text(hjust = 0.5))
# print(gg2)
ggsave(filename = paste0(plotsDir,"hierarchical-cluster-time-plot.png"), plot = gg2, width = 10, height = 8, dpi = 600)

gg3 = ggplot2::ggplot(data = as.tibble(test) %>% filter(type == 'optimization'), mapping = aes(x = k,y = silhouette,colour = name)) +
  geom_line() +
  ggtitle('Average Silhouette Width x Cluster Method') +
  labs(x = 'Number of Clusters', y = 'Average. Silhouette Width', colour = 'Method') +
  theme(plot.title = element_text(hjust = 0.5))
# print(gg3)
ggsave(filename = paste0(plotsDir,"optimization-cluster-avg-silhouette-plot.png"), plot = gg3, width = 10, height = 8, dpi = 600)

gg4 = ggplot2::ggplot(data = as.tibble(test) %>% filter(type == 'optimization'), mapping = aes(x = k,y = elapsedTime,colour = name)) +
  geom_line() +
  ggtitle('Cluster Method x Elapsed Time') +
  labs(x = 'Number of Clusters', y = 'Elapsed Time', colour = 'Method') +
  theme(plot.title = element_text(hjust = 0.5))
# print(gg4)
ggsave(filename = paste0(plotsDir,"optimization-cluster-time-plot.png"), plot = gg4, width = 10, height = 8, dpi = 600)
gc()
tables = list()
names = c('ward.D','ward.D2','complete', 'average','single','mcquitty','pam','kmedoid')
for (i in 1:length(names)){
  clusters = as.tibble(test) %>% filter(name == names[i])
  maxSilhouette = clusters$k[which(clusters$silhouette == max(clusters$silhouette))]
  if (names[i] == 'pam'){
    curCluster = funPam(k = maxSilhouette,dissDistance = dissDistance)
  } else if (names[i] == 'kmedoid'){
    curCluster = funKMedoid(k = maxSilhouette,matrixDistance = matrixDistance)
  } else{
    curCluster = funHierarch(k = maxSilhouette,dissMatrix = dissDistance, method = names[i])
  }
  print(paste0(names[i],':'))
  print(paste0('maxSilhouette: ',maxSilhouette))
  emails = rownames(matrixDistance)
  
  clustersTab = as.tibble(matrix(data = c(emails,curCluster), ncol = 2, dimnames = list(rep(1:length(emails)),c('email','cluster'))))
  tags = t(sapply(X = clustersTab[,1]$email, FUN = getMainTags,usersTagsMatrix = usersTags,qtt = 5))
  clustersTab = add_column(.data = as.tibble(clustersTab),
                           tag1 = substr(x = tags[,2],start = 1,stop = 12),
                           tag2 = substr(x = tags[,3],start = 1,stop = 12),
                           tag3 = substr(x = tags[,4],start = 1,stop = 12),
                           tag4 = substr(x = tags[,5],start = 1,stop = 12),
                           tag5 = substr(x = tags[,6],start = 1,stop = 12),
                           .after = ncol(clustersTab))
  
  clustersTab = arrange(clustersTab,cluster,email)
  usersClusters = clustersTab %>% count(cluster) %>% arrange(desc(n)) %>% rename(users = n)
    
  finalClustersTab = filter(clustersTab,cluster == -1)
  for(j in 1:maxSilhouette){
    clusterTab = clustersTab %>% filter(cluster == j)
    indexes = sample(1:nrow(clusterTab),min(5,nrow(clusterTab)))
    finalClustersTab = bind_rows(finalClustersTab,slice(clusterTab,indexes))
  }
  tables[[i]] = list(clustersTab = finalClustersTab,
                     name = names[i],
                     usersClusters = usersClusters)
  print(usersClusters)
  
  rTable = tables[[i]]$usersClusters
  rTable = rownames_to_column(rTable, var = " ")
  newTable = xtable::xtable(rTable)
  label(newTable) = paste0("tab:users-clusters-table")
  caption(newTable) = paste0("Users per clusters for ",paste(toupper(substr(tables[[i]]$name, 1, 1)), substr(tables[[i]]$name, 2, nchar(tables[[i]]$name)), sep=""), " cluster method.")
  align(newTable) = "|r|c|c|r|"
  print.xtable(newTable
               ,sanitize.colnames.function = function(x) return(paste0("\\cellcolor[HTML]{000000}{\\color[HTML]{FFFFFF}",x,"}"))
               ,include.rownames = FALSE
               ,file = paste0(tablesDir,"users-clusters-",tables[[i]]$name,"-table.tex")
               ,latex.environments = c('adjustbox')
               ,caption.placement = 'top'
               ,label.placement = 'top')
  
  rTable = tables[[i]]$clustersTab %>% select(cluster,tag1,tag2,tag3,tag4,tag5)
  rTable = rownames_to_column(rTable, var = " ")
  newTable = xtable::xtable(rTable)
  label(newTable) = paste0("tab:users-clusters-tags-table")
  caption(newTable) = paste0("Users versus tags for ",paste(toupper(substr(tables[[i]]$name, 1, 1)), substr(tables[[i]]$name, 2, nchar(tables[[i]]$name)), sep=""), " cluster method.")
  align(newTable) = "|r|r|c|p{2.5cm}|p{2.5cm}|p{2.5cm}|p{2.5cm}|p{2.5cm}|"
  
  print.xtable(newTable
               ,sanitize.colnames.function = function(x) return(paste0("\\cellcolor[HTML]{000000}{\\color[HTML]{FFFFFF}",x,"}"))
               ,include.rownames = FALSE
               ,hline.after = rep(1:nrow(rTable))
               ,file = paste0(tablesDir,"users-clusters-tags-",tables[[i]]$name,"-table.tex")
               ,latex.environments = c('adjustbox')
               ,caption.placement = 'top'
               ,label.placement = 'top')
    
}
