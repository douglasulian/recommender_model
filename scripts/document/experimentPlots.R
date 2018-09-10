list.of.packages = c('SPOT','ggplot2','tibble','dplyr','xtable','tm','SnowballC','RColorBrewer','wordcloud')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
debugSource(fileName = 'scripts//evaluation.R')
# Load
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(SPOT)
library(ggplot2)
library(tibble)
library(dplyr)
library(xtable)

getPossibleDistancesFileName = function(alphaIndex, betaIndex, lambdaIndex){
  strAlphaIndex  = substr(x = paste0(sprintf(alphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strBetaIndex   = substr(x = paste0(sprintf(betaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLambdaIndex = substr(x = paste0(sprintf(lambdaIndex,fmt = '%f'),'0000'),start = 1,stop = 4)
  newFileName    = paste0('distances-','.*','-',strAlphaIndex,'-',strBetaIndex,'-',strLambdaIndex,'.RData')
  
  return(newFileName)
}


getPossibleSolutionsFileName = function(clusterMethod ,
                                        tagsMethod    ,
                                        articlesMethod,
                                        alphaIndex    ,
                                        betaIndex     ,
                                        lambdaIndex   ,
                                        gamaIndex     ,
                                        zetaIndex     ,
                                        noClustersK   ){
  strClusterMethod  = substr(x = clusterMethod,start = 1,stop = 1)
  strTagsMethod     = substr(x = tagsMethod,start = 1,stop = 1)
  strArticlesMethod = substr(x = articlesMethod,start = 1,stop = 1)
  strAlphaIndex     = substr(x = paste0(sprintf(alphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strBetaIndex      = substr(x = paste0(sprintf(betaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLambdaIndex    = substr(x = paste0(sprintf(lambdaIndex,fmt = '%f'),'0000'),start = 1,stop = 4)
  if (strTagsMethod == 't') gamaIndex = round(gamaIndex)
  strGamaIndex      = substr(x = paste0(sprintf(gamaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  if (strArticlesMethod == 't') zetaIndex = round(zetaIndex)
  strZetaIndex      = substr(x = paste0(sprintf(zetaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strNoClustersK    = paste0('00000',sprintf(round(noClustersK),fmt = '%i'))
  strNoClustersK    = substr(x = strNoClustersK,start = nchar(strNoClustersK) - 4,stop = nchar(strNoClustersK))
  
  newFileName       = paste0('solutions-','.*','-'
                             ,strClusterMethod ,'-'
                             ,strTagsMethod    ,'-'
                             ,strArticlesMethod,'-'
                             ,strAlphaIndex    ,'-'
                             ,strBetaIndex     ,'-'
                             ,strLambdaIndex   ,'-'
                             ,strGamaIndex     ,'-'
                             ,strZetaIndex     ,'-'
                             ,strNoClustersK             
                             ,'.RData')
  
  return(newFileName)
}

getClustersPossibleFileName = function(alphaIndex, betaIndex, lambdaIndex, clusterMethod, noClustersK){
  strAlphaIndex  = substr(x = paste0(sprintf(alphaIndex ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strBetaIndex   = substr(x = paste0(sprintf(betaIndex  ,fmt = '%f'),'0000'),start = 1,stop = 4)
  strLambdaIndex = substr(x = paste0(sprintf(lambdaIndex,fmt = '%f'),'0000'),start = 1,stop = 4)
  strNoClustersK = paste0('00000',sprintf(noClustersK,fmt = '%i'))
  strNoClustersK = substr(x = strNoClustersK,start = nchar(strNoClustersK) - 4,stop = nchar(strNoClustersK))
  newFileName    = paste0('clusters-','.*','-',strAlphaIndex,'-',strBetaIndex,'-',strLambdaIndex,'-',clusterMethod,'-',strNoClustersK,'.RData')
  return(newFileName)
}

headerNames = function(x) {
  x = vapply(X = x,FUN = function(y) {
    if (y == 'clusterMethod') return('cluster')
    else if (y == 'tagsMethod') return('tags')
    else if (y == 'n') return('runs')
    else if (y == 'articlesMethod') return('articles')
    else if (y == 'second') return('2qt')
    else if (y == 'fourth') return('4qt')
    else if (y == 'median') return('med')
    else if (y == 'experimentCode') return(' ')

    else if (y == 'name') return('Experiment')
    else if (y == 'truePositives') return('TP')
    else if (y == 'trueNegatives') return('TN')
    else if (y == 'falsePositives') return('FP')
    else if (y == 'falseNegatives') return('FN')
    else if (y == 'precision') return('P')
    else if (y == 'recall') return('R')
    else if (y == 'fallout') return('F')
    else if (y == 'missRate') return('M')
    else if (y == 'inverseRecall') return('IR')
    else if (y == 'inversePrecision') return('IP')
    else if (y == 'informedness') return('Inf')
    else if (y == 'markedness') return('Mark')
    else if (y == 'matthewsCorr') return('MC')
    else return(y)
  },FUN.VALUE = character(1))
  return(paste0("\\cellcolor[HTML]{303030}{\\color[HTML]{FFFFFF}",x,"}"))
}

formatName = function(name){
  newName = experimentCode(name)
  newName = paste0('(',newName,') ')
  if (substr(x = name,start = 1,stop = 1) == 'w') {
    newName = paste0(newName,'Ward.D')
  } else {
    newName = paste0(newName,'PAM')
  }
  newName = paste0(newName,'/')
  if (substr(x = name,start = 2,stop = 2) == 'i') {
    newName = paste0(newName,'Index')
  } else {
    newName = paste0(newName,'Top N')
  }
  newName = paste0(newName,'/')
  if (substr(x = name,start = 3,stop = 3) == 'i') {
    newName = paste0(newName,'Index')
  } else {
    newName = paste0(newName,'Top N')
  }
  return(newName)
}

experimentCode = function(name){
  newName = ''
  if (name == 'kii') newName = 'a'
  else if (name == 'kit') newName = 'b'
  else if (name == 'kti') newName = 'c'
  else if (name == 'ktt') newName = 'd'
  else if (name == 'wii') newName = 'e'
  else if (name == 'wit') newName = 'f'
  else if (name == 'wti') newName = 'g'
  else if (name == 'wtt') newName = 'h'
  return(newName)
}

cacheDir       = 'cache//6000_all_clusters'
plotsDir       = 'document//plots//'
tablesDir      = 'document//tables//'
files = list.files(pattern = 'exp.*RData',path = cacheDir)

experimentsResultData = list()
experimentsModelData = list()
clustersData = list()
tagsData = list()
for (i in 1:length(files)) {
  load(file = paste0(cacheDir,'//',fileName = files[i]))
  experimentsResultData$runs            = c(experimentsResultData$runs          ,rep(1:length(experiment$currentResult$y)))
  experimentsResultData$name            = c(experimentsResultData$name          ,rep(substr(experiment$fileName,start = 5,stop = 7),length(experiment$currentResult$y)))
  experimentsResultData$result          = c(experimentsResultData$result        ,-experiment$currentResult$y)
  experimentsResultData$fileName        = c(experimentsResultData$fileName      ,rep(files[i],length(experiment$currentResult$y)))
  experimentsResultData$clusterMethod   = c(experimentsResultData$clusterMethod ,rep(experiment$clusterMethod ,length(experiment$currentResult$y)))
  experimentsResultData$articlesMethod  = c(experimentsResultData$articlesMethod,rep(experiment$articlesMethod,length(experiment$currentResult$y)))
  experimentsResultData$tagsMethod      = c(experimentsResultData$tagsMethod    ,rep(experiment$tagsMethod    ,length(experiment$currentResult$y)))

  experimentsModelData$name             = c(experimentsModelData$name           ,rep(substr(experiment$fileName,start = 5,stop = 7),6))
  experimentsModelData$fileName         = c(experimentsModelData$fileName       ,rep(files[i],6))
  experimentsModelData$clusterMethod    = c(experimentsModelData$clusterMethod  ,rep(experiment$clusterMethod ,6))
  experimentsModelData$articlesMethod   = c(experimentsModelData$articlesMethod ,rep(experiment$articlesMethod,6))
  experimentsModelData$tagsMethod       = c(experimentsModelData$tagsMethod     ,rep(experiment$tagsMethod    ,6))
  
  dmodeltheta = experiment$currentResult$modelFit$dmodeltheta / max(experiment$currentResult$modelFit$dmodeltheta) * 100
  experimentsModelData$importance = c(experimentsModelData$importance ,dmodeltheta[4])
  experimentsModelData$best       = c(experimentsModelData$best       ,experiment$currentResult$xbest[4])
  experimentsModelData$variable   = c(experimentsModelData$variable   ,'alpha')
  experimentsModelData$importance = c(experimentsModelData$importance ,dmodeltheta[5])
  experimentsModelData$best       = c(experimentsModelData$best       ,experiment$currentResult$xbest[5])
  experimentsModelData$variable   = c(experimentsModelData$variable   ,'beta')
  experimentsModelData$importance = c(experimentsModelData$importance ,dmodeltheta[3])
  experimentsModelData$best       = c(experimentsModelData$best       ,experiment$currentResult$xbest[3])
  experimentsModelData$variable   = c(experimentsModelData$variable   ,'lambda')
  experimentsModelData$importance = c(experimentsModelData$importance ,dmodeltheta[1])
  experimentsModelData$best       = c(experimentsModelData$best       ,experiment$currentResult$xbest[1])
  experimentsModelData$variable   = c(experimentsModelData$variable   ,'gamma')
  experimentsModelData$importance = c(experimentsModelData$importance ,dmodeltheta[2])
  experimentsModelData$best       = c(experimentsModelData$best       ,experiment$currentResult$xbest[2])
  experimentsModelData$variable   = c(experimentsModelData$variable   ,'zeta')
  experimentsModelData$importance = c(experimentsModelData$importance ,dmodeltheta[6])
  experimentsModelData$best       = c(experimentsModelData$best       ,experiment$currentResult$xbest[6])
  experimentsModelData$variable   = c(experimentsModelData$variable   ,'k')
  
  clusterFile = as.character(list.files(path = paste0(cacheDir,'//'),
             pattern = getClustersPossibleFileName(alphaIndex = experiment$currentResult$xbest[4], 
                                                   betaIndex = experiment$currentResult$xbest[5], 
                                                   lambdaIndex = experiment$currentResult$xbest[3], 
                                                   clusterMethod = experiment$clusterMethod, 
                                                   noClustersK = round(experiment$currentResult$xbest[6]))))
  if (length(clusterFile) > 0 & nchar(clusterFile) > 0) {
    load(file = paste0(cacheDir,'//',clusterFile))
  }
  else{
    browser()
  }
  
  clustersSumm = as.tibble(cluster$cluster$cluster)
  clustersSumm = add_column(clustersSumm, name = formatName(rep(substr(experiment$fileName,start = 5,stop = 7),nrow(clustersSumm))))
  clustersSumm = add_column(clustersSumm, fileName = rep(files[i],nrow(clustersSumm)))
  if (i == 1) {
    clustersData = clustersSumm
  }
  else {
    clustersData = bind_rows(clustersData,clustersSumm)
  }
  solutionFile = as.character(list.files(path = paste0(cacheDir,'//'),
                                        pattern = getPossibleSolutionsFileName(alphaIndex = experiment$currentResult$xbest[4], 
                                                                               betaIndex = experiment$currentResult$xbest[5], 
                                                                               lambdaIndex = experiment$currentResult$xbest[3], 
                                                                               clusterMethod = experiment$clusterMethod, 
                                                                               noClustersK = round(experiment$currentResult$xbest[6]),
                                                                               tagsMethod = experiment$tagsMethod,
                                                                               articlesMethod = experiment$articlesMethod,
                                                                               gamaIndex = experiment$currentResult$xbest[1],
                                                                               zetaIndex = experiment$currentResult$xbest[2]
                                                                               )))
  if (length(solutionFile) > 0 & nchar(solutionFile) > 0) {
    load(file = paste0(cacheDir,'//',solutionFile))
  }
  else{
    browser()
  }
  solutionData = as.tibble(solution[1:15])
  solutionData = add_column(solutionData, name = formatName(substr(experiment$fileName,start = 5,stop = 7)))
  solutionData = add_column(solutionData, fileName = files[i])
  if (i == 1) {
    solutionsData = solutionData
  }
  else {
    solutionsData = bind_rows(solutionsData,solutionData)
  }
  
  distancesFile = as.character(list.files(path = paste0(cacheDir,'//'),
                                         pattern = getPossibleDistancesFileName(alphaIndex = experiment$currentResult$xbest[4], 
                                                                                betaIndex = experiment$currentResult$xbest[5], 
                                                                                lambdaIndex = experiment$currentResult$xbest[3])))
  if (length(distancesFile) > 0 & nchar(distancesFile) > 0) {
    load(file = paste0(cacheDir,'//',distancesFile))
  }
  else{
    browser()
  }
  if (class(distances$data) == 'list') {
    usersTagsMatrix = distances$data$usersTagsMatrix
  }
  else{
    usersTagsMatrix = distances$usersTagsMatrix
  }
  usersTagsNormMTR = getNormMatrix(usersTagsMatrix, byCol = FALSE)
  usersClustersMTR = getUsersClustersMatrix(cluster$cluster$cluster)
  tagsClustersMTR  = getTagsClustersMatrix(usersTagsNormMTR,usersClustersMTR)
  tagsClusters = as.tibble(rownames_to_column(as.data.frame(tagsClustersMTR),var = 'tag'))
  
  tagsData[[i]] = list(tagsClusters = tagsClusters,
                       name = substr(experiment$fileName,start = 5,stop = 7))
  
}

experimentsResultData = as.tibble(experimentsResultData)
experimentsResultData = add_column(experimentsResultData, formatedName   = vapply(X = experimentsResultData$name,FUN = formatName,FUN.VALUE = character(1)))
experimentsResultData = add_column(experimentsResultData, experimentCode = vapply(X = experimentsResultData$name,FUN = experimentCode,FUN.VALUE = character(1)))
experimentsResultData = experimentsResultData %>% filter(nchar(fileName) == 83)

experimentsModelData = as.tibble(experimentsModelData)
experimentsModelData = add_column(experimentsModelData, formatedName   = vapply(X = experimentsModelData$name,FUN = formatName,FUN.VALUE = character(1)))
experimentsModelData = add_column(experimentsModelData, experimentCode = vapply(X = experimentsModelData$name,FUN = experimentCode,FUN.VALUE = character(1)))
experimentsModelData = experimentsModelData %>% filter(nchar(fileName) == 83)

solutionsData = solutionsData %>% filter(nchar(fileName) == 83)
clustersData = clustersData %>% filter(nchar(fileName) == 83)

sizeLegendTitleFontSize = 20
sizeLegendFontSize = 16
axisTitleSize = 20
axisTextSize = 10
stripTextSize = 10

title1 = 'Informedness versus SPO Run over 8 different experiments.'
title2 = 'Informedness Box Plot of SPO Run over 8 different experiments.'
plotFile1 = paste0(plotsDir,"experiments-results-plot.png")
plotFile2 = paste0(plotsDir,"experiments-results-box-plot.png")
plotFile3 = paste0(plotsDir,"experiments-users-per-clusters-plot.png")

expData = experimentsResultData %>% group_by(experimentCode,clusterMethod,tagsMethod,articlesMethod)

gg = ggplot(data = expData, mapping = aes(x = runs,y = result,colour = formatedName)) + geom_line() + theme(strip.text.x = element_text(size = stripTextSize),plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.text = element_text(size = sizeLegendFontSize), legend.title = element_text(size = sizeLegendTitleFontSize), axis.text = element_text(size = axisTextSize), axis.title = element_text(size = axisTitleSize)) + 
  labs(x = 'Runs', y = 'Informedness', colour = "Cluster/Tags/Articles") +
  facet_grid(~ formatedName)
ggsave(filename = plotFile1, plot = gg, width = 18, height = 8,dpi = 600)

gg2 = ggplot(data = expData, mapping = aes(x = formatedName,y = result,colour = formatedName)) + geom_boxplot() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom", legend.text = element_text(size = sizeLegendFontSize), legend.title = element_text(size = sizeLegendTitleFontSize), axis.text = element_text(size = axisTextSize), axis.title = element_text(size = axisTitleSize)) + 
  labs(x = 'Experiment', y = 'Informedness', colour = "Cluster/Tags/Articles")
ggsave(filename = plotFile2, plot = gg2, width = 18, height = 8, dpi = 600)

tableLabel = 'Experiment Summary'
tableCaption = 'Experiment Summary'
tableFile    = paste0(tablesDir,'experiment-summary-table.tex')
header = "|l|c|l|l|l|r|r|r|r|r|r|r|r|"
table = xtable(expData %>% summarize(n = n(), mean = mean(result), sd = sd(result), min = min(result), second = quantile(result)[2], median = median(result), fourth = quantile(result)[4], max = max(result)))
label(table) = tableLabel
caption(table) = tableCaption
align(table) = header
digits(table) = c(0,0,0,0,0,4,4,4,4,4,4,4,4)
print.xtable(table,size = '\\tiny', sanitize.colnames.function = headerNames ,include.rownames = FALSE ,hline.after = rep(1:nrow(table)) ,file = tableFile, caption.placement = 'top' ,label.placement = 'top')


expModelData = experimentsModelData %>% group_by(experimentCode,clusterMethod,tagsMethod,articlesMethod,variable)
write.csv2(x = expModelData,file = paste0(tablesDir,'modelData.csv'),sep = ';',row.names = TRUE,col.names = TRUE, fileEncoding = 'UTF-8')

clustersPlotData = clustersData %>% 
                      group_by(cluster,name) %>% 
                        summarise(total_cnt=n()) %>% 
                          ungroup() %>% 
                            select(-cluster) %>% 
                              group_by(name) %>%
                                arrange(desc(name),desc(total_cnt)) %>% 
                                  mutate(rank = row_number())

gg3 = ggplot(data = clustersPlotData, mapping = aes(x = rank,y = total_cnt,colour = name)) + geom_line() + theme(strip.text.x = element_text(size = stripTextSize),plot.title = element_text(hjust = 0.5), legend.position = "right", legend.text = element_text(size = sizeLegendFontSize), legend.title = element_text(size = sizeLegendTitleFontSize), axis.text = element_text(size = axisTextSize), axis.title = element_text(size = axisTitleSize)) + 
  labs(x = 'Cluster', y = 'Users', colour = "Cluster/Tags/Articles") +
  scale_x_continuous(breaks = seq(0, 80, by = 5)) +
  scale_y_continuous(breaks = seq(0, 3000, by = 200))

ggsave(filename = plotFile3, plot = gg3, width = 12, height = 8, dpi = 600)

table2Label = 'results-metrics-summary-table'
table2Caption = "Experiment's Results Metrics Summary"
table2File    = paste0(tablesDir,'results-metrics-summary-table.tex')
resultsData = solutionsData %>% mutate(totalRecomm = truePositives + falsePositives, totalNotRecomm = trueNegatives+falseNegatives) %>%
                                select(name,
                                       truePositives,
                                       falsePositives,
                                       totalRecomm,
                                       trueNegatives,
                                       falseNegatives, 
                                       totalNotRecomm,
                                       precision, 
                                       recall, 
                                       fallout, 
                                       missRate, 
                                       inverseRecall,
                                       inversePrecision,
                                       informedness,
                                       markedness,
                                       matthewsCorr) %>% 
                                          column_to_rownames(var = "name") %>% 
                                             as.data.frame() %>% 
                                                t() %>%
                                                   as.data.frame() %>% 
                                                      rownames_to_column(var = 'Variable')

table2 = xtable(resultsData)
label(table2) = table2Label
caption(table) = table2Caption
align(table2) = c('l','|l','|r','|r','|r','|r','|r','|r','|r','|r|')

# align(table2) = matrix(data = c(c('l','|l','|c','|c','|c','|c','|c','|c','|c','|c|'),
#                                 rep(c('l','|l','|r','|r','|r','|r','|r','|r','|r','|r|'),nrow(resultsData))), nrow = nrow(resultsData)+1,byrow = TRUE)

mdat <- matrix(data = 4,nrow = nrow(resultsData), ncol = ncol(resultsData) + 1)
mdat[1:6,2:10] = 0
mdat[,1] = 0
digits(table2) = mdat
print.xtable(table2,
             size = '\\tiny', 
             sanitize.colnames.function = headerNames,
             include.rownames = FALSE,
             hline.after = rep(1:nrow(table2)),
             file = table2File, 
             caption.placement = 'top',
             label.placement = 'top',
             rotate.colnames = TRUE)


# View(solutionsData)
tableLaTex = paste0('\\begin{table}','\n')
tableLaTex = paste0(tableLaTex,'\\caption{PAM experiments top 6 clusters word clouds, according to user count.}','\n')
tableLaTex = paste0(tableLaTex,'\\label{pam-wordclouds-table}','\n')
tableLaTex = paste0(tableLaTex,'\\begin{adjustbox}{width=\\textwidth}','\n')
# \begin{figure}
tableLaTex = paste0(tableLaTex,'\\setlength{\\tabcolsep}{3pt}','\n')
tableLaTex = paste0(tableLaTex,'\\begin{tabular}{|c|c|c|c|}','\n')
for (k in 1:2) {
  if (k == 2) {
    tableLaTex = paste0(tableLaTex,'\\end{tabular}','\n')
    tableLaTex = paste0(tableLaTex,'\\end{adjustbox}','\n')
    tableLaTex = paste0(tableLaTex,'\\end{table}','\n')
    tableLaTex = paste0(tableLaTex,'\\begin{table}','\n')  
    tableLaTex = paste0(tableLaTex,'\\caption{Ward.D experiments top 6 clusters word clouds, according to user count.}','\n')
    tableLaTex = paste0(tableLaTex,'\\label{ward-d-wordclouds-table}','\n')
    tableLaTex = paste0(tableLaTex,'\\begin{adjustbox}{width=\\textwidth}','\n')
    tableLaTex = paste0(tableLaTex,'\\setlength{\\tabcolsep}{3pt}','\n')
    tableLaTex = paste0(tableLaTex,'\\begin{tabular}{|c|c|c|c|}','\n')
  }
  tableLaTex = paste0(tableLaTex,'\\hline','\n')
  for (i in (k * k + (k - 1)):(k * k + 3 + (k - 1))) {
    # if (j == 1 | j == 5) {
    #   
    # }
    tableLaTex = paste0(tableLaTex,'\\cellcolor[HTML]{303030}{\\color[HTML]{FFFFFF}\\footnotesize ',formatName(tagsData[[i]]$name),'}')
    if (i != 4 & i != 8) {
      tableLaTex = paste0(tableLaTex,'&')
    }
    tableLaTex = paste0(tableLaTex,'\n')
  }
  tableLaTex = paste0(tableLaTex,'\\\\ \\hline','\n')
  for (j in 1:6) {
    # 1 = 1-4 = k*k+(k-1),k*k+3+(k-1) = 1*1+1-1   = 1, 1*1+3+1-1 = 4
    # 2 = 5-8 = k*k+(k-1),k*k+3+(k-1) = 2*2+(2-1) = 5, 2*2+3+2-1 = 8
    for (i in (k * k + (k - 1)):(k * k + 3 + (k - 1))) {
      clusterTotals = clustersData %>% 
        ungroup() %>% 
        filter(grepl(tagsData[[i]]$name,fileName)) %>% 
        group_by(cluster) %>% 
        summarize(n = n()) %>% 
        arrange(desc(n)) %>% 
        top_n(6,n)
      set.seed(1)
      clusterIndex = which(colnames(tagsData[[i]]$tagsClusters) == as.numeric(clusterTotals[j,1]))
      clusterTags = bind_cols(tagsData[[i]]$tagsClusters[,1],tagsData[[i]]$tagsClusters[,clusterIndex])
      names(clusterTags) = c('tag','freq')
      clusterTags = clusterTags %>% filter(freq > 0)
      fileName = paste0('wordcloud-',tagsData[[i]]$name,'-',j,'.png')
      # png(paste0('document//plots//',fileName), width = 600, height = 600)
      # wordcloud(words = clusterTags$tag, 
      #           freq = clusterTags$freq, 
      #           min.freq = 0,
      #           max.words = 200, 
      #           random.order = FALSE, 
      #           rot.per = 0,
      #           colors = brewer.pal(8, "Dark2"),
      #           use.r.layout = FALSE)  
      # dev.off()
      tableLaTex = paste0(tableLaTex,'\\shortstack{\\subfloat{\\Includegraphics[width=0.20\\linewidth]{plots/',fileName,'}}')
      tableLaTex = paste0(tableLaTex,'\\\\{\\tiny (',letters[i],'-',j,') ',as.numeric(clusterTotals[j,2]),' users}}')
      if (i != 4 & i != 8) {
        tableLaTex = paste0(tableLaTex,'&')
      }
      tableLaTex = paste0(tableLaTex,'\n')
    }
    tableLaTex = paste0(tableLaTex,'\\\\ \\hline','\n')
  }
}
tableLaTex = paste0(tableLaTex,'\\end{tabular}','\n')
tableLaTex = paste0(tableLaTex,'\\end{adjustbox}','\n')

tableLaTex = paste0(tableLaTex,'\\end{table}')

write(x = tableLaTex,file = 'document//tables//wordcloud.tex')

# \begin{figure}
# \setlength{\tabcolsep}{3pt}	
# \begin{tabular}{|c|c|c|c|}
# \hline
# {\small (a) PAM/Index/Index}&
# {\small (a) PAM/Index/Index}&
# {\small (a) PAM/Index/Index}&
# {\small (a) PAM/Index/Index}\\ \hline
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}&
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}&
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}&
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}\\ 
# \hline
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}&
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}&
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}&
# \shortstack{\subfloat{\Includegraphics[width=0.23\linewidth]{plots/wordcloud-kii-1.png}}\\{\tiny 393 users}}\\ \hline
# \end{tabular}
# \end{figure}