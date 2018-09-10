debugSource(fileName = 'scripts/dataBase.R')
debugSource(fileName = 'scripts/plots.R')
debugSource(fileName = 'scripts/distances.R')

trainningConnection = getDataBaseConnection(schema = 'trainning6000', dbUser = "cl-us-gzh", dbHost = "10.238.4.109", dbName = "cl-us-gzh", dbPass = "cl-us-gzh")
plotsDir       = 'document//plots//'

# Plot the popularity penalty index over
articlesPopularityIndexVector = getArticlesPopularityIndexVector(trainningConnection)
articlesPopularityIndexVector = setPopularityIndex(articlesPopularityIndexVector)

titleText = 'Popularity Index x Total Access Count'
gg = ggplot2::ggplot(data = articlesPopularityIndexVector, 
                     mapping = aes(popularity,popularityIndex,colour = popularityIndex)) + 
  geom_line() + 
  ggtitle(titleText) + 
  labs(x = 'Total Access Count', y = 'Popularity Index', colour = 'Popularity Index') +
  scale_colour_gradient(high = '#15314B', low = '#50A6E7') +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 350)
print(gg)
ggsave(filename = paste0(plotsDir,"popularity-index-plot.png"),
       plot = gg, 
       width = 10, 
       height = 8, 
       dpi = 600)

indexes = c(1,0.25,0.1)
days = 100
gg2 = list()
titleText = ''
for (i in 1:length(indexes)) {
  attenuatedSample = as.data.frame(matrix(rep(1:days), ncol = 1, dimnames = list(NULL, c("time"))))
  attenuatedSample$attenuation = vapply(X = attenuatedSample,
                                        FUN = getForgettingIndex,
                                        FUN.VALUE = numeric(days),
                                        lambdaIndex = indexes[[i]])
  
  gg2[[i]] = ggplot2::ggplot(data = attenuatedSample, 
                            mapping = aes(x = time, y = attenuation, colour = attenuation)) + 
    geom_line() + 
    ggtitle(paste0('Coef.: ',printNice(value = indexes[[i]],size = 5, precision = 3))) + 
    labs(x = 'Time (days)', y = 'Attenuation Coefficient', colour = '') +
    scale_colour_gradient(high = '#15314B', low = '#50A6E7') +
    theme(plot.title = element_text(hjust = 0.5)) +  
    xlim(0, 100) + 
    ylim(0, 1)
}
ggg = grid.arrange(gg2[[1]], gg2[[2]],gg2[[3]], ncol = 3, top = 'Attenuation Curve over 100 days with 1, 0.25 and 0.1 coefficients')

print(ggg)
ggsave(filename = paste0(plotsDir,"attenuation-index-plot.png"),
       plot = ggg, 
       width = 30, 
       height = 8, 
       dpi = 600)




