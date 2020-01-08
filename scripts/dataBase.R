#### Libraries ####
list.of.packages = c('RPostgreSQL','dplyr','tidyr')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

# Loads RPostgreSQL tools
library(RPostgreSQL)
library(dplyr)
library(tidyr)

debugSource("scripts/util.R")

#### Code ####
clearConnections = function(){
  
  # Disconects any existing connection to the database
  lapply(X = dbListConnections(dbDriver("PostgreSQL")),FUN = dbDisconnect)
  
}

# Creates the database connection
getDataBaseConnection = function(schema,dbUser,dbHost,dbName,dbPass, dbPort = 5432){
  # Loads required package: PostgreSQL
  pg = dbDriver("PostgreSQL")
  
  # Creates the database connection
  con = dbConnect(pg, user = dbUser, password = dbPass, host = dbHost, port = dbPort, dbname = dbName)
  dbGetQuery(con,paste('set search_path to ',schema))
  return(con)
}

# Extracts the articles x tags matrix from the database
getArticlesTagsMatrix = function(con, from = as.Date("01-01-1970", format = "%d-%m-%Y"), justPushes = FALSE){
  if (justPushes) {
    whre = 'where ar.push_time is not null '
  }
  else{
    whre = 'where ar.push_time is null '
  }
  if (from > as.Date("01-01-1970", format = "%d-%m-%Y")){
    whre = paste0(whre," and exists (select 1 from events e where ar.id = e.article_id and e.data_evento >= '",format(from,"%Y-%m-%d"),"')")
    # Extracts articles versus tags table, with count of occurences  
    articlesTags = dbGetQuery(con, paste0("select ar.id,coalesce(case when trim(tg.no_plural) = '' then '<NA>' else trim(tg.no_plural) end ,'<NA>') as",'"tagName"',",count(1)
                            from articles ar
                            left join articles_tags art on ar.id = art.article_id
                            left join tags tg on art.tag_id = tg.id ",whre,"
                            group by tg.no_plural, ar.id
                            order by ar.id desc, tg.no_plural desc"))
  }
  else{
    # Extracts articles versus tags table, with count of occurences  
    articlesTags = dbGetQuery(con, paste0("select ar.id,coalesce(case when trim(tg.no_plural) = '' then '<NA>' else trim(tg.no_plural) end ,'<NA>') as",'"tagName"',",count(1)
                            from articles ar
                            left join articles_tags art on ar.id = art.article_id
                            left join tags tg on art.tag_id = tg.id ",whre,"
                            group by tg.no_plural, ar.id
                            order by ar.id desc, tg.no_plural desc"))    
  }
  

  
  # Converts table into matrix
  articlesTagsMatrix = spread(articlesTags,key = tagName,value = count, fill = 0)
  articlesTagsMatrix = arrange(articlesTagsMatrix,desc(id))
  # Sets rownames on the DF and removes id column
  rownames(articlesTagsMatrix) = articlesTagsMatrix[,1]
  articlesTagsMatrix$id = NULL
  
  return(articlesTagsMatrix)
}


# Extracts the articles x tags matrix from the database
getArticlesTagsSparseMatrix = function(con, from = as.Date("01-01-1970", format = "%d-%m-%Y"), justPushes = FALSE){
  if (justPushes) {
    whre = 'where ar.push_time is not null '
  }
  else{
    whre = 'where ar.push_time is null '
  }
  if (from > as.Date("01-01-1970", format = "%d-%m-%Y")){
    whre = paste0(whre," and exists (select 1 from events e where ar.id = e.article_id and e.data_evento >= '",format(from,"%Y-%m-%d"),"')")
    # Extracts articles versus tags table, with count of occurences  
    articlesTags = dbGetQuery(con, paste0("select ar.id as ",'"articleId"',",coalesce(case when trim(tg.no_plural) = '' then '<NA>' else trim(tg.no_plural) end ,'<NA>') as",'"tagName"',",count(1)
                                          from articles ar
                                          left join articles_tags art on ar.id = art.article_id
                                          left join tags tg on art.tag_id = tg.id ",whre,"
                                          group by tg.no_plural, ar.id
                                          order by ar.id desc, tg.no_plural desc"))
  }
  else{
    # Extracts articles versus tags table, with count of occurences  
    articlesTags = dbGetQuery(con, paste0("select ar.id as ",'"articleId"',",coalesce(case when trim(tg.no_plural) = '' then '<NA>' else trim(tg.no_plural) end ,'<NA>') as",'"tagName"',",count(1)
                            from articles ar
                            left join articles_tags art on ar.id = art.article_id
                            left join tags tg on art.tag_id = tg.id ",whre,"
                            group by tg.no_plural, ar.id
                            order by ar.id desc, tg.no_plural desc"))    
  }
  
  
  tags = as.tibble(articlesTags) %>% select(tagName) %>% distinct(tagName) %>% arrange(-desc(tagName))
  tags = add_column(tags, tagIndex = seq(nrow(tags)))
  
  articles = as.tibble(articlesTags) %>% select(articleId) %>% distinct(articleId) %>% arrange(desc(articleId))
  articles = add_column(articles, articleIndex = seq(nrow(articles)))
  
  articlesTags = articlesTags %>% left_join(tags,by = "tagName" )
  articlesTags = articlesTags %>% left_join(articles,by = "articleId" )
  
  articlesTagsSparseMatrix = sparseMatrix(i = articlesTags$articleIndex,
                                           j = articlesTags$tagIndex,
                                           x = articlesTags$count,
                                           dims = c(length(articles$articleId),length(tags$tagName)),
                                           dimnames = list(c(articles$articleId),c(tags$tagName)))
  
  # Converts table into matrix
  #articlesTagsMatrix = spread(articlesTags,key = tagName,value = count, fill = 0)
  #articlesTagsMatrix = arrange(articlesTagsMatrix,desc(id))
  # Sets rownames on the DF and removes id column
  #rownames(articlesTagsMatrix) = articlesTagsMatrix[,1]
  #articlesTagsMatrix$id = NULL
  
  return(articlesTagsSparseMatrix)
}

# Retrieves max timestamp from events
getEventsMaxTime = function(con, from = as.Date("01-01-1970", format = "%d-%m-%Y")){
  if (from > as.Date("01-01-1970", format = "%d-%m-%Y")){
    return(as.numeric(dbGetQuery(con, paste0("select max(extract(epoch from e.data_evento)) from events e where e.data_evento >= '",format(from,"%Y-%m-%d"),"'"))))
  }
  else{
    return(as.numeric(dbGetQuery(con, "select max(extract(epoch from e.data_evento)) from events e")))
  }
}

getUsersArticlesMatrices = function(con, from = as.Date("01-01-1970", format = "%d-%m-%Y"), withPushes = FALSE){
  # Extracts users versus articles table, with avverage age for each article  
  if (withPushes) {
    whre = ''
  }
  else{
    whre = 'where a.push_time is null '
  }
  
  if (from > as.Date("01-01-1970", format = "%d-%m-%Y")){
    if (whre == ''){
      whre = 'where '
    }
    else{
      whre = paste0(whre,' and ')
    }
    whre = paste0(whre," e.data_evento >= '",format(from,"%Y-%m-%d"),"' ")
  }
  usersArticles = dbGetQuery(con, paste0('select u.email,
                                                   e.article_id as "articleId" ,
                                                   avg(extract(epoch from e.data_evento)) as "avgTime",
                                                   count(e.article_id) as "count"
                                              from users u
                                             inner join events e on u.email = e.email
                                             inner join articles a on e.article_id = a.id 
                                             ',whre,'
                                             group by u.email,e.article_id,a.popularity'))    
  # Converts table into matrix
  usersArticlesMatrix = usersArticles
  usersArticlesMatrix$avgTime = NULL
  usersArticlesMatrix = spread(usersArticlesMatrix,key = email,value = count, fill = 0)
  usersArticlesMatrix = arrange(usersArticlesMatrix,desc(articleId))
  # Sets rownames on the DF and removes id column
  rownames(usersArticlesMatrix) = usersArticlesMatrix[,1]
  usersArticlesMatrix$articleId = NULL
  
  # Converts table into matrix
  usersArticlesAvgTimeMatrix = usersArticles
  usersArticlesAvgTimeMatrix$count = NULL
  usersArticlesAvgTimeMatrix = spread(usersArticlesAvgTimeMatrix,key = email,value = avgTime, fill = 0)
  usersArticlesAvgTimeMatrix = arrange(usersArticlesAvgTimeMatrix,desc(articleId))
  # Sets rownames on the DF and removes id column
  rownames(usersArticlesAvgTimeMatrix) = usersArticlesAvgTimeMatrix[,1]
  usersArticlesAvgTimeMatrix$articleId = NULL
  
  return(list(usersArticlesMatrix        = usersArticlesMatrix,
              usersArticlesAvgTimeMatrix = usersArticlesAvgTimeMatrix))
}


getUsersArticlesSparseMatrices = function(con, from = as.Date("01-01-1970", format = "%d-%m-%Y"), withPushes = FALSE, alphaIndex){
  # Extracts users versus articles table, with avverage age for each article  
  if (withPushes) {
    whre = ''
  }
  else{
    whre = 'where a.push_time is null '
  }
  
  if (from > as.Date("01-01-1970", format = "%d-%m-%Y")){
    if (whre == ''){
      whre = 'where '
    }
    else{
      whre = paste0(whre,' and ')
    }
    whre = paste0(whre," e.data_evento >= '",format(from,"%Y-%m-%d"),"' ")
  }
  usersArticles = dbGetQuery(con, paste0('select u.email,
                                         e.article_id as "articleId" ,
                                         ((select max(extract(epoch from m.data_evento)) from events m) - avg(extract(epoch from e.data_evento)))/60/60/24 as "avgTime",
                                         count(e.article_id) as "count"
                                         from users u
                                         inner join events e on u.email = e.email
                                         inner join articles a on e.article_id = a.id 
                                         ',whre,'
                                         group by u.email,e.article_id,a.popularity'))    
  
  users = as.tibble(usersArticles) %>% select(email) %>% distinct(email) %>% arrange(-desc(email))
  users = add_column(users, userIndex = seq(nrow(users)))
  
  articles = as.tibble(usersArticles) %>% select(articleId) %>% distinct(articleId) %>% arrange(desc(articleId))
  articles = add_column(articles, articleIndex = seq(nrow(articles)))
  
  usersArticles = usersArticles %>% left_join(users,by = "email" )
  usersArticles = usersArticles %>% left_join(articles,by = "articleId" )
  
  usersArticles$avgTimeAtten = getAttenCoeff(usersArticles$avgTime,alphaIndex)
  
  usersArticlesSparseMatrix = sparseMatrix(i = usersArticles$articleIndex,
                                           j = usersArticles$userIndex,
                                           x = usersArticles$count,
                                           dims = c(length(articles$articleId),length(users$email)),
                                           dimnames = list(c(articles$articleId),c(users$email)))
  
  usersArticlesAvgTimeSparseMatrix = sparseMatrix(i = usersArticles$articleIndex,
                                                  j = usersArticles$userIndex,
                                                  x = usersArticles$avgTime,
                                                  dims = c(length(articles$articleId),length(users$email)),
                                                  dimnames = list(c(articles$articleId),c(users$email)))
  
  usersArticlesAttenCoeffMatrix = sparseMatrix(i = usersArticles$articleIndex,
                                               j = usersArticles$userIndex,
                                               x = usersArticles$avgTimeAtten,
                                               dims = c(length(articles$articleId),length(users$email)),
                                               dimnames = list(c(articles$articleId),c(users$email)))
  
  
  return(list(usersArticlesSparseMatrix        = usersArticlesSparseMatrix,
              usersArticlesAvgTimeSparseMatrix = usersArticlesAvgTimeSparseMatrix,
              usersArticlesAttenCoeffMatrix    = usersArticlesAttenCoeffMatrix,
              usersArticles                    = usersArticles,
              articles                         = articles))
}

getUsersArticlesPushM = function(con){
  usersArticles = dbGetQuery(con, paste0('select u.email,
                                                 e.article_id as "articleId" ,
                                                 1 as "count"
                                            from users u
                                           inner join events e on u.email = e.email
                                           inner join articles a on e.article_id = a.id 
                                           where a.push_time is not null 
                                           group by u.email,e.article_id,a.popularity'))

  # Converts table into matrix
  usersArticlesM = usersArticles
  usersArticlesM = spread(usersArticlesM,key = email,value = count, fill = 0)
  usersArticlesM = arrange(usersArticlesM,desc(articleId))
  # Sets rownames on the DF and removes id column
  rownames(usersArticlesM) = usersArticlesM[,1]
  usersArticlesM$articleId = NULL
  
  return(usersArticlesM)
}


# Calculates popularity penalty index
getArticlesPopularityIndexVector = function(con, from = as.Date("01-01-1970", format = "%d-%m-%Y")) {
  if (from > as.Date("01-01-1970", format = "%d-%m-%Y")){
    articlesPopularityIndexVector = dbGetQuery(con, paste0('select ar.id as "articleId",', 
                                                           'count(distinct e.email) as "popularity"',
                                                       'from articles ar ',
                                                 'inner join events e on ar.id = e.article_id ',
                                                                     "where e.data_evento >='",format(from,"%Y-%m-%d"),"'", 
                                                      'group by ar.id order by ar.id desc'))
  }
  else{
    articlesPopularityIndexVector = dbGetQuery(con, 'select id as "articleId",
                                                            popularity as "popularity" 
                                                       from articles
                                                      group by id
                                                      order by id desc')
  }
  articlesPopularityIndexVector = arrange(articlesPopularityIndexVector,desc(articleId))
  return(articlesPopularityIndexVector)
}

# Applies popularity penalty index
getArticlesUserUserTimeDiffTable = function(con){
  # articlesUserUserTimeDiffTable = dbGetQuery(con, 'select a.id as "articleId",
  #                                                          userA.email as "userA",
  #                                                          userB.email as "userB",
  #                                                          avg(extract(epoch from eventsUserA.data_evento)) - avg(extract(epoch from eventsUserB.data_evento)) as diff
  #                                                     from users userA
  #                                                    inner join events eventsUserA on userA.email = eventsUserA.email
  #                                                    inner join articles a on eventsUserA.article_id = a.id
  #                                                    inner join events eventsUserB on eventsUserB.article_id = a.id
  #                                                    inner join users userB on userB.email = eventsUserB.email
  #                                                    where userA.email != userB.email
  #                                                    group by a.id,userA.email,userB.email
  #                                                   order by userA.email,userB.email,a.id desc')

  
  articlesUserUserTimeDiffTable = dbGetQuery(con, 'select articleid as "articleId",
                                                          usera as "userA",
                                                          userb as "userB",
                                                          diff
                                                     from user_user_time_diff uu')
    return(articlesUserUserTimeDiffTable)
}

# Extracts the tags from the database
getTags = function(con){
  tags = dbGetQuery(con, 'select id,name from tags')
  return(tags)
}

# Updates the database with tags without plural
updateTagsNoPlural <- function(con, tags){
  updateStatement = paste0("update tags set no_plural = name")
  dbExecute(con,updateStatement)
  indexes = tags[,2] != tags[,3]
  tagsToUpdate = tags[indexes,]
  updateStatement = paste0("update tags set no_plural = (case ",paste0(paste0(" when id = ",as.vector(tagsToUpdate$id)," then '",as.vector(tagsToUpdate$stem),"' "),collapse = '')," end) where id in (",paste0(as.vector(tagsToUpdate$id),sep = '',collapse = ','),")",collapse = '')
  return(dbExecute(con,updateStatement))
}

getTrainningData = function(con){ 
  articlesTagsMatrix            = getArticlesTagsMatrix(con)
  articlesPopularityIndexVector = getArticlesPopularityIndexVector(con)
  usersArticlesMatrices         = getUsersArticlesMatrices(con)

  eventsMaxTime                 = getEventsMaxTime(con) 
  articlesPopularityIndexVector = setPopularityIndex(articlesPopularityIndexVector)
  
  usersArticlesTimeDiffMatrix   = getUsersArticlesTimeDiffMatrix(eventsMaxTime,usersArticlesMatrices$usersArticlesAvgTimeMatrix)

  trainningData = list(articlesTagsMatrix            = articlesTagsMatrix,
                       usersArticlesMatrix           = usersArticlesMatrices$usersArticlesMatrix,
                       usersArticlesTimeDiffMatrix   = usersArticlesTimeDiffMatrix,
                       articlesPopularityIndexVector = articlesPopularityIndexVector,
                       usersArticlesAvgTimeMatrix    = usersArticlesMatrices$usersArticlesAvgTimeMatrix) 
  return(trainningData)
}
getTrainningSparseData = function(con, alphaIndex){ 
  eventsMaxTime                     = getEventsMaxTime(con) 
  articlesTagsSparseMatrix          = getArticlesTagsSparseMatrix(con)
  articlesPopularityIndexVector     = getArticlesPopularityIndexVector(con)
  usersArticlesSparseMatrices       = getUsersArticlesSparseMatrices(con, alphaIndex = alphaIndex)
  usersArticlesTimeDiffSparseMatrix = usersArticlesSparseMatrices$usersArticlesAvgTimeSparseMatrix
  
  articlesPopularityIndexVector      = setPopularityIndex(articlesPopularityIndexVector)
  #usersArticlesTimeDiffSparseMatrix  = getUsersArticlesTimeDiffSparseMatrix(eventsMaxTime,usersArticlesSparseMatrices$usersArticlesAvgTimeSparseMatrix)
  
  trainningData = list(articlesTagsMatrix            = articlesTagsSparseMatrix,
                       usersArticlesMatrix           = usersArticlesSparseMatrices$usersArticlesSparseMatrix,
                       usersArticlesTimeDiffMatrix   = usersArticlesTimeDiffSparseMatrix,
                       usersArticlesAttenCoeffMatrix = usersArticlesSparseMatrices$usersArticlesAttenCoeffMatrix,
                       articlesPopularityIndexVector = articlesPopularityIndexVector,
                       usersArticles                 = usersArticlesSparseMatrices$usersArticles,
                       articles                      = usersArticlesSparseMatrices$articles) 
  return(trainningData)
}
getTrainningDataFrom = function(con,from){
  articlesTagsMatrix            = getArticlesTagsMatrix(con,from)
  articlesPopularityIndexVector = getArticlesPopularityIndexVector(con,from)
  usersArticlesMatrices         = getUsersArticlesMatrices(con,from)
  eventsMaxTime                 = getEventsMaxTime(con,from) 
  
  articlesPopularityIndexVector      = setPopularityIndex(articlesPopularityIndexVector)
  usersArticlesTimeDiffMatrix        = getUsersArticlesTimeDiffMatrix(eventsMaxTime,usersArticlesMatrices$usersArticlesAvgTimeMatrix)
  
  trainningData = list(articlesTagsMatrix            = articlesTagsMatrix,
                       usersArticlesMatrix           = usersArticlesMatrices$usersArticlesMatrix,
                       usersArticlesTimeDiffMatrix   = usersArticlesTimeDiffMatrix,
                       articlesPopularityIndexVector = articlesPopularityIndexVector) 
  return(trainningData)
}

getTestingData = function(con){
  articlesTagsMTS          = getArticlesTagsMatrix(con = con)
  usersArticlesMatricesTS  = getUsersArticlesMatrices(con = con)
  
  testingData = list(articlesTagsMTS         = articlesTagsMTS,
                     usersArticlesMatricesTS = usersArticlesMatricesTS)
  
  return(testingData)
}

# Applies the popularity index on the popularity index vector based on article count
setPopularityIndex = function(articlesPopularityIndexVector){
  articlesPopularityIndexVector$popularityIndex = vapply(X = articlesPopularityIndexVector$popularity,FUN = getPopularityIndex,FUN.VALUE = numeric(1)) 
  return(articlesPopularityIndexVector)
}

# Calculates the popularity based on the number of accesses it received
getPopularityIndex = function(articleAccesses){
  return(1/(log1p(articleAccesses)))
}

# Extracts the users x articles time difference matrix from the database
getUsersArticlesTimeDiffMatrix = function(eventsMaxTime, usersArticlesAvgTimeMatrix){
  usersArticlesTimeDiffMatrix = (eventsMaxTime - usersArticlesAvgTimeMatrix)/60/60/24
  return(usersArticlesTimeDiffMatrix)  
}

getUsersArticlesTimeDiffSparseMatrix = function(eventsMaxTime, usersArticlesAvgTimeSparseMatrix){
  usersArticlesTimeDiffSparseMatrix = (eventsMaxTime - usersArticlesAvgTimeSparseMatrix)/60/60/24
  return(usersArticlesTimeDiffSparseMatrix)  
}

writeClustersToDB = function(clusters, con){
  dbSendQuery(con, "delete from clusters")
  dbSendQuery(con, "copy clusters from stdin")
  postgresqlCopyInDataframe(con, as.data.frame(clusters))
}


writeClustersProfilesToDB = function(clustersProfiles, con){
  
  dbSendQuery(con, "delete from clusters_profiles")
  dfClustersProfiles = as.data.frame(melt(t(result$clustersProfiles), varnames = c("cluster","tag"), value.name = "rec_idx"))
  print(nrow(dfClustersProfiles))
  dbSendQuery(trainningConnection, "copy clusters_profiles(cluster,tag,rec_idx) from stdin")
  postgresqlCopyInDataframe(trainningConnection, dfClustersProfiles)
  
}
