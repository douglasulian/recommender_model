#### Libraries ####
list.of.packages = c('RPostgreSQL','dplyr','tidyr')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

# Loads RPostgreSQL tools
library(RPostgreSQL)
library(dplyr)
library(tidyr)

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
getArticlesTagsMatrix = function(con, justPushes = FALSE){
  if (justPushes) {
    where = 'where ar.push_time is not null '
  }
  else{
    where = 'where ar.push_time is null '
  }
  # Extracts articles versus tags table, with count of occurences  
  articlesTags = dbGetQuery(con, paste0("select ar.id,coalesce(case when trim(tg.no_plural) = '' then '<NA>' else trim(tg.no_plural) end ,'<NA>') as",'"tagName"',",count(1)
                            from articles ar
                            left join articles_tags art on ar.id = art.article_id
                            left join tags tg on art.tag_id = tg.id ",where,"
                            group by tg.no_plural, ar.id
                            order by ar.id desc, tg.no_plural desc"))
  
  # Converts table into matrix
  articlesTagsMatrix = spread(articlesTags,key = tagName,value = count, fill = 0)
  articlesTagsMatrix = arrange(articlesTagsMatrix,desc(id))
  # Sets rownames on the DF and removes id column
  rownames(articlesTagsMatrix) = articlesTagsMatrix[,1]
  articlesTagsMatrix$id = NULL
  
  return(articlesTagsMatrix)
}

# Retrieves max timestamp from events
getEventsMaxTime = function(con){
  return(as.numeric(dbGetQuery(con, "select max(extract(epoch from e.data_evento)) from events e")))
}

getUsersArticlesMatrices = function(con, withPushes = FALSE){
  # Extracts users versus articles table, with avverage age for each article  
  if (withPushes) {
    where = ''
  }
  else{
    where = 'where a.push_time is null '
  }
  usersArticles = dbGetQuery(con, paste0('select u.email,
                                                 e.article_id as "articleId" ,
                                                 avg(extract(epoch from e.data_evento)) as "avgTime",
                                                 count(e.article_id) as "count"
                                            from users u
                                           inner join events e on u.email = e.email
                                           inner join articles a on e.article_id = a.id 
                                           ',where,'
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
getArticlesPopularityIndexVector = function(con){
  articlesPopularityIndexVector = dbGetQuery(con, 'select id as "articleId",
                                                          popularity as "popularity" 
                                                     from articles
                                                    group by id
                                                    order by id desc')
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


