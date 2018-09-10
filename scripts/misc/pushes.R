library(jsonlite)

uri = paste0('http://api-online.rbs.com.br/notifications.json?',
             'sort=-_created&',
             'max_results=50&',
             'page=1&',
             'projection={"payload":1,"_created":1,"send_time":1}&',
             'where={"_created":{"$lt": "Mon, 18 Dec 2017 23:00:00 GMT"}}')
firstPage = fromJSON(uri)
pages = trunc(firstPage$`_meta`$total/50)+1
articles = data.frame("","","","","","",stringsAsFactors = FALSE)
colnames(articles) = c("id","send_time","created","external_id","article_id","exposed_id")
for (j in 1:5){
  uri = paste0('http://api-online.rbs.com.br/notifications.json?',
               'sort=-_created&',
               'max_results=50&',
               'page=',j,'&',
               'projection={"payload":1,"_created":1,"send_time":1}&',
               'where={"_created":{"$lt": "Mon, 18 Dec 2017 23:00:00 GMT"}}')
  print(uri)
  curPage = fromJSON(uri)
  
  size = curPage$`_meta`$max_results
  length(curPage$`_items`)
  for (i in 1:size){
    item = curPage$`_items`[i,]
    articles = rbind(articles,c(item$`_id`,
                                item$send_time,
                                item$`_created`,
                                item$payload$external_id,
                                item$payload$article_id,
                                item$payload$exposed_id))
  }
  
}

View(articles)