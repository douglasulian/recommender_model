library(dplyr)
library(tibble)
library(Matrix)
users = as.tibble(usersArticles) %>% select(email) %>% distinct(email) %>% arrange(-desc(email))
users = add_column(users, userIndex = seq(nrow(users)))

articles = as.tibble(usersArticles) %>% select(articleId) %>% distinct(articleId) %>% arrange(-desc(articleId))
articles = add_column(articles, articleIndex = seq(nrow(articles)))

usersArticles = usersArticles %>% left_join(users,by = "email" )
usersArticles = usersArticles %>% left_join(articles,by = "articleId" )


usersArticlesSparseMatrix = sparseMatrix(i = usersArticles$userIndex,
                                         j = usersArticles$articleIndex,
                                         x = usersArticles$count,
                                         dims = c(length(users$email),length(articles$articleId)),
                                         dimnames = list(c(users$email),c(articles$articleId)))


usersArticlesMatrix = usersArticles
usersArticlesMatrix$avgTime = NULL
usersArticlesMatrix = spread(usersArticlesMatrix,key = email,value = count, fill = 0)
usersArticlesMatrix = arrange(usersArticlesMatrix,desc(articleId))
rownames(usersArticlesMatrix) = usersArticlesMatrix[,1]
usersArticlesMatrix$articleId = NULL
usersArticlesMatrix$email = NULL
usersArticlesMatrix$userIndex.x = NULL
usersArticlesMatrix$articleIndex.x = NULL
usersArticlesMatrix$userIndex.y = NULL
usersArticlesMatrix$articleIndex.y = NULL

object.size(usersArticlesMatrix)/1024/1024/2014

View(usersArticlesMatrix)
usersArticlesSparseMatrix = sparseMatrix(i = usersArticles$userIndex,
                                         j = usersArticles$articleIndex,
                                         x = usersArticles$count,
                                         dims = c(length(users$email),length(articles$articleId)),
                                         dimnames = list(c(users$email),c(articles$articleId)))
object.size(usersArticlesSparseMatrix)/1024/1024/2014

sum(usersArticlesMatrix)
sum(usersArticlesSparseMatrix)

x = 

usersArticlesMatrix
usersArticlesSparseMatrix
