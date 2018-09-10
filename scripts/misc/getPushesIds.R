# install.packages('RecordLinkage')
library(jsonlite)
library(RecordLinkage)
Br_Loc = Sys.getlocale(category = "LC_ALL")
Sys.setlocale(category = "LC_ALL", locale = "us")


# pushes = read.csv2('pushes2.csv',encoding = 'latin1')
# pushes$Titulo = iconv(x = as.vector(pushes$Titulo),from = 'latin1',to = 'UTF-8')
# pushes$id = ""

# pushesLeft = pushes[pushes[,'id']=="",]
j = 1
pushesLeft = pushesLeftOctober
for (j in 1:nrow(pushesLeft)){
  titulo = pushesLeft[j,'Titulo']
  titulo = paste0(' ',titulo,' ')
  
  # diaDe = format(x = strptime(x = as.vector(pushesLeft[j,'Data']),format = '%d/%m/%Y')-(15*60*60*24),format = "%a, %d %b %Y")
  # diaAte = format(x = strptime(x = as.vector(pushesLeft[j,'Data']),format = '%d/%m/%Y')+(1*60*60*24),format = "%a, %d %b %Y")
  diaDe = format(x = strptime(x = '01/10/2017',format = '%d/%m/%Y'),format = "%a, %d %b %Y")
  diaAte = format(x = strptime(x = '31/10/2017',format = '%d/%m/%Y'),format = "%a, %d %b %Y")
  dataDe = paste0(diaDe,' 00:00:00 GMT')
  dataAte = paste0(diaAte,' 23:59:00 GMT')
  
  titulos = ""
  
  erro = FALSE
  achou = FALSE
  pos = as.vector(gregexpr(pattern =' ',titulo)[[1]])
  for (i in  1:(length(pos)-3)){
    erro = FALSE
    palavra = substr(x = titulo,start = pos[i]+1
                               ,stop  = pos[i+1]-1)
    # print(palavra)
    result = tryCatch({
      pagina = getPages(titulo = titulo,dataDe = dataDe,dataAte = dataAte,palavra)
      titulos = unlist(pagina$`_items`$headline)
    }, error=function(e){
      # print(paste0(length(titulos)," Erro em ",j," ",pushesLeft$Titulo[j]))
      erro = TRUE
    })
    if (!erro & (length(titulos)>0)){
      achou = TRUE
      break
    }
  }
  if (achou){
    if (length(titulos) > 1){
      for (k in 1:length(titulos)){
        if (levenshteinSim(str1 = titulo,str2 = titulos[k])> 0.5){
            pushesLeft[j,'id'] = pagina$`_items`$`_id`[[k]]
            print(pagina$`_items`$`_id`[[k]])
            print(titulos[k])
            break
        }
      }
    }
    else if (length(titulos)==1 & nchar(titulos) > 0){
      pushesLeft[j,'id'] = pagina$`_items`$`_id`[[1]]
      print(pagina$`_items`$`_id`[[1]])
      print(titulos[1])
    }
    else{
      print(paste0(length(titulos)," Não achou ",j," ",pushesLeft$Titulo[j]))
    }
  }
  else{
    print(paste0(length(titulos)," Não achou ",j," ",pushesLeft$Titulo[j]))
  }
}

sum(pushesLeft[,'id']!='')
sum(pushesLeft[,'id']=='')
# pushes[pushes[,'id']=="",] = pushesLeft

pushesFound = pushesLeft[pushesLeft[,'id']!='',]
pushesFound$Titulo = NULL
pushesFound$Data = NULL
pushesFound$Hora = NULL

pushesLeftOctober = pushes[pushes[,'id']=="",][months(strptime(pushes[pushes[,'id']=="",'Data'],'%d/%m/%Y')) == 'October',]
View(pushesLeftOctober)
write.csv2(x=pushesLeftOctober,file = 'lista.txt')

pushes[pushes[,'id']=="",][months(strptime(pushes[pushes[,'id']=="",'Data'],'%d/%m/%Y')) == 'October',] = pushesLeft
pushesFound = pushesLeft[pushesLeft[,'id']!='',]

View(pushesLeftOctober)
write(x = paste0( "update articles set push_time = to_timestamp('",pushesFound[,'Data_Hora'],"','DD/MM/YYYY HH:MI:SS AM') where id = '",pushesFound[,'id'], "';"),file = 'update_pushes2.3.sql')













getUri = function(dataDe,dataAte,palavra){
  uri = paste0('http://api-online.rbs.com.br/articles.json?',
               'max_results=50&',
               'page=1&',
               'sort=-_published&',
               'projection={"headline.text":1,"_published":1}&',
               'where={"$and":[{"_published":{"$gt": "',dataDe,'"}},',
               '{"_published":{"$lt": "',dataAte,'"}},',
               '{"headline.text":{"$regex":".*',palavra,'.*"}}]}')
  return(uri)
}

getPages = function(titulo,dataDe,dataAte,palavra){
  uri = getUri(dataDe = dataDe,dataAte = dataAte,palavra = palavra)
  firstPage = fromJSON(URLencode(uri))
  return(firstPage)
}