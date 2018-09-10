list.of.packages = c('rslp','hunspell')
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Loads RSLP Stemmer for Portuguese Lenguage
library(rslp)

# Loads hunspell for spell checking of portuguese words
library(hunspell)

# Transform the word either removing it's plural 
# termination or reducing it to its stem, correcting
# or not it's spelling first
stemmer = function(word,correct = FALSE,justPlural = TRUE,rules,dict){
  result = ""
  words = unlist(strsplit(word,"-"))
  if (correct){
    spellState = hunspell_check(words,dict)
    suggestions = hunspell_suggest(words,dict)
    i = 0
    for ( i in 1:length(spellState)){
      if (!spellState[i]){
        words[i] = unique(rapply(suggestions[i], function(x) head(x, 1)))
      }
    }
  }
  if (justPlural){
    results = lapply(words,function(x) return(rslp:::apply_rules(x,name = "Plural",steprules = rules)))
  }
  else{
    results = lapply(words,function(x) return(rslp::rslp(words = x,steprules = rules)))
  }
  
  if (correct){
    results = lapply(results,function(x) return(rslp:::remove_accents(x)))
  }
  result = paste(unlist(results),collapse = "-")
  return (as.character(result))
}

# Generates tags withou plural
getTagsNoPlural <- function(tagsDF){
  tags = as.tibble(tagsDF)
  rules = rslp::extract_rules()
  noPlural = vapply(X = vapply(X = tags[,2],FUN = as.character,FUN.VALUE = character(nrow(tags))),FUN = stemmer,FUN.VALUE = character(1),correct = FALSE,justPlural = TRUE,rules = rules)
  tags = add_column(tags,stem=noPlural,.after = 'name')
  # result = 1-count(distinct(.data=tags[,3]))/count(distinct(.data=tags[,2]))
  return(tags)
}

# Retrieves and updates the database removing plural from tags
setNoPlural <- function(con){
  tags = getTags(con = con)
  tagsNoPlural = getTagsNoPlural(tagsDF = tags)
  updateTagsNoPlural(con = con, tags = tagsNoPlural)
}

# Connects to the database and extracts the data
# save.image(file = 'distances_all_samples.RData')
# system.time({setNoPlural(con = connSample20)})
# system.time({setNoPlural(con = connSample200)})
# system.time({setNoPlural(con = connSample2000)})
# system.time({setNoPlural(con = fullDataBase)})
