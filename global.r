require('BBmisc', quietly=TRUE)
require('memoise', quietly=TRUE)
require('plyr', quietly=TRUE)
require('dplyr', quietly=TRUE)
require('magrittr', quietly=TRUE)
require('stringr', quietly=TRUE)
require('stringi', quietly=TRUE)
require('quanteda', quietly=TRUE)

## Preparing the parallel cluster using the cores
doParallel::registerDoParallel(cores = 2)

## ==============================================================================
## convert blogs, news and twitter files into one dfm files
convertDFM2 <- function(type, parallel = FALSE, sampling = 1){
  types = c('de_DE', 'ru_RU', 'en_US', 'fi_FI')
  if(!type %in% types) stop("Please choose a type within 'de_DE', 'ru_RU', 'en_US' or 'fi_FI'.")
  if(!is.numeric(sampling)) sampling = as.numeric(sampling)
  
  lan = switch(type, 
               de_DE = 'german',
               ru_RU = 'russian',
               en_US = 'english',
               fi_FI = 'finnish')
  files = list.files(paste0('data/final/', type, '/'))
  fPath = paste0('data/final/', type, '/')
  fv = paste0(fPath, files)[-1] #The project require only news and twitter but excludes blogs
  
  corpUS = llply(fv, function(x, parallel = parallel){
    ## readChar
    con = file(x, open = 'rb')
    text = readChar(con, file.info(x)$size, useBytes = T)
    close(con); rm(con)
    text = strsplit(text, '\n', fixed = T, useBytes = T)[[1]] %>% str_replace_all('\r', '')
    
    ## sampling how many percent (%) from whole population
    text = text[sample(seq(length(text)), length(text) * sampling)]
    corpus(text)
  }, .parallel = parallel)
  
  corpUS = corpUS[[1]] + corpUS[[2]]
  
  ## Create n-grams Tokenization and transfer to dfm (Document Feature Matrix)
  mydfm = dfm(corpUS, verbose = TRUE, toLower = TRUE, removeTwitter = TRUE, 
              removeNumbers = TRUE, ignoredFeatures = stopwords(lan))
  
  corpUS = collocations (corpUS, method = 'lr')
  return(list(mydfm = mydfm, corpUS = tbl_df(corpUS)))
}

## ---------------------------------------------------------------------------------
## convert and save whole population into rds file as dictionary, easier for ngrams word prediction.
#'@ en_US <- convertDFM2('en_US', sampling = 0.1)
#'@ saveRDS(en_US, 'Final-Project-Submission/data/en_US.rds')

## Load RDS files
#'@ en_US <- suppressAll(readRDS('data/en_US.rds'))

## =================================================================================
# Using "memoise" to automatically cache the results
selectData <- #memoise(
  function(word = as.character(word)) {
    # Careful not to let just any name slip in here; a
    # malicious user could manipulate this value.
    #'@ if(!is.null(word)& !is.character(word) & !is.numeric(word)) 
    #'@   stop('Please key in English characters or numbers!')
    
    ## http://rpubs.com/Hsing-Yi/176027
    if(input$name %in% en_US$corpUS$word1) {
      corpUS = en_US$corpUS %>% filter(word1 == input$name) %>% tbl_df
    } else if(input$name %in% paste(en_US$corpUS$word1, en_US$corpUS$word2)) {
      corpUS = en_US$corpUS %>% filter(word1 == strsplit(input$name, ' ')[[1]] & 
                                         word2 == strsplit(input$name, ' ')[[2]]) %>% tbl_df
    } else {
      corpUS = 'Unknown word(s).'
    }
    return(list(mydfm = en_US$mydfm, corpUS = corpUS))
    }#)
