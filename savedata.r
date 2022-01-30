## I am using 'quanteda' package to read and convert as well as save the data set for shiny app using.
##   1stly save time: pre-load existing files during opening shiny app compare to processing upon request, 
##   2ndly save capacity: file size smaller than zipped file moreover converted format data.
if(suppressMessages(!require('BBmisc'))) suppressMessages(install.packages('BBmisc'))

suppressMessages(library('BBmisc'))
pkgs <<- c('plyr', 'dplyr', 'magrittr', 'stringr', 'stringi', 'rvest', 'quanteda', 'doParallel')
suppressAll(lib(pkgs))
rm(pkgs)

## Preparing the parallel cluster using the cores
registerDoParallel(cores = 2)

if(!file.exists('data/')) dir.create('data/')
destfile <- 'Coursera-SwiftKey.zip'

if(!file.exists(paste0('data/', destfile))) {
  download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip', 
                destfile = paste0('data/', destfile))
}

if(!file.exists(paste0('data/final'))) {
  ## Unzip the dataset
  unzip(paste0('data/', destfile), exdir = 'data/')
}
rm(destfile)

## ------------------------------------------------------------------------------------
convertDFM <- function(type, parallel = FALSE, sampling = 1){
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
  fv = paste0(fPath, files)
  
  dtMtx = llply(fv, function(x, parallel = parallel){
    ## readChar
    con = file(x, open = 'rb')
    text = readChar(con, file.info(x)$size, useBytes = T)
    close(con); rm(con)
    text = strsplit(text, '\n', fixed = T, useBytes = T)[[1]] %>% str_replace_all('\r', '')
    
    ## sampling how many percent (%) from whole population
    textV <- text[sample(seq(length(text)), length(text) * sampling)]
    
    corpUS = corpus(textV)
    
    ## Create n-grams Tokenization and transfer to dfm (Document Feature Matrix)
    mydfm = dfm(corpUS, verbose = TRUE, toLower = TRUE, removeTwitter = TRUE, 
                removeNumbers = TRUE, ignoredFeatures = stopwords(lan))
    
    corpUS = collocations (corpUS, method = 'lr') %>% tbl_df
    
    return(list(txt = text, mydfm = mydfm, corpUS = corpUS))
  }, .parallel = parallel)
  names(dtMtx) = c('blogs', 'news', 'twitter')
  return(dtMtx)
}

## ------------------------------------------------------------------------------------
## read data and auto convert to few format for ngram analysis.
## sampling 1% data from population.
de_DE <- convertDFM('de_DE', sampling = 0.01)
ru_RU <- convertDFM('ru_RU', sampling = 0.01)
en_US <- convertDFM('en_US', sampling = 0.01)
fi_FI <- convertDFM('fi_FI', sampling = 0.01)

## http://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/
## For Final-Project-Submissioin shiny app use.
saveRDS(de_DE, 'Final-Project-Submission-nGram/data/de_DE.rds')
saveRDS(ru_RU, 'Final-Project-Submission-nGram/data/ru_RU.rds')
saveRDS(en_US, 'Final-Project-Submission-nGram/data/en_US.rds')
saveRDS(fi_FI, 'Final-Project-Submission-nGram/data/fi_FI.rds')

unlink('data/final', recursive = TRUE)
