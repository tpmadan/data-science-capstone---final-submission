library('shiny')
library('shinyjs')
library('DT')
library('plyr')
library('dplyr')
library('magrittr')
library('stringi')

## Load RDS files
en_US <- suppressAll(readRDS('data/en_US.rds'))

server = function(input, output) {

  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE))    
  
  shinyjs::onclick("update", shinyjs::html("time", date()))
  
  observe({
    shinyjs::toggleClass("bestMatch", "big", input$big)
  })
  
  output$text1 <- renderText({
    if(!is.null(input$name) & input$name != "")
      mydfm = en_US$mydfm
    else
      mydfm = 0
    
    paste0('Document-feature matrix of: ', 
           as.character(dim(mydfm))[1], 
           ' documents, ',as.character(dim(mydfm))[2],' features.')
  })

  output$text2 <- renderText({
    if(!is.null(input$name) & input$name != "")
      mydfm = en_US$mydfm
    else
      mydfm = 0
    paste0('Document-feature matrix of: ', 
           as.character(dim(mydfm))[1], 
           ' documents, ',as.character(dim(mydfm))[2],' features.')
  })
  
  output$table <- DT::renderDataTable({
    corpUS = en_US$corpUS
    
    ## http://rpubs.com/Hsing-Yi/176027
    #'@ if(!is.null(input$name) | input$name != "") {
      criteria = strsplit(input$name, ' ')[[1]]
      
      len = length(criteria)
      if(len == 1) {
        corpUS %<>% filter(word1 == criteria[1]) %>% tbl_df
      } else if(len == 2) {
        corpUS %<>% filter(word1 == criteria[1] & 
                           word2 == criteria[2]) %>% tbl_df
      #'@ } else if(len == 3) {
      #'@   corpUS %<>% filter(word1 == criteria[1] & 
      #'@                    word2 == criteria[2] & 
      #'@                    word3 == criteria[3]) %>% tbl_df
      } else {
        corpUS = data.frame() %>% tbl_df
      }
    DT::datatable(corpUS)
  })
  
  output$text3 <- renderText({
    corpUS = en_US$corpUS
    
    ## http://rpubs.com/Hsing-Yi/176027
    #'@ if(!is.null(input$name) | input$name != "") {
    criteria = strsplit(input$name, ' ')[[1]]
    
    len = length(criteria)
    if(len == 1) {
      corpUS %<>% filter(word1 == criteria[1]) %>% tbl_df %>% .[1, 2] %>% unlist
    } else if(len == 2) {
      corpUS %<>% filter(word1 == criteria[1] & 
                           word2 == criteria[2]) %>% tbl_df %>% .[1, 3] %>% unlist
    #'@ } else if(len == 3) {
    #'@   corpUS %<>% filter(word1 == criteria[1] & 
    #'@                        word2 == criteria[2] & 
    #'@                        word3 == criteria[3]) %>% tbl_df %>% .[1, ]
    } else {
      corpUS = 'Unknown predictive next word.'
    }
    return(corpUS)
  })
  
  observeEvent(input$reset, {
    shinyjs::reset("myapp")
  })    
}
