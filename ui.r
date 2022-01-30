library('shiny')
library('shinyjs')

# Define UI for application that draws a histogram
ui = fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(list(.big = "font-size: 2em")),
  div(id = "myapp",
      h2("Coursera Data Science Capstone - Final Project Submission"),
      checkboxInput("big", "Bigger text", FALSE),
      textInput("name", "Text Input :", ""),
      p('You can type up to 2 words in sequence with space as system will auto detect the word(s) and suggest you the best predictive next word in table format.'),
      a(id = "toggleAdvanced", "Show/hide advanced info", href = "#"),
      shinyjs::hidden(
        div(id = "advanced",
            p('- Presentation:', HTML("<a href='https://beta.rstudioconnect.com/tpmadan/Final-Project-Submission-Slides/'>Coursera Data Science Capstone - Final Project Submission (Slides)</a>")),
            p('- Author Profile:', HTML("<a href='https://beta.rstudioconnect.com/tpmadan/ryo-eng/'>®γσ, Tpmadan </a>")),
            p('- GitHub:', HTML("<a href='https://github.com/tpmadan/Coursera-Data-Science-Capstone'>Source Code</a>"))
        )
      ),
      p("Timestamp: ",
        span(id = "time", date()),
        a(id = "update", "Update", href = "#")
      ),
      actionButton("reset", "Reset form")
  ),
  mainPanel(
    verbatimTextOutput('text1'),
    verbatimTextOutput('text2'),
    br(),
    div(id = "bestMatch", p('The best match next predictive word is :', htmlOutput('text3'))),
    hr(),
    DT::dataTableOutput('table')
  )
)
