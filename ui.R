
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:

#connect libraries required
if(!require(shiny)){
  library(shiny)  
}
if(!require(shinyjs)){
  library(shinyjs)  
}
if(!require(shinyBS)){
  library(shinyBS)
}

#funaction to create multyline textareas in shiny
textAreaInput <- function(inputID, label, value="", rows=10, columns=80) {
  HTML(paste0('<div class="form-group shiny-input-container">
              <label for="', inputID, '">', label,'</label>
              <textarea id="', inputID, '" rows="', rows,'" cols="', 
              columns,'">', value, '</textarea></div>'))
}

shinyUI(fluidPage(useShinyjs(),

  titlePanel("Model testing platform"),
  navbarPage("model_platform",
             tabPanel("Settings",
                      fluidRow(column(2,
                                       sliderInput("predictors", "Qty of predictors selected", min=1, max=100, 1)
                                      ,textInput("observations", "Qty of observations", value=100, placeholder = "Enter the number of observations")
                                      ,selectInput("y_dependable","Select predictor as Y",choices = c("V1"),width=100)
                                      ,selectInput("models", "Model type selected"
                                                   , choices = c(   "binomial"
                                                                   ,"gaussian"
                                                                   ,"poisson"
                                                                   ,"quasibinomial"
                                                                   ,"quasipoisson"
                                                                 )
                                                  )
                                      ,textAreaInput("summary","Parameters to model",rows=4, columns=40)
                                      
                                      ,h5(strong("Performance,seconds"))
                                      ,verbatimTextOutput("performance")
                                      ,actionButton("go_button", "Run model calculations!")
                                     ),
                               
                               column(4,
                                       h5(strong("Model summary"))
                                      ,verbatimTextOutput("wide_summary")
                                      ),
                               column(6,
                                      h5(strong("Tool description"))
                                      ,verbatimTextOutput("help")
                               ) 
                               
                              )
                     )
  
  
            )
      )
)