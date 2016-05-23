
# This is the server logic for a Shiny web application.
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

x_variables <-NA
df <- NA
time <-0


shinyServer(function(input, output,session) {
  
  observe({
    #to secure non-editable elements
    disable("summary")
    disable("y_dependable") #the column with good/bad values
    disable("performance")
    
    #to define the theoretical dataframe for further runs
    setNames <-paste("V",c(1:input$predictors),sep="")
    df <<- as.data.frame(matrix(rnorm(as.integer(input$observations)*input$predictors),nrow=(as.integer(input$observations)),ncol=(input$predictors)))
    df[,1] <<- sample(0:1, as.integer(input$observations), replace=T)
    names(df)<-setNames
    
    #write the essential data about modelling
    updateTextInput(session,"summary",value=NULL)
    
    
    updateTextInput(session,"summary",value=paste( "Predictor qty =",input$predictors,"\n"
                                                  ,"Observation qty =",input$observations,"\n"
                                                  ,"Dependent variable =",input$y_dependable,"\n"
                                                  ,"Model type =",input$models,"\n"
                                                  
                                                 )
                    )

    
    
    
  })
  
  observeEvent(input$predictors,{
    
    setId <- 1:input$predictors
    setNames <-paste("V",c(1:input$predictors),sep="")
    #updateSelectInput(session,"y_dependable",choices = c(setNames,setId))
    
    observe({
  
      x_variables <<- c(setNames)[which((c(setNames) %in% input$y_dependable)==FALSE)]
      if (length(x_variables)==1) stop("You must choose more then 1 predictor!")
    })
    
  })
  
  
  observeEvent(input$go_button,{
    disable("go_button")
    
    if (length(x_variables)==0) stop("You must choose more then 1 predictor!")

    formulaText <- formula(paste(input$y_dependable,"~",paste(x_variables,collapse="+")))
    
    time <<- system.time(
                          fullmodel <- glm(formulaText, family = eval(input$models), data = df)
                        )
    #updateTextInput(session,"performance",value = paste (names(time[1]),names(time[2]),names(time[3]),"\n",time[1],time[2],time[3]))
    output$performance <- renderPrint({
      time
    })
    
    
    output$wide_summary <- renderPrint({
      summary(fullmodel)
    })

    
    enable("go_button")
    
  }) 
  
  output$help <- renderPrint({
    p("What for the Shiny app is?
The initial goal of this Shiny app is to demonstrate how to use different functions of “glm” family (their output and performance indicators (time)) such as:
1.	binomial
2.	gaussian
3.	poisson
4.	quasibinomial
5.	quasipoisson
      
      
All function settings are considered to be default but for the data frame dimensions and model type (types of “glm“ family). The data frame is filled up with “rnorm” function.
      
The description of App’s elements: 
      
sliderInput(‘predictors’,’Qty of predictors selected’) – define the number of predictors to be used in the theoretical data frame. 
The default value is “1”. You must pick up, at least, two predictors otherwise the script will throw the warning and stop the App.
      
textInput(‘observations’,’Qty of observations’) – define the number of observation
      
selectInput(‘y_dependable’,’,’Select predictor as Y’) – define the dependant variable. The element is disabled. On default it is “V1” (contains 1 or 0 values).
      
selectInput(‘models’, ‘Model type selected’) – define the type of model selected.
      
When all elements are defined -> press the button ‘Run model calculations!’"
    )

    
  })
  
})
