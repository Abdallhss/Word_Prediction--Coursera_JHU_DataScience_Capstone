#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source("./final.R")

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- shinyUI(dashboardPage(
    dashboardHeader(titleWidth = 320, title = "Word Predictions using Ngrams:",
                    dropdownMenu(
                        type = "notifications", 
                        icon = icon("question-circle"),
                        badgeStatus = NULL,
                        headerText = "",
                        notificationItem("More info", icon = icon("file"),
                                         href = "https://rpubs.com/Abdallhss/593046")
                    )
                                   
    ),
    dashboardSidebar(width = 320,
                     box(width = 20,
                         h2("How to use:"),
                         h4("1. Enter your sentence."),
                         h4("2. Choose the number of predictions"),
                         h4("3. Should predict stop words [I, he, you, a, the,...]"),
                         background = "olive"),
                     box(width=20,
                         radioButtons("n", h3("Number of predictions:"),
                                  c("1" = 1, "5" = 5, "10" = 10, "all" = -1)),background="light-blue"),
                     box(width=20,
                         radioButtons("type", h3("Include stop words:"),
                                c("Sure" = "Stop", "Nope" = "Non_stop")),
                         background="light-blue")
                    
    )
                                 
                     
                     #submitButton(text = "Apply Changes", icon = NULL, width = NULL)
    ,
    dashboardBody(
        
    fluidRow(
        box(width = 12,
            textInput("text", label=h3("Enter a sentence:"), value = ""),
            h3("Reconstructed sentence:"),
            h4(textOutput("word_eval")),
            background = "light-blue"
            )
        ),
    fluidRow(
        column(12,dataTableOutput('predict'))
    )
    )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output){
    
    word_predict_1 <- reactive({
        if(input$text=="") {
            "Please enter a word."
        }
        else {
            word_str    <- word_parse(input$text,as.integer(10))
            preds   <- word_predict(word_str)
            if (input$type == "Stop"){
                next_word <- preds$stop
            }
            else{
                next_word <- preds$non_stop
            }
            next_word <- next_word[,c(4,5)]
            if (input$n == -1){
                next_word
            }
            else{
                next_word[1:input$n,] 
            }
        }
    })   
    
    word_output_1 <- reactive({
        if(input$text=="") {
            "Please enter a sentence"
        }
        else {
            word_str    <- word_parse(input$text,as.integer(10))
            word_str
        }
    })
    
    
    
    output$predict <- renderDataTable({
        word_predict_1()
    })
    
    
    output$word_eval <- renderText({
        word_output_1()
    })
    #output$msgOutput <- renderMenu({
    #  dropdownMenu(type="messages", "test")
    #})
})  

# Run the application 
shinyApp(ui = ui, server = server)

