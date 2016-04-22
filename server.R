library(shiny)
library(stringr) 
library(tm)
library(data.table)
source("data/tokenize.R")
source("data/predictWords2.R")
source("data/global.R")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    output$text2 <- renderText({ 
        predictWord2(input$text)[1]
    })
    
    output$text3 <- renderText({ 
        predictWord2(input$text)[2]
    })
    
    output$text4 <- renderText({ 
        predictWord2(input$text)[3]
    })
    
    output$text5 <- renderText({ 
        predictWord2(input$text)[4]
    })
    
}
)