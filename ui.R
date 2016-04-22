
library(shiny)

shinyUI(fluidPage(style="color:#838487;font-family: 'arial';background-color:#034F84",
                  
    verticalLayout(
        titlePanel(h1("What's the next word?",align="center",
                      style="color:#838487;font-family: 'georgia';padding: 25px 30px")),
        br(),

        mainPanel(h4("Insert your text below:"),
                  textInput("text", label = NULL, 
                            value = "Write here!")),
        wellPanel(h4("In the area below you can see the most probable word predicted by the simple ",
                    a("Backoff Model.", 
                      href = "https://en.wikipedia.org/wiki/Katz's_back-off_model")),br(),
                  h3("Model's main prediction:", align = "center"), 
                  h2(textOutput("text2"), align = "center", style="color:#B93A32"),    
                  br(),
                  h4("Alternative predictions:", align = "center"),
                  h3(textOutput("text3"),textOutput("text4"),textOutput("text5"),align="center", style="color:darkgreen"),
                  br(),
                  br(),
                  h4("If there is no prediction available the model returns 'NA' ",
                    align="center", style = "font-family: 'times';text-decoration: underline"),
                  
                   style="color:#004d99;font-family: 'arial';background-color:#92B6D5")
))
)

