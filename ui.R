# Load libraries
library(shiny)


shinyUI(
        pageWithSidebar(
                headerPanel("Word Prediction"),
                # Select inputs
                sidebarPanel(
                        textInput("userInput", "Partial Sentence:", value = "Merry"),
                        
                        submitButton("Submit Sentence"),
                        
                        br()                
                ),
                
                
                mainPanel(
                        # use tab format to display outputs
                        tabsetPanel(
                                
                                # tab 1        
                                tabPanel("App Info",
                                         withMathJax(),
                                         includeMarkdown("include.Rmd")),
                                
                                # tab 2        
                                tabPanel("Word Prediction",
                                         mainPanel(
                                                 p("The following are the top 6 predicted words based on your partial sentence input.  This list is rank ordered from top to bottom representing highest to lowest probability.  The word in ", span(strong("blue"), style = "color:blue"), "represents the highest probable word to follow your input."),
                                                 h1(textOutput("top1"), align = "center", style = "color:blue"),
                                                 h2(textOutput("top2"), align = "center"),
                                                 h3(textOutput("top3"), align = "center"),
                                                 h4(textOutput("top4"), align = "center"),
                                                 h5(textOutput("top5"), align = "center"),
                                                 h6(textOutput("top6"), align = "center"))),
                                
                                #tab 3
                                tabPanel("Word Cloud",
                                         mainPanel(
                                                 p("The following is a word cloud of the top 50 predicted words based on your partial sentence input.  The size and color of the words are relative to there probability of occurrence."),
                                                 plotOutput(outputId = "plot1")))
                        )
                )
        )
)
