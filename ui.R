library(shiny)

shinyUI(fluidPage(
    titlePanel("Passes Visualization"),
    
    sidebarLayout(
        sidebarPanel(
            p("Mark where the pass took place"),
            radioButtons("team", "Select Team:", choices = c("Jags", "Opponent")),
            actionButton("clearButton", "Clear Passes"),
            verbatimTextOutput("passCounts")
        ),
        
        mainPanel(
            plotOutput("pitch", click = "plot_click", height = 600, width = 800),
            plotOutput("passChart", height = 400, width = 600)
        )
    )
))

