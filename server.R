library(shiny)
library(ggplot2)
source("pitchCreate.R")

shinyServer(function(input, output, session) {
    passes <- reactiveVal(data.frame(x = numeric(0), y = numeric(0), team = character(0)))
    jags_pass_counts <- reactiveVal(c(def_third = 0, middle_third = 0, final_third = 0))
    opponent_pass_counts <- reactiveVal(c(def_third = 0, middle_third = 0, final_third = 0))
    
    observeEvent(input$plot_click, {
        click <- input$plot_click
        if (!is.null(click$x)) {
            new_pass <- data.frame(x = click$x, y = click$y, team = input$team)
            passes_data <- passes()
            passes(rbind(passes_data, new_pass))
            
            # Update pass counts based on the clicked area
            if (click$x <= 40) {
                if (input$team == "Jags") {
                    jags_pass_counts(c(def_third = jags_pass_counts()[["def_third"]] + 1, 
                                       middle_third = jags_pass_counts()[["middle_third"]],
                                       final_third = jags_pass_counts()[["final_third"]]))
                } else {
                    opponent_pass_counts(c(def_third = opponent_pass_counts()[["def_third"]],
                                           middle_third = opponent_pass_counts()[["middle_third"]],
                                           final_third = opponent_pass_counts()[["final_third"]] + 1))
                }
            } else if (click$x <= 80) {
                if (input$team == "Jags") {
                    jags_pass_counts(c(def_third = jags_pass_counts()[["def_third"]],
                                       middle_third = jags_pass_counts()[["middle_third"]] + 1,
                                       final_third = jags_pass_counts()[["final_third"]]))
                } else {
                    opponent_pass_counts(c(def_third = opponent_pass_counts()[["def_third"]],
                                           middle_third = opponent_pass_counts()[["middle_third"]] + 1,
                                           final_third = opponent_pass_counts()[["final_third"]]))
                }
            } else {
                if (input$team == "Jags") {
                    jags_pass_counts(c(def_third = jags_pass_counts()[["def_third"]],
                                       middle_third = jags_pass_counts()[["middle_third"]],
                                       final_third = jags_pass_counts()[["final_third"]] + 1))
                } else {
                    opponent_pass_counts(c(def_third = opponent_pass_counts()[["def_third"]] + 1,
                                           middle_third = opponent_pass_counts()[["middle_third"]],
                                           final_third = opponent_pass_counts()[["final_third"]]))
                }
            }
        }
    })
    
    observeEvent(input$clearButton, {
        passes(data.frame(x = numeric(0), y = numeric(0), team = character(0)))
        jags_pass_counts(c(def_third = 0, middle_third = 0, final_third = 0))
        opponent_pass_counts(c(def_third = 0, middle_third = 0, final_third = 0))
    })
    
    output$pitch <- renderPlot({
        pitch <- create_Pitch(JdeP = TRUE, 
                              grass_colour = "#538032", 
                              line_colour =  "#ffffff", 
                              background_colour = "#538032", 
                              goal_colour = "#000000")
        
        passes_data <- passes()
        
        # Draw passes as dots and color them based on team
        dots <- geom_point(data = passes_data, aes(x = x, y = y, color = team), size = 4)
        
        pitch + dots +
            scale_color_manual(values = c("Jags" = "red", "Opponent" = "blue")) +
            coord_fixed(ratio = 3.75/4)
    })
    
    output$passCounts <- renderPrint({
        cat("Jags Passes:\n")
        cat("Defensive Third Passes:", jags_pass_counts()[["def_third"]], "\n")
        cat("Middle Third Passes:", jags_pass_counts()[["middle_third"]], "\n")
        cat("Final Third Passes:", jags_pass_counts()[["final_third"]], "\n")
        
        cat("\nOpponent Passes:\n")
        cat("Defensive Third Passes:", opponent_pass_counts()[["def_third"]], "\n")
        cat("Middle Third Passes:", opponent_pass_counts()[["middle_third"]], "\n")
        cat("Final Third Passes:", opponent_pass_counts()[["final_third"]], "\n")
    })
    
    
    
    output$passChart <- renderPlot({
        # Calculate total passes for each team
        total_jags_passes <- sum(jags_pass_counts())
        total_opponent_passes <- sum(opponent_pass_counts())
        
        # Calculate pass percentages for each area and team
        pass_percentages <- data.frame(
            Team = rep(c("Jags", "Opponent"), each = 3),
            Area = c("Defensive Third", "Middle Third", "Final Third"),
            Percentage = c(jags_pass_counts()[["def_third"]] / total_jags_passes * 100,
                           jags_pass_counts()[["middle_third"]] / total_jags_passes * 100,
                           jags_pass_counts()[["final_third"]] / total_jags_passes * 100,
                           opponent_pass_counts()[["def_third"]] / total_opponent_passes * 100,
                           opponent_pass_counts()[["middle_third"]] / total_opponent_passes * 100,
                           opponent_pass_counts()[["final_third"]] / total_opponent_passes * 100)
        )
        
        ggplot(pass_percentages, aes(x = Area, y = Percentage, fill = Team)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Pass Area", y = "Pass Percentage", fill = "Team") +
            scale_fill_manual(values = c("Jags" = "red", "Opponent" = "blue")) +
            theme_minimal() +
            ylim(0, 100)  # Set y-axis limits to 0-100 for percentages
    })
})
