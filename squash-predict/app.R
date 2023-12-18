library(shiny)
library(dplyr)
library(ggplot2)
library(pubtheme)

# load predict function
source("fit.model.final.R")

all_players <- NA


# UI Definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* this will affect all the pre elements */
      pre {
        background-color: #E6F0FF;
        border-width : 0;
        font-family: 'Times New Roman', Times, serif;
        text-align: center;
        font-size: 16px;
      }"))
  ),
  tags$div(
    style = "padding-top: 10px; padding-right: 20px; padding-bottom: 20px; margin-bottom: 20px;",
    tags$div(
      style = "background-color: #a6cbee; border: 2px solid #555555; border-radius: 15px; box-shadow: 2px 2px 10px #888888; width: 50%;",
      tags$h1(
        style = "padding-left: 10px;",  # Adjusted padding-left for the main title text
        "Squash Match Head-To-Head Prediction"
      )
    ),
    tags$div(
      style = "border: 2px solid #555555; border-radius: 15px; width: 50%; margin-top: 10px; padding: 10px;",
      "The platform aims to predict match outcomes dynamically, adjusting predictions based on the current scores of the match. PSA Tournaments results up until 26 Nov 2023 were used to train model."
    )
  ),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #E6F0FF;",
      # Add radio buttons for gender selection
      radioButtons("gender", label = HTML("<u>Select Gender</u>"), choices = list("Men" = "men", "Women" = "women"), selected = "men"),
      
      selectInput("player1", label = "Player 1", choices = all_players, selected = "no selection"),
      selectInput("player2", label = "Player 2", choices = all_players, selected = "no selection"),
    
      tags$hr(), # Horizontal line for separation
      
      wellPanel(
        h3(
          "Current Scores",
          style = "text-decoration: underline; padding: 10px; margin: 0; margin-right: 0;"  # Underline the text
        ),
        sliderInput("p1_sets_won", "Games won by Player 1", min = 0, max = 3, value = 0, step = 1),
        sliderInput("p2_sets_won", "Games won by Player 2", min = 0, max = 3, value = 0, step = 1)
      ),
      
      tags$hr(), # Horizontal line for separation
      
      selectInput("n_seasons", label = "Predict based on past seasons", choices = c(1,2,3,4,5), selected = 1)
    ),
    mainPanel(
      # Add a div to display the win probability
      conditionalPanel(
        condition = "input.player1 != 'NA' & input.player2 != 'NA'",
        tags$div(
          style = "background-color: #E6F0FF; border: 2px solid #555555; border-radius: 15px; padding: 5px 10px; margin-bottom: 20px;",
          tags$h2(verbatimTextOutput("text")),
          # tags$h2(htmlOutput("results_title")),
          tags$h2(plotOutput("results"))
        )
      )
    )
  )
)


# Server Logic
server <- function(input, output, session) {
  
  localreactive <- reactiveValues(model=NA)

  observeEvent(c(input$gender, input$n_seasons), {
    # update model and player names when the gender or n_seasons changes
    if (input$n_seasons == 1) {
      models <- readRDS("models/model_game_win_2023-2024.rds")
    } else if (input$n_seasons == 2) {
      models <- readRDS("models/model_game_win_2022-2024.rds")
    } else if (input$n_seasons == 3) {
      models <- readRDS("models/model_game_win_2021-2024.rds")
    } else if (input$n_seasons == 4) {
      models <- readRDS("models/model_game_win_2020-2024.rds")
    } else {
      models <- readRDS("models/model_game_win_2019-2024.rds")
    }
    
    if (input$gender == "men") {
      localreactive$model <- models[[1]]
    } else {
      localreactive$model <- models[[2]]
    }
    
    all_players <- gsub("`", "", names(coefficients(localreactive$model))[-1])
    updateSelectInput(session, "player1", choices = all_players, selected = "no selection")
    updateSelectInput(session, "player2", choices = all_players, selected = "no selection")
    
    output$results <- NULL
    # output$results_title <- renderText("")
    output$text <- renderText("")
  })
  
  observeEvent(c(input$player1, input$player2, input$p1_sets_won, input$p2_sets_won), {
    # output probabilities for player1 player2
    if (input$player1 != "NA" & input$player2 != "NA") {
      if (input$player1 == input$player2) {
        output$results <- NULL
        # output$results_title <- renderText("")
        output$text <- renderText("Player 1 and Player 2 cannot be the same person.")
      } else {
        output$text <- renderText("")
        
        # output$results_title <- renderText(paste("<pre>Score Prediction for<b>", input$player1, "</b>vs<b>", input$player2, "</b></pre>"))
        
        probs <- predict_score_prob(localreactive$model, input$player1, input$player2,
                                    input$p1_sets_won, input$p2_sets_won)
        
        # scores_const <- c("0-0", "1-0", "0-1", "2-0", "1-1", "0-2", "3-0", "2-1", "1-2", "0-3", 
        #                   "3-1", "2-2", "1-3", "3-2", "2-3")
        scores <- factor(c("3-0", "3-1", "3-2", "2-3",  "1-3", "0-3"), 
                            levels = rev(c("3-0", "3-1", "3-2", "2-3",  "1-3", "0-3")))
        probs <- c(probs[7], probs[11], probs[14], probs[15], probs[13], probs[10])
        df <- data.frame(scores, probs)
        
        title <- paste("Score Prediction for", input$player1, "vs", input$player2)
        subtitle <- paste0("Match Win Probability = ", round(sum(df[1:3,]$probs)*100,2), "%")
        output$results <- renderPlot({
          g <- ggplot(df, aes(x=probs, y=scores)) + 
            geom_col(width = 0.8, fill = "#a6cbee") +
            geom_text(aes(label=paste0(round(probs*100,2), "%")), hjust=-0.1) +
            labs(title  = title,
               subtitle = subtitle,
               x = 'Probability (%)',
               y = "Scores") 
          g %>%
            pub(type='bar')
        })
      }
    } else {
      output$text <- renderText("")
      # output$results_title <- renderText("")
    }
  })
}



# Create and Run the App
shinyApp(ui = ui, server = server)
