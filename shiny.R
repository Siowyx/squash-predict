library(shiny)
library(dplyr)


# pre load all models
# models1 <- readRDS("models/model_dynamic2023-2024.rds")
# models2 <- readRDS("models/model_dynamic2022-2024.rds")
# models3 <- readRDS("models/model_dynamic2021-2024.rds")
models4 <- readRDS("models/model_dynamic2020-2024.rds")
models5 <- readRDS("models/model_dynamic2019-2024.rds")

# predict function from fit.model3.R
predict_dynamic_win_prob <- function(model, p1, p2, p1_sets_won, p2_sets_won) {
  all_players <- gsub("`", "", names(coefficients(model))[-1])
  all_players <- all_players[all_players != "p1_sets_won" & all_players != "p2_sets_won"]
  
  new_data <- data.frame(matrix(ncol = length(all_players) + 2, nrow = 1))
  colnames(new_data) <- c(all_players, 'p1_sets_won', 'p2_sets_won')
  
  coef <- rep(0, length(all_players))
  coef[which(all_players == p1)] <- 1
  coef[which(all_players == p2)] <- -1
  new_data[1,] <- c(coef, p1_sets_won, p2_sets_won)
  
  win_prob <- predict(model, newdata = new_data, type = "response")
  
  return(win_prob[[1]])
}


# UI Definition
ui <- fluidPage(
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
      
      
      # Conditionally display dropdowns based on gender selection
      conditionalPanel(
        condition = "input.gender == 'women'",
        selectInput("player1", label = "Player 1", choices = c("testing", "test2"), selected = "no selection"),
        selectInput("player2", label = "Player 2", choices = c("testing333", "test2"), selected = "no selection")
      ),
      
      conditionalPanel(
        condition = "input.gender == 'men'",
        selectInput("player1", label = "Player 1", choices = c("testingmen", "test2"), selected = "no selection"),
        selectInput("player2", label = "Player 2", choices = c("testingmen333", "test2"), selected = "no selection")
      ),
      
      tags$hr(), # Horizontal line for separation
      
      wellPanel(
        h3(
          "Current Scores",
          style = "text-decoration: underline; padding: 10px; margin: 0; margin-right: 0;"  # Underline the text
        ),
        sliderInput("p1_sets_won", "Games won by Player 1", min = 0, max = 2, value = 0, step = 1),
        sliderInput("p2_sets_won", "Games won by Player 2", min = 0, max = 2, value = 0, step = 1)
      ),
      
      tags$hr(), # Horizontal line for separation
      
      selectInput("n_seasons", label = "Predict based on past seasons", choices = c(1,2,3,4,5), selected = 5)
    ),
    mainPanel(
      # Add a div to display the win probability
      conditionalPanel(
        condition = "input.player1 != 'no selection' & input.player2 != 'no selection'",
        tags$div(
          style = "background-color: #E6F0FF; border: 2px solid #555555; border-radius: 15px; padding: 5px 10px; margin-bottom: 20px;",
          tags$h2(paste0(verbatimTextOutput("player1"), "Winning Percentage against ", verbatimTextOutput("player2"), ":")),
          tags$h2(verbatimTextOutput("win_percent"))
        )
      )
    )
  )
)


# Server Logic
server <- function(input, output, session) {
  # give input values access to u
  output$player1 <- renderPrint({
      input$player1
    })
  output$player2 <- renderPrint({
      input$player2
    })
  output$p1_sets_won <- renderPrint({
      input$p1_sets_won
    })
  output$p2_sets_won <- renderPrint({
      input$p2_sets_won
    })
  
  # Reactive: Load models based on the number of past seasons selection
  selected_models <- reactive({
    if (input$n_seasons == 1) {
      models <- models1
    } else if (input$n_seasons == 2) {
      models <- models2
    } else if (input$n_seasons == 3) {
      models <- models3
    } else if (input$n_seasons == 4) {
      models <- models4
    } else {
      models <- models5
    }
    return(models)
  })
  
  # Reactive: Load model based on the gender selection
  selected_model <- reactive({
    if (input$gender == "men") {
      model <- models[[1]]
    } else {
      model <- models[[2]]
    }
    return(model)
  })
  
  # Output the prediction
  output_prediction <- reactive({
    models <- selected_models()
    model <- selected_model()
    
    prob <- predict_dynamic_win_prob(model, input$player1, input$player2, input$p1_sets_won, input$p2_sets_won)
    return(prob)
  })
  
  output$win_percent <- reactive(renderPrint({
    round(output_prediction() * 100, 1)
  }))
  
}



# Create and Run the App
shinyApp(ui = ui, server = server)
