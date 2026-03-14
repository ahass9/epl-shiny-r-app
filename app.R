library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Load data
df <- read.csv("data/epl_final.csv")

# Clean columns
df$Season <- trimws(as.character(df$Season))
df$HomeTeam <- trimws(as.character(df$HomeTeam))
df$AwayTeam <- trimws(as.character(df$AwayTeam))
df$FullTimeResult <- trimws(as.character(df$FullTimeResult))

# Get all unique teams
teams <- sort(unique(c(df$HomeTeam, df$AwayTeam)))

ui <- fluidPage(
  titlePanel("English Premier League Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "season",
        label = "Choose a season:",
        choices = sort(unique(df$Season)),
        selected = sort(unique(df$Season))[1]
      ),
      
      selectInput(
        inputId = "team",
        label = "Choose a team:",
        choices = teams,
        selected = teams[1]
      )
    ),
    
    mainPanel(
      h3("Filtered Matches"),
      DTOutput("matches_table"),
      
      h3("Match Result Summary"),
      plotOutput("results_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered dataframe
  filtered_data <- reactive({
    df |>
      filter(
        Season == input$season,
        HomeTeam == input$team | AwayTeam == input$team
      )
  })
  
  # Create win/loss/draw summary
  result_summary <- reactive({
    filtered_data() |>
      mutate(
        team_result = case_when(
          HomeTeam == input$team & FullTimeResult == "H" ~ "Win",
          AwayTeam == input$team & FullTimeResult == "A" ~ "Win",
          HomeTeam == input$team & FullTimeResult == "A" ~ "Loss",
          AwayTeam == input$team & FullTimeResult == "H" ~ "Loss",
          TRUE ~ "Draw"
        )
      ) |>
      count(team_result)
  })
  
  # Table output
  output$matches_table <- renderDT({
    filtered_data() |>
      select(
        Season,
        HomeTeam,
        AwayTeam,
        FullTimeHomeGoals,
        FullTimeAwayGoals,
        FullTimeResult
      ) |>
      datatable(options = list(pageLength = 10))
  })
  
  # Plot output
  output$results_plot <- renderPlot({
    ggplot(result_summary(), aes(x = team_result, y = n, fill = team_result)) +
      geom_col() +
      labs(
        title = paste("Results for", input$team, "in", input$season),
        x = "Match Result",
        y = "Count"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)