library(shiny)
source("playerPageModule1.R")

ui <- fluidPage(
  titlePanel("NBA 2025 Draft Recap"),
  
  tags$style(HTML("
    .pick-assessment-container {
      max-width: 800px;
      margin: 30px auto;
      text-align: center;
      background-color: #1a1a1a;
      padding: 30px;
      border-radius: 12px;
      box-shadow: 0px 4px 10px rgba(0,0,0,0.4);
    }

    .pick-assessment-container h2 {
      font-family: 'Georgia', serif;
      font-size: 30px;
      font-weight: 700;
      color: #f0f0f0;
      margin-bottom: 20px;
      text-transform: uppercase;
      letter-spacing: 1px;
    }

    .pick-assessment-container p {
      font-family: 'Georgia', serif;
      font-size: 18px;
      line-height: 1.8;
      color: #e0e0e0;
      margin-bottom: 16px;
      text-align: justify;
    }

    .pick-assessment-container p:first-of-type::first-letter {
      float: left;
      font-size: 45px;
      line-height: 1;
      padding-right: 6px;
      font-weight: bold;
      color: #ffffff;
    }
  ")),
  tags$style(HTML("
  .dtfc-fixed-left {
    background-color: #212529 !important;
    color: white !important;
  }
  
  .dtfc-fixed-left table {
    background-color: #212529 !important;
  }
")),
  
  playerPageUI("player_page")
)


server <- function(input, output, session) {

  final_draft_synergy_percentiled1 <- readRDS("data/final_draft_synergy_percentiled.rds")
  team_playtype_percentiled1 <- readRDS("data/team_playtype_percentiled.rds")
  nba_full_combined_percentiled1 <- readRDS("data/nba_full_combined_percentiled.rds")
  nba_team_data <- readRDS("data/nba_team_data.rds")
  team_colors <- readxl::read_excel("data/Drafted Teams.xlsx")
  player_headshots <- readxl::read_excel("data/Post-Draft Headshots.xlsx")
  player_breakdowns <- readxl::read_excel("data/Post-Draft Writeups.xlsx")
  fit_func <- readRDS("functions/find_team_stat_weakness_and_matches.rds")
  
  
  
  
  playerPageServer(
    id = "player_page",
    final_draft_synergy_percentiled = final_draft_synergy_percentiled1,
    team_playtype_percentiled = team_playtype_percentiled1,
    nba_full_combined_percentiled = nba_full_combined_percentiled1,
    nba_data = nba_team_data,
    theme_colors = team_colors,
    player_headshots = player_headshots,
    player_breakdowns = player_breakdowns,
    fit_function = fit_func
  )
}

shinyApp(ui, server)


