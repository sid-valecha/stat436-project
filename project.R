# Load necessary libraries
library(tidyverse)
library(shiny)
library(lubridate)
library(shinythemes)
# https://rstudio.github.io/shinythemes/

# Load the datasets (change to your path)
# new paths: move the data folder into your working directory, and just use 
# the "data/{file name}.csv" relative path instead of using an absolute path, which makes it easier to work with. 
adv_data <- read_csv("data/advanced.csv")
team_trad_data <- read_csv("data/team_traditional.csv")
team_adv_data <- read_csv("data/team_advanced.csv")
trad_data <- read_csv("data/traditional.csv")

# Step 1: Handle Missing Data and Ensure Correct Data Types

# Convert `date` columns to Date type for consistency and filter to 2010-2011 season onwards
season_start_date <- as.Date("2010-10-26")
trad_data <- trad_data %>% mutate(date = as.Date(date)) %>% filter(date >= season_start_date)
adv_data <- adv_data %>% mutate(date = as.Date(date)) %>% filter(date >= season_start_date)
team_trad_data <- team_trad_data %>% mutate(date = as.Date(date)) %>% filter(date >= season_start_date)
team_adv_data <- team_adv_data %>% mutate(date = as.Date(date)) %>% filter(date >= season_start_date)

# Drop rows with any missing values in the datasets
trad_data <- trad_data %>% drop_na()
adv_data <- adv_data %>% drop_na()
team_trad_data <- team_trad_data %>% drop_na()
team_adv_data <- team_adv_data %>% drop_na()

# Step 2: Merge Player and Team Data
# Merge player-level traditional and advanced data
player_combined <- left_join(trad_data, adv_data, by = c("gameid", "date", "playerid", "team")) %>%
  drop_na()

player_combined <- player_combined %>%
  select(
    gameid, date, playerid, player = player.x, team, home = home.x, away = away.x, MIN = MIN.x, PTS, FGM, FGA, 
    `FG%`, `3PM`, `3PA`, `3P%`, FTM, FTA, `FT%`, OREB, DREB, REB, AST, STL, BLK, TOV, PF, `+/-`, win = win.x, season = season.x,
    OFFRTG, DEFRTG, NETRTG, `AST%`, `AST/TO`, `AST RATIO`, `OREB%`, `DREB%`, `REB%`, `TO RATIO`, `EFG%`, `TS%`, `USG%`, PACE, PIE
  ) %>%
  drop_na()

# Merge team-level traditional and advanced data
team_combined <- left_join(team_trad_data, team_adv_data, by = c("gameid", "date", "teamid")) %>%
  drop_na()

team_combined <- team_combined %>%
  select(
    gameid, date, teamid, team = team.x, home = home.x, away = away.x, MIN = MIN.x, PTS, FGM, FGA, 
    `FG%`, `3PM`, `3PA`, `3P%`, FTM, FTA, `FT%`, OREB, DREB, REB, AST, TOV, STL, BLK, PF, `+/-`, win = win.x, season = season.x,
    OFFRTG, DEFRTG, NETRTG, `AST%`, `AST/TO`, `AST RATIO`, `OREB%`, `DREB%`, `REB%`, `TOV%`, `EFG%`, `TS%`, PACE, PIE
  ) %>%
  drop_na()

bucks_player_data <- player_combined %>% filter(team == "MIL") # Filter for Milwaukee Bucks players only
bucks_team_data <- team_combined %>% filter(teamid == "1610612749") # Filter for Milwaukee Bucks team only

# current roster
current_roster_ids <- c(203081, 203114, 1630699, 1626171, 1641753, 201572, 1631157, 1631260, 1626192, 203507, 1641748) # current roster IDs, 11 out of 18 current players have played for the bucks before, so we will be focusing on their data

# Filter Bucks player data for current roster
bucks_player_data <- bucks_player_data %>%
  filter(playerid %in% current_roster_ids)

#dropdown menu options create
player_choices <- player_combined %>%
  filter(playerid %in% current_roster_ids) %>%
  select(playerid, player) %>%
  unique() %>%
  arrange(player)

opponent_choices <- bucks_team_data %>%
  filter(home != "MIL" | away != "MIL") %>%
  mutate(opponent = ifelse(home == "MIL", away, home)) %>%
  select(opponent) %>%
  distinct() %>%
  arrange(opponent)

ui <- fluidPage(
  theme = shinytheme("flatly"),  # bootstrap theme
  
  # found bucks color codes at : https://teamcolorcodes.com/milwaukee-bucks-color-codes/
  
  # Bg Styling
  tags$style(
    HTML("
      body {
        background-color: #EEE1C6;
      }
      .titlePanel {
        background-color: #00471B;
        color: white;
        padding: 10px;
        text-align: center;
        font-size: 2em;
        font-weight: bold;
      }
      .panel-title {
        color: #00471B;
        font-weight: bold;
      }
      .well {
        background-color: #FFFFFF;
        border: none;
      }
    ")
  ),
  
  # Title Panel
  titlePanel(
    tags$div(
      "Milwaukee Bucks Performance Analysis",
      style = "font-size: 2em; font-weight: bold; color: white; text-align: center; padding: 10px 0; background-color: #00471B;"
    )
  ),
  
  # Player Analysis Panel
  tags$div(
    h3("Player Analysis", style = "color: #00471B; font-weight: bold; padding-top: 20px;"),
    sidebarLayout(
      sidebarPanel(
        tags$h4("Player Selection", style = "color: #00471B;"),
        selectInput("player", "Select Player", choices = setNames(player_choices$playerid, player_choices$player)),
        helpText("Explore individual player trends for scoring, shooting efficiency, and turnovers.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Scoring Trends", plotOutput("scoring_trend")),
          tabPanel("Shooting Efficiency", plotOutput("shooting_efficiency")),
          tabPanel("Turnover Trends", plotOutput("turnover_comparison"))
        )
      )
    )
  ),
  
  # Spacer
  br(), hr(style = "border-color: #00471B;"), br(),
  
  
  # Team Analysis Panel
  tags$div(
    h3("Team Analysis", style = "color: #00471B; font-weight: bold; padding-top: 20px;"),
    sidebarLayout(
      sidebarPanel(
        tags$h4("Opponent Selection", style = "color: #00471B;"),
        selectInput("opponent", "Select Opponent", choices = opponent_choices$opponent),
        helpText("Analyze team performance metrics against selected opponents.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Offensive Rating Comparison", plotOutput("off_rating_vs_opponent")),
          tabPanel("Defensive Rating Comparison", plotOutput("def_rating_vs_opponent")),
          tabPanel("League Average Comparison", plotOutput("league_avg_comparison"))
        )
      )
    )
  ),
  br(), hr(style = "border-color: #00471B;"), br(),

)

# Server Logic
server <- function(input, output) {
  
  # --- Player Analysis Plots ---
  
  # 1. Scoring Trends Plot
  output$scoring_trend <- renderPlot({
    player_data <- bucks_player_data %>% filter(playerid == input$player)
    ggplot(player_data, aes(x = date, y = PTS)) +
      geom_line(color = "#00471B", size = 1) +
      geom_smooth(method = "lm", color = "#0077c0", linetype = "dashed") +
      labs(title = "Player Scoring Trends Over Time", x = "Date", y = "Points") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B")
      )
  })

  # 2. Shooting Efficiency Plot  
  output$shooting_efficiency <- renderPlot({
    player_data <- bucks_player_data %>% filter(playerid == input$player)
    ggplot(player_data, aes(x = `FG%`, y = `3P%`)) +
      geom_point(color = "#00471B") +
      geom_smooth(method = "lm", color = "#0077c0") +
      labs(title = "Shooting Efficiency (FG% vs 3P%)", x = "Field Goal %", y = "3 Point %") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B")
      )
  })
  
  # 3. Turnover Comparison Plot
  output$turnover_comparison <- renderPlot({
    player_data <- bucks_player_data %>% filter(playerid == input$player)
    ggplot(player_data, aes(x = date, y = TOV)) +
      geom_col(fill = "#0077c0") +
      labs(title = "Player Turnovers Over Time", x = "Date", y = "Turnovers") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B")
      )
  })
  
  # --- Team Analysis Plots ---
  
  # 1. Offensive Rating vs Opponent
  output$off_rating_vs_opponent <- renderPlot({
    team_data <- bucks_team_data %>% filter(home == input$opponent | away == input$opponent)
    ggplot(team_data, aes(x = date, y = OFFRTG, color = team)) +
      geom_line(size = 1) +
      labs(title = paste("Offensive Rating vs", input$opponent), x = "Date", y = "Offensive Rating") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B"),
        legend.position = "bottom"
      ) +
      scale_color_manual(values = setNames(c("#00471B", "#0077c0"), c("MIL", input$opponent)))
  })
  
  # 2. Defensive Rating vs Opponent
  output$def_rating_vs_opponent <- renderPlot({
    team_data <- bucks_team_data %>% filter(home == input$opponent | away == input$opponent)
    ggplot(team_data, aes(x = date, y = DEFRTG, color = team)) +
      geom_line(size = 1) +
      labs(title = paste("Defensive Rating vs", input$opponent), x = "Date", y = "Defensive Rating") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B"),
        legend.position = "bottom"
      ) +
      scale_color_manual(values = setNames(c("#00471B", "#0077c0"), c("MIL", input$opponent)))
  })
  
  # 3. League Average Comparison Plot
  output$league_avg_comparison <- renderPlot({
    league_avg_data <- bucks_team_data %>%
      group_by(date) %>%
      summarise(league_off_rating = mean(OFFRTG, na.rm = TRUE), league_def_rating = mean(DEFRTG, na.rm = TRUE))
    
    bucks_data <- bucks_team_data %>% filter(team == "MIL")
    
    ggplot() +
      geom_line(data = league_avg_data, aes(x = date, y = league_off_rating), color = "grey", linetype = "dotted", size = 1) +
      geom_line(data = bucks_data, aes(x = date, y = OFFRTG), color = "#00471B", size = 1.2) +
      geom_line(data = league_avg_data, aes(x = date, y = league_def_rating), color = "grey", linetype = "dashed", size = 1) +
      geom_line(data = bucks_data, aes(x = date, y = DEFRTG), color = "#0077c0", size = 1.2) +
      labs(
        title = "League vs Milwaukee Bucks: Offensive and Defensive Ratings",
        x = "Date",
        y = "Rating"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(color = "#00471B")
      )
  })
  
}

#run app
shinyApp(ui = ui, server = server)