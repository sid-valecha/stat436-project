# Load necessary libraries
library(tidyverse)
library(shiny)
library(lubridate)


# Load the datasets (change to your path)
# you can also move the data folder into your working directory, remove the path and just use 
# the "/data/{file name}.csv" relative path instead of using an absolute path
adv_data <- read_csv("/Users/sidvalecha/Desktop/Fall 2024/Stat 436/project/data/advanced.csv")
team_trad_data <- read_csv("/Users/sidvalecha/Desktop/Fall 2024/Stat 436/project/data/team_traditional.csv")
team_adv_data <- read_csv("/Users/sidvalecha/Desktop/Fall 2024/Stat 436/project/data/team_advanced.csv")
trad_data <- read_csv("/Users/sidvalecha/Desktop/Fall 2024/Stat 436/project/data/traditional.csv")

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

# Shiny UI
ui <- fluidPage(
  titlePanel("Milwaukee Bucks Performance Analysis"),

  # Player Analysis Panel
  h3("Player Analysis"),
  sidebarLayout(
    sidebarPanel(
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
  ),
  
  
  # Spacer
  br(), hr(), br(),
  
  # Team Analysis Panel
  h3("Team Analysis"),
  sidebarLayout(
    sidebarPanel(
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
  
)

# Server Logic
server <- function(input, output) {
  
  # --- Player Analysis Plots ---
  
  # 1. Scoring Trends Plot
  output$scoring_trend <- renderPlot({
    player_data <- bucks_player_data %>% filter(playerid == input$player)
    ggplot(player_data, aes(x = date, y = PTS)) +
      geom_line(color = "blue") +
      geom_smooth(method = "lm", color = "red", linetype = "dashed") +
      labs(title = "Player Scoring Trends Over Time", x = "Date", y = "Points") +
      theme_minimal()
  })
  
  # 2. Shooting Efficiency Plot
  output$shooting_efficiency <- renderPlot({
    player_data <- bucks_player_data %>% filter(playerid == input$player)
    ggplot(player_data, aes(x = `FG%`, y = `3P%`)) +
      geom_point(color = "purple") +
      geom_smooth(method = "lm", color = "darkgreen") +
      labs(title = "Shooting Efficiency (FG% vs 3P%)", x = "Field Goal %", y = "3 Point %") +
      theme_minimal()
  })
  
  # 3. Turnover Comparison Plot
  output$turnover_comparison <- renderPlot({
    player_data <- bucks_player_data %>% filter(playerid == input$player)
    ggplot(player_data, aes(x = date, y = TOV)) +
      geom_col(fill = "red") +
      labs(title = "Player Turnovers Over Time", x = "Date", y = "Turnovers") +
      theme_minimal()
  })
  
  # --- Team Analysis Plots ---
  
  # 1. Offensive Rating vs Opponent
  output$off_rating_vs_opponent <- renderPlot({
    team_data <- bucks_team_data %>% filter(home == input$opponent | away == input$opponent)
    ggplot(team_data, aes(x = date, y = OFFRTG, color = team)) +
      geom_line() +
      labs(title = paste("Offensive Rating vs", input$opponent), x = "Date", y = "Offensive Rating") +
      theme_minimal() +
      scale_color_manual(values = setNames(c("blue", "darkorange"), c("MIL", input$opponent)))
  })
  
  # 2. Defensive Rating vs Opponent
  output$def_rating_vs_opponent <- renderPlot({
    team_data <- bucks_team_data %>% filter(home == input$opponent | away == input$opponent)
    ggplot(team_data, aes(x = date, y = DEFRTG, color = team)) +
      geom_line() +
      labs(title = paste("Defensive Rating vs", input$opponent), x = "Date", y = "Defensive Rating") +
      theme_minimal() +
      scale_color_manual(values = setNames(c("blue", "darkorange"), c("MIL", input$opponent)))
  })
  
  # 3. League Average Comparison Plot
  output$league_avg_comparison <- renderPlot({
    league_avg_data <- bucks_team_data %>%
      group_by(date) %>%
      summarise(league_off_rating = mean(OFFRTG, na.rm = TRUE), league_def_rating = mean(DEFRTG, na.rm = TRUE))
    
    bucks_data <- bucks_team_data %>% filter(team == "MIL")
    
    ggplot() +
      geom_line(data = league_avg_data, aes(x = date, y = league_off_rating), color = "grey", linetype = "dotted") +
      geom_line(data = bucks_data, aes(x = date, y = OFFRTG), color = "blue") +
      geom_line(data = league_avg_data, aes(x = date, y = league_def_rating), color = "grey", linetype = "dashed") +
      geom_line(data = bucks_data, aes(x = date, y = DEFRTG), color = "green") +
      labs(
        title = "League vs Milwaukee Bucks: Offensive and Defensive Ratings",
        x = "Date",
        y = "Rating"
      ) +
      scale_y_continuous("Rating") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
