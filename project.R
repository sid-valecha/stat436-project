# Load necessary libraries
library(tidyverse)
library(shiny)
library(lubridate)
library(shinythemes)
library(rsconnect)
library(plotly)

# https://rstudio.github.io/shinythemes/

# Load the datasets (change to your path)
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

# Add a Points per minute column
bucks_player_data <- bucks_player_data %>% 
  mutate(PPM = PTS / MIN)

# Create season boundaries
season_boundaries <- tibble(
  date = c("2010-10-26", "2011-12-25", "2012-10-30", "2013-10-29", "2014-10-28", 
           "2015-10-27", "2016-10-25", "2017-10-17", "2018-10-16", "2019-10-22", 
           "2020-12-22", "2021-10-19", "2022-10-18"),
  season = c("2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", 
             "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23")
)

season_boundaries$date <- as.Date(season_boundaries$date)


league_averages <- team_combined %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    league_off_rating = mean(OFFRTG, na.rm = TRUE),
    league_def_rating = mean(DEFRTG, na.rm = TRUE)
  )



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
        helpText("Explore individual player trends for scoring, shooting efficiency, and points per minute.")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Scoring Trends", plotOutput("scoring_trend")),
          tabPanel("Shooting Efficiency", plotOutput("shooting_efficiency")),
          tabPanel("Points per Minute", plotOutput("shooting_percentages"))
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
          tabPanel("Defensive Rating Comparison", plotOutput("def_rating_vs_opponent"))
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
      geom_vline(data = season_boundaries, aes(xintercept = date), linetype = "dashed", color = "gray") +
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
    ggplot(player_data, aes(x = `FG%`)) +
      geom_density(fill = "#00471B", alpha = 0.7) +
      geom_density(aes(x = `3P%`), fill = "#0077c0", alpha = 0.7) +
      labs(title = "Shooting Efficiency Distribution", x = "Percentage", y = "Density", fill = "Metric") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B"),
        legend.position = "bottom"
      )
  })
  
  # 3.PPM Plot over time
  output$shooting_percentages <- renderPlot({
    player_data <- bucks_player_data %>% filter(playerid == input$player)
    ggplot(player_data, aes(x = date, y = PPM)) +
      geom_line(color = "#00471B", size = 1) +
      geom_smooth(method = "lm", color = "#0077c0", linetype = "dashed") +
      labs(title = "Points Per Minute Over Time", x = "Date", y = "Points Per Minute") +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B")
      )
  })
  
  # --- Team Analysis Plots ---
  
  # 1. Offensive Rating vs Opponent
  output$off_rating_vs_opponent <- renderPlot({
    # Filter Bucks team data for games against the selected opponent
    team_data <- bucks_team_data %>% filter(home == input$opponent | away == input$opponent)
    
    # Filter opponent team data for games against the Bucks
    opponent_team_data <- team_combined %>%
      filter((home == "MIL" | away == "MIL") & team == input$opponent)
    
    # Get relevant league average data dynamically
    league_avg_data <- league_averages %>% filter(year %in% unique(year(team_data$date)))
    
    # Dynamically generate color labels for the opponent
    color_mapping <- c("Bucks" = "#00471B", "League Avg" = "orange")
    color_mapping[input$opponent] <- "#0077c0"  # Add the opponent dynamically
    
    ggplot() +
      # Bucks Offensive Rating
      geom_line(data = team_data, aes(x = date, y = OFFRTG, color = "Bucks"), size = 1) +
      # Opponent Offensive Rating
      geom_line(data = opponent_team_data, aes(x = date, y = OFFRTG, color = input$opponent), size = 1) +
      # League Average Offensive Rating
      geom_line(data = league_avg_data, aes(x = as.Date(paste0(year, "-07-01")), y = league_off_rating, color = "League Avg"), linetype = "dashed", size = 1) +
      # Apply dynamic colors
      scale_color_manual(values = color_mapping) +
      labs(
        title = paste("Offensive Rating vs", input$opponent),
        x = "Date",
        y = "Offensive Rating",
        color = "Team"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B"),
        legend.position = "bottom"
      )
  })
  
  
  # 2. Defensive Rating vs Opponent
  output$def_rating_vs_opponent <- renderPlot({
    # Filter Bucks team data for games against the selected opponent
    team_data <- bucks_team_data %>% filter(home == input$opponent | away == input$opponent)
    
    # Filter opponent team data for games against the Bucks
    opponent_team_data <- team_combined %>%
      filter((home == "MIL" | away == "MIL") & team == input$opponent)
    
    # Get relevant league average data dynamically
    league_avg_data <- league_averages %>% filter(year %in% unique(year(team_data$date)))
    
    # Dynamically generate color labels for the opponent
    color_mapping <- c("Bucks" = "#00471B", "League Avg" = "orange")
    color_mapping[input$opponent] <- "#0077c0"  # Add the opponent dynamically
    
    ggplot() +
      # Bucks Defensive Rating
      geom_line(data = team_data, aes(x = date, y = DEFRTG, color = "Bucks"), size = 1) +
      # Opponent Defensive Rating
      geom_line(data = opponent_team_data, aes(x = date, y = DEFRTG, color = input$opponent), size = 1) +
      # League Average Defensive Rating
      geom_line(data = league_avg_data, aes(x = as.Date(paste0(year, "-07-01")), y = league_def_rating, color = "League Avg"), linetype = "dashed", size = 1) +
      # Apply dynamic colors
      scale_color_manual(values = color_mapping) +
      labs(
        title = paste("Defensive Rating vs", input$opponent),
        x = "Date",
        y = "Defensive Rating",
        color = "Team"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(color = "#00471B", size = 16, face = "bold"),
        axis.title = element_text(color = "#00471B"),
        legend.position = "bottom"
      )
  })
  
  
}

#run app
shinyApp(ui = ui, server = server)