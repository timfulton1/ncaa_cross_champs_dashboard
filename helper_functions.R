# Load packages
library(shiny)  # to run the app
library(bslib)  # ui toolkit 
library(bsicons)  # icons
library(janitor)  # cleaning
library(stringr)  # cleaning
library(dplyr)  # cleaning  
library(glue)  # concatenate
library(english)  # cleaning
library(lubridate)  # cleaning
library(forcats)  # reordering factors levels
library(ggplot2)  # plotting
library(ggtext)  # adding text to plot 
library(gt)  # tables

# Load data
team_results <- read.csv("data/team_results.csv") %>% clean_names()
ind_results <- read.csv("data/individual_results.csv") %>% clean_names()
team_results_women <- read.csv("data/team_results_women.csv") %>% clean_names()
ind_results_women <- read.csv("data/individual_results_women.csv") %>% clean_names()
color_gradient <- read.csv("data/color_gradient.csv")

#### Men's Data ####
## Clean Team Data ----
# Only top 10 finishes
team_results <- team_results %>% 
  filter(place <= 10) %>% 
  relocate(year, .before = place) %>% 
  left_join(color_gradient, by = "place")

# Create df for top finishes and ranks
team_ranks <- team_results %>% 
  group_by(team) %>% 
  summarise(top1_count = sum(place <= 1),
            top4_count = sum(place <= 4),
            top10_count = sum(place <= 10)
            ) %>% 
  mutate(top4_count = top4_count - top1_count,
         top10_count = top10_count - (top4_count + top1_count),
         top1_rank = min_rank(desc(top1_count)),
         top4_rank = min_rank(desc(top4_count)),
         top10_rank = min_rank(desc(top10_count)))

# Create function for ordinals
ordinal <- function(x) {
  suffix <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  if (x %% 100 %in% 11:13) {
    return(paste0(x, "th"))
  }
  return(paste0(x, suffix[x %% 10 + 1]))
}

# Assign ordinals to new columns
team_ranks$top1_ordinal <- sapply(team_ranks$top1_rank, ordinal)
team_ranks$top4_ordinal <- sapply(team_ranks$top4_rank, ordinal)
team_ranks$top10_ordinal <- sapply(team_ranks$top10_rank, ordinal)

# Update team ordinals if counts are 0
team_ranks$top1_ordinal <- ifelse(team_ranks$top1_count == 0, "None", team_ranks$top1_ordinal)
team_ranks$top4_ordinal <- ifelse(team_ranks$top4_count == 0, "None", team_ranks$top4_ordinal)
team_ranks$top10_ordinal <- ifelse(team_ranks$top10_count == 0, "None", team_ranks$top10_ordinal)


## Clean Individual Data for Team results----
# Remove spaces before athlete names
ind_results$athlete <- str_trim(ind_results$athlete, side = "left")

# Only top 10 athletes
ind_results <- ind_results %>% 
  filter(place <= 10) %>% 
  relocate(year, .before = place) 

# Create df for top finishes and ranks by athlete and team
ind_ranks_by_team <- ind_results %>% 
  group_by(athlete, team) %>% 
  summarise(top1_count = sum(place <= 1),
            top4_count = sum(place <= 4),
            top10_count = sum(place <= 10)) %>% 
  mutate(top1_rank = min_rank(top1_count),
         top4_rank = min_rank(desc(top4_count)),
         top10_rank = min_rank(desc(top10_count)))

# Create df for individual winner counts by team
ind_winners_by_team <- ind_ranks_by_team %>% 
  group_by(team) %>% 
  summarise(ind_winner_count = sum(top1_count)) %>% 
  mutate(ind_winner_rank = min_rank(desc(ind_winner_count)))

# Assign ordinals to new columns
ind_winners_by_team$ind_winner_ordinal <- sapply(ind_winners_by_team$ind_winner_rank, ordinal)

# Join individual results with teams
team_ranks <- team_ranks %>% 
  left_join(ind_winners_by_team)

# Update individual ordinals if counts are 0
team_ranks$ind_winner_count <- ifelse(is.na(team_ranks$ind_winner_count), 0, team_ranks$ind_winner_count)
team_ranks$ind_winner_ordinal <- ifelse(team_ranks$ind_winner_count == 0, "None", team_ranks$ind_winner_ordinal)

## Clean Individual Data for Individual results----

# Update the time format so all end in 1 decomal
ind_results$time <- gsub("(\\d+):(\\d+).(\\d)\\d$", "\\1:\\2.\\3", ind_results$time)

# Create a new column for seconds to use later when calculating the winning margin
ind_results$seconds<- period_to_seconds(ms(ind_results$time)) 

# Create df for top finishes by athlete only
ind_ranks <- ind_results %>% 
  group_by(athlete) %>% 
  summarise(top1_count = sum(place <= 1),
            top10_count = sum(place <= 10),
            fastest_time = min(time),
            highest_place = min(place))

ind_ranks$highest_place_ordinal <- sapply(ind_ranks$highest_place, ordinal)


## Drop down lists ----
# Assign variables to objects used in drop down lists
teams <- unique(sort(team_results$team))
years <- unique(sort(team_results$year))
athletes <- unique(sort(ind_results$athlete))



### Filters ###
## Filtering function for selected team results ----
selected_team_ranks <- function(selected_team){
  
  selected_team_ranks_df <- team_ranks %>% 
    filter(team == selected_team)
  
  top1_count <- selected_team_ranks_df[,2] 
  
  top4_count <- selected_team_ranks_df[,3] 
  
  top10_count <- selected_team_ranks_df[,4] 
  
  top1_rank <- selected_team_ranks_df[,8] 
  
  top4_rank <- selected_team_ranks_df[,9] 
  
  top10_rank <- selected_team_ranks_df[,10] 
  
  ind_win_count <- selected_team_ranks_df[,11] 
  
  ind_win_rank <- selected_team_ranks_df[,13] 
  
  return(c(top1_count, top4_count, top10_count, top1_rank, top4_rank, top10_rank, ind_win_count, ind_win_rank))
}


## Filtering function for selected individual results table ----
selected_ind_results_df <- function(selected_ind){
  
  selected_ind_results_df <- ind_results %>% 
    filter(athlete == selected_ind) %>% 
    select(-points, -class, -athlete, -seconds) %>% 
    rename("Year" = "year") %>% 
    rename("Place" = "place") %>% 
    #rename("Athlete" = "athlete") %>% 
    rename("Team" = "team") %>% 
    rename("Time" = "time") %>% 
    relocate(Time, .before = Team)
  
  return(selected_ind_results_df)
}



## Filtering function for selected individual ranks ----
selected_ind_rank_df <- function(selected_ind){
  
  selected_ind_rank_df <- ind_ranks %>% 
    filter(athlete == selected_ind)
  
  ind_wins <- selected_ind_rank_df[,2] 
  
  ind_top10 <- selected_ind_rank_df[,3] 
  
  fastest_time <- selected_ind_rank_df[,4] 
  
  highest_finish <- selected_ind_rank_df[,6] 
  
  return(c(ind_wins, ind_top10, fastest_time, highest_finish))
}


## Filtering function for selected year team results ----
selected_year_team_results <- function(selected_year){
  
  # Get table
  selected_year_df <- team_results %>% 
    filter(year == selected_year) %>% 
    select(place, team, score) %>% 
    rename("Place" = "place",
           "Team" = "team",
           "Points" = "score")
  
  return(selected_year_df)
}


## Filtering function for selected year team winning margin ----
selected_year_team_win_margin <- function(selected_year){
  
  # Get table
  selected_year_df <- team_results %>% 
    filter(year == selected_year) %>% 
    select(place, team, score) %>% 
    rename("Place" = "place",
           "Team" = "team",
           "Points" = "score")
  
  # Get winning margin
  team_win_margin <- selected_year_df[2, 3] - selected_year_df[1, 3]
  
  return(team_win_margin)
}

## Filtering function for selected year individual results ----
selected_year_ind_results <- function(selected_year){
  
  selected_year_ind_df <- ind_results %>% 
    filter(year == selected_year) %>% 
    select(place, athlete, team, time) %>% 
    rename("Place" = "place",
           "Athlete" = "athlete",
           "Team" = "team",
           "Time" = "time")
  
  return(selected_year_ind_df)
}

## Filtering function for selected year individual winning margin ----
selected_year_ind_win_margin <- function(selected_year){
  
  selected_year_ind_df <- ind_results %>% 
    filter(year == selected_year) %>% 
    select(place, athlete, team, time) %>% 
    rename("Place" = "place",
           "Athlete" = "athlete",
           "Team" = "team",
           "Time" = "time")
  
  ind_win_margin <- period_to_seconds(ms(selected_year_ind_df[2, 4])) - period_to_seconds(ms(selected_year_ind_df[1, 4]))

  
  return(ind_win_margin)
}


## Plotting function for selected team results ----
selected_team_plot <- function(selected_team){
  
  selected_team_df <- team_results %>% 
    filter(team == selected_team) %>% 
    select(year, place, score, color) %>% 
    rename("Year" = "year",
           "Place" = "place",
           "Points" = "score")
  
  selected_team_ind_winners <-ind_results %>% 
    filter(team == selected_team) %>% 
    filter(place == 1) %>% 
    select(year, place) %>% 
    rename("Year" = "year",
           "Winner" = "place")
  
  selected_team_df <- selected_team_df %>% 
    full_join(selected_team_ind_winners, by = "Year")

  selected_team_plot <- selected_team_df %>% 
    ggplot(mapping = aes(x = Year, y = Place, label = Place, color = color)) +
    theme_classic() +
    theme(
      panel.grid.major.x = element_line(color = "gray50", linetype = "dotted"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.x.top = element_text(size = 15, margin = margin(b = 10)),
      axis.title.y = element_text(size = 15),
    ) +
    geom_point(
      size = 8.5
    ) +
    geom_point(
      inherit.aes = FALSE,
      aes(x = Year, y = Winner,color = "#969CA3"),
      shape = 1,
      size = 9,
      stroke = 1.2
    ) +
    geom_text(
      color = "gray10",
      size = 4.75,
      fontface = "bold"
    ) +
    scale_color_identity() +
    guides(color = "none") +
    scale_x_continuous(
      limits = c(1965, 2023),
      breaks = seq(1965, 2023, 1),
      labels = function(x) substring(as.character(x), 3, 4),
      expand = expansion(c(0.01, 0.01)),
      position = "top"
    ) +
    scale_y_reverse(
      limits = c(10, 1),
      breaks = seq(1, 10, 1),
      expand = expansion(c(0.06, 0.08))
    )
  
  return(selected_team_plot)
}

## Creating the overall metrics data frame for Individual ----
                                        
ind_avg_time_byplace_df <- ind_results %>% 
  filter(year >= 1976) %>% 
  group_by(place) %>% 
  summarise(avg_top10 = round(mean(seconds), 1)) 

ind_margins_df <- ind_results %>% 
  filter(year >= 1976) %>% 
  filter(place %in% c(1, 2)) %>%
  group_by(year) %>% 
  summarize(winning_margin = abs(diff(seconds[order(place)])))

overall_ind_metrics_df <- tibble(metric = c("ind_margin_min", "ind_margin_max", "ind_margin_avg", "avg_win_time", "top5_threshold", "top10_threshold", "fastest_winning_time", "slowest_winning_time", "top2_threshold"),
                                 value = c(0.5, 45, round(mean(ind_margins_df$winning_margin), 1), "29:23.3", "29:48.6", "30:02.8", "28:06.6", "30:44.9", "29:33.4"))


## Creating the overall metrics data frame for Team ----

team_avg_points_byplace_df <- team_results %>% 
  group_by(place) %>% 
  summarise(avg_top10 = round(mean(score))) 


team_margins_df <- team_results %>% 
  filter(place %in% c(1, 2)) %>%
  group_by(year) %>% 
  summarize(winning_margin = abs(diff(score[order(place)])))

overall_team_metrics_df <- tibble(metric = c("team_margin_min", "team_margin_max", "team_margin_avg", "avg_win_points", "podium_points", "top10_points", "lowest_winning_score", "highest winning score", "top2_points"),
                                 value = c(0, 150, round(mean(team_margins_df$winning_margin)), team_avg_points_byplace_df[[1,2]], team_avg_points_byplace_df[[4,2]], team_avg_points_byplace_df[[10,2]], 17, 149, 118))


#### Women's Data ####
## Clean Team Data ----
# Only top 10 finishes
team_results_women <- team_results_women %>% 
  filter(place <= 10) %>% 
  relocate(year, .before = place) %>% 
  left_join(color_gradient, by = "place")

# Create df for top finishes and ranks
team_ranks_women <- team_results_women %>% 
  group_by(team) %>% 
  summarise(top1_count = sum(place <= 1),
            top4_count = sum(place <= 4),
            top10_count = sum(place <= 10)
  ) %>% 
  mutate(top4_count = top4_count - top1_count,
         top10_count = top10_count - (top4_count + top1_count),
         top1_rank = min_rank(desc(top1_count)),
         top4_rank = min_rank(desc(top4_count)),
         top10_rank = min_rank(desc(top10_count)))

# Create function for ordinals
ordinal <- function(x) {
  suffix <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  if (x %% 100 %in% 11:13) {
    return(paste0(x, "th"))
  }
  return(paste0(x, suffix[x %% 10 + 1]))
}

# Assign ordinals to new columns
team_ranks_women$top1_ordinal <- sapply(team_ranks_women $top1_rank, ordinal)
team_ranks_women$top4_ordinal <- sapply(team_ranks_women $top4_rank, ordinal)
team_ranks_women$top10_ordinal <- sapply(team_ranks_women $top10_rank, ordinal)

# Update team ordinals if counts are 0
team_ranks_women$top1_ordinal <- ifelse(team_ranks_women$top1_count == 0, "None", team_ranks_women$top1_ordinal)
team_ranks_women$top4_ordinal <- ifelse(team_ranks_women$top4_count == 0, "None", team_ranks_women$top4_ordinal)
team_ranks_women$top10_ordinal <- ifelse(team_ranks_women$top10_count == 0, "None", team_ranks_women$top10_ordinal)


## Clean Individual Data for Team results----
# Remove spaces before athlete names

ind_results_women <- ind_results_women %>% 
  rename("athlete" = "name")

ind_results_women$athlete <- str_trim(ind_results_women$athlete, side = "left")

# Only top 10 athletes
ind_results_women <- ind_results_women %>% 
  filter(place <= 10) %>% 
  relocate(year, .before = place) 

# Create df for top finishes and ranks by athlete and team
ind_ranks_by_team_women <- ind_results_women %>% 
  group_by(athlete, team) %>% 
  summarise(top1_count = sum(place <= 1),
            top4_count = sum(place <= 4),
            top10_count = sum(place <= 10)) %>% 
  mutate(top1_rank = min_rank(top1_count),
         top4_rank = min_rank(desc(top4_count)),
         top10_rank = min_rank(desc(top10_count)))

# Create df for individual winner counts by team
ind_winners_by_team_women <- ind_ranks_by_team_women %>% 
  group_by(team) %>% 
  summarise(ind_winner_count = sum(top1_count)) %>% 
  mutate(ind_winner_rank = min_rank(desc(ind_winner_count)))

# Assign ordinals to new columns
ind_winners_by_team_women$ind_winner_ordinal <- sapply(ind_winners_by_team_women$ind_winner_rank, ordinal)

# Join individual results with teams
team_ranks_women <- team_ranks_women %>% 
  left_join(ind_winners_by_team_women)

# Update individual ordinals if counts are 0
team_ranks_women$ind_winner_count <- ifelse(is.na(team_ranks_women$ind_winner_count), 0, team_ranks_women$ind_winner_count)
team_ranks_women$ind_winner_ordinal <- ifelse(team_ranks_women$ind_winner_count == 0, "None", team_ranks_women$ind_winner_ordinal)

## Clean Individual Data for Individual results----

# Update the time format so all end in 1 decomal
ind_results_women$time <- gsub("(\\d+):(\\d+).(\\d)\\d$", "\\1:\\2.\\3", ind_results_women$time)

# Create a new column for seconds to use later when calculating the winning margin
ind_results_women$seconds <- period_to_seconds(ms(ind_results_women$time)) 

# Create df for top finishes by athlete only
ind_ranks_women <- ind_results_women %>% 
  group_by(athlete) %>% 
  summarise(top1_count = sum(place <= 1),
            top10_count = sum(place <= 10),
            fastest_time = min(time),
            highest_place = min(place))

ind_ranks_women$highest_place_ordinal <- sapply(ind_ranks_women$highest_place, ordinal)


## Drop down lists ----
# Assign variables to objects used in drop down lists
teams_women <- unique(sort(team_results_women$team))
years_women <- unique(sort(team_results_women$year))
athletes_women <- unique(sort(ind_results_women$athlete))

### Filters ###
## Filtering function for selected team results ----
selected_team_ranks_women <- function(selected_team){
  
  selected_team_ranks_df <- team_ranks_women %>% 
    filter(team == selected_team)
  
  top1_count <- selected_team_ranks_df[,2] 
  
  top4_count <- selected_team_ranks_df[,3] 
  
  top10_count <- selected_team_ranks_df[,4] 
  
  top1_rank <- selected_team_ranks_df[,8] 
  
  top4_rank <- selected_team_ranks_df[,9] 
  
  top10_rank <- selected_team_ranks_df[,10] 
  
  ind_win_count <- selected_team_ranks_df[,11] 
  
  ind_win_rank <- selected_team_ranks_df[,13] 
  
  return(c(top1_count, top4_count, top10_count, top1_rank, top4_rank, top10_rank, ind_win_count, ind_win_rank))
}


## Filtering function for selected individual results table ----
selected_ind_results_df_women <- function(selected_ind){
  
  selected_ind_results_df <- ind_results_women %>% 
    filter(athlete == selected_ind) %>% 
    select(-athlete, -seconds) %>% 
    rename("Year" = "year") %>% 
    rename("Place" = "place") %>% 
    #rename("Athlete" = "athlete") %>% 
    rename("Team" = "team") %>% 
    rename("Time" = "time") %>% 
    relocate(Time, .before = Team)
  
  return(selected_ind_results_df)
}


## Filtering function for selected individual ranks ----
selected_ind_rank_df_women <- function(selected_ind){
  
  selected_ind_rank_df <- ind_ranks_women %>% 
    filter(athlete == selected_ind)
  
  ind_wins <- selected_ind_rank_df[,2] 
  
  ind_top10 <- selected_ind_rank_df[,3] 
  
  fastest_time <- selected_ind_rank_df[,4] 
  
  highest_finish <- selected_ind_rank_df[,6] 
  
  return(c(ind_wins, ind_top10, fastest_time, highest_finish))
}


## Filtering function for selected year team results ----
selected_year_team_results_women <- function(selected_year){
  
  # Get table
  selected_year_df <- team_results_women %>% 
    filter(year == selected_year) %>% 
    select(place, team, score) %>% 
    rename("Place" = "place",
           "Team" = "team",
           "Points" = "score")
  
  return(selected_year_df)
}


## Filtering function for selected year team winning margin ----
selected_year_team_win_margin_women <- function(selected_year){
  
  # Get table
  selected_year_df <- team_results_women %>% 
    filter(year == selected_year) %>% 
    select(place, team, score) %>% 
    rename("Place" = "place",
           "Team" = "team",
           "Points" = "score")
  
  # Get winning margin
  team_win_margin <- selected_year_df[2, 3] - selected_year_df[1, 3]
  
  return(team_win_margin)
}

## Filtering function for selected year individual results ----
selected_year_ind_results_women <- function(selected_year){
  
  selected_year_ind_df <- ind_results_women %>% 
    filter(year == selected_year) %>% 
    select(place, athlete, team, time) %>% 
    rename("Place" = "place",
           "Athlete" = "athlete",
           "Team" = "team",
           "Time" = "time")
  
  return(selected_year_ind_df)
}

## Filtering function for selected year individual winning margin ----
selected_year_ind_win_margin_women <- function(selected_year){
  
  selected_year_ind_df <- ind_results_women %>% 
    filter(year == selected_year) %>% 
    select(place, athlete, team, time) %>% 
    rename("Place" = "place",
           "Athlete" = "athlete",
           "Team" = "team",
           "Time" = "time")
  
  ind_win_margin <- period_to_seconds(ms(selected_year_ind_df[2, 4])) - period_to_seconds(ms(selected_year_ind_df[1, 4]))
  
  
  return(ind_win_margin)
}


## Plotting function for selected team results ----
selected_team_plot_women <- function(selected_team){
  
  selected_team_df <- team_results_women %>% 
    filter(team == selected_team) %>% 
    select(year, place, score, color) %>% 
    rename("Year" = "year",
           "Place" = "place",
           "Points" = "score")
  
  selected_team_ind_winners <- ind_results_women %>% 
    filter(team == selected_team) %>% 
    filter(place == 1) %>% 
    select(year, place) %>% 
    rename("Year" = "year",
           "Winner" = "place")
  
  selected_team_df <- selected_team_df %>% 
    full_join(selected_team_ind_winners, by = "Year")
  
  selected_team_plot <- selected_team_df %>% 
    ggplot(mapping = aes(x = Year, y = Place, label = Place, color = color)) +
    theme_classic() +
    theme(
      panel.grid.major.x = element_line(color = "gray50", linetype = "dotted"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.x.top = element_text(size = 15, margin = margin(b = 10)),
      axis.title.y = element_text(size = 15),
    ) +
    geom_point(
      size = 8.5
    ) +
    geom_point(
      inherit.aes = FALSE,
      aes(x = Year, y = Winner, color = "#969CA3"),
      shape = 1,
      size = 9,
      stroke = 1.2
    ) +
    geom_text(
      color = "gray10",
      size = 4.75,
      fontface = "bold"
    ) +
    scale_color_identity() +
    guides(color = "none") +
    scale_x_continuous(
      limits = c(1981, 2023),
      breaks = seq(1981, 2023, 1),
      labels = function(x) substring(as.character(x), 3, 4),
      expand = expansion(c(0.01, 0.01)),
      position = "top"
    ) +
    scale_y_reverse(
      limits = c(10, 1),
      breaks = seq(1, 10, 1),
      expand = expansion(c(0.06, 0.08)))
  
  return(selected_team_plot)
}

## Creating the overall metrics data frame for Individual ----

ind_avg_time_byplace_df_women <- ind_results_women %>% 
  filter(year >= 2000) %>% 
  group_by(place) %>% 
  summarise(avg_top10 = round(mean(seconds), 1)) 

ind_margins_df_women <- ind_results_women %>% 
  filter(year >= 2000) %>% 
  filter(place %in% c(1, 2)) %>%
  group_by(year) %>% 
  summarize(winning_margin = abs(diff(seconds[order(place)])))

overall_ind_metrics_df_women <- tibble(metric = c("ind_margin_min", "ind_margin_max", "ind_margin_avg", "avg_win_time", "top5_threshold", "top10_threshold", "fastest_winning_time", "slowest_winning_time", "top2_threshold"),
                                 value = c(0.6, 26.8, round(mean(ind_margins_df_women$winning_margin), 1), "19:43.2", "20:02.3", "20:12.4", "18:55.2", "20:30.5", "19:51.1"))


## Creating the overall metrics data frame for Team ----

team_avg_points_byplace_df_women <- team_results_women %>% 
  group_by(place) %>% 
  summarise(avg_top10 = round(mean(score))) 


team_margins_df_women <- team_results_women %>% 
  filter(place %in% c(1, 2)) %>%
  group_by(year) %>% 
  summarize(winning_margin = abs(diff(score[order(place)])))

overall_team_metrics_df_women <- tibble(metric = c("team_margin_min", "team_margin_max", "team_margin_avg", "avg_win_points", "podium_points", "top10_points", "lowest_winning_score", "highest winning score", "top2_points"),
                                  value = c(1, 90, round(mean(team_margins_df_women$winning_margin)), team_avg_points_byplace_df_women[[1,2]], team_avg_points_byplace_df_women[[4,2]], team_avg_points_byplace_df_women[[10,2]], 36, 195, 130))

