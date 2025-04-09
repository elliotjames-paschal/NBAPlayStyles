# Data Preprocessing ----- 

library(dplyr)

box_scores <- read.csv("/Users/elliotpaschal/Downloads/boxscore.csv")

numeric_cols <- c("FG", "FGA", "X3P", "X3PA", "FT", "FTA", 
                  "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "X...")

box_scores[numeric_cols] <- lapply(box_scores[numeric_cols], function(x) as.numeric(as.character(x)))

# Aggregate to team-level per game
team_stats <- box_scores %>%
  group_by(game_id, teamName) %>%
  summarise(
    # Scoring Profile
    PPG = sum(PTS, na.rm = TRUE),    # Total team points
    TFGA = sum(FGA, na.rm = TRUE),   # Total team field goal attempts
    TFGM = sum(FG, na.rm = TRUE),    # Total team field goals made
    T3PA = sum(X3PA, na.rm = TRUE),  # Total 3-point attempts
    T3PM = sum(X3P, na.rm = TRUE),   # Total 3-pointers made
    TFTA = sum(FTA, na.rm = TRUE),   # Total free throw attempts
    TFTM = sum(FT, na.rm = TRUE),    # Total free throws made
    
    # Playmaking & Turnovers
    APG = sum(AST, na.rm = TRUE),    # Assists per game
    TTOV = sum(TOV, na.rm = TRUE),   # Turnovers per game
    
    # Rebounding & Defense
    TRPG = sum(TRB, na.rm = TRUE),   # Total rebounds per game
    TORB = sum(ORB, na.rm = TRUE),   # Offensive rebounds
    TDRB = sum(DRB, na.rm = TRUE),   # Defensive rebounds
    TSPG = sum(STL, na.rm = TRUE),   # Steals per game
    TBPG = sum(BLK, na.rm = TRUE),   # Blocks per game
    TPF = sum(PF, na.rm = TRUE),     # Total personal fouls
    
    # Plus/Minus
    PlusMinus = sum(`X...`, na.rm = TRUE)  # Team total +/- for the game (point differential)
  ) %>%
  ungroup() %>%
  mutate(
    # Derived Shooting Metrics 
    T3PAr = ifelse(TFGA > 0, T3PA / TFGA, NA),                                    # Three-Point Attempt Rate
    TFTAr = ifelse(TFGA > 0, TFTA / TFGA, NA),                                    # Free Throw Attempt Rate
    eFG = ifelse(TFGA > 0, (TFGM + 0.5 * T3PM) / TFGA, NA),                       # Effective FG%
    TS = ifelse((TFGA + 0.44 * TFTA) > 0, PPG / (2 * (TFGA + 0.44 * TFTA)), NA),  # True Shooting Percentage
    
    # Assist Metrics 
    AST_PCT = ifelse((TFGM + APG) > 0, APG / (TFGM + APG), NA),  # Assist Percentage
    AST_TOV = ifelse(TTOV > 0, APG / TTOV, NA),                  # Assist-to-Turnover Ratio
    
    # Rebounding Percentages 
    ORB_PCT = ifelse((TORB + TDRB) > 0, TORB / (TORB + TDRB), NA),  # Offensive Rebound Percentage
    DRB_PCT = ifelse((TORB + TDRB) > 0, TDRB / (TORB + TDRB), NA)   # Defensive Rebound Percentage
  )

season_data <- read.csv("/Users/elliotpaschal/Downloads/games.csv") %>%
  select(game_id, seasonStartYear)

team_stats <- team_stats %>%
  left_join(season_data, by="game_id")

glimpse(team_stats)

# Clustering ----- 

library(dplyr)
library(cluster)  
library(ggplot2)
library(ggdendro)

# Keep team and season info but exclude them from scaling
team_info <- team_stats %>%
  select(teamName, seasonStartYear, game_id, -PlusMinus)  # Keep identifiers

selected_features <- team_stats %>%
  select(PPG, TFGA, TFGM, T3PA, T3PM, TFTA, TFTM, APG, TTOV, TRPG, TORB, TDRB,
         TSPG, TBPG, TPF, T3PAr, TFTAr, eFG, TS, AST_PCT, AST_TOV, ORB_PCT, DRB_PCT)

season_clusters <- list()
clustered_seasons <- data.frame()
seasons <- unique(team_stats$seasonStartYear)
#season_team_info <- data.frame()

# Cluster by season
for (season in seasons) {
  
  cat("Processing season:", season, "\n")
  
  # Keep team names separate so we can add back after cluster
  season_team_info <- team_stats %>%
    filter(seasonStartYear == season) %>%
    select(teamName, seasonStartYear)
  
  # Prepare numeric data for clustering
  season_data <- team_stats %>%
    filter(seasonStartYear == season) %>%
    select(all_of(names(selected_features)))
  
  # Normalize the data (Z-score transformation)
  season_scaled <- scale(season_data)
  dist_matrix <- dist(season_scaled, method = "euclidean")
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  # Store the model
  season_clusters[[as.character(season)]] <- hc
  
  K <- 3 # Seems most reasonable
  season_data$cluster <- cutree(hc, K)  # Assign cluster labels
  
  # Store in new df
  #season_data <- season_data %>%
    #mutate(seasonStartYear = season)  # Ensure seasonStartYear is retained
  
  season_data <- bind_cols(season_team_info, season_data)
  
  # Store in new df
  clustered_seasons <- bind_rows(clustered_seasons, season_data)
}

View(clustered_seasons)

# Aggregate to get cluster averages per season
cluster_averages <- clustered_seasons %>%
  group_by(seasonStartYear, cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")

View(cluster_averages)

# Data Analysis ----- 

library(ggdendro)
library(dendextend) 
library(dplyr)
# install.packages("openintro") # Contains dataset for NBA Champs
library(openintro)
data("nba_finals") # NBA champs


## Dendrogram ----- 

season_to_plot <- "2019"  # Choose a season
hc <- season_clusters[[as.character(season_to_plot)]]  # Get hierarchical clustering object

# Cut tree at K clusters
K <- 3  # Number of clusters
clusters <- cutree(hc, K)  # Assign cluster labels

# Convert to dendrogram and cut at K clusters
dend <- as.dendrogram(hc)
dend <- cut(dend, h = hc$height[length(hc$height) - (K - 1)])$upper  # Keep only the top part

# Color branches by cluster
dend <- dend %>%
  set("branches_k_color", k = K) %>%
  set("branches_lwd", 2)  # Make branches thicker for visibility

# Plot truncated dendrogram
plot(dend, main = paste("Team Playing Style Clustering -", season_to_plot), horiz = TRUE)

## Average Statistics Table ----- 

# Select key features to analyze playing style
key_stats <- c("PPG", "eFG", "AST_TOV", "TRPG", "TSPG", "TBPG")

# Standardize the key stats for comparison
cluster_averages_scaled <- cluster_averages %>%
  group_by(seasonStartYear) %>%
  mutate(across(all_of(key_stats), ~ scale(.))) %>%  # Standardize within each year
  ungroup()

# Compute percentiles for each cluster within its season
cluster_averages_percentiles <- cluster_averages %>%
  group_by(seasonStartYear) %>%
  mutate(
    PPG_pct = percent_rank(PPG),         # High PPG = High-Scoring
    eFG_pct = percent_rank(eFG),         # Efficiency in shooting
    T3PAr_pct = percent_rank(T3PAr),     # 3-Point Attempt Rate
    TFTAr_pct = percent_rank(TFTAr),     # Free Throw Attempt Rate
    defense_pct = percent_rank(TSPG + TBPG),  # Steals & Blocks = Defensive
    AST_TOV_pct = percent_rank(AST_TOV), # Passing Efficiency
    TRPG_pct = percent_rank(TRPG),       # Rebounding Emphasis
    ORB_PCT_pct = percent_rank(ORB_PCT), # Offensive Rebounding
    DRB_PCT_pct = percent_rank(DRB_PCT)  # Defensive Rebounding
  ) %>%
  ungroup()

cluster_averages_percentiles <- cluster_averages_percentiles %>%
  mutate(
    playstyles_list = apply(
      cbind(
        ifelse(PPG_pct >= 0.75 & eFG_pct >= 0.75, "Elite Scoring & Efficient", NA),
        ifelse(PPG_pct >= 0.75, "High-Scoring", NA),
        ifelse(T3PAr_pct >= 0.75 & eFG_pct >= 0.5, "Three-Point Heavy & Efficient", NA),
        ifelse(T3PAr_pct >= 0.75, "Three-Point Heavy", NA),
        ifelse(TFTAr_pct >= 0.75, "Free-Throw Heavy & Inside Attack", NA),
        ifelse(defense_pct >= 0.75 & TRPG_pct >= 0.5, "Defensive & Strong Rebounding", NA),
        ifelse(defense_pct >= 0.75, "Defensive-Oriented", NA),
        ifelse(PPG_pct >= 0.5 & T3PAr_pct >= 0.5 & AST_TOV_pct >= 0.5, "Fast-Paced & Playmaking", NA),
        ifelse(PPG_pct <= 0.5 & TRPG_pct >= 0.75, "Slow-Paced & Defensive", NA),
        ifelse(TRPG_pct >= 0.75 & ORB_PCT_pct >= 0.75, "Elite Rebounding Team", NA),
        ifelse(TRPG_pct >= 0.75, "Rebounding-Focused", NA)
      ), 1, function(x) paste(na.omit(x), collapse = ", ")
    ),
    # Assign "Balanced Playstyle" if no other label was assigned
    playstyle = ifelse(playstyles_list == "", "Balanced Playstyle", playstyles_list)
  )

# View the updated labels
View(cluster_averages_percentiles)

# View summarized cluster styles per season
cluster_summary <- cluster_averages_percentiles %>%
  select(seasonStartYear, cluster, playstyle)

View(cluster_summary)

## Style of Championship Team ----- 

# Filter for seasons between 1996 and 2019
champions_filtered <- nba_finals %>%
  filter(year >= 1996 & year <= 2019) %>%
  rename(seasonStartYear = year)  # Rename 'year' to match clustered_seasons

# Merge winners into clustered_seasons based on seasonStartYear
clustered_seasons <- clustered_seasons %>%
  left_join(champions_filtered %>% select(seasonStartYear, winner), by = "seasonStartYear")

# Step 1: Count how often the champion was in each cluster for that season
champion_cluster_yearly <- clustered_seasons %>%
  filter(teamName == winner) %>%  # Keep only championship winners
  group_by(seasonStartYear, winner, cluster) %>%
  summarise(times_in_cluster = n(), .groups = "drop") %>%
  
  # Step 2: Compute total times champion appears in any cluster that year
  group_by(seasonStartYear, winner) %>%
  mutate(
    total_appearances = sum(times_in_cluster),  # Total occurrences of the winner
    percent_time_in_cluster = (times_in_cluster / total_appearances) * 100  # Compute %
  ) %>%
  ungroup() %>%
  arrange(seasonStartYear, winner, desc(percent_time_in_cluster))  # Sort by year and dominance

View(champion_cluster_yearly)

# Data Visualization for Docs ----- 

library(gt)
library(dplyr)

## Table for original variables -----
variables_table <- tibble::tibble(
  Category = c(
    "Scoring Profile", "Scoring Profile", "Scoring Profile", "Scoring Profile", "Scoring Profile", "Scoring Profile",
    "Scoring Profile", "Scoring Profile", "Playmaking & Turnovers", "Playmaking & Turnovers",
    "Rebounding & Defense", "Rebounding & Defense", "Rebounding & Defense", "Rebounding & Defense",
    "Rebounding & Defense", "Rebounding & Defense", "Plus/Minus", "Derived Shooting Metrics", 
    "Derived Shooting Metrics", "Derived Shooting Metrics", "Derived Shooting Metrics", 
    "Assist Metrics", "Assist Metrics", "Rebounding Percentages", "Rebounding Percentages"
  ),
  Variable = c(
    "PPG", "TFGA", "TFGM", "T3PA", "T3PM", "TFTA", "TFTM", "PlusMinus",
    "APG", "TTOV", "TRPG", "TORB", "TDRB", "TSPG", "TBPG", "TPF", 
    "PlusMinus", "T3PAr", "TFTAr", "eFG", "TS", "AST_PCT", "AST_TOV", 
    "ORB_PCT", "DRB_PCT"
  ),
  Description = c(
    "Points Per Game", "Total Field Goal Attempts", "Total Field Goals Made",
    "Three-Point Attempts", "Three-Pointers Made", "Free Throw Attempts", "Free Throws Made",
    "Total team +/- for the game",
    "Assists Per Game", "Turnovers Per Game",
    "Total Rebounds Per Game", "Offensive Rebounds", "Defensive Rebounds", "Steals Per Game", 
    "Blocks Per Game", "Total Personal Fouls",
    "Team total +/- for the game (point differential)",
    "Three-Point Attempt Rate (3PA / FGA)", "Free Throw Attempt Rate (FTA / FGA)",
    "Effective Field Goal Percentage", "True Shooting Percentage",
    "Assist Percentage (APG / (TFGM + APG))", "Assist-to-Turnover Ratio",
    "Offensive Rebounding Percentage", "Defensive Rebounding Percentage"
  )
)

# Create a gt table for visualization
variables_table %>%
  gt() %>%
  tab_header(
    title = "Variables Used in Box Score Data",
    subtitle = "Grouped by Category"
  ) %>%
  cols_label(
    Category = "Category",
    Variable = "Variable",
    Description = "Description"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  ) %>%
  gtsave("boxscore_variables_table.png")  # Saves as an image for easy sharing
## Table for playing styles -----

# Define a table to explain how playstyles are determined
playstyle_criteria <- tibble(
  Playstyle = c(
    "Elite Scoring & Efficient",
    "High-Scoring",
    "Three-Point Heavy & Efficient",
    "Three-Point Heavy",
    "Free-Throw Heavy & Inside Attack",
    "Defensive & Strong Rebounding",
    "Defensive-Oriented",
    "Fast-Paced & Playmaking",
    "Slow-Paced & Defensive",
    "Elite Rebounding Team",
    "Rebounding-Focused",
    "Balanced Playstyle"
  ),
  Criteria = c(
    "PPG percentile ≥ 75% & eFG percentile ≥ 75%",
    "PPG percentile ≥ 75%",
    "3PAr percentile ≥ 75% & eFG percentile ≥ 50%",
    "3PAr percentile ≥ 75%",
    "FTAr percentile ≥ 75%",
    "Defense percentile ≥ 75% & TRPG percentile ≥ 50%",
    "Defense percentile ≥ 75%",
    "PPG percentile ≥ 50% & 3PAr percentile ≥ 50% & AST/TOV percentile ≥ 50%",
    "PPG percentile ≤ 50% & TRPG percentile ≥ 75%",
    "TRPG percentile ≥ 75% & ORB_PCT percentile ≥ 75%",
    "TRPG percentile ≥ 75%",
    "No other label assigned"
  )
)

# Create a gt table
table_gt <- playstyle_criteria %>%
  gt() %>%
  tab_header(
    title = "Determination of Playstyles",
    subtitle = "Criteria for Categorizing Playstyles Based on Statistical Percentiles"
  ) %>%
  cols_label(
    Playstyle = "Playstyle Category",
    Criteria = "Statistical Criteria"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14),
    row.striping.include_table_body = TRUE
  ) %>%
  opt_table_outline() %>%
  opt_row_striping() 

# Save the table as a PNG
gtsave(table_gt, "playstyle_criteria.png")



## Table for full year by year cluster style -----

# Create a gt table from cluster_summary
table_gt <- cluster_summary %>%
  gt() %>%
  tab_header(
    title = "Cluster Playstyles by Season",
    subtitle = "Summary of Playstyles Assigned to Each Cluster Per Season"
  ) %>%
  cols_label(
    seasonStartYear = "Season",
    cluster = "Cluster",
    playstyle = "Playstyle"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14),
    row.striping.include_table_body = TRUE
  ) %>%
  opt_table_outline() %>%
  opt_row_striping()

# View the table in RStudio
table_gt

# Save the table as a PNG file
gtsave(table_gt, "cluster_summary.png")

## Table for style of Championship Team (% of time in cluster) -----
library(gt)
library(dplyr)

# Create a gt table from champion_cluster_yearly
table_gt <- champion_cluster_yearly %>%
  gt() %>%
  tab_header(
    title = "NBA Champions Cluster Breakdown",
    subtitle = "Cluster Assignments of Championship Teams (1996-2019)"
  ) %>%
  cols_label(
    seasonStartYear = "Season",
    winner = "Champion Team",
    cluster = "Cluster",
    times_in_cluster = "Appearances in Cluster",
    total_appearances = "Total Appearances",
    percent_time_in_cluster = "Percentage Time in Cluster"
  ) %>%
  fmt_number(
    columns = percent_time_in_cluster,
    decimals = 1,  # Format percentage with 1 decimal place
    suffixing = TRUE
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.subtitle.font.size = px(14),
    row.striping.include_table_body = TRUE
  ) %>%
  opt_table_outline() %>%
  opt_row_striping()

# View the table in RStudio
table_gt

# Save the table as a PNG file
gtsave(table_gt, "champion_cluster_yearly.png")
