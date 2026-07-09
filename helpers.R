# variable labels for use in tables and plots
var_labels <- c(
  player             = "Player",
  tour               = "Tour",
  surface            = "Court Surface",
  round              = "Tournament Round",
  handed             = "Handedness",
  year               = "Year",
  serve_pts          = "Serve Points",
  aces               = "Aces",
  dfs                = "Double Faults",
  first_in           = "First Serves In",
  first_won          = "First Serve Points Won",
  second_in          = "Second Serves In",
  second_won         = "Second Serve Points Won",
  bk_pts             = "Break Points Faced",
  bp_saved           = "Break Points Saved",
  return_pts         = "Return Points",
  return_pts_won     = "Return Points Won",
  winners            = "Winners",
  winners_fh         = "Forehand Winners",
  winners_bh         = "Backhand Winners",
  unforced           = "Unforced Errors",
  unforced_fh        = "Forehand Unforced Errors",
  unforced_bh        = "Backhand Unforced Errors",
  total_pts          = "Total Points",
  first_in_pct       = "First Serve In %",
  ace_pct            = "Ace %",
  first_won_pct      = "First Serve Points Won %",
  second_won_pct     = "Second Serve Points Won %",
  total_won_pct      = "Total Points Won %",
  df_pct             = "Double Fault %",
  bp_saved_pct       = "Break Points Saved %",
  serve_pts_won_pct  = "Serve Points Won %",
  return_pts_won_pct = "Return Points Won %",
  winner_pct         = "Winner %",
  unforced_pct       = "Unforced Error %",
  winner_fh_pct      = "Forehand Winner %",
  winner_bh_pct      = "Backhand Winner %",
  unforced_fh_pct    = "Forehand Unforced Error %",
  unforced_bh_pct    = "Backhand Unforced Error %",
  fh_eff_pct         = "Forehand Effectiveness %",
  bh_eff_pct         = "Backhand Effectiveness %",
  aggression_pct     = "Aggression %"
)

# truncated labels just for the radar chart 
radar_labels <- c(
  total_won_pct      = "Total Won %",
  serve_pts_won_pct  = "Serve Won %",
  return_pts_won_pct = "Return Won %",
  ace_pct            = "Ace %",
  aggression_pct     = "Aggression %",
  fh_eff_pct         = "FH Effect. %",
  bh_eff_pct         = "BH Effect. %"
)

# defining min/max to scale each variable of the radar chart
radar_bounds <- data.frame(
  total_won_pct      = c(0.60, 0.40),
  ace_pct            = c(0.25, 0.00),
  serve_pts_won_pct  = c(0.75, 0.45),
  return_pts_won_pct = c(0.55, 0.20),
  aggression_pct     = c(0.50, 0.20),
  fh_eff_pct         = c(0.05, -0.08),
  bh_eff_pct         = c(0.03, -0.10)
)
rownames(radar_bounds) <- c("max", "min")

# function to build the df that feeds radarchart()
build_radar_data <- function(data, player_a, player_b) {
  player_rows <- data |> # getting the player data
    filter(player %in% c(player_a, player_b)) |> # select the two players
    group_by(player) |>
    summarize(across(c(total_won_pct, ace_pct, serve_pts_won_pct, return_pts_won_pct,
                       aggression_pct, fh_eff_pct, bh_eff_pct),
                     ~ mean(.x, na.rm = TRUE))) |> # calculating the means of each variable for each player
    as.data.frame() # radarchart() needs data.frame format
  
  rownames(player_rows) <- player_rows$player # add the players as row names, which is how radarchart() will get these labels
  player_rows$player <- NULL # column of player names no longer needed
  
  
  out <- rbind(radar_bounds, player_rows) # combine the variable min/max with the player data into one data frame
  colnames(out) <- radar_labels[colnames(out)] # uses the proper variable labels from radar_labels in helpers.R
  out # return out
}

# function to build the radar chart companion table 
build_radar_table <- function(data, player_a, player_b) {
  data |>
    filter(player %in% c(player_a, player_b)) |> # filters for the two players
    select(player, n, total_won_pct, ace_pct, serve_pts_won_pct, return_pts_won_pct,
           aggression_pct, fh_eff_pct, bh_eff_pct) |> # selects all the radar chart variables, and also n
    rename_with(~ var_labels[.x], .cols = -c(player, n)) |> # use var_labels for renaming all variables except n and player
    rename(Player = player, `Charted Matches` = n) # manually renaming n and player
}

# list of numeric variables to be used as options for plots/tables
numeric_vars <- c(
  "Year"                        = "year",
  "Serve Points"                = "serve_pts",
  "Aces"                        = "aces",
  "Double Faults"               = "dfs",
  "First Serves In"             = "first_in",
  "First Serve Points Won"      = "first_won",
  "Second Serves In"            = "second_in",
  "Second Serve Points Won"     = "second_won",
  "Break Points Faced"          = "bk_pts",
  "Break Points Saved"          = "bp_saved",
  "Return Points"               = "return_pts",
  "Return Points Won"           = "return_pts_won",
  "Winners"                     = "winners",
  "Forehand Winners"            = "winners_fh",
  "Backhand Winners"            = "winners_bh",
  "Unforced Errors"             = "unforced",
  "Forehand Unforced Errors"    = "unforced_fh",
  "Backhand Unforced Errors"    = "unforced_bh",
  "Total Points"                = "total_pts",
  "First Serve In %"            = "first_in_pct",
  "Ace %"                       = "ace_pct",
  "First Serve Points Won %"    = "first_won_pct",
  "Second Serve Points Won %"   = "second_won_pct",
  "Total Points Won %"          = "total_won_pct",
  "Double Fault %"              = "df_pct",
  "Break Points Saved %"        = "bp_saved_pct",
  "Serve Points Won %"          = "serve_pts_won_pct",
  "Return Points Won %"         = "return_pts_won_pct",
  "Winner %"                    = "winner_pct",
  "Unforced Error %"            = "unforced_pct",
  "Forehand Winner %"           = "winner_fh_pct",
  "Backhand Winner %"           = "winner_bh_pct",
  "Forehand Unforced Error %"   = "unforced_fh_pct",
  "Backhand Unforced Error %"   = "unforced_bh_pct",
  "Aggression %"                = "aggression_pct",
  "Forehand Effectiveness %"    = "fh_eff_pct",
  "Backhand Effectiveness %"    = "bh_eff_pct"
)

# list of categorical variables to be used as options for plots/tables
categorical_vars <- c(
  "Tour"          = "tour",
  "Court Surface" = "surface",
  "Handedness"    = "handed",
  "Round"         = "round",
  "Round Groups"  = "round_groups"
)

# condense 11 levels of round down to 4
round_groups <- c(
  "Q1"   = "Qualifying",
  "Q2"   = "Qualifying",
  "Q3"   = "Qualifying",
  "R128" = "Early Rounds",
  "R64"  = "Early Rounds",
  "R32"  = "Early Rounds",
  "R16"  = "Late Rounds",
  "QF"   = "Late Rounds",
  "RR"   = "Late Rounds", # RR is only applicable to invitation-only tour finals tournaments and therefore is considered late rounds
  "SF"   = "Semifinal/Final",
  "F"    = "Semifinal/Final"
)

# round group levels
round_group_levels <- c("Qualifying", "Early Rounds", "Late Rounds", 
                        "Semifinal/Final")

# Numeric variables offered as sidebar sliders for row subsetting
subset_vars <- c(
  "Year"         = "year",
  "Total Points" = "total_pts",
  "Aces" = "aces",
  "Winners" = "winners",
  "Unforced" = "unforced"
)

# define slider ranges for the numeric subset variables
# note: to keep these sliders reasonable, some of these ranges will exclude the longest match in tennis history https://en.wikipedia.org/wiki/Isner%E2%80%93Mahut_match_at_the_2010_Wimbledon_Championships
slider_ranges <- list(
  year       = list(min = 1960, max = 2026, step = 1),
  total_pts  = list(min = 10,    max = 570,  step = 10),
  aces  = list(min = 0,    max = 60,  step = 5),
  winners  = list(min = 0,    max = 130,  step = 5),
  unforced  = list(min = 0,    max = 90,  step = 5)
)

# Load .rds data
CombinedData <- readRDS("CombinedData.rds")

# Make list of eligible players for comparison 
eligible_players <- CombinedData |>
  count(player) |>
  filter(n >= 1) |> # currently allowing all players regardless of matches, but may reconsider
  pull(player) |>
  sort()

