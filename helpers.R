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