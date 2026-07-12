# Tennis Data Exploration

ST 558, Summer 2026, Project 2, Ryan Friedman

## Purpose

This application is used for exploring professional tennis match data.

## About the Data

The dataset for this analysis comes from the Tennis Match Charting Project (MCP), available at the [Match Charting Project](https://github.com/JeffSackmann/tennis_MatchChartingProject) GitHub repository. The MCP aims to build a detailed public record of professional tennis matches (ATP and WTA Tours). MCP data covers over 11,000 men's and women's singles matches, including both match summary statistics and shot-by-shot detail. The project is open to the public both for accessing the data and for contributing new charted matches. This analysis uses only the match summary data, retrieved on 7/6/2026.

Note: this is not a comprehensive dataset of all professional tennis matches. Because charting is done by individual volunteer contributors, the data tends to over-represent the biggest tournaments and the most popular players.

## Description of Variables

Most variable names are self-explanatory to anyone familiar with the basics of tennis, so they are not all listed here. A few points are worth clarifying:

- **Percentage variables** were created to normalize metrics across match length. Fifteen aces in a short match means something different than fifteen aces in a long one, so raw counts can mislead. For example, Ace % is the share of a player's service points that were aces, and Forehand Winner % is the share of total points that ended with the player hitting a forehand winner.
- **Aggression %** is a custom metric created for this analysis (not a standard tennis statistic). It is the sum of Winner % and Unforced Error %. A higher value indicates a more aggressive style.
- **Forehand Effectiveness %** and **Backhand Effectiveness %** are also custom metrics, each representing that wing's Winner % minus its Unforced Error %. A positive value means the shot produces more winners than errors.

## How to Use

1.  Use the sidebar to subset the data. Select the desired categorical levels and numeric ranges, then click the **Subset the Data!** button to pull a subset from the 23,230 match summary results. All tabs reflect the current subset and update when this button is clicked.
2.  The **Data Download** tab displays a table of the subset. Click column headers to sort ascending or descending, and use the search box to filter further. Click the **Download Data** button at the bottom to save the table as a .csv file.
3.  The **Data Exploration** tab investigates the subset in more depth, across three sub-tabs:
    - **Categorical Summaries** — select one or two categorical variables to display bar charts and a counts table.
    - **Numeric Summaries** — select one or two numeric variables, plus an optional categorical variable to group by. With one variable, a box plot and histogram are shown; with two, a scatter plot. A summary table of center and spread appears below the plots.
    - **Player Comparison** — select one or two players to display a radar chart and summary table of their average performance metrics across the subset.

## Other Notes

- The [Isner–Mahut match at the 2010 Wimbledon Championships](https://en.wikipedia.org/wiki/Isner%E2%80%93Mahut_match_at_the_2010_Wimbledon_Championships) is excluded from the data. At 980 total points (versus 569 for the next-longest charted match), it is such an extreme outlier that including it would stretch the numeric subsetting sliders to the point of being unusable.
- The Player Comparison tab averages each metric across all of a player's matches within the current subset. Averages built on only a few matches can be misleading. The number of charted matches per player is shown in the table below the radar chart, and a warning appears when a player has fewer than five.
- Because the Match Charting Project is crowdsourced, the data over-represents the biggest tournaments and the most popular players. All summaries describe the charted sample, not the full population of professional tennis matches.
