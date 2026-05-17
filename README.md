# European Soccer Analytics

This project was completed as part of a group.
The R pipeline was built collaboratively. Power BI report,
logistic regression model, and GitHub documentation built independently.

The dataset covers 25,979 matches across 11 European leagues from 
2008 to 2016, including match results, player ratings, team tactics, 
and Bet365 betting odds.

## Tools
- R — data extraction, cleaning, feature engineering, analysis
- Power BI — interactive 5-page report
- Packages: DBI, RSQLite, dplyr, ggplot2, lubridate, scales

## Key Findings
1. Home win percentage declined from 47% to 44% between 2008-2016
2. Bet365 underprices strong favorites — teams with 65%+ implied
   probability win more often than the odds predict
3. Defensive pressure correlates +0.32 with points per match while
   build-up speed shows near-zero correlation
4. Logistic regression model predicts home win outcomes with
   64.9% accuracy across 22,591 matches

## Files
- `european-soccer-analytics.R` — full R pipeline
- `European Soccer Analytics.pbix` — Power BI report
- `goals_by_league.csv` — avg goals per match by league
- `home_advantage.csv` — home/draw/away win % by season
- `upset_rate.csv` — betting calibration data
- `team_style.csv` — tactical ratings and points per match
- `match_clean.csv` — full cleaned match dataset
- `summary.txt` — key numbers and findings
