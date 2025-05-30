# NBA Play Styles and Playoff Success: The Impact of Three-Point Shooting and Team Strategies

**Authors:** Hartej Singh, Ryan Bell, Daniel Zhou, Elliot Paschal, Amanda Huang  
**Course:** BUSN 41201 Big Data, University of Chicago Booth School of Business  
**Date:** March 15, 2025

## Project Overview

This project investigates the evolution of NBA play styles from 1996 to 2019 and their correlation with playoff success.  
Using big data techniques, we analyze detailed game, box score, and play-by-play datasets to understand how strategic trends—such as the rise of three-point shooting—impact a team's chances of making the playoffs.

Our analysis combines exploratory data analysis, text feature extraction, clustering, marginal regressions, and predictive modeling (PCA and LASSO logistic regression) to identify the play styles and metrics that most influence postseason success.

## Main Findings

- **Three-point shooting** has become a dominant offensive strategy in the NBA over the past two decades.
- **Successful teams** combine high three-point attempt rates with strong assist-to-turnover ratios and high shooting efficiency (eFG% and TS%).
- **Clustering analysis** revealed a shift toward offensive efficiency and perimeter-oriented playstyles.
- **Regression and LASSO models** showed that assists, three-point makes, and shooting efficiency are key predictors of playoff qualification.
- Modern NBA championship teams tend to be offensively specialized rather than defense-heavy, challenging traditional views.

## Data Sources

- [NBA Play-by-Play, Box Scores, and Games Dataset (Kaggle)](https://www.kaggle.com/datasets/patrickhallila1994/nba-data-from-basketball-reference)
- [Apache Arrow Documentation](https://arrow.apache.org/docs/r/)
- [BigMemory Package Documentation](http://www.stat.yale.edu/~mjk56/Research/Prospectus/bigmemoRy-vignette.pdf)

## Individual Contributions

`Big Data 2 (1).R` – Code contributed by Elliot Paschal.  
This script corresponds to Section 2.2 of the paper, "Refined Clustering Based on Player Statistics."  
It processes NBA box score data from 1996 to 2019, normalizes team statistics season-by-season, and performs hierarchical clustering (Ward’s method) to identify distinct team play styles within each season.  
The script assigns play style labels to clusters based on statistical profiles, allowing for analysis of how NBA play styles evolved over time.
