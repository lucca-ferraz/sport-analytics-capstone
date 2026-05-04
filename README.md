# sport-analytics-capstone
Project repository for the SMGT490: Sport Analytics Capstone class. 

In the modern era of Name, Image, and Likeness (NIL) and the college football transfer
portal, roster construction in college football has shifted dramatically toward player
mobility, increasing the importance of accurately evaluating transfer talent. This study
develops a data-driven framework to project individual player performance and quantify
transfer value across all positions. Using player-season data from 2021–2026, we model
future performance via Pro Football Focus Wins Above Average (PFF WAA) as a unified
measure of player impact.

We integrate detailed player tracking and grading data from Pro Football Focus with publicly
available statistics and recruiting information from CFBfastR to construct a comprehensive
feature set encompassing production, athleticism, experience, and recruiting pedigree.
Position-specific models are trained to predict next-season WAA, leveraging both Random
Forest and XGBoost algorithms, with final model selection determined by out-of-sample
performance via leave-one-season-out cross-validation.

Our results demonstrate that model performance varies by position, with running back,
linebacker, and defensive line projections performing better using Random Forest, while
other positions performed better using XGBoost.. While projections remain challenging due
to role heterogeneity and limited sample sizes, the proposed framework offers competitive
predictive accuracy relative to existing benchmarks and provides interpretable insights into
the drivers of player success.

This work establishes a scalable approach for evaluating transfer portal decisions, enabling
programs to optimize roster construction strategies and allocate NIL resources more
efficiently across positions.

The shiny app with individual player results for the 2026 season can be found here: https://xhbhit-lucca0ferraz.shinyapps.io/finalapp/
