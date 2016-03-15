#!/usr/bin/env - Rscript nameofthefile.R
library("DataCombine")
#libray("dplyr")
#libray("ggplot2")

# --- Read in and Summarize Data ---
mydata = read.csv("analysisdata.csv")
#head(mydata)
#summary(mydata)



# --- Describing & Visualizing the Data ---
#      -Creating and Developing Models-


# - Linear Regression Models -

#mylinreg <- lm(kills ~ deaths + assists + double_kills + triple_kills + quadra_kills + penta_kills + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_time_crowd_control_dealt + gold_earned + winner + minions_killed + tower_kills + inhibitor_kills + team_first_blood + team_first_tower + team_first_inhibitor + team_first_baron + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + days_in_game + days_since_first_game + pct_days_in_game + f0 + f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9 + top + jungle + mid + bot_carry + bot_support, data=allmydata)

#summary(mylinreg)

#data(allmydata)
#attach(allmydata)
#plot(wards_placed, assists)
#abline(mylinreg)
#detach(allmydata)

# -Probit Regression Models-

#myprobit <- glm(winner ~ kills + deaths + assists + double_kills + triple_kills + quadra_kills + penta_kills + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_time_crowd_control_dealt + gold_earned + minions_killed + tower_kills + inhibitor_kills + team_first_blood + team_first_tower + team_first_inhibitor + team_first_baron + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + days_in_game + days_since_first_game + pct_days_in_game + f000 + f001 + top + jungle + mid + bot_carry + bot_support, family=binomial(link="probit"), data=allmydata)
#summary(myprobit)
#confint(myprobit)

# --- Final Output & Clean-up ---

#detach(package:"DataCombine")





# --- Generating Lag Variables for each of the Players ---
#                - I did this in Excel -

# One month in epoch milliseconds = 2629743000
# Three months = 7889229000

#rm0lags <- slide(rm0, Var = "f00", slideBy = -3)
#head(rm0lags)
