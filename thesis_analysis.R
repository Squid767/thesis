#!/usr/bin/env - Rscript nameofthefile.R
library("DataCombine")
#library("dplyr")
#library("ggplot2")
#library("stargazer")

# --- Read in and Summarize Data ---
#mydata = read.csv("analysisdata.csv")
#head(mydata)
#summary(mydata)






#rm0 <- read.csv("rm0.csv")
#rm1 <- read.csv("rm1.csv")
#rm2 <- read.csv("rm2.csv")
#rm3 <- read.csv("rm3.csv")
#rm4 <- read.csv("rm4.csv")
#rm5 <- read.csv("rm5.csv")
#rm6 <- read.csv("rm6.csv")
#rm7 <- read.csv("rm7.csv")
#rm8 <- read.csv("rm8.csv")
rmall <- read.csv("rmall.csv")
#rmasolo <- subset(rmall, queue == "RANKED_SOLO_5x5")
#rmateam <- subset(rmall, queue == "RANKED_TEAM_5x5")

mylinreg0 <- lm(gold_earned ~ kda + one_month_sumgames + one_month_fcount + wards_placed + total_damage_taken + wards_killed + total_damage_dealt_to_champions + minions_killed + tower_kills, data=rmall)
mylinreg1 <- lm(gold_earned ~ kda + three_month_sumgames + three_month_fcount + wards_placed + total_damage_taken + wards_killed + total_damage_dealt_to_champions + minions_killed + tower_kills, data=rmall)

#+ team_inhibitor_kills + team_dragon_kills + team_baron_kills + gold_spent + team_first_tower + team_tower_kills + total_damage_dealt + sum_games_by_id + alltmfamcount + three_month_sumgames + three_month_fcount 

#mylinreg1 <- lm(kda ~ three_month_sumgames + three_month_fcount + sum_games_by_id + alltmfamcount + wards_placed + wards_killed + total_damage_dealt_to_champions + gold_spent + minions_killed, data=rm7)
#mylinreg2 <- lm(kda ~ three_month_sumgames + three_month_fcount + sum_games_by_id + alltmfamcount + wards_placed + wards_killed + total_damage_dealt_to_champions + gold_spent + minions_killed, data=rm8)

summary(mylinreg0)
summary(mylinreg1)
#summary(mylinreg2)







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
#myproreg0 <- glm(winner ~ three_month_sumgames + three_month_fcount + sum_games_by_id + alltmfamcount + wards_placed + total_damage_taken + total_damage_dealt + wards_killed + total_damage_dealt_to_champions + gold_earned + gold_spent + minions_killed + tower_kills + inhibitor_kills + team_first_blood + team_first_tower + team_first_inhibitor + team_first_baron + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rmall)
#myprobit <- glm(winner ~ kills + deaths + assists + double_kills + triple_kills + quadra_kills + penta_kills + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_time_crowd_control_dealt + gold_earned + minions_killed + tower_kills + inhibitor_kills + team_first_blood + team_first_tower + team_first_inhibitor + team_first_baron + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + days_in_game + days_since_first_game + pct_days_in_game + f000 + f001 + top + jungle + mid + bot_carry + bot_support, family=binomial(link="probit"), data=allmydata)

#summary(myproreg0)
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




#stargazer(mylinreg0, title="Gold Earned")
