#!/usr/bin/env - Rscript nameofthefile.R
library("DataCombine")
#library("dplyr")
#library("ggplot2")
library("stargazer")

# One month in epoch milliseconds = 2629743000
# Three months = 7889229000


# --- Read in and Summarize Data ---
# ----------------------------------

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

#head(rmall)stargazer(linear.1, linear.2, probit.model, title="Regression Results", align=TRUE, dep.var.labels=c("Overall Rating","High Rating"), covariate.labels=c("Handling of Complaints","No Special Privileges", "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"), omit.stat=c("LL","ser","f"), no.space=TRUE)
#summary(rmall)


#---Some Basic Histograms-

#hist(rmall$gold_earned)
#hist(rmall$kda)
#hist(rmall$one_month_sumgames)
#hist(rmall$one_month_fcount)
#hist(rmall$three_month_sumgames)
#hist(rmall$three_month_fcount)
#hist(rmall$minions_killed)


# --- Describing & Visualizing the Data ---
# ---- Creating and Developing Models-------

# Vars Removed from regression: top (base-case for roles), first_blood_assist (singularities --> NA), inhibitor_kills(low t-value, only significant at 0.1), team_first_baron & team_first_inhibitor & first_tower_kill & first_inhibitor_kill & first_inhibitor_assist(low t-value, insignificant)
base <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rmall)
base_with_roles <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support, data=rmall)
one_month_exp <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + one_month_sumgames, data=rmall)
one_month_fam <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + one_month_fcount, data=rmall)
one_month_all <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + one_month_sumgames + one_month_fcount, data=rmall)
three_month_exp <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + three_month_sumgames, data=rmall)
three_month_fam <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + three_month_fcount, data=rmall)
three_month_all <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + three_month_sumgames + three_month_fcount, data=rmall)
all_lags <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + one_month_sumgames + one_month_fcount + three_month_sumgames + three_month_fcount, data=rmall)


#summary(base)
#summary(base_with_roles)
#summary(one_month_all)
#summary(three_month_all)
#summary(all_lags)


# - Linear Regression Models -

#mylinreg <- lm(gold_earned ~ kda, data=rmall)
#data(rmall)
#attach(rmall)
#plot(gold_earned, kda)
#abline(mylinreg)
#detach(rmall)


# --- Final Output of Basic Regression Tables  ---
# ------------------------------------------

#stargazer(base, title="Gold Earned")

#stargazer(base, title="Base Regression", type = "text", out="reg_base.txt")
#stargazer(base_with_roles, title="Base Role Regression", type = "text", out="reg_base_roles.txt")
#stargazer(one_month_all, title="One Month Lagged", type = "text", out="reg_one_month_lag.txt")
#stargazer(three_month_all, title="Three Mongh Lagged", type = "text", out="reg_three_month_lag.txt")
#stargazer(all_lags, title="All Lags", type = "text", out="reg_all_lags.txt")




# _________________________________________________________________
#-------HISTOS AND REGS FOR EACH ROLE BREAKDOWN--------------
#___________________________________________________________________

#rma_top <- subset(rmall, top == 1)
#rma_jungle <- subset(rmall, jungle == 1)
#rma_mid <- subset(rmall, mid == 1)
#rma_bot_carry <- subset(rmall, bot_carry == 1)
#rma_bot_support <- subset(rmall, bot_support == 1)

#hist(rma_top$gold_earned, breaks=20)
#hist(rma_jungle$gold_earned, breaks=20)
#hist(rma_mid$gold_earned, breaks=20)
#hist(rma_bot_carry$gold_earned, breaks=20)
#hist(rma_bot_support$gold_earned, breaks=20)

#summary(rma_top$gold_earned)
#summary(rma_jungle$gold_earned)
#summary(rma_mid$gold_earned)
#summary(rma_bot_carry$gold_earned)
#summary(rma_bot_support$gold_earned)


# --- File Cleanup ---
# --------------------

#detach(package:"DataCombine")
#detach(package:"dplyr")
#detach(package:"ggplot2")
#detach(package:"stargazer")
