#!/usr/bin/env - Rscript nameofthefile.R
library("DataCombine")
#library("dplyr")
#library("ggplot2")
library("stargazer")

# One month in epoch milliseconds = 2629743000
# Three months = 7889229000


# --- Read in and Summarize Data ---
# ----------------------------------

rmfinal <- read.csv("rmfinal.csv")
rmasolo <- subset(rmfinal, queue == "RANKED_SOLO_5x5")
rmateam <- subset(rmfinal, queue == "RANKED_TEAM_5x5")

#summary(rmasolo)
#summary(rmateam)
#head(rmfinal)stargazer(linear.1, linear.2, probit.model, title="Regression Results", align=TRUE, dep.var.labels=c("Overall Rating","High Rating"), covariate.labels=c("Handling of Complaints","No Special Privileges", "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"), omit.stat=c("LL","ser","f"), no.space=TRUE)
#summary(rmfinal)


#---Some Basic Histograms-

#hist(rmfinal$gold_earned)
#hist(rmfinal$kda)
#hist(rmfinal$one_month_sumgames)
#hist(rmfinal$one_month_fcount)
#hist(rmfinal$three_month_sumgames)
#hist(rmfinal$three_month_fcount)
#hist(rmfinal$minions_killed)

rmfinal$one_rm_squared <- (rmfinal$one_month_rm_present_games*rmfinal$one_month_rm_present_games)
rmfinal$three_rm_squared <- (rmfinal$three_month_rm_present_games*rmfinal$three_month_rm_present_games)


# --- Describing & Visualizing the Data ---
# ---- Creating and Developing Models------- kda

# Vars Removed from regression: top (base-case for roles), first_blood_assist (singularities --> NA), inhibitor_kills(low t-value, only significant at 0.1), team_first_baron & team_first_inhibitor & first_tower_kill & first_inhibitor_kill & first_inhibitor_assist(low t-value, insignificant)
#base <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rmfinal)
#base_with_roles <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support, data=rmfinal)
#one_month_exp <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + one_month_sumgames, data=rmfinal)
#one_month_fam <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + one_month_fcount, data=rmfinal)
#one_month_all <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + one_month_sumgames + one_month_fcount, data=rmfinal)
#three_month_exp <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + three_month_sumgames, data=rmfinal)
#three_month_fam <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + three_month_fcount, data=rmfinal)
#three_month_all <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + jungle + mid + bot_carry + bot_support + three_month_sumgames + three_month_fcount, data=rmfinal)

#This is the reg with duo_bools_included
#all_lags_gold <- lm(gold_earned ~ one_month_sumgames + one_month_fcount + three_month_sumgames + three_month_fcount + one_exp_sqared + one_fam_sqared + three_exp_sqared + three_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + rm0_duo + rm1_duo + rm2_duo + rm3_duo + rm4_duo + rm5_duo + rm6_duo + rm7_duo + rm8_duo + is_preseason, data=rmfinal)

#base case same as above but with om_pct_one_third & tm_pct_one_third  ------ familiarity is the number of games that were played with one or more teammate in them
#all_lags_gold <- lm(gold_earned ~ one_month_sumgames + one_month_rm_present_games + three_month_sumgames + three_month_rm_present_games + one_exp_sqared + three_exp_sqared + one_rm_squared + three_rm_squared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + om_pct_two_third + om_pct_three_third + tm_pct_two_third + tm_pct_three_third + is_preseason, data=rmfinal)
#all_lags_kda <- lm(kda ~ one_month_sumgames + one_month_rm_present_games + three_month_sumgames + three_month_rm_present_games + one_exp_sqared + three_exp_sqared + one_rm_squared + three_rm_squared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + om_pct_two_third + om_pct_three_third + tm_pct_two_third + tm_pct_three_third + is_preseason, data=rmfinal)





#one-month fam
#base case is top lane, is_rm_0, solo_queue ------ familiarity is the number of teammates present across the number of games in the qualifying period
one_month_fam_gold <- lm(gold_earned ~ one_month_sumgames + one_month_fcount + one_exp_sqared + one_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + is_preseason, data=rmfinal)
one_month_fam_kda <- lm(kda ~ one_month_sumgames + one_month_fcount + one_exp_sqared + one_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + is_preseason, data=rmfinal)

#three-month fam
three_month_fam_gold <- lm(gold_earned ~ three_month_sumgames + three_month_fcount + three_exp_sqared + three_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + is_preseason, data=rmfinal)
three_month_fam_kda <- lm(kda ~ three_month_sumgames + three_month_fcount + three_exp_sqared + three_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + is_preseason, data=rmfinal)

#one-month rm
#base case is top lane, is_rm_0, solo_queue ------ familiarity is the number of games played with at least one roster member on the team in the qualifying period
one_month_rm_gold <- lm(gold_earned ~ one_month_sumgames + one_month_rm_present_games + one_exp_sqared + one_rm_squared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + om_pct_two_third + om_pct_three_third + is_preseason, data=rmfinal)
one_month_rm_kda <- lm(kda ~ one_month_sumgames + one_month_rm_present_games + one_exp_sqared + one_rm_squared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + om_pct_two_third + om_pct_three_third + is_preseason, data=rmfinal)

#three-month rm
three_month_rm_gold <- lm(gold_earned ~ three_month_sumgames + three_month_rm_present_games + three_exp_sqared + three_rm_squared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + tm_pct_two_third + tm_pct_three_third + is_preseason, data=rmfinal)
three_month_rm_kda <- lm(kda ~ three_month_sumgames + three_month_rm_present_games + three_exp_sqared + three_rm_squared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + tm_pct_two_third + tm_pct_three_third + is_preseason, data=rmfinal)


summary(one_month_fam_gold)
summary(one_month_fam_kda)
summary(three_month_fam_gold)
summary(three_month_fam_kda)
summary(one_month_rm_gold)
summary(one_month_rm_kda)
summary(three_month_rm_gold)
summary(three_month_rm_kda)





# - Segmented Slopes -

#rm15kdown <- subset(rmfinal, gold_earned <= 15000)
#rm15kup <- subset(rmfinal, gold_earned > 15000)
#rm3down <- subset(rmfinal, kda <= 3)
#rm3up <- subset(rmfinal, kda > 3)

#fifteen_down_all_lags_gold <- lm(gold_earned ~ one_month_sumgames + one_month_fcount + three_month_sumgames + three_month_fcount + one_exp_sqared + one_fam_sqared + three_exp_sqared + three_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + rm0_duo + rm1_duo + rm2_duo + rm3_duo + rm4_duo + rm5_duo + rm6_duo + rm7_duo + rm8_duo + is_preseason, data=rm15kdown)
#three_down_all_lags_kda <- lm(kda ~ one_month_sumgames + one_month_fcount + three_month_sumgames + three_month_fcount + one_exp_sqared + one_fam_sqared + three_exp_sqared + three_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + rm0_duo + rm1_duo + rm2_duo + rm3_duo + rm4_duo + rm5_duo + rm6_duo + rm7_duo + rm8_duo + is_preseason, data=rm3down)
#fifteen_up_all_lags_gold <- lm(gold_earned ~ one_month_sumgames + one_month_fcount + three_month_sumgames + three_month_fcount + one_exp_sqared + one_fam_sqared + three_exp_sqared + three_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + rm0_duo + rm1_duo + rm2_duo + rm3_duo + rm4_duo + rm5_duo + rm6_duo + rm7_duo + rm8_duo + is_preseason, data=rm15kup)
#three_up_all_lags_kda <- lm(kda ~ one_month_sumgames + one_month_fcount + three_month_sumgames + three_month_fcount + one_exp_sqared + one_fam_sqared + three_exp_sqared + three_fam_sqared + duo_queue + team_ranked + jungle + mid + bot_carry + bot_support + is_rm_1 + is_rm_2 + is_rm_3 + is_rm_4 + is_rm_5 + is_rm_6 + is_rm_7 + is_rm_8 + rm0_duo + rm1_duo + rm2_duo + rm3_duo + rm4_duo + rm5_duo + rm6_duo + rm7_duo + rm8_duo + is_preseason, data=rm3up)


#summary(fifteen_down_all_lags_gold)
#summary(three_down_all_lags_kda)
#summary(fifteen_up_all_lags_gold)
#summary(three_up_all_lags_kda)



# - Linear Regression Models -

#mylinreg <- lm(gold_earned ~ kda, data=rmfinal)
#data(rmfinal)
#attach(rmfinal)
#plot(gold_earned, kda)
#abline(mylinreg)
#detach(rmfinal)


# --- Final Output of Basic Regression Tables  ---
# ------------------------------------------

#stargazer(base, title="Gold Earned")

#stargazer(base, title="Base Regression", type = "text", out="reg_base.txt")
#stargazer(base_with_roles, title="Base Role Regression", type = "text", out="reg_base_roles.txt")
#stargazer(one_month_all, title="One Month Lagged", type = "text", out="reg_one_month_lag.txt")
#stargazer(three_month_all, title="Three Mongh Lagged", type = "text", out="reg_three_month_lag.txt")

#stargazer(all_lags_gold, title="All Lags - Gold Earned", type = "text", out="gold_earned_all_lags.txt")
#stargazer(all_lags_kda, title="All Lags - KDA", type = "text", out="kda_all_lags.txt")





# _________________________________________________________________
#-------HISTOS AND REGS FOR EACH ROLE BREAKDOWN--------------
#___________________________________________________________________

#rma_top <- subset(rmfinal, top == 1)
#rma_jungle <- subset(rmfinal, jungle == 1)
#rma_mid <- subset(rmfinal, mid == 1)
#rma_bot_carry <- subset(rmfinal, bot_carry == 1)
#rma_bot_support <- subset(rmfinal, bot_support == 1)

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

#base_top <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rma_top
#base_jungle <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rma_jungle
#base_mid <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rma_mid)
#base_bot_carry <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rma_bot_carry)
#base_bot_support <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills, data=rma_bot_support)

#one_month_exp <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + one_month_sumgames, data=rma_top)
#one_month_fam <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + one_month_fcount, data=rma_top)
#one_month_all <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + one_month_sumgames + one_month_fcount, data=rma_top)
#three_month_exp <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + three_month_sumgames, data=rma_top)
#three_month_fam <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + three_month_fcount, data=rma_top)
#three_month_all <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + three_month_sumgames + three_month_fcount, data=rma_top)
#ll_lags <- lm(gold_earned ~ kda + sight_wards_bought_in_game + vision_wards_bought_in_game + wards_placed + wards_killed + total_damage_taken + total_damage_dealt + total_damage_dealt_to_champions + minions_killed + tower_kills + first_blood_kill + first_tower_assist + team_first_blood + team_first_tower + team_first_dragon + team_tower_kills + team_inhibitor_kills + team_dragon_kills + team_baron_kills + one_month_sumgames + one_month_fcount + three_month_sumgames + three_month_fcount, data=rma_top)


# --- File Cleanup ---
# --------------------

#detach(package:"DataCombine")
#detach(package:"dplyr")
#detach(package:"ggplot2")
#detach(package:"stargazer")


#summary(rmfinal$one_month_sumgames)
#hist(rmfinal$one_month_sumgames, breaks=20)
