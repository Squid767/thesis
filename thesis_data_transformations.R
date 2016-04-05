#!/usr/bin/env - Rscript nameofthefile.R
#library("DataCombine")
#library("dplyr")
#library("ggplot2")
#library("stargazer")

# One month in epoch milliseconds = 2629743000
# Three months = 7889229000

rmall <- read.csv("rmall.csv")
rmasolo <- subset(rmall, queue == "RANKED_SOLO_5x5")
rmateam <- subset(rmall, queue == "RANKED_TEAM_5x5")

#----- Transformations ----- 
# - Summoner Fixed Effects -

rmall$is_rm_0 <- ifelse(rmall$summoner_id==40532886,1,0)
rmall$is_rm_1 <- ifelse(rmall$summoner_id==31307983,1,0)
rmall$is_rm_2 <- ifelse(rmall$summoner_id==48565039,1,0)
rmall$is_rm_3 <- ifelse(rmall$summoner_id==25279066,1,0)
rmall$is_rm_4 <- ifelse(rmall$summoner_id==31995521,1,0)
rmall$is_rm_5 <- ifelse(rmall$summoner_id==26243614,1,0)
rmall$is_rm_6 <- ifelse(rmall$summoner_id==50879700,1,0)
rmall$is_rm_7 <- ifelse(rmall$summoner_id==30601794,1,0)
rmall$is_rm_8 <- ifelse(rmall$summoner_id==21269208,1,0)

# - Squared Exp and Familiarities -
rmall$one_exp_sqared <- rmall$one_month_sumgames*rmall$one_month_sumgames
rmall$one_fam_sqared <- rmall$one_month_fcount*rmall$one_month_fcount
rmall$three_exp_sqared <- rmall$three_month_sumgames*rmall$three_month_sumgames
rmall$three_fam_sqared <- rmall$three_month_fcount*rmall$three_month_fcount

# - Familiarity with 0, 1, or 2+ roster members -

rmall$solo_queue <- ifelse(rmall$tm00count+rmall$tm01count+rmall$tm02count+rmall$tm03count==0,1,0)
rmall$duo_queue <- ifelse(rmall$tm00count+rmall$tm01count+rmall$tm02count+rmall$tm03count==1,1,0)
rmall$team_ranked <- ifelse(rmall$tm00count+rmall$tm01count+rmall$tm02count+rmall$tm03count>=2,1,0)

# - Specific Player Duo_Queue -

rmall$rm0_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==40532886,1,ifelse(rmall$tm01==40532886,1,ifelse(rmall$tm02==40532886,1,ifelse(rmall$tm03==40532886,1,0)))),0)
rmall$rm1_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==31307983,1,ifelse(rmall$tm01==31307983,1,ifelse(rmall$tm02==31307983,1,ifelse(rmall$tm03==31307983,1,0)))),0)
rmall$rm2_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==48565039,1,ifelse(rmall$tm01==48565039,1,ifelse(rmall$tm02==48565039,1,ifelse(rmall$tm03==48565039,1,0)))),0)
rmall$rm3_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==25279066,1,ifelse(rmall$tm01==25279066,1,ifelse(rmall$tm02==25279066,1,ifelse(rmall$tm03==25279066,1,0)))),0)
rmall$rm4_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==31995521,1,ifelse(rmall$tm01==31995521,1,ifelse(rmall$tm02==31995521,1,ifelse(rmall$tm03==31995521,1,0)))),0)
rmall$rm5_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==26243614,1,ifelse(rmall$tm01==26243614,1,ifelse(rmall$tm02==26243614,1,ifelse(rmall$tm03==26243614,1,0)))),0)
rmall$rm6_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==50879700,1,ifelse(rmall$tm01==50879700,1,ifelse(rmall$tm02==50879700,1,ifelse(rmall$tm03==50879700,1,0)))),0)
rmall$rm7_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==30601794,1,ifelse(rmall$tm01==30601794,1,ifelse(rmall$tm02==30601794,1,ifelse(rmall$tm03==30601794,1,0)))),0)
rmall$rm8_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==21269208,1,ifelse(rmall$tm01==21269208,1,ifelse(rmall$tm02==21269208,1,ifelse(rmall$tm03==21269208,1,0)))),0)

#This config format throws a NA for all bools in reg
#rmall$rm8_duo <- ifelse(rmall$duo_queue==1,ifelse(rmall$tm00==21269208,1,ifelse(rmall$tm01==21269208,1,ifelse(rmall$tm02==21269208,1,ifelse(rmall$tm03==21269208,1,ifelse(rmall$summoner_id==21269208,NA,0))))),0)


# - Preseason or Not -
# 1416452400000 start of 2014 preseason
# 1421290800000 end of 2014 preseason
# 1447210800000 start of 2015 preseason
# 1452740400000 end of 2015 preseason

rmall$is_preseason <- ifelse(rmall$match_timestamp>=1416452400000, ifelse(rmall$match_timestamp<=1421290800000,1,0), 0)
rmall$is_preseason <- ifelse(rmall$match_timestamp>=1447210800000, ifelse(rmall$match_timestamp<=1452740400000,1,0), 0)

# --- Export ---


write.csv(rmall, file="rmfinal.csv")