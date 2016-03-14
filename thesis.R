#!/usr/bin/env - Rscript nameofthefile.R



# --- Read in and Summarize Data ---
mydata = read.csv("normalized.csv")
#head(mydata)
#summary(mydata)


#	--- Adding New Columns ---

#Create variable that represents what tm is the summoner_id for that row
mydata$failingtm <- ifelse(mydata$summoner_id==mydata$tm0,0,
	ifelse(mydata$summoner_id==mydata$tm1,1,
		ifelse(mydata$summoner_id==mydata$tm2,2,
			ifelse(mydata$summoner_id==mydata$tm3,3,
				ifelse(mydata$summoner_id==mydata$tm4,4,NA)))))


#Create variable that represents what tms are not the summoner_id for that row
mydata$passingtm <- ifelse(mydata$failingtm==0,4321,
	ifelse(mydata$failingtm==1,4320,
		ifelse(mydata$failingtm==2,4310,
			ifelse(mydata$failingtm==3,4210,
				ifelse(mydata$failingtm==4,3210,NA)))))


#Create a set of variables that are teammates who are not the summoner_id
mydata$tm00 <- ifelse(mydata$passingtm==4321,mydata$tm1,
	ifelse(mydata$passingtm==4320,mydata$tm0,
		ifelse(mydata$passingtm==4310,mydata$tm0,
			ifelse(mydata$passingtm==4210,mydata$tm0,
				ifelse(mydata$passingtm==3210,mydata$tm0, NA)))))

mydata$tm01 <- ifelse(mydata$passingtm==4321,mydata$tm2,
	ifelse(mydata$passingtm==4320,mydata$tm2,
		ifelse(mydata$passingtm==4310,mydata$tm1,
			ifelse(mydata$passingtm==4210,mydata$tm1,
				ifelse(mydata$passingtm==3210,mydata$tm1, NA)))))

mydata$tm02 <- ifelse(mydata$passingtm==4321,mydata$tm3,
	ifelse(mydata$passingtm==4320,mydata$tm3,
		ifelse(mydata$passingtm==4310,mydata$tm3,
			ifelse(mydata$passingtm==4210,mydata$tm2,
				ifelse(mydata$passingtm==3210,mydata$tm2, NA)))))

mydata$tm03 <- ifelse(mydata$passingtm==4321,mydata$tm4,
	ifelse(mydata$passingtm==4320,mydata$tm4,
		ifelse(mydata$passingtm==4310,mydata$tm4,
			ifelse(mydata$passingtm==4210,mydata$tm4,
				ifelse(mydata$passingtm==3210,mydata$tm3, NA)))))



# --- Sort the Data ---
#Sorting the data by match_timestamp

attach(mydata)
sorteddata <- mydata[order(match_timestamp),]
detach(mydata)


# --- Generating the Experience Dummy Variables & Columns---

#The Average Game Length across all divisions is 34 minutes long http://www.leaguemath.com/match-duration-analysis/
#The Average Game Length has been ~32.5 minutes long since Nov 11, 2015 http://boards.na.leagueoflegends.com/en/c/developer-corner/1VymTVKu-game-pacing-and-you
#			http://jstationx.com/2015/11/10/league-of-legends-patch-5-22-release-date/
#
# Beginning of Data --> 1447239600000 == 34 min avg
# 1447239600000 --> End of Data == 32.5 min avg
#Num of Milliseconds in a Day == 86,400,000
#Num of Milliseconds in 34 min == 2040000
#Num of Milliseconds in 32.5 min == 1950000

sorteddata$longavg <- ifelse(mydata$match_timestamp<1447239600000,1,0)
sorteddata$bool <- ifelse(sorteddata$kills>=0,1,0)
sorteddata$sum_games_by_id <- ave(sorteddata$bool,sorteddata$summoner_id, FUN = cumsum)
sorteddata$is_first_game <- ifelse(sorteddata$sum_games_by_id==1,1,0)

sorteddata$milliseconds_in_game <- ifelse(sorteddata$longavg==1,sorteddata$sum_games_by_id*2040000,sorteddata$sum_games_by_id*2040000)
sorteddata$days_in_game <- sorteddata$milliseconds_in_game/86400000
sorteddata$workdays_in_game_8hrs <- sorteddata$days_in_game*3

sorteddata$first_game_timestamp <- ifelse(sorteddata$is_first_game==1,sorteddata$match_timestamp,0)
sorteddata$first_game_timestamp_by_id <- ave(sorteddata$first_game_timestamp,sorteddata$summoner_id, FUN = sum)
sorteddata$milliseconds_since_first_game <- sorteddata$match_timestamp-sorteddata$first_game_timestamp_by_id
sorteddata$days_since_first_game <- sorteddata$milliseconds_since_first_game/86400000
sorteddata$workdays_since_first_game_8hrs <- sorteddata$days_since_first_game*3
sorteddata$pct_days_in_game <- ifelse(sorteddata$days_since_first_game==0,1,
	ifelse(sorteddata$days_since_first_game<=sorteddata$days_in_game,1,sorteddata$days_in_game/sorteddata$days_since_first_game))


# --- Generating the Familiarity Dummy Variables & Columns ---

sorteddata$tm00count <- ifelse(sorteddata$tm00==sorteddata$rm0,1,
	ifelse(sorteddata$tm00==sorteddata$rm1,1,
		ifelse(sorteddata$tm00==sorteddata$rm2,1,
			ifelse(sorteddata$tm00==sorteddata$rm3,1,
				ifelse(sorteddata$tm00==sorteddata$rm4,1,
					ifelse(sorteddata$tm00==sorteddata$rm5,1,
						ifelse(sorteddata$tm00==sorteddata$rm6,1,
							ifelse(sorteddata$tm00==sorteddata$rm7,1,
								ifelse(sorteddata$tm00==sorteddata$rm8,1,
									ifelse(sorteddata$tm00==sorteddata$rm9,1,0))))))))))

sorteddata$tm01count <- ifelse(sorteddata$tm01==sorteddata$rm0,1,
	ifelse(sorteddata$tm01==sorteddata$rm1,1,
		ifelse(sorteddata$tm01==sorteddata$rm2,1,
			ifelse(sorteddata$tm01==sorteddata$rm3,1,
				ifelse(sorteddata$tm01==sorteddata$rm4,1,
					ifelse(sorteddata$tm01==sorteddata$rm5,1,
						ifelse(sorteddata$tm01==sorteddata$rm6,1,
							ifelse(sorteddata$tm01==sorteddata$rm7,1,
								ifelse(sorteddata$tm01==sorteddata$rm8,1,
									ifelse(sorteddata$tm01==sorteddata$rm9,1,0))))))))))

sorteddata$tm02count <- ifelse(sorteddata$tm02==sorteddata$rm0,1,
	ifelse(sorteddata$tm02==sorteddata$rm1,1,
		ifelse(sorteddata$tm02==sorteddata$rm2,1,
			ifelse(sorteddata$tm02==sorteddata$rm3,1,
				ifelse(sorteddata$tm02==sorteddata$rm4,1,
					ifelse(sorteddata$tm02==sorteddata$rm5,1,
						ifelse(sorteddata$tm02==sorteddata$rm6,1,
							ifelse(sorteddata$tm02==sorteddata$rm7,1,
								ifelse(sorteddata$tm02==sorteddata$rm8,1,
									ifelse(sorteddata$tm02==sorteddata$rm9,1,0))))))))))

sorteddata$tm03count <- ifelse(sorteddata$tm03==sorteddata$rm0,1,
	ifelse(sorteddata$tm03==sorteddata$rm1,1,
		ifelse(sorteddata$tm03==sorteddata$rm2,1,
			ifelse(sorteddata$tm03==sorteddata$rm3,1,
				ifelse(sorteddata$tm03==sorteddata$rm4,1,
					ifelse(sorteddata$tm03==sorteddata$rm5,1,
						ifelse(sorteddata$tm03==sorteddata$rm6,1,
							ifelse(sorteddata$tm03==sorteddata$rm7,1,
								ifelse(sorteddata$tm03==sorteddata$rm8,1,
									ifelse(sorteddata$tm03==sorteddata$rm9,1,0))))))))))

sorteddata$tm00famcount <- ave(sorteddata$tm00count,sorteddata$summoner_id, FUN = cumsum)
sorteddata$tm01famcount <- ave(sorteddata$tm01count,sorteddata$summoner_id, FUN = cumsum)
sorteddata$tm02famcount <- ave(sorteddata$tm02count,sorteddata$summoner_id, FUN = cumsum)
sorteddata$tm03famcount <- ave(sorteddata$tm03count,sorteddata$summoner_id, FUN = cumsum)

sorteddata$alltmfamcount <- sorteddata$tm00famcount+sorteddata$tm01famcount+sorteddata$tm02famcount+sorteddata$tm03famcount

sorteddata$f0 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<0.100,1,0)
sorteddata$f1 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.100,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.199,1,0),0)
sorteddata$f2 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.200,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.299,1,0),0)
sorteddata$f3 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.300,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.399,1,0),0)
sorteddata$f4 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.400,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.499,1,0),0)
sorteddata$f5 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.500,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.599,1,0),0)
sorteddata$f6 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.600,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.699,1,0),0)
sorteddata$f7 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.700,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.799,1,0),0)
sorteddata$f8 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.800,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<=0.899,1,0),0)
sorteddata$f9 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.900,1,0)

	# -Broke familiarity into Thirds-
sorteddata$f00 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<0.3333,1,0)
sorteddata$f01 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.3333,
	ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<0.6666,1,0),0)
sorteddata$f02 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.6666,1,0)

	# -Broke familiarity into Not, and Somewhat +
sorteddata$f000 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id<0.2,1,0)
sorteddata$f001 <- ifelse(sorteddata$alltmfamcount/sorteddata$sum_games_by_id>=0.2,1,0)


# --- Creating the Position Dummy Variable ---

sorteddata$top <- ifelse(sorteddata$lane=="TOP",1,0)
sorteddata$jungle <- ifelse(sorteddata$lane=="JUNGLE",1,0)
sorteddata$mid <- ifelse(sorteddata$lane=="MIDDLE",1,0)
sorteddata$bot_carry <- ifelse(sorteddata$role=="DUO_CARRY",1,0)
sorteddata$bot_support <- ifelse(sorteddata$role=="DUO_SUPPORT",1,0)


# --- Transforming the Data ---
# Creating a dataframe of RANKED_SOLO_5x5 data and a dataframe of TEAM_RANKED_5x5 data
# from the comprehensive dataframe of All Matches. Also player specific dataframes for each of the roster members. 
# This is not a scalable approach to handling panel data, but will work in this case due to the small number of players (8).
# Finally there is a complete and unedited dataset "allmydata"

mysolodata <- subset(sorteddata, queue == "RANKED_SOLO_5x5")
myteamdata <- subset(sorteddata, queue == "RANKED_TEAM_5x5")
rm0 <- subset(sorteddata, summoner_id == 40532886)
rm1 <- subset(sorteddata, summoner_id == 31307983)
rm2 <- subset(sorteddata, summoner_id == 48565039)
rm3 <- subset(sorteddata, summoner_id == 25279066)
rm4 <- subset(sorteddata, summoner_id == 31995521)
rm5 <- subset(sorteddata, summoner_id == 26243614)
rm6 <- subset(sorteddata, summoner_id == 50879700)
rm7 <- subset(sorteddata, summoner_id == 30601794)
rm8 <- subset(sorteddata, summoner_id == 21269208)

allmydata <- sorteddata


# --- Summarizing and Saving the Final Dataset ---

#head(allmydata)
#summary(allmydata)

#write.csv(allmydata, file="allmydata.csv")


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

