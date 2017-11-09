#####################################
## Plot Daily ELO Changes per Team ##
#####################################

suppressPackageStartupMessages(library(googleVis))

# Add ID Variable to Team Lists
{# 1
AnaheimDucks["Team"] <- "Anaheim Ducks"
#2
ArizonaCoyotes["Team"] <- "Arizona Coyotes"
#3
BostonBruins["Team"] <- "Boston Bruins"
#4
BrooklynAmericans["Team"] <- "Brooklyn Americans"
#5
BuffaloSabres["Team"] <- "Buffalo Sabres"
#6
CalgaryFlames["Team"] <- "Calgary Flames"
#7
CarolinaHurricanes["Team"] <- "Carolina Hurricanes"
#8 
ChicagoBlackhawks["Team"] <- "Chicago Blackhawks"
#9
ClevelandBarons["Team"] <- "Cleveland Barons"
#10
ColoradoAvalanche["Team"] <- "Colorado Avalanche"
#11
ColumbusBlueJackets["Team"] <- "Columbus Blue Jackets"
#12
DallasStars["Team"] <- "Dallas Stars"
#13
DetroitRedWings["Team"] <- "Detroit Red Wings"
#14
EdmontonOilers["Team"] <- "Edmonton Oilers"
#15
FloridaPanthers["Team"] <- "Florida Panthers"
#16
LosAngelesKings["Team"] <- "Los Angeles Kings"
#17
MinnesotaWild["Team"] <- "Minnesota Wild"
#18 
MontrealCanadiens["Team"] <- "Montreal Canadiens"
#19
MontrealMaroons["Team"] <- "Montreal Maroons"
#20
MontrealWanderers["Team"] <- "Montreal Wanderers"
#21
NashvillePredators["Team"] <- "Nashville Predators"
#22
NewJerseyDevils["Team"] <- "New Jersey Devils"
#23
NewYorkIslanders["Team"] <- "New York Islanders"
#24
NewYorkRangers["Team"] <- "New York Rangers"
#25
OttawaSenators["Team"] <- "Ottawa Senators"
#26
PhiladelphiaFlyers["Team"] <- "Philadelphia Flyers"
#27
PhiladelphiaQuakers["Team"] <- "Philadelphia Quakers"
#28
PittsburghPenguins["Team"] <- "Pittsburgh Penguins"
#29
SanJoseSharks["Team"] <- "San Jose Sharks"
#30
St.LouisBlues["Team"] <- "St. Louis Blues"
#31
St.LouisEagles["Team"] <- "St. Louis Eagles"
#32
TampaBayLightning["Team"] <- "Tampa Bay Lightning"
#33
TorontoMapleLeafs["Team"] <- "Toronto Maple Leafs"
#34
VancouverCanucks["Team"] <- "Vancouver Canucks"
#35
WashingtonCapitals["Team"] <- "Washington Capitals"
#36
WinnipegJets["Team"] <- "Winnipeg Jets"
}

# List all teams
TeamList <- list(AnaheimDucks, ArizonaCoyotes, BostonBruins,
                 BrooklynAmericans, BuffaloSabres, ClevelandBarons,
                 CalgaryFlames, CarolinaHurricanes, ChicagoBlackhawks,
                 ColoradoAvalanche, ColumbusBlueJackets, DallasStars,
                 DetroitRedWings, EdmontonOilers, FloridaPanthers, 
                 LosAngelesKings, MinnesotaWild, MontrealCanadiens, 
                 MontrealMaroons, MontrealWanderers, NashvillePredators, 
                 NewJerseyDevils, NewYorkIslanders, NewYorkRangers,
                 OttawaSenators, PhiladelphiaFlyers, PhiladelphiaQuakers,
                 PittsburghPenguins, SanJoseSharks, St.LouisBlues,
                 St.LouisEagles, TampaBayLightning, TorontoMapleLeafs, 
                 VancouverCanucks, WashingtonCapitals, WinnipegJets)

ELOMaster <- do.call("rbind", TeamList)
ELOMaster <- ELOMaster[, -2]


myStateSettings <-'
{"xZoomedDataMin":1199145600000,"colorOption":"2",
"duration":{"timeUnit":"Y","multiplier":1},"yLambda":1,
"yAxisOption":"4","sizeOption":"_UNISIZE",
"iconKeySettings":[],"xLambda":1,"nonSelectedAlpha":0,
"xZoomedDataMax":1262304000000,"iconType":"LINE",
"dimensions":{"iconDimensions":["dim0"]},
"showTrails":false,"uniColorForNonSelected":false,
"xAxisOption":"_TIME","orderedByX":false,"playDuration":15000,
"xZoomedIn":false,"time":"2010","yZoomedDataMin":0,
"yZoomedIn":false,"orderedByY":false,"yZoomedDataMax":100}
'

History = gvisMotionChart(ELOMaster, idvar = "Team", timevar = "Date")
plot(History)





