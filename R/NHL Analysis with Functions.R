##################################################################
## Historical Analysis as a Function for Readability            ##
## No Overtimes between 1943 - 1983 for Regular Season          ##
## Shootouts instituted in 2005-2006 Season                     ##
## This analysis does not include preseason or exhibition games ##
##################################################################



memory.limit(size=70000)
start = Sys.time()

options(java.parameters = "-Xmx6g")
# Library
{
library(XLConnect)
library(XLConnectJars)
library(lubridate)
library(scales)
library(plyr)
suppressPackageStartupMessages(library(googleVis))
}
successrate <- c()



            {
            nhlwb<-loadWorkbook("F:/NHL Stats.xlsx")
            Standings191718 <- readWorksheet(nhlwb, sheet="Standings")
            
            Schedule191718 <- readWorksheet(nhlwb, sheet="Schedule1718")
            Schedule1819 <- readWorksheet(nhlwb, sheet="Schedule1819")
            Schedule1920 <- readWorksheet(nhlwb, sheet="Schedule1920")
            Schedule2021 <- readWorksheet(nhlwb, sheet="Schedule2021")
            Schedule2122 <- readWorksheet(nhlwb, sheet="Schedule2122")
            Schedule2223 <- readWorksheet(nhlwb, sheet="Schedule2223")
            Schedule2324 <- readWorksheet(nhlwb, sheet="Schedule2324")
            Schedule2425 <- readWorksheet(nhlwb, sheet="Schedule2425")
            Schedule2526 <- readWorksheet(nhlwb, sheet="Schedule2526")
            Schedule2627 <- readWorksheet(nhlwb, sheet="Schedule2627")
            Schedule2728 <- readWorksheet(nhlwb, sheet="Schedule2728")
            Schedule2829 <- readWorksheet(nhlwb, sheet="Schedule2829")
            Schedule2930 <- readWorksheet(nhlwb, sheet="Schedule2930")
            Schedule3031 <- readWorksheet(nhlwb, sheet="Schedule3031")
            Schedule3132 <- readWorksheet(nhlwb, sheet="Schedule3132")
            Schedule3233 <- readWorksheet(nhlwb, sheet="Schedule3233")
            Schedule3334 <- readWorksheet(nhlwb, sheet="Schedule3334")
            Schedule3435 <- readWorksheet(nhlwb, sheet="Schedule3435")
            Schedule3536 <- readWorksheet(nhlwb, sheet="Schedule3536")
            Schedule3637 <- readWorksheet(nhlwb, sheet="Schedule3637")
            Schedule3738 <- readWorksheet(nhlwb, sheet="Schedule3738")
            Schedule3839 <- readWorksheet(nhlwb, sheet="Schedule3839")
            Schedule3940 <- readWorksheet(nhlwb, sheet="Schedule3940")
            Schedule4041 <- readWorksheet(nhlwb, sheet="Schedule4041")
            Schedule4142 <- readWorksheet(nhlwb, sheet="Schedule4142")
            Schedule4243 <- readWorksheet(nhlwb, sheet="Schedule4243")
            Schedule4344 <- readWorksheet(nhlwb, sheet="Schedule4344")
            Schedule4445 <- readWorksheet(nhlwb, sheet="Schedule4445")
            Schedule4546 <- readWorksheet(nhlwb, sheet="Schedule4546")
            Schedule4647 <- readWorksheet(nhlwb, sheet="Schedule4647")
            Schedule4748 <- readWorksheet(nhlwb, sheet="Schedule4748")
            Schedule4849 <- readWorksheet(nhlwb, sheet="Schedule4849")
            Schedule4950 <- readWorksheet(nhlwb, sheet="Schedule4950")
            Schedule5051 <- readWorksheet(nhlwb, sheet="Schedule5051")
            Schedule5152 <- readWorksheet(nhlwb, sheet="Schedule5152")
            Schedule5253 <- readWorksheet(nhlwb, sheet="Schedule5253")
            Schedule5354 <- readWorksheet(nhlwb, sheet="Schedule5354")
            Schedule5455 <- readWorksheet(nhlwb, sheet="Schedule5455")
            Schedule5556 <- readWorksheet(nhlwb, sheet="Schedule5556")
            Schedule5657 <- readWorksheet(nhlwb, sheet="Schedule5657")
            Schedule5758 <- readWorksheet(nhlwb, sheet="Schedule5758")
            Schedule5859 <- readWorksheet(nhlwb, sheet="Schedule5859")
            Schedule5960 <- readWorksheet(nhlwb, sheet="Schedule5960")
            Schedule6061 <- readWorksheet(nhlwb, sheet="Schedule6061")
            Schedule6162 <- readWorksheet(nhlwb, sheet="Schedule6162")
            Schedule6263 <- readWorksheet(nhlwb, sheet="Schedule6263")
            Schedule6364 <- readWorksheet(nhlwb, sheet="Schedule6364")
            Schedule6465 <- readWorksheet(nhlwb, sheet="Schedule6465")
            Schedule6566 <- readWorksheet(nhlwb, sheet="Schedule6566")
            Schedule6667 <- readWorksheet(nhlwb, sheet="Schedule6667")
            Schedule6768 <- readWorksheet(nhlwb, sheet="Schedule6768")
            Schedule6869 <- readWorksheet(nhlwb, sheet="Schedule6869")
            Schedule6970 <- readWorksheet(nhlwb, sheet="Schedule6970")
            Schedule7071 <- readWorksheet(nhlwb, sheet="Schedule7071")
            Schedule7172 <- readWorksheet(nhlwb, sheet="Schedule7172")
            Schedule7273 <- readWorksheet(nhlwb, sheet="Schedule7273")
            Schedule7374 <- readWorksheet(nhlwb, sheet="Schedule7374")
            Schedule7475 <- readWorksheet(nhlwb, sheet="Schedule7475")
            Schedule7576 <- readWorksheet(nhlwb, sheet="Schedule7576")
            Schedule7677 <- readWorksheet(nhlwb, sheet="Schedule7677")
            Schedule7778 <- readWorksheet(nhlwb, sheet="Schedule7778")
            Schedule7879 <- readWorksheet(nhlwb, sheet="Schedule7879")
            Schedule7980 <- readWorksheet(nhlwb, sheet="Schedule7980")
            Schedule8081 <- readWorksheet(nhlwb, sheet="Schedule8081")
            Schedule8182 <- readWorksheet(nhlwb, sheet="Schedule8182")
            Schedule8283 <- readWorksheet(nhlwb, sheet="Schedule8283")
            Schedule8384 <- readWorksheet(nhlwb, sheet="Schedule8384")
            Schedule8485 <- readWorksheet(nhlwb, sheet="Schedule8485")
            Schedule8586 <- readWorksheet(nhlwb, sheet="Schedule8586")
            Schedule8687 <- readWorksheet(nhlwb, sheet="Schedule8687")
            Schedule8788 <- readWorksheet(nhlwb, sheet="Schedule8788")
            Schedule8889 <- readWorksheet(nhlwb, sheet="Schedule8889")
            Schedule8990 <- readWorksheet(nhlwb, sheet="Schedule8990")
            Schedule9091 <- readWorksheet(nhlwb, sheet="Schedule9091")
            Schedule9192 <- readWorksheet(nhlwb, sheet="Schedule9192")
            Schedule9293 <- readWorksheet(nhlwb, sheet="Schedule9293")
            Schedule9394 <- readWorksheet(nhlwb, sheet="Schedule9394")
            Schedule9495 <- readWorksheet(nhlwb, sheet="Schedule9495")
            Schedule9596 <- readWorksheet(nhlwb, sheet="Schedule9596")
            Schedule9697 <- readWorksheet(nhlwb, sheet="Schedule9697")
            Schedule9798 <- readWorksheet(nhlwb, sheet="Schedule9798")
            Schedule9899 <- readWorksheet(nhlwb, sheet="Schedule9899")
            Schedule9900 <- readWorksheet(nhlwb, sheet="Schedule9900")
            Schedule0001 <- readWorksheet(nhlwb, sheet="Schedule0001")
            Schedule0102 <- readWorksheet(nhlwb, sheet="Schedule0102")
            Schedule0203 <- readWorksheet(nhlwb, sheet="Schedule0203")
            Schedule0304 <- readWorksheet(nhlwb, sheet="Schedule0304")
            ## 2004-2005 Season Cancelled Due to Player Lockout ##
            Schedule0506 <- readWorksheet(nhlwb, sheet="Schedule0506")
            Schedule0607 <- readWorksheet(nhlwb, sheet="Schedule0607")
            Schedule0708 <- readWorksheet(nhlwb, sheet="Schedule0708")
            Schedule0809 <- readWorksheet(nhlwb, sheet="Schedule0809")
            Schedule0910 <- readWorksheet(nhlwb, sheet="Schedule0910")
            Schedule1011 <- readWorksheet(nhlwb, sheet="Schedule1011")
            Schedule1112 <- readWorksheet(nhlwb, sheet="Schedule1112")
            Schedule1213 <- readWorksheet(nhlwb, sheet="Schedule1213")
            Schedule1314 <- readWorksheet(nhlwb, sheet="Schedule1314")
            Schedule1415 <- readWorksheet(nhlwb, sheet="Schedule1415")
            Schedule1516 <- readWorksheet(nhlwb, sheet="Schedule1516")
            Schedule1617 <- readWorksheet(nhlwb, sheet="Schedule1617")
            Schedule1718 <- readWorksheet(nhlwb, sheet="Schedule201718")
            }
            
            # Set parameter Values
            homeice <<- 20 # Home Ice Advantage
            k <<- 8 # How far to move Ratings after each match
            SC <<- 1/3 # How far to regress back to the mean between seasons
            
            # Create Empty Matrices to Store Every ELO for each Team & Date
            ArizonaCoyotes <- data.frame()
            BostonBruins <- data.frame()
            BuffaloSabres <- data.frame()
            CalgaryFlames <- data.frame()
            CarolinaHurricanes <- data.frame()
            ChicagoBlackhawks <- data.frame()
            ColoradoAvalanche <- data.frame()
            DallasStars <- data.frame()
            DetroitRedWings <- data.frame()
            EdmontonOilers <- data.frame()
            LosAngelesKings <- data.frame()
            MontrealCanadiens <- data.frame()
            NewJerseyDevils <- data.frame()
            NewYorkIslanders <-data.frame()
            NewYorkRangers <- data.frame()
            PhiladelphiaFlyers <- data.frame()
            PittsburghPenguins <- data.frame()
            St.LouisBlues <- data.frame()
            TorontoMapleLeafs <- data.frame()
            VancouverCanucks <- data.frame()
            WashingtonCapitals <- data.frame()
            SanJoseSharks <- data.frame()
            OttawaSenators <- data.frame()
            TampaBayLightning <- data.frame()
            AnaheimDucks <- data.frame()
            FloridaPanthers <- data.frame()
            NashvillePredators <- data.frame()
            WinnipegJets <- data.frame()
            ColumbusBlueJackets <- data.frame()
            MinnesotaWild <- data.frame()
            ClevelandBarons <- data.frame()
            BrooklynAmericans <- data.frame()
            MontrealMaroons <- data.frame()
            MontrealWanderers <- data.frame()
            PhiladelphiaQuakers <- data.frame()
            St.LouisEagles <- data.frame()
            VegasGoldenKnights <- data.frame()
            
            
            
            Schedule191718[,1] = as.Date(Schedule191718[,1])
            names(Schedule191718)[names(Schedule191718)=="ELO.1"] <- "ELO"
            names(Schedule191718)[names(Schedule191718)=="ELO.CHANGE.HOME."] <- "ELO.CHANGE"
            names(Schedule191718)[names(Schedule191718)=="ELO.CHANGE.AWAY."] <- "ELO.CHANGE"
            
            for(i in c(1:nrow(Schedule191718))){
        
              Schedule191718[i, 6] = Schedule191718[i, 4] - Schedule191718[i, 5]
            }
            #Populate the Standings191718 Dataframe
            for(i in c(1:nrow(Standings191718))){
              for(j in c(1:nrow(Schedule191718))){
                #Count Games Played (GP)
                if(Schedule191718[j, 2] == Standings191718[i,1] && Schedule191718[j, 1] <= Sys.Date()){
                  Standings191718[i, 4] = Standings191718[i,4] + 1
                }
                if(Schedule191718[j, 3] == Standings191718[i,1] && Schedule191718[j, 1] <= Sys.Date()){
                  Standings191718[i, 4] = Standings191718[i,4] + 1
                }
                #Count Games Won (W)
                if(Schedule191718[j, 2] == Standings191718[i,1] && Schedule191718[j, 6] > 0){
                  Standings191718[i, 5] = Standings191718[i,5] + 1
                }
                if(Schedule191718[j, 3] == Standings191718[i,1] && Schedule191718[j, 6] < 0){
                  Standings191718[i, 5] = Standings191718[i,5] + 1
                }
                # Counts Games Lost in Regulation Time (L)
                if(Schedule191718[j, 2] == Standings191718[i,1] && Schedule191718[j, 6] < 0 && Schedule191718[j, 7] == "N"){
                  Standings191718[i, 6] = Standings191718[i,6] + 1
                }
                if(Schedule191718[j, 3] == Standings191718[i,1] && Schedule191718[j, 6] > 0 && Schedule191718[j, 7] == "N"){
                  Standings191718[i, 6] = Standings191718[i,6] + 1
                }
                # Counts Games Lost in  Overime (L)
                if(Schedule191718[j, 2] == Standings191718[i,1] && Schedule191718[j, 6] < 0 && Schedule191718[j, 7] == "Y"){
                  Standings191718[i, 7] = Standings191718[i,7] + 1
                }
                if(Schedule191718[j, 3] == Standings191718[i,1] && Schedule191718[j, 6] > 0 && Schedule191718[j, 7] == "Y"){
                  Standings191718[i, 7] = Standings191718[i,7] + 1
                }
                # Counting Goals For
                if(Schedule191718[j, 2] == Standings191718[i,1] && Schedule191718[j, 1] <= Sys.Date()){
                  Standings191718[i, 10] = Standings191718[i,10] + Schedule191718[j, 4]
                }
                if(Schedule191718[j, 3] == Standings191718[i,1] && Schedule191718[j, 1] <= Sys.Date()){
                  Standings191718[i, 10] = Standings191718[i,10] + Schedule191718[j,5]
                }
                # Counting Goals Against
                if(Schedule191718[j, 2] == Standings191718[i,1] && Schedule191718[j, 1] <= Sys.Date()){
                  Standings191718[i, 11] = Standings191718[i,11] + Schedule191718[j,5]
                }
                if(Schedule191718[j, 3] == Standings191718[i,1] && Schedule191718[j, 1] <= Sys.Date()){
                  Standings191718[i, 11] = Standings191718[i,11] + Schedule191718[j,4]
                }
                # Calculating Regulation + Overtime Wins (ROW)
                if(Schedule191718[j, 2] == Standings191718[i,1] && Schedule191718[j, 6] > 0 && Schedule191718[j,8] == "N"){
                  Standings191718[i, 9] = Standings191718[i,9] + 1
                }
                if(Schedule191718[j, 3] == Standings191718[i,1] && Schedule191718[j, 6] < 0 && Schedule191718[j, 8] == "N"){
                  Standings191718[i, 9] = Standings191718[i,9] + 1
                }
              }
              # Calculate Points
              Standings191718[i,8] = 2*Standings191718[i,5] + Standings191718[i,7]
              # Calculate Goal Differential
              Standings191718[i,12] = Standings191718[i,10] - Standings191718[i, 11]
            }
            
            # Build a function to calculate win probability for the home team
            winprob = function(home, away){
              for(i in c(1:nrow(Standings191718))){
                if(Standings191718[i,1]==home){
                  ELOh = Standings191718[i,13]
                }
                else if(Standings191718[i,1]==away){
                  ELOv = Standings191718[i,13]
                }
              }
              
              Eh = round(1 / (1 + 10 ^ (-(ELOh - ELOv + homeice)/400)), 2)
              output = c(Eh, ELOh, ELOv)
              return(output)
            }
            
            # Calculate Probability of Winning and ELO Change
            for(i in c(1:nrow(Schedule191718))){
              if(Schedule191718[i, 1] <= Sys.Date()){
                Schedule191718[i,9] = winprob(Schedule191718[i,2], Schedule191718[i, 3])[1]
                Schedule191718[i, 10] = 1 - Schedule191718[i,9]
                Schedule191718[i, 15] = winprob(Schedule191718[i,2], Schedule191718[i, 3])[2]
                Schedule191718[i, 16] = winprob(Schedule191718[i,2], Schedule191718[i, 3])[3]
                Schedule191718[i, 14] = Schedule191718[i, 15] - Schedule191718[i, 16]
             
                if(Schedule191718[i,1] <= Sys.Date()){
                  if(Schedule191718[i,6]>0 && Schedule191718[i,8]=="N"){
                    outcome=1
                  }
                  else if(Schedule191718[i,6] <0 && Schedule191718[i,8]=="N"){
                    outcome = 0
                  }
                  else{outcome = 0.5}
                  if(Schedule191718[i,13]=="N"){
                    I = 1
                  }
                  else{I=1.5}
                  if(Schedule191718[i, 6] != 0){
                    M = max(1, log(abs(Schedule191718[i,6] - 0.85*((winprob(Schedule191718[i,2], Schedule191718[i, 3])[2]-
                                                                    winprob(Schedule191718[i,2], Schedule191718[i, 3])[3]+homeice)/100)+exp(1)-1)))
                  }
                  else{M = 1}
                  Schedule191718[i,11] = k*M*I*(outcome-Schedule191718[i,9])
                  Schedule191718[i,12] = -Schedule191718[i,11]
                  for(j in c(1:nrow(Standings191718))){
                    if(Standings191718[j,1]==Schedule191718[i,2]){
                      Standings191718[j,13] = Standings191718[j,13] + Schedule191718[i,11]
                    }
                    else if(Standings191718[j,1] == Schedule191718[i,3]){
                      Standings191718[j,13] = Standings191718[j,13] + Schedule191718[i,12]
                    }
                  }
                }  
              }
              
              # This is the beginning of the new code!!!! MAY NOT WORK!!!!
                
                if(Schedule191718[i, 2] == "Toronto Maple Leafs"){
                  TorontoMapleLeafs <<- rbind(TorontoMapleLeafs, Schedule191718[i, c(1, 11, 15)])
                }
                if(Schedule191718[i, 3] == "Toronto Maple Leafs"){
                  TorontoMapleLeafs <<- rbind(TorontoMapleLeafs, Schedule191718[i, c(1, 12, 16)])
                }
                if(Schedule191718[i, 2] == "Montreal Wanderers"){
                  MontrealWanderers <<- rbind(MontrealWanderers, Schedule191718[i, c(1, 11, 15)])
                }
                if(Schedule191718[i, 3] == "Montreal Wanderers"){
                  MontrealWanderers <<- rbind(MontrealWanderers, Schedule191718[i, c(1, 12, 16)])
                }
                if(Schedule191718[i, 2] == "St. Louis Eagles"){
                  St.LouisEagles <<- rbind(St.LouisEagles, Schedule191718[i, c(1, 11, 15)])
                }
                if(Schedule191718[i, 3] == "St. Louis Eagles"){
                  St.LouisEagles <<- rbind(St.LouisEagles, Schedule191718[i, c(1, 12, 16)])
                }
                if(Schedule191718[i, 2] == "Montreal Canadiens"){
                  MontrealCanadiens <<- rbind(MontrealCanadiens, Schedule191718[i, c(1, 11, 15)])
                }
                if(Schedule191718[i, 3] == "Montreal Canadiens"){
                  MontrealCanadiens <<- rbind(MontrealCanadiens, Schedule191718[i, c(1, 12, 16)])
                }
                
                # This is the end of the new code!!
              
            }
            
            
            
            
            
            Year = year(as.POSIXlt(Schedule191718[nrow(Schedule191718),1]))
            Year = rep(Year, nrow(Standings191718))
            Standings191718 = cbind(Standings191718, Year)
            
            analysis <- function(Sch, PStand){
              names(Sch)[names(Sch)=="ELO.1"] <- "ELO"
              names(Sch)[names(Sch)=="ELO.CHANGE.HOME."] <- "ELO.CHANGE"
              names(Sch)[names(Sch)=="ELO.CHANGE.AWAY."] <- "ELO.CHANGE"
              
              Standings <- readWorksheet(nhlwb, sheet="Standings")
              Sch[,1] = as.Date(Sch[,1])
              
              
              
              for(i in c(1:nrow(Sch))){
                if(Sch[i, 1] < Sys.Date()){
                Sch[i, 6] = Sch[i,4] - Sch[i, 5]
                }
                else{Sch[i, 6] = 0}
              }
              
              for(i in c(1:nrow(Standings))){
                if(PStand[i,4] != 0) {Standings[i,13] = PStand[i,13] + (SC)*(1500 - PStand[i,13])}
                else{
                  Standings[i,13] = 1400
                }
              }
              # Populate the Standings Dataframe
              for(i in c(1:nrow(Standings))){
                for(j in c(1:nrow(Sch))){
                  #Count Games Played (GP)
                  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] !=0){
                    Standings[i, 4] = Standings[i,4] + 1
                  }
                  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] !=0){
                    Standings[i, 4] = Standings[i,4] + 1
                  }
                  #Count Games Won (W)
                  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] > 0){
                    Standings[i, 5] = Standings[i,5] + 1
                  }
                  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] < 0){
                    Standings[i, 5] = Standings[i,5] + 1
                  }
                  # Counts Games Lost in Regulation Time (L)
                  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] < 0 && Sch[j, 7] == "N"){
                    Standings[i, 6] = Standings[i,6] + 1
                  }
                  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] > 0 && Sch[j, 7] == "N"){
                    Standings[i, 6] = Standings[i,6] + 1
                  }
                  # Counts Games Lost in  Overime (L)
                  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] < 0 && Sch[j, 7] == "Y"){
                    Standings[i, 7] = Standings[i,7] + 1
                  }
                  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] > 0 && Sch[j, 7] == "Y"){
                    Standings[i, 7] = Standings[i,7] + 1
                  }
                  # Counting Goals For
                  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] !=0){
                    Standings[i, 10] = Standings[i,10] + Sch[j, 4]
                  }
                  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] !=0){
                    Standings[i, 10] = Standings[i,10] + Sch[j,5]
                  }
                  # Counting Goals Against
                  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] !=0){
                    Standings[i, 11] = Standings[i,11] + Sch[j,5]
                  }
                  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] !=0){
                    Standings[i, 11] = Standings[i,11] + Sch[j,4]
                  }
                  # Calculating Regulation + Overtime Wins (ROW)
                  if(Sch[j, 2] == Standings[i,1] && Sch[j, 6] > 0 && Sch[j,8] == "N"){
                    Standings[i, 9] = Standings[i,9] + 1
                  }
                  if(Sch[j, 3] == Standings[i,1] && Sch[j, 6] < 0 && Sch[j, 8] == "N"){
                    Standings[i, 9] = Standings[i,9] + 1
                  }
                }
                # Calculate Points
                Standings[i,8] = 2*Standings[i,5] + Standings[i,7]
                # Calculate Goal Differential
                Standings[i,12] = Standings[i,10] - Standings[i, 11]
              }
              
              
              
              # Build a function to calculate win probability for the home team
              winprob = function(home, away){
                for(i in c(1:nrow(Standings))){
                  if(Standings[i,1]==home){
                    ELOh = Standings[i,13]
                  }
                  else if(Standings[i,1]==away){
                    ELOv = Standings[i,13]
                  }
                }
                
                Eh = round(1 / (1 + 10 ^ (-(ELOh - ELOv + homeice)/400)), 2)
                output = c(Eh, ELOh, ELOv)
                return(output)
              }
              
              # Calculate Probability of Winning and ELO Change
              for(i in c(1:nrow(Sch))){
                if(Sch[i, 1] <= Sys.Date()){
                  Sch[i,9] = winprob(Sch[i,2], Sch[i, 3])[1]
                  Sch[i, 10] = 1 - Sch[i,9]
                  Sch[i, 15] = winprob(Sch[i,2], Sch[i, 3])[2]
                  Sch[i, 16] = winprob(Sch[i,2], Sch[i, 3])[3]
                  Sch[i, 14] = Sch[i, 15] - Sch[i, 16]
                  if(!is.na(Sch[i,6])){
                    if(Sch[i,6]>0 && Sch[i,8]=="N"){
                      outcome=1
                    }
                    else if(Sch[i,6] <0 && Sch[i,8]=="N"){
                      outcome = 0
                    }
                    else{outcome = 0.5}
                    if(Sch[i,13]=="N"){
                      I = 1
                    }
                    else{I=1.5}
                    M = max(1, log(abs(Sch[i,6] - 0.85*((winprob(Sch[i,2], Sch[i, 3])[2]-
                                                          winprob(Sch[i,2], Sch[i, 3])[3]+homeice)/100)+exp(1)-1)))
                    Sch[i,11] = k*M*I*(outcome-Sch[i,9])
                    Sch[i,12] = -Sch[i,11]
                    for(j in c(1:nrow(Standings))){
                      if(Standings[j,1]==Sch[i,2]){
                        Standings[j,13] = Standings[j,13] + Sch[i,11]
                      }
                      else if(Standings[j,1] == Sch[i,3]){
                        Standings[j,13] = Standings[j,13] + Sch[i,12]
                      }
                    }
                  } 
                }
                
                
                
                
                
                
                
              ## This is the new CODE!!!!! MAY NOT WORK
                # 1
              if(Sch[i, 2] == "Toronto Maple Leafs"){
                TorontoMapleLeafs <<- rbind(TorontoMapleLeafs, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Toronto Maple Leafs"){
                TorontoMapleLeafs <<- rbind(TorontoMapleLeafs, Sch[i, c(1, 12, 16)])
              }
                # 2
              else if(Sch[i, 2] == "Anaheim Ducks"){
                AnaheimDucks <<- rbind(AnaheimDucks, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Anaheim Ducks"){
                AnaheimDucks <<- rbind(AnaheimDucks, Sch[i, c(1, 12, 16)])
              }
                # 3
              else if(Sch[i, 2] == "Arizona Coyotes"){
                ArizonaCoyotes <<- rbind(ArizonaCoyotes, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Arizona Coyotes"){
                ArizonaCoyotes <<- rbind(ArizonaCoyotes, Sch[i, c(1, 12, 16)])
              }  
                # 4
              else if(Sch[i, 2] == "Boston Bruins"){
                BostonBruins <<- rbind(BostonBruins, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Boston Bruins"){
                BostonBruins <<- rbind(BostonBruins, Sch[i, c(1, 12, 16)])
              }
                #5
              else if(Sch[i, 2] == "Brooklyn Americans"){
                BrooklynAmericans <<- rbind(BrooklynAmericans, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Brooklyn Americans"){
                BrooklynAmericans <<- rbind(BrooklynAmericans, Sch[i, c(1, 12, 16)])
              }
                #6
              else if(Sch[i, 2] == "Buffalo Sabres"){
                BuffaloSabres <<- rbind(BuffaloSabres, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Buffalo Sabres"){
                BuffaloSabres <<- rbind(BuffaloSabres, Sch[i, c(1, 12, 16)])
              }
                # 7
              else if(Sch[i, 2] == "Calgary Flames"){
                CalgaryFlames <<- rbind(CalgaryFlames, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Calgary Flames"){
                CalgaryFlames <<- rbind(CalgaryFlames, Sch[i, c(1, 12, 16)])
              }
                # 8
              else if(Sch[i, 2] == "Carolina Hurricanes"){
                CarolinaHurricanes <<- rbind(CarolinaHurricanes, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Carolina Hurricanes"){
                CarolinaHurricanes <<- rbind(CarolinaHurricanes, Sch[i, c(1, 12, 16)])
              }
                # 9
              else if(Sch[i, 2] == "Chicago Blackhawks"){
                ChicagoBlackhawks <<- rbind(ChicagoBlackhawks, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Chicago Blackhawks"){
                ChicagoBlackhawks <<- rbind(ChicagoBlackhawks, Sch[i, c(1, 12, 16)])
              }
                # 10
              else if(Sch[i, 2] == "Cleveland Barons"){
                ClevelandBarons <<- rbind(ClevelandBarons, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Cleveland Barons"){
                ClevelandBarons <<- rbind(ClevelandBarons, Sch[i, c(1, 12, 16)])
              }
                # 11
              else if(Sch[i, 2] == "Colorado Avalanche"){
                ColoradoAvalanche <<- rbind(ColoradoAvalanche, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Colorado Avalanche"){
                ColoradoAvalanche <<- rbind(ColoradoAvalanche, Sch[i, c(1, 12, 16)])
              }
                # 12
              else if(Sch[i, 2] == "Columbus Blue Jackets"){
                ColumbusBlueJackets <<- rbind(ColumbusBlueJackets, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Columbus Blue Jackets"){
                ColumbusBlueJackets <<- rbind(ColumbusBlueJackets, Sch[i, c(1, 12, 16)])
              }
                # 13
              else if(Sch[i, 2] == "Dallas Stars"){
                DallasStars <<- rbind(DallasStars, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Dallas Stars"){
                DallasStars <<- rbind(DallasStars, Sch[i, c(1, 12, 16)])
              }
                # 14
              else if(Sch[i, 2] == "Detroit Red Wings"){
                DetroitRedWings <<- rbind(DetroitRedWings, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Detroit Red Wings"){
                DetroitRedWings <<- rbind(DetroitRedWings, Sch[i, c(1, 12, 16)])
              }
                # 15
              else if(Sch[i, 2] == "Edmonton Oilers"){
                EdmontonOilers <<- rbind(EdmontonOilers, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Edmonton Oilers"){
                EdmontonOilers <<- rbind(EdmontonOilers, Sch[i, c(1, 12, 16)])
              }
                # 16
              else if(Sch[i, 2] == "Florida Panthers"){
                FloridaPanthers <<- rbind(FloridaPanthers, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Florida Panthers"){
                FloridaPanthers <<- rbind(FloridaPanthers, Sch[i, c(1, 12, 16)])
              }
                # 17
              else if(Sch[i, 2] == "Los Angeles Kings"){
                LosAngelesKings <<- rbind(LosAngelesKings, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Los Angeles Kings"){
                LosAngelesKings <<- rbind(LosAngelesKings, Sch[i, c(1, 12, 16)])
              }
                # 18
              else if(Sch[i, 2] == "Minnesota Wild"){
                MinnesotaWild <<- rbind(MinnesotaWild, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Minnesota Wild"){
                MinnesotaWild <<- rbind(MinnesotaWild, Sch[i, c(1, 12, 16)])
              }
                # 19
              else if(Sch[i, 2] == "Montreal Canadiens"){
                MontrealCanadiens <<- rbind(MontrealCanadiens, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Montreal Canadiens"){
                MontrealCanadiens <<- rbind(MontrealCanadiens, Sch[i, c(1, 12, 16)])
              }
                # 20
              else if(Sch[i, 2] == "Montreal Maroons"){
                MontrealMaroons <<- rbind(MontrealMaroons, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Montreal Maroons"){
                MontrealMaroons <<- rbind(MontrealMaroons, Sch[i, c(1, 12, 16)])
              }
                # 21
              else if(Sch[i, 2] == "Montreal Wanderers"){
                MontrealWanderers <<- rbind(MontrealWanderers, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Montreal Wanderers"){
                MontrealWanderers <<- rbind(MontrealWanderers, Sch[i, c(1, 12, 16)])
              }
                # 22
              else if(Sch[i, 2] == "Nashville Predators"){
                NashvillePredators <<- rbind(NashvillePredators, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Nashville Predators"){
                NashvillePredators <<- rbind(NashvillePredators, Sch[i, c(1, 12, 16)])
              }
                # 23
              else if(Sch[i, 2] == "New Jersey Devils"){
                NewJerseyDevils <<- rbind(NewJerseyDevils, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "New Jersey Devils"){
                NewJerseyDevils <<- rbind(NewJerseyDevils, Sch[i, c(1, 12, 16)])
              }
                # 24
              else if(Sch[i, 2] == "New York Islanders"){
                NewYorkIslanders <<- rbind(NewYorkIslanders, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "New York Islanders"){
                NewYorkIslanders <<- rbind(NewYorkIslanders, Sch[i, c(1, 12, 16)])
              }
                # 25
              else if(Sch[i, 2] == "New York Rangers"){
                NewYorkRangers <<- rbind(NewYorkRangers, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "New York Rangers"){
                NewYorkRangers <<- rbind(NewYorkRangers, Sch[i, c(1, 12, 16)])
              }
                # 26
              else if(Sch[i, 2] == "Ottawa Senators"){
                OttawaSenators <<- rbind(OttawaSenators, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Ottawa Senators"){
                OttawaSenators <<- rbind(OttawaSenators, Sch[i, c(1, 12, 16)])
              }
                # 27
              else if(Sch[i, 2] == "Philadelphia Flyers"){
                PhiladelphiaFlyers <<- rbind(PhiladelphiaFlyers, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Philadelphia Flyers"){
                PhiladelphiaFlyers <<- rbind(PhiladelphiaFlyers, Sch[i, c(1, 12, 16)])
              }
                # 28
              else if(Sch[i, 2] == "Philadelphia Quakers"){
                PhiladelphiaQuakers <<- rbind(PhiladelphiaQuakers, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Philadelphia Quakers"){
                PhiladelphiaQuakers <<- rbind(PhiladelphiaQuakers, Sch[i, c(1, 12, 16)])
              }
                # 29
              else if(Sch[i, 2] == "Pittsburgh Penguins"){
                PittsburghPenguins <<- rbind(PittsburghPenguins, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Pittsburgh Penguins"){
                PittsburghPenguins <<- rbind(PittsburghPenguins, Sch[i, c(1, 12, 16)])
              }
                #30
              else if(Sch[i, 2] == "San Jose Sharks"){
                SanJoseSharks <<- rbind(SanJoseSharks, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "San Jose Sharks"){
                SanJoseSharks <<- rbind(SanJoseSharks, Sch[i, c(1, 12, 16)])
              }
                # 31
              else if(Sch[i, 2] == "St. Louis Blues"){
                St.LouisBlues <<- rbind(St.LouisBlues, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "St. Louis Blues"){
                St.LouisBlues <<- rbind(St.LouisBlues, Sch[i, c(1, 12, 16)])
              }
                # 32
              if(Sch[i, 2] == "St. Louis Eagles"){
                St.LouisEagles <<- rbind(St.LouisEagles, Sch[i, c(1, 11, 15)])
              }
              if(Sch[i, 3] == "St. Louis Eagles"){
                St.LouisEagles <<- rbind(St.LouisEagles, Sch[i, c(1, 12, 16)])
              }
                # 33
              else if(Sch[i, 2] == "Tampa Bay Lightning"){
                TampaBayLightning <<- rbind(TampaBayLightning, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Tampa Bay Lightning"){
                TampaBayLightning <<- rbind(TampaBayLightning, Sch[i, c(1, 12, 16)])
              }
                # 34
              else if(Sch[i, 2] == "Vancouver Canucks"){
                VancouverCanucks <<- rbind(VancouverCanucks, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Vancouver Canucks"){
                VancouverCanucks <<- rbind(VancouverCanucks, Sch[i, c(1, 12, 16)])
              }
                # 35
              else if(Sch[i, 2] == "Washington Capitals"){
                WashingtonCapitals <<- rbind(WashingtonCapitals, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Washington Capitals"){
                WashingtonCapitals <<- rbind(WashingtonCapitals, Sch[i, c(1, 12, 16)])
              }
                # 36
              else if(Sch[i, 2] == "Winnipeg Jets"){
                WinnipegJets <<- rbind(WinnipegJets, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Winnipeg Jets"){
                WinnipegJets <<- rbind(WinnipegJets, Sch[i, c(1, 12, 16)])
              }
                # 37
              else if(Sch[i, 2] == "Vegas Golden Knights"){
                VegasGoldenKnights <<- rbind(VegasGoldenKnights, Sch[i, c(1, 11, 15)])
              }
              else if(Sch[i, 3] == "Vegas Golden Knights"){
                VegasGoldenKnights <<- rbind(VegasGoldenKnights, Sch[i, c(1, 12, 16)])
              }
              ## This is the end of the new code!
                

                
                
                
                
                
                
                
                              }
              
          
              
              Year = year(as.POSIXlt(Sch[nrow(Sch),1]))
              Year = rep(Year, nrow(Standings))
              Standings = cbind(Standings, Year)
              
              return(list(Standings=Standings, Schedule=Sch))
            }
            
            # Output Statements
            output <- analysis(Schedule1819, Standings191718)
            Standings1819 <- output$Standings
            Schedule1819 <- output$Schedule
            output <- analysis(Schedule1920, Standings1819)
            Standings1920 <- output$Standings
            Schedule1920 <- output$Schedule
            output <- analysis(Schedule2021, Standings1920)
            Standings2021 <- output$Standings
            Schedule2021 <- output$Schedule
            output <- analysis(Schedule2122, Standings2021)
            Standings2122 <- output$Standings
            Schedule2122 <- output$Schedule
            output <- analysis(Schedule2223, Standings2122)
            Standings2223 <- output$Standings
            Schedule2223 <- output$Schedule
            output <- analysis(Schedule2324, Standings2223)
            Standings2324 <- output$Standings
            Schedule2324 <- output$Schedule
            output <- analysis(Schedule2425, Standings2324)
            Standings2425 <- output$Standings
            Schedule2425 <- output$Schedule
            output <- analysis(Schedule2526, Standings2425)
            Standings2526 <- output$Standings
            Schedule2526 <- output$Schedule
            output <- analysis(Schedule2627, Standings2526)
            Standings2627 <- output$Standings
            Schedule2627 <- output$Schedule
            output <- analysis(Schedule2728, Standings2627)
            Standings2728 <- output$Standings
            Schedule2728 <- output$Schedule
            output <- analysis(Schedule2829, Standings2728)
            Standings2829 <- output$Standings
            Schedule2829 <- output$Schedule
            output <- analysis(Schedule2930, Standings2829)
            Standings2930 <- output$Standings
            Schedule2930 <- output$Schedule
            output <- analysis(Schedule3031, Standings2930)
            Standings3031 <- output$Standings
            Schedule3031 <- output$Schedule
            output <- analysis(Schedule3132, Standings3031)
            Standings3132 <- output$Standings
            Schedule3132 <- output$Schedule
            output <- analysis(Schedule3233, Standings3132)
            Standings3233 <- output$Standings
            Schedule3233 <- output$Schedule
            output <- analysis(Schedule3334, Standings3233)
            Standings3334 <- output$Standings
            Schedule3334 <- output$Schedule
            output <- analysis(Schedule3435, Standings3334)
            Standings3435 <- output$Standings
            Schedule3435 <- output$Schedule
            output <- analysis(Schedule3536, Standings3435)
            Standings3536 <- output$Standings
            Schedule3536 <- output$Schedule
            output <- analysis(Schedule3637, Standings3536)
            Standings3637 <- output$Standings
            Schedule3637 <- output$Schedule
            output <- analysis(Schedule3738, Standings3637)
            Standings3738 <- output$Standings
            Schedule3738 <- output$Schedule
            output <- analysis(Schedule3839, Standings3738)
            Standings3839 <- output$Standings
            Schedule3839 <- output$Schedule
            output <- analysis(Schedule3940, Standings3839)
            Standings3940 <- output$Standings
            Schedule3940 <- output$Schedule
            output <- analysis(Schedule4041, Standings3940)
            Standings4041 <- output$Standings
            Schedule4041 <- output$Schedule
            output <- analysis(Schedule4142, Standings4041)
            Standings4142 <- output$Standings
            Schedule4142 <- output$Schedule
            output <- analysis(Schedule4243, Standings4142)
            Standings4243 <- output$Standings
            Schedule4243 <- output$Schedule
            output <- analysis(Schedule4344, Standings4243)
            Standings4344 <- output$Standings
            Schedule4344 <- output$Schedule
            output <- analysis(Schedule4445, Standings4344)
            Standings4445 <- output$Standings
            Schedule4445 <- output$Schedule
            output <- analysis(Schedule4546, Standings4445)
            Standings4546 <- output$Standings
            Schedule4546 <- output$Schedule
            output <- analysis(Schedule4647, Standings4546)
            Standings4647 <- output$Standings
            Schedule4647 <- output$Schedule
            output <- analysis(Schedule4748, Standings4647)
            Standings4748 <- output$Standings
            Schedule4748 <- output$Schedule
            output <- analysis(Schedule4849, Standings4748)
            Standings4849 <- output$Standings
            Schedule4849 <- output$Schedule
            output <- analysis(Schedule4950, Standings4849)
            Standings4950 <- output$Standings
            Schedule4950 <- output$Schedule
            output <- analysis(Schedule5051, Standings4950)
            Standings5051 <- output$Standings
            Schedule5051 <- output$Schedule
            output <- analysis(Schedule5152, Standings5051)
            Standings5152 <- output$Standings
            Schedule5152 <- output$Schedule
            output <- analysis(Schedule5253, Standings5152)
            Standings5253 <- output$Standings
            Schedule5253 <- output$Schedule
            output <- analysis(Schedule5354, Standings5253)
            Standings5354 <- output$Standings
            Schedule5354 <- output$Schedule
            output <- analysis(Schedule5455, Standings5354)
            Standings5455 <- output$Standings
            Schedule5455 <- output$Schedule
            output <- analysis(Schedule5556, Standings5455)
            Standings5556 <- output$Standings
            Schedule5556 <- output$Schedule
            output <- analysis(Schedule5657, Standings5556)
            Standings5657 <- output$Standings
            Schedule5657 <- output$Schedule
            output <- analysis(Schedule5758, Standings5657)
            Standings5758 <- output$Standings
            Schedule5758 <- output$Schedule
            output <- analysis(Schedule5859, Standings5758)
            Standings5859 <- output$Standings
            Schedule5859 <- output$Schedule
            output <- analysis(Schedule5960, Standings5859)
            Standings5960 <- output$Standings
            Schedule5960 <- output$Schedule
            output <- analysis(Schedule6061, Standings5960)
            Standings6061 <- output$Standings
            Schedule6061 <- output$Schedule
            output <- analysis(Schedule6162, Standings6061)
            Standings6162 <- output$Standings
            Schedule6162 <- output$Schedule
            output <- analysis(Schedule6263, Standings6162)
            Standings6263 <- output$Standings
            Schedule6263 <- output$Schedule
            output <- analysis(Schedule6364, Standings6263)
            Standings6364 <- output$Standings
            Schedule6364 <- output$Schedule
            output <- analysis(Schedule6465, Standings6364)
            Standings6465 <- output$Standings
            Schedule6465 <- output$Schedule
            output <- analysis(Schedule6566, Standings6465)
            Standings6566 <- output$Standings
            Schedule6566 <- output$Schedule
            output <- analysis(Schedule6667, Standings6566)
            Standings6667 <- output$Standings
            Schedule6667 <- output$Schedule
            output <- analysis(Schedule6768, Standings6667)
            Standings6768 <- output$Standings
            Schedule6768 <- output$Schedule
            output <- analysis(Schedule6869, Standings6768)
            Standings6869 <- output$Standings
            Schedule6869 <- output$Schedule
            output <- analysis(Schedule6970, Standings6869)
            Standings6970 <- output$Standings
            Schedule6970 <- output$Schedule
            output <- analysis(Schedule7071, Standings6970)
            Standings7071 <- output$Standings
            Schedule7071 <- output$Schedule
            output <- analysis(Schedule7172, Standings7071)
            Standings7172 <- output$Standings
            Schedule7172 <- output$Schedule
            output <- analysis(Schedule7273, Standings7172)
            Standings7273 <- output$Standings
            Schedule7273 <- output$Schedule
            output <- analysis(Schedule7374, Standings7273)
            Standings7374 <- output$Standings
            Schedule7374 <- output$Schedule
            output <- analysis(Schedule7475, Standings7374)
            Standings7475 <- output$Standings
            Schedule7475 <- output$Schedule
            output <- analysis(Schedule7576, Standings7475)
            Standings7576 <- output$Standings
            Schedule7576 <- output$Schedule
            output <- analysis(Schedule7677, Standings7576)
            Standings7677 <- output$Standings
            Schedule7677 <- output$Schedule
            output <- analysis(Schedule7778, Standings7677)
            Standings7778 <- output$Standings
            Schedule7778 <- output$Schedule
            output <- analysis(Schedule7879, Standings7778)
            Standings7879 <- output$Standings
            Schedule7879 <- output$Schedule
            output <- analysis(Schedule7980, Standings7879)
            Standings7980 <- output$Standings
            Schedule7980 <- output$Schedule
            output <- analysis(Schedule8081, Standings7980)
            Standings8081 <- output$Standings
            Schedule8081 <- output$Schedule
            output <- analysis(Schedule8182, Standings8081)
            Standings8182 <- output$Standings
            Schedule8182 <- output$Schedule
            output <- analysis(Schedule8283, Standings8182)
            Standings8283 <- output$Standings
            Schedule8283 <- output$Schedule
            output <- analysis(Schedule8384, Standings8283)
            Standings8384 <- output$Standings
            Schedule8384 <- output$Schedule
            output <- analysis(Schedule8485, Standings8384)
            Standings8485 <- output$Standings
            Schedule8485 <- output$Schedule
            output <- analysis(Schedule8586, Standings8485)
            Standings8586 <- output$Standings
            Schedule8586 <- output$Schedule
            output <- analysis(Schedule8687, Standings8586)
            Standings8687 <- output$Standings
            Schedule8687 <- output$Schedule
            output <- analysis(Schedule8788, Standings8687)
            Standings8788 <- output$Standings
            Schedule8788 <- output$Schedule
            output <- analysis(Schedule8889, Standings8788)
            Standings8889 <- output$Standings
            Schedule8889 <- output$Schedule
            output <- analysis(Schedule8990, Standings8889)
            Standings8990 <- output$Standings
            Schedule8990 <- output$Schedule
            output <- analysis(Schedule9091, Standings8990)
            Standings9091 <- output$Standings
            Schedule9091 <- output$Schedule
            output <- analysis(Schedule9192, Standings9091)
            Standings9192 <- output$Standings
            Schedule9192 <- output$Schedule
            output <- analysis(Schedule9293, Standings9192)
            Standings9293 <- output$Standings
            Schedule9293 <- output$Schedule
            output <- analysis(Schedule9394, Standings9293)
            Standings9394 <- output$Standings
            Schedule9394 <- output$Schedule
            output <- analysis(Schedule9495, Standings9394)
            Standings9495 <- output$Standings
            Schedule9495 <- output$Schedule
            output <- analysis(Schedule9596, Standings9495)
            Standings9596 <- output$Standings
            Schedule9596 <- output$Schedule
            output <- analysis(Schedule9697, Standings9596)
            Standings9697 <- output$Standings
            Schedule9697 <- output$Schedule
            output <- analysis(Schedule9798, Standings9697)
            Standings9798 <- output$Standings
            Schedule9798 <- output$Schedule
            output <- analysis(Schedule9899, Standings9798)
            Standings9899 <- output$Standings
            Schedule9899 <- output$Schedule
            output <- analysis(Schedule9900, Standings9899)
            Standings9900 <- output$Standings
            Schedule9900 <- output$Schedule
            output <- analysis(Schedule0001, Standings9900)
            Standings0001 <- output$Standings
            Schedule0001 <- output$Schedule
            output <- analysis(Schedule0102, Standings0001)
            Standings0102 <- output$Standings
            Schedule0102 <- output$Schedule
            output <- analysis(Schedule0203, Standings0102)
            Standings0203 <- output$Standings
            Schedule0203 <- output$Schedule
            output <- analysis(Schedule0304, Standings0203)
            Standings0304 <- output$Standings
            Schedule0304 <- output$Schedule
            output <- analysis(Schedule0506, Standings0304)
            Standings0506 <- output$Standings
            Schedule0506 <- output$Schedule
            output <- analysis(Schedule0607, Standings0506)
            Standings0607 <- output$Standings
            Schedule0607 <- output$Schedule
            output <- analysis(Schedule0708, Standings0607)
            Standings0708 <- output$Standings
            Schedule0708 <- output$Schedule
            output <- analysis(Schedule0809, Standings0708)
            Standings0809 <- output$Standings
            Schedule0809 <- output$Schedule
            output <- analysis(Schedule0910, Standings0809)
            Standings0910 <- output$Standings
            Schedule0910 <- output$Schedule
            output <- analysis(Schedule1011, Standings0910)
            Standings1011 <- output$Standings
            Schedule1011 <- output$Schedule
            output <- analysis(Schedule1112, Standings1011)
            Standings1112 <- output$Standings
            Schedule1112 <- output$Schedule
            output <- analysis(Schedule1213, Standings1112)
            Standings1213 <- output$Standings
            Schedule1213 <- output$Schedule
            output <- analysis(Schedule1314, Standings1213)
            Standings1314 <- output$Standings
            Schedule1314 <- output$Schedule
            output <- analysis(Schedule1415, Standings1314)
            Standings1415 <- output$Standings
            Schedule1415 <- output$Schedule
            output <- analysis(Schedule1516, Standings1415)
            Standings1516 <- output$Standings
            Schedule1516 <- output$Schedule
            output <- analysis(Schedule1617, Standings1516)
            Standings1617 <<- output$Standings
            Schedule1617 <<- output$Schedule
            output <- analysis(Schedule1718, Standings1617)
            Standings1718 <<- output$Standings
            Schedule1718 <<- output$Schedule
            rm(output)
            
            Correct <<- 0
            Incorrect <<- 0
            Total <<- 0
            
            
            Prop.Find <- function(Schedule){
              for(i in c(1:nrow(Schedule))){
                if(Schedule[i,1] < Sys.Date()){
                  if(Schedule[i, 9] > Schedule[i, 10]){
                    if(Schedule[i, 6] > 0){
                      Correct <<- Correct + 1
                      Total <<- Total + 1
                    }
                    else{
                      Incorrect <<- Incorrect + 1
                      Total <<- Total + 1
                    }
                  }
                  else if(Schedule[i,9] < Schedule[i, 10]){
                    if(Schedule[i, 6] < 0 ){
                      Correct <<- Correct + 1
                      Total <<- Total + 1
                    }
                    else{
                      Incorrect <<- Incorrect + 1
                      Total <<- Total + 1
                    }
                  }
                }
              }
            }
            
            Prop.Find(Schedule191718)
            Prop.Find(Schedule1819)
            Prop.Find(Schedule1920)
            
            Prop.Find(Schedule2021)
            Prop.Find(Schedule2122)
            Prop.Find(Schedule2223)
            Prop.Find(Schedule2324)
            Prop.Find(Schedule2425)
            Prop.Find(Schedule2526)
            Prop.Find(Schedule2627)
            Prop.Find(Schedule2728)
            Prop.Find(Schedule2829)
            Prop.Find(Schedule2930)
            
            Prop.Find(Schedule3031)
            Prop.Find(Schedule3132)
            Prop.Find(Schedule3233)
            Prop.Find(Schedule3334)
            Prop.Find(Schedule3435)
            Prop.Find(Schedule3536)
            Prop.Find(Schedule3637)
            Prop.Find(Schedule3738)
            Prop.Find(Schedule3839)
            Prop.Find(Schedule3940)
            
            Prop.Find(Schedule4041)
            Prop.Find(Schedule4142)
            Prop.Find(Schedule4243)
            Prop.Find(Schedule4344)
            Prop.Find(Schedule4445)
            Prop.Find(Schedule4546)
            Prop.Find(Schedule4647)
            Prop.Find(Schedule4748)
            Prop.Find(Schedule4849)
            Prop.Find(Schedule4950)
            
            Prop.Find(Schedule5051)
            Prop.Find(Schedule5152)
            Prop.Find(Schedule5253)
            Prop.Find(Schedule5354)
            Prop.Find(Schedule5455)
            Prop.Find(Schedule5556)
            Prop.Find(Schedule5657)
            Prop.Find(Schedule5758)
            Prop.Find(Schedule5859)
            Prop.Find(Schedule5960)
            
            Prop.Find(Schedule6061)
            Prop.Find(Schedule6162)
            Prop.Find(Schedule6263)
            Prop.Find(Schedule6364)
            Prop.Find(Schedule6465)
            Prop.Find(Schedule6566)
            Prop.Find(Schedule6667)
            Prop.Find(Schedule6768)
            Prop.Find(Schedule6869)
            Prop.Find(Schedule6970)
            
            Prop.Find(Schedule7071)
            Prop.Find(Schedule7172)
            Prop.Find(Schedule7273)
            Prop.Find(Schedule7374)
            Prop.Find(Schedule7475)
            Prop.Find(Schedule7576)
            Prop.Find(Schedule7677)
            Prop.Find(Schedule7778)
            Prop.Find(Schedule7879)
            Prop.Find(Schedule7980)
            
            Prop.Find(Schedule8081)
            Prop.Find(Schedule8182)
            Prop.Find(Schedule8283)
            Prop.Find(Schedule8384)
            Prop.Find(Schedule8485)
            Prop.Find(Schedule8586)
            Prop.Find(Schedule8687)
            Prop.Find(Schedule8788)
            Prop.Find(Schedule8889)
            Prop.Find(Schedule8990)
            
            Prop.Find(Schedule9091)
            Prop.Find(Schedule9192)
            Prop.Find(Schedule9293)
            Prop.Find(Schedule9394)
            Prop.Find(Schedule9495)
            Prop.Find(Schedule9596)
            Prop.Find(Schedule9697)
            Prop.Find(Schedule9798)
            Prop.Find(Schedule9899)
            Prop.Find(Schedule9900)
            
            Prop.Find(Schedule0001)
            Prop.Find(Schedule0102)
            Prop.Find(Schedule0203)
            Prop.Find(Schedule0304)
            # 2004-2005 Season Cancelled Due to Player Lockout
            Prop.Find(Schedule0506)
            Prop.Find(Schedule0607)
            Prop.Find(Schedule0708)
            Prop.Find(Schedule0809)
            Prop.Find(Schedule0910)
            
            Prop.Find(Schedule1011)
            Prop.Find(Schedule1112)
            Prop.Find(Schedule1213)
            Prop.Find(Schedule1314)
            Prop.Find(Schedule1415)
            Prop.Find(Schedule1516)
            Prop.Find(Schedule1617)
            Prop.Find(Schedule1718)
  


       
## Dropping Defunct Teams
dropped <- c()
for(i in c(1:nrow(Standings1718))){
  if(Standings1718[i, 4]==0){
    dropped <- append(dropped, i)
  }
}
Standings1718 = Standings1718[-dropped,]
Standings1718 = Standings1718[order(-Standings1718[, 8], Standings1718[, 4], 
                                    -Standings1718[, 9]),]

# Creating a Pop-up Table for Tonight's Games
 TodaysPredictions = data.frame()
 
 for(i in c(1:nrow(Schedule1718))){
   if(Schedule1718[i, 1] == Sys.Date()){
     TodaysPredictions = rbind(TodaysPredictions, Schedule1718[i,])
   }
 }
 
 TodaysPredictions = TodaysPredictions[,-c(4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16)]
 TodaysPredictions[,4] = percent(TodaysPredictions[,4])
 TodaysPredictions[,5] = percent(TodaysPredictions[,5])
 TodaysPredictions = rename(TodaysPredictions, c("P.H.WINS." = "Probability Home Team Wins", "P.A.WINS." = "Probability Away Team Wins"))
 Table <- gvisTable(TodaysPredictions, options=list(title="Today's Predictions Given the Complete History of the NHL"))
 plot(Table)
end = Sys.time()
end-start

