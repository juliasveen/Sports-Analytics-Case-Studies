library(dplyr)
library(caTools)
library(margins)
library(ggplot2)
getwd()
setwd( "/Users/juliasveen/Desktop")
WSOC_Shots=read.csv("WSOC_Shots.csv")
WSOC_Assists=read.csv("WSOC_Assists.csv")

WSOC_Shots$Goal <- ifelse(WSOC_Shots$shot.outcome.name == "Goal", 1, 0)
WSOC_Shots <- WSOC_Shots %>%mutate(under_pressure = 
                                  ifelse(is.na(under_pressure), 0, 
                                  as.integer(under_pressure)))

set.seed(123)
Shot_Test <- subset(WSOC_Shots, competition_id == 49)
Shot_Train <- subset(WSOC_Shots, competition_id != 49)

# xG Model 1 
  xG_1=glm(Goal~possession+
            DistToGoal+DistToKeeper+density.incone+distance.ToD1.360+
            distance.ToD2.360,data=Shot_Train,
            family="binomial")
  summary(xG_1)
  
  # marginal effects
  xG1_Margins=margins(xG_1)
  summary(xG1_Margins)
  
  # generate probs of making the shot using test data
  Shot_Test$GoalProb=predict(xG_1,newdata=Shot_Test,type="response")
  
  # validate model using brier score 
  mean((Shot_Test$Goal-Shot_Test$GoalProb)^2,na.rm = TRUE)
    # = 0.07937969
  
  # new logistic regression model
  MM_2=glm(WIN~ORTG+DRTG+SOS,family="binomial",data=MM_Train)
  summary(MM_2)  
  
# xG Model 2 
  xG_2=glm(Goal~DistToGoal+position.id+InCone.GK+DefArea+
             DistToKeeper+density.incone,data=Shot_Train,family="binomial")
  summary(xG_2)
  
  # marginal effects
  xG2_Margins=margins(xG_2)
  summary(xG2_Margins)
  
  # generate probs of making the shot using test data
  Shot_Test$GoalProb2=predict(xG_2,newdata=Shot_Test,type="response")
  
  # validate model using brier score 
  mean((Shot_Test$Goal-Shot_Test$GoalProb2)^2,na.rm = TRUE)
  # = 0.07970982
  
# xG Model 3 
  xG_3=glm(Goal~DistToGoal+DistToKeeper+density.incone+distance.ToD1.360+
             position.id+InCone.GK+DefArea+DistToKeeper,data=Shot_Train,
           family="binomial")
  summary(xG_3)
 
  # marginal effects
  xG3_Margins=margins(xG_3)
  summary(xG3_Margins)
  
  # generate probs of making the shot using test data
  Shot_Test$GoalProb3=predict(xG_3,newdata=Shot_Test,type="response")
  
  # validate model using brier score 
  mean((Shot_Test$Goal-Shot_Test$GoalProb3)^2,na.rm = TRUE)
  # = 0.07984583

# Final xG model (4th) 
  xG_Final=glm(Goal~possession+DistToGoal+DistToKeeper+density.incone+InCone.GK+
                 DefendersBehindBall+distance.ToD1.360,data=Shot_Train,
           family="binomial")
  summary(xG_Final)
  sum(Shot_Test$Goal) 
  97/1009 # = about 9.6%
  
  # marginal effects
  xGFinal_Margins=margins(xG_Final)
  summary(xGFinal_Margins)
  
  # generate probs of making the shot using test data
  Shot_Test$GoalProb4=predict(xG_Final,newdata=Shot_Test,type="response")
  
  # validate model using brier score 
  mean((Shot_Test$Goal-Shot_Test$GoalProb4)^2,na.rm = TRUE)
    # = 0.0793328
  
# Q1: assess performance of attackers 
  Shot_Test$xG=predict(xG_Final,newdata=Shot_Test,type='response',na.rm=TRUE)
  unique(WSOC_Shots$position.name) 
 
  WSOC_Attackers <- Shot_Test %>%
    filter(position.name == "Center Attacking Midfield" |
             position.name == "Left Attacking Midfield" |
             position.name == "Right Attacking Midfield") %>%
    group_by(player.name) %>%
    summarize(
      Shots = sum(!is.na(xG)),  # Count non-NA values for Shots
      xG = sum(xG, na.rm = TRUE),  # Sum xG while ignoring NAs
      Goals = sum(Goal, na.rm = TRUE),  # Sum Goals while ignoring NAs
      Performance = Goals-xG) %>% arrange(desc(Performance))
  
  head(WSOC_Attackers)
  tail(WSOC_Attackers)
  
# Q2: assess performance of each team
  Team_Performance <- Shot_Test %>%
    group_by(team.name) %>%
    summarize(
      Shots = sum(!is.na(xG)),  # Count non-NA values for Shots
      xG = sum(xG, na.rm = TRUE),  # Sum xG while ignoring NAs
      Goals = sum(Goal, na.rm = TRUE),  # Sum Goals while ignoring NAs
      Performance = Goals-xG) %>% arrange(desc(Performance))
  
  head(Team_Performance)
  tail(Team_Performance)
  
# scatterplots
  ggplot(WSOC_Attackers, aes(x = xG, y = Goals, label = player.name)) +
    geom_point()+geom_text(vjust = -0.25, hjust = -0.1)+
    labs(x = "Expected Goals (xG)", y = "Actual Goals", title = 
           "Players' Actual Goals vs. Expected Goals (xG)")+
    theme_light()  
  
  ggplot(Team_Performance, aes(x = xG, y = Goals, label = team.name)) +
    geom_point()+geom_text(vjust = -0.25, hjust = -0.1)+
    labs(x = "Expected Goals (xG)", y = "Actual Goals", title = 
        "Teams' Actual Goals vs. Expected Goals (xG)")+
    theme_light()  
