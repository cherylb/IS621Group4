





library(RCurl)
library(rjson)
library(dplyr)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(lattice)


# count missing data
countMissing <- function(input){
  d <- input[is.na(input) != FALSE]
  return(length(d))
}
countdfMissing <- function(input){
  z<- sapply(input, function(x) countMissing(x))
  return(z)
}



# summary
summary(df)

smdf <- data.frame(sapply(df,mean))
smdf$median <- sapply(df,median)
smdf$sd <- sapply(df,sd)

(smdf)


# of missing records
(countMissing(df))
(countdfMissing(df))

#Clean Data
#
# Replace NA values with 0 - this assumes the NA is a zero value, not missing data.
# seems valid there would be many zero values for the variables like TEAM_BASERUN_CS which  is
# the number of stolen bases - that sort of thing
# 
df_clean <- df
df_clean[is.na(df_clean)] <- 0

# # these calculate the mean values for existing baserun data - could use this method to replace NA values
# # if the zero method (above) isn't valid - commeted out 
# m1 <- mean((df_clean%>%filter(!(is.na(TEAM_BASERUN_CS))))$TEAM_BASERUN_CS)
# m2 <- mean((df_clean%>%filter(!(is.na(TEAM_FIELDING_DP))))$TEAM_FIELDING_DP)
# 
# df_clean <- df_clean %>% mutate(TEAM_BASERUN_CS = ifelse(is.na(TEAM_BASERUN_CS), m1, TEAM_BASERUN_CS))
# df_clean <- df_clean %>% mutate(TEAM_FIELDING_DP = ifelse(is.na(TEAM_FIELDING_DP), m2, TEAM_FIELDING_DP))
# # possibly remove variable with very few entries 
# df_clean <- df_clean%>%select(-TEAM_BATTING_HBP)


# removing Index 
df_clean <- df_clean%>%select(-INDEX)


## scatterplot matirx - looking and relationship between different variables - takes a while to run

splom(df_clean, 
      main="Baseball Data")


## corr matrix - writes to CSV to review

(cm <- cor(df_clean))
write.csv(file='correlation', x= cm)

#Density Plots - looking at distribution of interesting variables

g1 <- ggplot(data = df_clean, aes(x = TARGET_WINS) )+ 
  geom_density(alpha = .2, fill = "003333")+
  ggtitle("Target Wins")


g2 <- ggplot(data = df_clean, aes(x = TEAM_BATTING_H) )+ 
  geom_density(alpha = .2, fill = "#CC6666")+
  ggtitle("Base hits by batters")


g3 <- ggplot(data = df, aes(x = TEAM_BATTING_BB) )+ 
  geom_density(alpha = .2, fill = "#66CC00")+
  ggtitle("Walks by Batters")

g4 <- ggplot(data = df, aes(x = TEAM_PITCHING_HR) )+ 
  geom_density(alpha = .2, fill = "#9933CC")+
  ggtitle("Home Runs Allowed")


grid.arrange(g1,g2,g3,g4, ncol=2, top ="Density Plots")


#Box Plots

g1<- ggplot(data = df_clean, aes(x=1, y = TARGET_WINS)) + geom_boxplot(fill = "white", colour = "#3399CC", 
                                                outlier.colour = "red", outlier.shape = 1)+
  xlab("") + ylab("wins") +
  ggtitle("Wins")

g2<- ggplot(data = df_clean, aes(x=2, y = TEAM_BATTING_H)) + geom_boxplot(fill = "white", colour = "#CC6666", 
                                                         outlier.colour = "blue", outlier.shape = 1)+
  xlab("") + ylab("Batting H") +
  ggtitle("Base hits by batters")

g3<- ggplot(data = df_clean, aes(x=2, y = TEAM_BATTING_BB)) + geom_boxplot(fill = "white", colour = "#66CC00", 
                                                            outlier.colour = "red", outlier.shape = 1)+
  xlab("") + ylab("Walks") +
  ggtitle("Walks by Batters")

g4<- ggplot(data = df_clean, aes(x=2, y = TEAM_PITCHING_HR)) + geom_boxplot(fill = "white", colour = "#9933CC", 
                                                           outlier.colour = "red", outlier.shape = 1)+
  xlab("") + ylab("HR allowed") +
  ggtitle("Pitching - HR Allowed")


grid.arrange(g1,g2,g3,g4, ncol=2, top ="Boxplots")
