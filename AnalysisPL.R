install.packages("tidyverse")
library("tidyverse")
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
#checking working directory
getwd()

#import .csv file to Global Environment
EplData <- read.csv("EplResults93-21.csv", header = TRUE, sep = ",")
EplData

#drop NA values using libraries
clean <- drop_na(EplData)

#extract month and date
str(clean)
clean$DateTimeN <- strptime(clean$DateTime, format = "%Y-%m-%dT%H:%M:%SZ")
clean$Day <- format(clean$DateTimeN, "%d")
clean$Month <- format(clean$DateTimeN, "%m")
clean <- clean[, !(names(clean) %in% c("DateTime", "DateTimeN"))]

#looking for data insight using pivot table
install.packages("psych")
library(psych)
pivot <- clean[ , c("Season", "FTHG", "FTAG", "HTHG", "HTAG", "HS", "AS", "HST", "AST", "HC", "AC", "HF", "AF", "HY", "AY", "HR", "AR", "Month", "Day")]
insight <- describe(pivot)

#VISUALISATION
#Goals Per Season
install.packages(ggsci)
install.packages(ggpubr)
library(ggsci)
library(ggpubr)
seasonwise <- aggregate(cbind(FTHG, FTAG, HTHG, HTAG, HS, AS, HST, AST, HC, AC, HF, AF, HY, AY, HR, AR) ~ Season, pivot, sum)
seasonwise <- seasonwise %>% mutate(GOALS=FTHG+FTAG)
ggplot(data=seasonwise, aes(x=GOALS, y=Season, fill=Season)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Goals Scored Per Season")

#compare Home Goals VS Away Goals
Epl_melted <- seasonwise[ , c("Season", "FTHG", "FTAG")]
EPL_melt = melt(Epl_melted)
EPL_melt <- rename(EPL_melt, HA = variable, GOALS= value)
ggplot(data=EPL_melt, aes(x=GOALS, y=Season, fill=HA)) +
  geom_bar(stat = "identity", position = "dodge")+
  theme(legend.title = element_blank())+
  scale_fill_discrete(labels= c('Home Goals', 'Away Goals')) +
  labs(title = "Home Goals VS Away Goals")

#Plotting a heatmap for distribution of goals per month
install.packages("reshape2")
library(reshape2)
install.packages("dplyr")
library(dplyr)
EPL_heat <- aggregate(cbind(FTHG, FTAG) ~ Month + Season, pivot, sum)
EPL_heat <- EPL_heat %>% mutate(GOALS=FTHG+FTAG)
EPL_heat <- EPL_heat[, !(names(EPL_heat) %in% c("FTHG", "FTAG"))]
EPL_heat <- pivot_wider(EPL_heat, names_from = Month, values_from = GOALS)
EPL_heat <- EPL_heat[-22,]
EPL_heat <- melt(EPL_heat)
colnames(EPL_heat) <- c("Season", "Month", "GOALS")
ggplot(EPL_heat, aes(Season, Month))+
  geom_tile(aes(fill= GOALS))+
  scale_fill_gradient(low = "green", high = "red")+
  labs(title ='Goals Distribution based on month' )

#GOALS PER MATCH
EPL_count <- aggregate(FTHG ~ Month + Season, pivot, length)
EPL_count <- pivot_wider(EPL_count, names_from = Month, values_from = FTHG)
EPL_count <- EPL_count[-22,]
EPL_count <- melt(EPL_count)
colnames(EPL_count) <- c("Season", "Month", "FTHG")
EPL_heat$GM = round(EPL_heat$GOALS / EPL_count$FTHG)
FINAL <- EPL_heat[, -3]
ggplot(EPL_heat, aes(Season, Month))+
  geom_tile(aes(fill= GM))+
  scale_fill_gradient(low = "green", high = "red")+
  labs(title = 'Goals per match based on month')

#COMEBACKS AFTER HALFTIME
install.packages("dplyr")
library(dplyr)
EPL15 <- clean %>% slice(c(5701:8050))
length(unique(HCOMEBACKS$HomeTeam))
FPL_result <- EPL15[ , c("Season", "HomeTeam", "AwayTeam", "HTR", "FTR")]
FPL_result <- FPL_result[FPL_result$HTR != "D", ]
FPL_result$Results <- if_else(FPL_result$HTR == FPL_result$FTR, "No Comeback", "Comeback")
COMEBACKS <- FPL_result[FPL_result$Results == "Comeback", ]
NOCOMEBACKS <- FPL_result[FPL_result$Results == " No Comeback", ]
HCOMEBACKS <- COMEBACKS[COMEBACKS$HTR == "A", ]
ACOMEBACKS <- COMEBACKS[COMEBACKS$HTR == "H", ]
ANALYSIS <- data.frame("Comeback", "HomeComeback", "AwayComeback")
ANALYSIS$Comeback <- length(COMEBACKS$Results == "Comeback" )
ANALYSIS$HomeComeback <- length(HCOMEBACKS$Results == "Comeback")
ANALYSIS$AwayComeback <- length(ACOMEBACKS$Results == "Comeback")
ANALYSIS <- ANALYSIS[ -c(1:3)]
BHCOMEBACKS <- HCOMEBACKS[HCOMEBACKS$HomeTeam %in% c("Arsenal", "Liverpool", "Chelsea", "Man United", "Tottenham", "Man City"), ]
BACOMEBACKS <- ACOMEBACKS[ACOMEBACKS$HomeTeam %in% c("Arsenal", "Liverpool", "Chelsea", "Man United", "Tottenham", "Man City"), ]
ANALYSIS$BIG6HomeComeback <- length(BHCOMEBACKS$Results == "Comeback")
ANALYSIS$BIG6AwayComeback <- length(BACOMEBACKS$Results == "Comeback")
ANALYSIS <- melt(ANALYSIS)
colnames(ANALYSIS) <- c("CASES", "NoOfComebacks")
ggplot(data=ANALYSIS, aes(x=CASES, y=NoOfComebacks, fill= CASES))+
  geom_bar(stat = "identity")+
  labs(title = "Number of Comebacks for different cases")+
  scale_fill_manual(values = c("Comeback" = "red",
                               "HomeComeback" = "blue",
                               "AwayComeback" = "green",
                               "BIG6HomeComeback" = "yellow",
                               "BIG6AwayComeback" = "pink"))+
  geom_text(aes(label = NoOfComebacks), vjust = 0)