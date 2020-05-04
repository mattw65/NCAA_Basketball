####################################
# APMA 3150 FINAL PROJECT ANALYSIS #
####################################
# Teddy Clark
# Selwyn Hector
# Matt Walsh
# Carl Zhang

# Analyzing NCAA Basketball Scores
# 1939-1995

# Read the basketball dataset
library(readxl)
data <- read_excel("basketball.xlsx")
basketball <- data.frame(data)

# Look at Some Data About the Set
# 1706 rows, 5 columns
dim(basketball)
dimnames(basketball)
is.matrix(basketball)
summary(basketball)

# Get years
years <- basketball[,1]
years

# Get scores of winners
winners_scores <- basketball[,c(3)]
winners_scores <- as.numeric(winners_scores)

# Get scores of losers
losers_scores <- basketball[,c(5)]
losers_scores <- as.numeric(losers_scores)

# Get scores overall
scores <- c(losers_scores, winners_scores)
scores <- as.numeric(scores)

mean(winners_scores)
# Winners average 77.17292
mean(losers_scores)
# Losers average 66.33646
mean(scores)
# Overall average is 71.75469

# Get differences between winners and losers
differences <- winners.scores - losers.scores
differences
# Average difference in score is 10.83646
mean(differences)
hist(differences)
# Histogram is skewed right, indicating that games are usually close.
# This makes sense given the high stakes of the NCAA tournament.
par(mfrow=c(1,2))

winner_fit <- lm(winners_scores ~ years)
# y = -365.1762 + .2233x
winner_fit
plot(years, winners_scores,
     main="Winner's Score Over Time",
     ylab="Winner Scores",
     xlab="Years")
abline(winner_fit, col="red")

loser_fit <- lm(losers_scores ~ years)
# y = -350.860 + .211x
loser_fit
plot(years, losers_scores,
     main="Loser's Score Over Time",
     ylab="Loser Scores",
     xlab="Years")
abline(loser_fit, col="red")

summary(winner_fit)$r.squared #.04999111
summary(loser_fit)$r.squared  #.05476326

# Residual Plots indicate random scatter
plot(residuals(loser_fit))
plot(residuals(winner_fit))

# Histograms of winners and losers scores show normal distribution
hist(winners_scores,
     main="Winner's Score Distribution",
     xlab="Score")
hist(losers_scores)

# Reformat dataset to be only numbers rather than characters for the scores
basketball.df <- data.frame(data)
basketball.df$Team.1.Score <- as.numeric(as.character(basketball.df$Team.1.Score))
basketball.df$Team.2.Score <- as.numeric(as.character(basketball.df$Team.2.Score))

# Counts of winners and losers scores
summary(as.factor(c(winners_scores, losers_scores)))[1:3]

list(winners_scores, losers_scores)

# Boxplot of winners and losers scores
boxplot(winners_scores, losers_scores)

par(mfrow=c(1,1))
qqnorm(winners_scores)
qqline(winners_scores)

qqnorm(losers_scores)
qqline(losers_scores)

# QQnorm plot
qqnorm(scores, main="Scoring QQplot")
qqline(scores)
plot(scores)

# Get scores by decade
df_40 <- subset(basketball.df, Year > 1939 & Year <= 1949)
df_50 <- subset(basketball.df, Year > 1949 & Year <= 1959)
df_60 <- subset(basketball.df, Year > 1959 & Year <= 1969)
df_70 <- subset(basketball.df, Year > 1969 & Year <= 1979)
df_80 <- subset(basketball.df, Year > 1979 & Year <= 1989)
df_90 <- subset(basketball.df, Year > 1989 & Year <= 1999)


# Plot 3 boxplots of scores by decade
# Winners scores, losers scores, all scores
par(mfrow=c(1,3))

boxplot(df_40$Team.1.Score, df_50$Team.1.Score, df_60$Team.1.Score, df_70$Team.1.Score, 
        df_80$Team.1.Score, df_90$Team.1.Score, names=c("40s", "50s", "60s", "70s", "80s", "90s"),
        main="Winning Team Scores")

boxplot(df_40$Team.2.Score, df_50$Team.2.Score, df_60$Team.2.Score, df_70$Team.2.Score, 
        df_80$Team.2.Score, df_90$Team.2.Score, names=c("40s", "50s", "60s", "70s", "80s", "90s"),
        main="Losing Team Scores")

boxplot(c(df_40$Team.1.Score, df_40$Team.2.Score), 
        c(df_50$Team.1.Score, df_50$Team.2.Score), 
        c(df_60$Team.1.Score, df_60$Team.2.Score), 
        c(df_70$Team.1.Score, df_70$Team.2.Score), 
        c(df_80$Team.1.Score, df_80$Team.2.Score), 
        c(df_90$Team.1.Score, df_80$Team.2.Score), 
        names=c("40s", "50s", "60s", "70s", "80s", "90s"),
        main="All Team scores")

# Analyze means and medians of individual decades
# > mean(c(df_40$Team.1.Score, df_40$Team.2.Score))
# [1] 48.37143
#> mean(c(df_50$Team.1.Score, df_50$Team.2.Score))
#[1] 69.90769
#> mean(c(df_60$Team.1.Score, df_60$Team.2.Score))
#[1] 73.4869
#> mean(c(df_70$Team.1.Score, df_70$Team.2.Score))
#[1] 76.98763
#> mean(c(df_80$Team.1.Score, df_80$Team.2.Score))
#[1] 70.75501
#> mean(c(df_90$Team.1.Score, df_90$Team.2.Score))
#[1] 74.0429

#> median(c(df_40$Team.1.Score, df_40$Team.2.Score))
#[1] 47
#> median(c(df_50$Team.1.Score, df_50$Team.2.Score))
#[1] 70
#> median(c(df_60$Team.1.Score, df_60$Team.2.Score))
#[1] 73
#> median(c(df_70$Team.1.Score, df_70$Team.2.Score))
#[1] 76
#> median(c(df_80$Team.1.Score, df_80$Team.2.Score))
#[1] 69
#> median(c(df_90$Team.1.Score, df_90$Team.2.Score))
#[1] 73

# Perform a one sample t test to see if mean of 1940s is different than 1990s
t.test(c(df_40$Team.1.Score, df_40$Team.2.Score), mu=mean(c(df_90$Team.1.Score, df_90$Team.2.Score)))
# P value is less than 2.2E-16
# Reject null hypothesis that two means are the same

# Perform a one sample t test to see if mean of 50s is different than 1990s
t.test(c(df_50$Team.1.Score, df_50$Team.2.Score), mu=mean(c(df_90$Team.1.Score, df_90$Team.2.Score)))
# P value is 1.402e-10
# Reject null hypothesis that means are the same

# T test for 80s vs 90s 
t.test(c(df_80$Team.1.Score, df_80$Team.2.Score), mu=mean(c(df_90$Team.1.Score, df_90$Team.2.Score)))
# P value is 4.578e-14
# Reject null hypothesis that means are the same

# Perform a wilcox test for 40s vs 90s (however we already know our data is normally distributed)
wilcox.test(c(df_40$Team.1.Score, df_40$Team.2.Score), c(df_90$Team.1.Score, df_90$Team.2.Score))
# P value is 2.2e-16 

# Show that distribution of data is normal so we can perform a t test
hist(scores, prob = T, main="NCAA Tournament Scoring", xlab="")
curve(dnorm(x, mean=mean(scores), sd=sd(scores)), add=T)

# Install package for special graphs
install.packages("tidyverse")

# Plot graphs for winners and losers score vs year again for reference
plot(basketball$Year, basketball$Team.1.Score)
abline(lm(basketball$Team.1.Score ~ basketball$Year))

plot(basketball$Year, basketball$Team.2.Score)
abline(lm(basketball$Team.2.Score ~ basketball$Year))

# Sort the score by team to see who has won the most
sort(table(basketball$Team.1),decreasing=TRUE) [1:5]
sort(table(basketball$Team.1),decreasing=FALSE)[1:5]

# Get number of wins for every team
sort(table(basketball$Team.1),decreasing=FALSE)

# Plot differences to view the maximum score difference between winner and losers
T1S<- as.numeric(basketball$Team.1.Score)
T2S<- as.numeric(basketball$Team.2.Score)
diff <- T1S-T2S
plot(diff)
max(diff) # 68
max(basketball$Team.1) # XAVIER (OHIO)

# See which teams have scored over 100 and how many times and against who
over100<- basketball[which(as.numeric(basketball$Team.1.Score)>100),]
sort(table(over100$Team.1), decreasing=TRUE)[1:5] # UCLA, KENTUCKY, UNLV, NORTH CAROLINA, DUKE
sort(table(over100$Team.2), decreasing=TRUE)[1:5] # KENTUCKY, LOYOLA-MARYMOUNT, MICHIGAN, AUBURN, AUSTIN PEAY

# Plot histogram of how many games were played per year. We have more data for later years
hist(as.numeric(basketball$Team.1.Score))
hist(as.numeric(basketball$Year))

# Use function to plot average score per year
avgperyear<-matrix(NA, nrow=1, ncol=57)
for (i in 1:57) {
        yeardat<-basketball[which(as.numeric(basketball$Year)==(1938+i)),]
        avg<-mean(as.numeric(yeardat$Team.1.Score))
        avgperyear[,i]<-avg
}
plot(1939:1995,avgperyear, col="red", main="Average Playoff Scores by Year", xlab="Year", ylab="Average Score")

# Install packages for animated plots
install.packages('gganimate')
install.packages('animation')
library(ggplot2)
library(gganimate)
library(dplyr)

#library(animate)

# Analyze who the top scoring schools are
tops<- matrix(NA, nrow=(57*5), ncol=6)
a<-1
for (i in 1939:1995) {
        for (c in 1:5) {
                tops[a,1]<-i
                a<-a+1
        }
}
# Initialize variables
ucla<-0
unc<-0
ken<-0
duke<-0
kansas<-0
y<-0
for (a in 1:(57)) {
        for (b in 1:1700) {
                if (as.numeric(basketball[b,]$Year)==(1938+a)) {
                        if (basketball[b,]$Team.1=="UCLA") {
                                ucla<-ucla+1
                        }
                        
                        if (basketball[b,]$Team.1=="NORTH CAROLINA") {
                                unc<-unc+1
                        }
                        
                        if (basketball[b,]$Team.1=="DUKE") {
                                duke<-duke+1
                        }
                        
                        if (basketball[b,]$Team.1=="KENTUCKY") {
                                ken<-ken+1
                        }
                        
                        if (basketball[b,]$Team.1=="KANSAS") {
                                kansas<-kansas+1
                        }
                        
                }
        }
        tops[5*(a-1)+1,2]<-ucla+10
        tops[5*(a-1)+2,2]<-unc+10
        tops[5*(a-1)+3,2]<-ken+10
        tops[5*(a-1)+4,2]<-duke+10
        tops[5*(a-1)+5,2]<-kansas+10
        tops[5*(a-1)+1,3]<-"UCLA"
        tops[5*(a-1)+2,3]<-"NORTH CAROLINA"
        tops[5*(a-1)+3,3]<-"KENTUCKY"
        tops[5*(a-1)+4,3]<-"DUKE"
        tops[5*(a-1)+5,3]<-"KANSAS"
}
tops<-data.frame(tops)
names(tops)<-c("Year", "Wins", "Team")
# Tops contains a data set of the top 5 teams with the most wins per year

# Plot an animated plot of these top teams and their wins throughout the years
ggplot(
        tops,
        aes(tops$Year, tops$Wins, group = Team, color = factor(Team))
) +
        geom_line() +
        scale_color_viridis_d() +
        labs(x = "Year", y = "Wins") +
        theme(legend.position = "top")+
        transition_reveal(as.numeric(Year))
anim_save("top5wins.gif")

# Look at qq plots of winning and losing teams to see if normal
qqnorm(team1score)
qqline(team1score)

qqnorm(team2score)
qqline(team2score)



