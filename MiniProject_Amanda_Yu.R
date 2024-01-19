#packages used
library(rvest)
library(dplyr)
library(stringr)

#scraping data from web page
clash_site <- 'https://clashroyale.fandom.com/wiki/Cards'
clash_page <- read_html(clash_site)
clash_table <- clash_page %>% html_nodes("table") %>% .[[1]]%>%html_table(fill=TRUE)
#source: https://www.youtube.com/watch?v=dP4_1ETMA5w&t=219s

#creating function to clean values and select numeric values
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
#source: http://stla.github.io/stlapblog/posts/Numextract.html

#creating data frame with cleaned data to perform regression analysis
clash.df <- na.omit(data.frame(Card = as.character(clash_table[[1]]),
                       Cost = as.numeric(clash_table[[2]]),
                       Health = as.numeric(numextract(gsub(",","",(clash_table[[3]])))),
                       Damage = as.numeric(numextract(gsub(",","",(clash_table[[4]])))),
                       HitSpeed = as.numeric(numextract(clash_table[[5]])),
                       DamageSec = as.numeric(numextract(clash_table[[6]])),
                       Spawn = as.numeric(numextract(clash_table[[7]])),
                       Range = as.numeric(numextract(clash_table[[8]])),
                       Count = as.numeric(numextract(clash_table[[9]]))))

attach(clash.df)

#predicting health using all variables except Card
healthRegression <- lm(Health ~ Cost + Damage + HitSpeed + DamageSec + Spawn + Range + Count, data = clash.df)
summary(healthRegression)  

clash.df$PredictedHealth <- predict(healthRegression, data = clash.df)

#removed insignificant variables
healthRegressionLowPValues <- lm(Health ~ Cost + Range + Count, data = clash.df)
summary(healthRegressionLowPValues)

clash.df$PredictedHealthLowPValues <- predict(healthRegressionLowPValues, data = clash.df)

#predicting damage using all variables except Card
damageRegression <- lm(Damage ~ Cost + Health + HitSpeed + DamageSec + Spawn + Range + Count, data = clash.df)
summary(damageRegression)  

clash.df$PredictedDamage <- predict(damageRegression, data = clash.df)

#removed insignificant variables
damageRegressionLowPValues <- lm(Damage ~ Cost + HitSpeed + DamageSec, data = clash.df)
summary(damageRegressionLowPValues)

clash.df$PredictedDamageLowPValues <- predict(damageRegressionLowPValues, data = clash.df)

