# R presentation. Spotify tracks dataset

#clear the environment 
rm(list=ls())

#install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("GGally")

#open libraries
library(dplyr)
library(ggplot2)
library(tidyverse) 
library(GGally)

#open the dataset, omit NA
load("C:/Users/olu.MINOR/Downloads/data.Rda")
data <- na.omit(data)

#take a random subsample
#data <- data[sample(nrow(data), size=10000), ]

#save random subsample 
#save(data,file="data.Rda")

########################################
#A BIT OF EXPLORATION
glimpse(data)

########################################
#RECODING VARIABLES

#recode duration from miliseconds to minutes
data$duration_min <- data$duration_ms/60000
data$duration_min <- round(data$duration_min, digits = 1)

#explicit - binary variable 
is.factor(data$explicit)
data$explicit <- as.factor(data$explicit)


#only keep year and no months
mode(data$release_date)
data$release_date <- as.Date(data$release_date)
data$year <- format(as.Date(data$release_date, format= "%Y/%m/%d"),"%Y")
table(data$year)

########################################
#MAXIMA AND MINIMA OF THE VARIABLES, QUANTILES

#Duration of songs
max(data$duration_min)
min(data$duration_min)

#Popularity
max(data$popularity)
quantile(data$popularity, na.rm = FALSE)

#Energy
max(data$energy)

#Danceability
max(data$danceability)

#Quietness
min(data$loudness)

#############################################
#GRAPHS

#song duration in minutes
#filter to get a prettier graph
duration_cut <- subset(data, data$duration_min < 20) 

#histogram before cut
table(data$duration_min)
hist(data$duration_min, breaks = 30,
     main = ("Distribution of Song Duration"), xlab = "Duration",
     ylab = "Count")

#histogram after cut
hist(duration_cut$duration_min, breaks = 30,
     main = ("Distribution of Song Duration"), xlab = "Duration",
     ylab = "Count")

#popularity
table(data$popularity)

hist(data$popularity,
     main = ("Distribution of Popularity"), xlab = "Popularity", ylab = "Count",
     )

#barplot for explicitness
p <- ggplot(data, aes(x = explicit)) + geom_bar() +
  labs(x = "Expliciteness all time", y = "Count") +
  theme_gray()

print(p)

#############################################
#haüfigkeiten?
#relative haüfigkeiten expliciteness all time
data %>%
  group_by(explicit) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#relative haüfigkeiten expliciteness 2021
exp2021 <- subset(data, data$year == 2021)
p21 <- ggplot(exp2021, aes(x = explicit)) + geom_bar() +
  labs(x = "Expliciteness 2021", y = "Count") +
  theme_gray()

print(p21)

exp2021 %>%
  group_by(explicit) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#haüfigkeiten expliciteness 1922
exp1922 <- subset(data, data$year == 1922)
p1922 <- ggplot(exp1922, aes(x = explicit)) + geom_bar() +
  labs(x = "Expliciteness 2021", y = "Count") +
  theme_gray()
print(p1922)


################################

# #expliciteness over time?
# table(data$exp)
# 
# aggregate(data["explicit"],by=data["year"],sum)
# 
# table(data$explicit)
# 
# exp_plot <- ggplot(data, aes(x = label_exp, y = year)) +
#   geom_col()
# print(exp_plot)

###############################
#VERY IMPORTANT QUESTIONS

#how long are most songs?
quantile(data$duration_min, na.rm = FALSE) # interesting stuff here
#what is the longest song?
long <- subset(data, data$duration_min == 71.1)
print(long)
#what is the shortest song?
short <- subset(data, data$duration_min == .1)
print(short)
#what is the most popular song?
#(in dem ungefilterten Dataset war das Justin Bieber,
#die einzige Eingabe mit 100)
pop <- subset(data, data$popularity == 91)
print(pop)
#most energetic song 
e <- subset(data, data$energy == 1)
print(e)
#most danceable song
dance <- subset(data, data$danceability == 0.979)
print(dance)
#quietest song
loud <- subset(data, data$loudness == -60)
print(loud)

#some correlation tables and plots for them
#danceability - popularity
cor.test(data$danceability, data$popularity)

plot(data$danceability, data$popularity,
     xlab = "Danceability", ylab = "Popularity")

#liveness-acousticness
cor.test(data$liveness, data$acousticness)

plot(data$liveness, data$acousticness,
     xlab = "Liveness", ylab = "Acousticness")

#energy - popularity
cor.test(data$energy, data$popularity)
plot(data$energy, data$popularity,
     xlab = "Energy", ylab = "Popularity")

#instrumentalness - length
cor.test(data$instrumentalness, data$duration_min)
plot(data$duration_min, data$instrumentalness,
     ylab = "Instrumentalness", xlab = "Duration (Minuten)")

#speechiness - length
cor.test(data$speechiness, data$duration_min)
plot(data$duration_min, data$speechiness,
     ylab = "Speechiness", xlab = "Duration (Minuten)")

########################################################
#Linear regression?
#What influences popularity?
########################################################

lmpop = lm(popularity ~ energy + danceability + duration_min + 
             acousticness + speechiness + loudness, data = data)
summary(lmpop)

#checking the model assumptions -> residuals should be
#approximately normal
hist(residuals(lmpop))

# heteroskedasticity = fitted value vs residuals plot
plot(fitted(lmpop), residuals(lmpop))

#add horizontal line at 0
abline(h = 0, lty = 2) #looks very fucking weird


########################################################
#last line