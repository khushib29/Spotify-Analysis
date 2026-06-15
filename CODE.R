install.packages("tidyverse")
library(tidyverse)
install.packages("psych")
library(psych)
install.packages("corrplot")
library(corrplot)
install.packages("GGally")
library(GGally)
install.packages("car")
library(car)
install.packages("IMTest")
library(IMTest)
install.packages("MASS")
library(MASS)

#checking missing values
colSums(is.na(high_popularity_spotify_data))

#percentage of missing 
missing_pct <- colSums(is.na(high_popularity_spotify_data))/nrow(high_popularity_spotify_data)*100

missing_pct

# total missinf values
sum(is.na(high_popularity_spotify_data))

# chexking duplicates
sum(duplicated(high_popularity_spotify_data))

#Convert Variables to Factors
high_popularity_spotify_data$playlist_genre <- as.factor(high_popularity_spotify_data$playlist_genre)

high_popularity_spotify_data$playlist_subgenre <- as.factor(high_popularity_spotify_data$playlist_subgenre)

high_popularity_spotify_data$mode <- as.factor(high_popularity_spotify_data$mode)

high_popularity_spotify_data$key <- as.factor(high_popularity_spotify_data$key)


#Descriptive Statistics

describe(
  high_popularity_spotify_data[,c(
    "track_popularity",
    "danceability",
    "energy",
    "loudness",
    "speechiness",
    "acousticness",
    "instrumentalness",
    "valence",
    "tempo"
  )]
)

#Skewness and Kurtosis
skewness(high_popularity_spotify_data$track_popularity)

kurtosis(high_popularity_spotify_data$track_popularity)


#Exploratory Data Analysis
#histogram

ggplot(high_popularity_spotify_data,
       aes(track_popularity))+
  geom_histogram(bins=20)

#box plot
ggplot(high_popularity_spotify_data,
       aes(y=track_popularity))+
  geom_boxplot()


#Association Analysis

# question 1 - is popularity associated with audio features
#Correlation Matrix

numdata <- high_popularity_spotify_data[,c(
  "track_popularity",
  "danceability",
  "energy",
  "loudness",
  "speechiness",
  "acousticness",
  "instrumentalness",
  "valence",
  "tempo"
)]

cor_matrix <- cor(numdata)

cor_matrix
round(cor_matrix,3)

#denaity plot
ggplot(high_popularity_spotify_data,
       aes(track_popularity))+
  geom_density()
# heatmap
corrplot(
  cor_matrix,
  method="color",
  type="upper"
)
#pearson correlation test

cor.test(
  high_popularity_spotify_data$track_popularity,
  high_popularity_spotify_data$danceability
)
#Group Comparison
#do genere differ in popularity
#boxplot
ggplot(
  high_popularity_spotify_data,
  aes(
    playlist_genre,
    track_popularity
  )
)+
  geom_boxplot()

#one way anova
#H0:all genre have same mean
anova1 <- aov(
  track_popularity~
    playlist_genre,
  data=high_popularity_spotify_data
)

summary(anova1)
#post hoc
TukeyHSD(anova1)

#Mode Comparison
#boxplot
ggplot(
  high_popularity_spotify_data,
  aes(
    mode,
    track_popularity
  )
)+
  geom_boxplot()
#independent t test
t.test(
  track_popularity~mode,
  data=high_popularity_spotify_data
)
#Chi-Square Test
#Is genre associated with mode?
table1 <- table(
  high_popularity_spotify_data$playlist_genre,
  high_popularity_spotify_data$mode
)

table1
chisq.test(table1)
#Multiple Linear Regression
#Main Prediction Model
model1 <- lm(
  track_popularity~
    danceability+
    energy+
    loudness+
    speechiness+
    acousticness+
    instrumentalness+
    valence+
    tempo,
  data=high_popularity_spotify_data
)
capture.output(
  summary(model1),
  file="regression_results.txt"
)

summary(model1)
#Model Diagnostics
#residual plot
plot(model1,1)
#qq plot
plot(model1,2)
#scale location
plot(model1,3)
#cook distance
plot(model1,4)


#multi collinearity
vif(model1)
