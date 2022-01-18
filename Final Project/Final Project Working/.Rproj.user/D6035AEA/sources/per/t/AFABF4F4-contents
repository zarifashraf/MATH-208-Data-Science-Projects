knitr::opts_chunk$set(
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE,
  fig.align = "center"
)



mydata <- read.csv("Final_Project_FlixGem.csv")

view(mydata)

mydata <- mydata %>% select(Title, Languages, Series.or.Movie, Hidden.Gem.Score, Runtime, Director, IMDb.Score, Rotten.Tomatoes.Score, 
                            Metacritic.Score, Release.Date, Summary)


mydata <- mydata %>% filter(Series.or.Movie == 'Movie')

mydata <- na.omit(mydata)

library(readxl)

mydata = mydata[complete.cases(mydata),]

mydata$Director= as.factor(mydata$Director)

mydata$Hidden.Gem.Score= round(mydata$Hidden.Gem.Score)

summary(mydata[, c(4,6)])

HG_H_index <- function(movie_scores){
  
  if (max(movie_scores) ==0) {
    return(0)
  }
  
  movie_scores= movie_scores[order(movie_scores, decreasing = TRUE)]
  
  tail(which(movie_scores >= seq_along(movie_scores)), 1)
}

HG_H_index_df = data.frame(Directors= unique(mydata$Director), HG_H_index = NA)

for(i in 1:nrow(HG_H_index_df)){
  HG_H_index_df$HG_H_index[i] = HG_H_index(mydata$Hidden.Gem.Score[mydata$Director == HG_H_index_df$Directors[i]])
}

HG_H_index_df = HG_H_index_df[order(HG_H_index_df$HG_H_index, decreasing = TRUE),] 

HG_H_index_df <- as.table(as.matrix(HG_H_index_df))

head(HG_H_index_df, 10)



