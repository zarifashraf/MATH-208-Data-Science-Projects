library(rpart)
library(rpart.plot)

data <- read.csv("C:/Users/User/Desktop/Final_Project_FlixGem.csv")
head(data)
names(data)


set.seed(123)
train.index=sample(1:dim(data)[1],dim(data)[1]*0.7)
train=data[train.index,]
valid=data[-train.index,]


model <- rpart(Hidden.Gem.Score~Languages+Runtime+IMDb.Score+Rotten.Tomatoes.Score+Metacritic.Score, data, parms=list(split=c("information","gini")),
      cp = 0.01, minsplit=20, minbucket=7, maxdepth=30)

summary(model)

dim(data)


default.model <- rpart(Hidden.Gem.Score~Languages+Runtime+IMDb.Score+Rotten.Tomatoes.Score+Metacritic.Score, data = train)
info.model <- rpart(Hidden.Gem.Score~Languages+Runtime+IMDb.Score+Rotten.Tomatoes.Score+Metacritic.Score, data = train, parms=list(split="information"))

overfit.model <- rpart(Hidden.Gem.Score~Languages+Runtime+IMDb.Score+Rotten.Tomatoes.Score+Metacritic.Score, data = train,
                       maxdepth= 5, minsplit=2,
                       minbucket = 1)

one.rule.model <- rpart(Hidden.Gem.Score~Languages+Runtime+IMDb.Score+Rotten.Tomatoes.Score+Metacritic.Score, data=train, maxdepth = 1)
rpart.plot(one.rule.model, main="Single Rule Model")

super.overfit.model <- rpart(Hidden.Gem.Score~Languages+Runtime+IMDb.Score+Rotten.Tomatoes.Score+Metacritic.Score, data = train, minsplit=2,
                       minbucket = 1, cp = 0.0001)
rpart.plot(super.overfit.model, main = "Really Overfit")

cost.driven.model <- rpart(Hidden.Gem.Score~Languages+Runtime+IMDb.Score+Rotten.Tomatoes.Score+Metacritic.Score,data=train,
                           parms=list(
                             loss=matrix(c(0,1,5,0),
                                         byrow=TRUE,
                                         nrow=2))
                           )


































