install.packages("ggpubr")


library(here)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(knitr)

options(scipen = 999)

mydata <- read.csv("Final_Project_FlixGem.csv")


mydata <- mydata %>% select(Title, Languages, Series.or.Movie, Hidden.Gem.Score, Runtime, Director, IMDb.Score, Rotten.Tomatoes.Score, 
                            Metacritic.Score, Release.Date, Summary)


mydata <- mydata %>% filter(Series.or.Movie == 'Movie')

nrow(mydata)
ncol(mydata)

mydata <- na.omit(mydata)

nrow(mydata)
ncol(mydata)
view(mydata)

plot1 <- ggplot(mydata,aes(x= Runtime, y= Hidden.Gem.Score, fill = Runtime)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() +  ggtitle("Plot 1") + theme_bw()
plot1

class(mydata$Runtime)


testdata <- mydata %>% mutate(FirstLanguage = sub(",.*", "", mydata$Languages)) 

plot2 <- ggplot(testdata,aes(x= Hidden.Gem.Score, y= FirstLanguage, fill = FirstLanguage))  + 
          stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() + ggtitle("Plot 2") + theme_bw()
plot2

langGem_summaries <- with(testdata, tapply(Hidden.Gem.Score, data.frame(FirstLanguage), mean))

langGem_summaries <- sort(langGem_summaries, decreasing = TRUE)
langGem_summaries <- langGem_summaries %>% kable(col.names = "Mean")

langRepeat <- table(testdata$FirstLanguage)
langRepeat <- sort(langRepeat, decreasing = TRUE)
langRepeat <- langRepeat %>% kable()

lang_n_Gem <- testdata %>% select(FirstLanguage, Hidden.Gem.Score)

mean(lang_n_Gem$Hidden.Gem.Score)

lang_n_Gem <- lang_n_Gem %>% mutate(Score_Range = cut(Hidden.Gem.Score, c(0, 3.55, 9.2))) %>% group_by(FirstLanguage)

lang_n_Gem_array <- xtabs(~FirstLanguage+Score_Range, data = lang_n_Gem)
lang_n_Gem_array


column_props <- apply(lang_n_Gem_array, c("Score_Range", "FirstLanguage"), sum) %>% prop.table(.,c(2))

plot3 <- barplot(column_props, col = lang_n_Gem$Score_Range, las = 2, cex.names = 0.6)
legend("topright", fill = lang_n_Gem$Score_Range, legend = levels(factor(lang_n_Gem$Score_Range)), title = "Score Range")


testdata <- testdata %>% mutate(Number_Of_Languages = (str_count(mydata$Languages, ',') + 1))

testdata <- testdata %>% mutate(Num_Languages = as.character(testdata$Number_Of_Languages))

class(testdata$Num_Languages)

view(testdata)
                                
plot4 <- ggplot(testdata,aes(x= Num_Languages, y= Hidden.Gem.Score, fill = Num_Languages)) + 
  stat_boxplot(geom = "errorbar", width = 0.25) + geom_boxplot() +  ggtitle("Plot 4") + theme_bw()


testdata %>% group_by(Num_Languages) %>% summarise(Mean = mean(Hidden.Gem.Score))

plot5 <- ggplot(mydata,aes(x= IMDb.Score, y= Hidden.Gem.Score)) + geom_point() + geom_smooth(method = "lm") + theme_bw()

plot6 <- ggplot(mydata,aes(x= Rotten.Tomatoes.Score, y= Hidden.Gem.Score)) + geom_point() + geom_smooth(method = "lm", col = "red") + theme_bw()

plot7 <- ggplot(mydata,aes(x= Metacritic.Score, y= Hidden.Gem.Score)) + geom_point() + geom_smooth(method = "lm", col = "darkgreen") + theme_bw()

figure <- ggarrange(plot5,plot6, plot7,
                    labels = c("IMDb", "Rotten Tomatoes", "Metacritic"),
                    ncol = 1, nrow = 3)
figure


testdata <- testdata %>% mutate(Release_Year = sub("-.*", "", testdata$Release.Date))

testdata <- testdata %>% mutate(Release_Year = as.numeric(testdata$Release_Year))

testdata <- testdata %>% mutate(Release_Year_Range = cut(Release_Year, c(1920, 1945, 1970, 1995, 2021)))

testdata <- testdata %>% mutate(ReleaseYear_Range = case_when(Release_Year_Range == "(1.92e+03,1.94e+03]" ~ "1920-1945", 
                                                              Release_Year_Range == "(1.94e+03,1.97e+03]" ~ "1945-1970",
                                                              Release_Year_Range == "(1.97e+03,2e+03]" ~ "1970-1995",
                                                              Release_Year_Range == "(2e+03,2.02e+03]" ~ "1995-2020"))

testdata <- testdata %>% select(Title:FirstLanguage, Num_Languages, Release_Year, ReleaseYear_Range)


ReleaseSummaries <- testdata %>% group_by(ReleaseYear_Range, Runtime) %>% summarise(MeanScore = mean(Hidden.Gem.Score))

ReleaseSummaries <- ReleaseSummaries %>% kable()

ReleaseSummaries



