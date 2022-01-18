install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggplot")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidymodels")
install.packages("shiny")

library(here)
library(tidyverse)

data(midwest)
midwest_modified<-midwest %>% select(county,state,popdensity,
                                     popwhite,popblack,
                                     popamerindian,popasian,
                                     popother,inmetro)

str(midwest_modified)

midwest_modified %>% slice(1:5) %>%
  select(county:popblack)

midwest_modified %>% slice(1:5) %>%
  select(county,popamerindian:popother)

midwest_modified %>% group_by(state) %>% summarise(Highest_Pop_Den = max(popdensity)) 

tibble1 <- midwest_modified %>% mutate(Metro = ifelse(inmetro == 1, "Metro", "NonMetro"))
tibble1

dens_table <- tibble1 %>%  group_by(state, Metro) %>% summarise(Highest_Pop_Den = max(popdensity))


pivot_wider(dens_table, names_from = Metro, values_from = Highest_Pop_Den)

midwest_modified %>% slice(1:5) %>% select(county, popdensity) %>% mutate(HighDens = ifelse(popdensity > 1500, "High", "NotHigh"))

midwest_modified <- midwest_modified %>% mutate(Metro = ifelse(inmetro == 1, "Metro", "NonMetro"))

midwest_modified <- midwest_modified %>% mutate(HighDens = ifelse(popdensity > 1500, "High", "NotHigh"))

pop_xtabs<-xtabs(
  I(popwhite+popblack+popamerindian+popasian+popother)~
    state+Metro+HighDens,data=midwest_modified)
pop_xtabs

pop_xtabs["IL",1,2]

apply(pop_xtabs, 3, sum)

pop_xtabs<-xtabs(
  I(popwhite+popblack+popamerindian+popasian+popother)~
    state+HighDens+HighDens,data=midwest_modified)

pop_xtabs


pop_xtabs %>% mutate(freq = round(cnt / sum(cnt), 3))


findPercentage <- function(x) {
  
  prop.table(x)
}


prop.table(pop_xtabs, margin = 1) * 100




class(pop_xtabs)
dim(pop_xtabs)
dimnames(pop_xtabs)


