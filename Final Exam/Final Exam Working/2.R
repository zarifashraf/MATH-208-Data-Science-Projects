install.packages("gridExtra")

library(gridExtra)

data("midwest")

midwest_modified<-midwest %>% select(county,state,popdensity,
                                     popwhite,popblack,
                                     popamerindian,popasian,
                                     popother,inmetro)

midwest_modified <- midwest_modified %>% mutate(Metro = ifelse(inmetro == 1, "Metro", "NonMetro"))

midwest_modified <- midwest_modified %>% mutate(HighDens = ifelse(popdensity > 1500, "High", "NotHigh"))


str(midwest_modified)

midwest_modified %>% slice(1:5) %>%
  select(county:popblack)

midwest_modified %>% slice(1:5) %>%
  select(county,popamerindian:HighDens)

p1<-ggplot(midwest_modified,aes(x=popdensity)) + geom_histogram(bins=30,fill="white",col="black") +
  ggtitle("Plot 1") + theme_bw() + scale_x_log10()

p1

p2<-ggplot(midwest_modified,aes(x=popdensity)) + geom_density() +
  ggtitle("Plot 2") + theme_bw()+ scale_x_log10()

p2

p3<-ggplot(midwest_modified,aes(x=popdensity)) + geom_boxplot() +
  ggtitle("Plot 3") + theme_bw()+ scale_x_log10()

p3

grid.arrange(grobs=list(p1,p2,p3),nrow=3,ncol=1)


midwest_modified_new <- midwest_modified %>% pivot_longer(cols = popwhite:popother, names_to = "Race_Variable", values_to = "Count") %>% 
                        select(county:state, Metro, Race_Variable:Count) %>% mutate(Count = as.double(Count))

midwest_modified_new

ggplot(midwest_modified_new, aes(x= Metro,fill= Race_Variable,y= Count)) + geom_bar(stat="identity") + ggtitle("Plot f") + theme_bw()

metro_race_summaries <- midwest_modified_new %>% group_by(Metro, Race_Variable) %>% summarise(Race_Count = sum(Count)) %>% 
  mutate(Race_Count = as.double(Race_Count)) %>% mutate(Proportion = Race_Count/sum(Race_Count))

metro_race_summaries

ggplot(metro_race_summaries, aes(x = Metro, fill = Race_Variable,y = Proportion)) + geom_bar(stat = "identity") + ggtitle("Plot g") + theme_bw()


       