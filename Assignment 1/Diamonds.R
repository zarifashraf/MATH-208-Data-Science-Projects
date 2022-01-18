library(tidyverse)
data(diamonds)

mode(diamonds)
class(diamonds)

nrow(diamonds)
ncol(diamonds)

diamonds[12345,]$depth

diamonds_imp <- diamonds
diamonds_imp$x_imp <- diamonds$x/25.4
diamonds_imp$y_imp <- diamonds$y/25.4
diamonds_imp$z_imp <- diamonds$z/25.4
head(diamonds_imp)

over_under <- c()
for(i in 1:nrow(diamonds_imp))
{ d_delete <- diamonds_imp[-i,]
over_under[i] <- diamonds_imp$price[i]-median(d_delete[d_delete$color==diamonds_imp$color[i],]$price)}
diamonds_imp$over_under <- over_under

Expensive <- diamonds[diamonds$price>18800,]
Expensive
