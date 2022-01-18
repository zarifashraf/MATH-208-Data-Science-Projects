library(tidyverse)

board_of_directors <- list(  
  
  Exec = tibble(    
    Name=c("Grace Yi", "Bruno Remillard"),      
    Position=c("President", "President-Elect"),    
    Term_End=c("2022-06-30","2022-06-30")
  ),    
  
  Regional_Reps = list(    
    Atlantic_Region = tibble(    
      Name=c("Michael McIsaac", "Wilson Lu"),      
      Term_End=c("2022-06-30","2023-06-30")      
    ),      
    Quebec=tibble(    
      Name=c("Paramita Saha Chaudhuri", "Cody Hyndman",    
             "Johanna Neslehova", "Denis Talbot"),      
      Term_End=c("2022-06-30","2022-06-30","2023-06-30","2023-06-30")       
    )
  )
)

board_of_directors$Exec[1,2]
board_of_directors[[2]][1,]
board_of_directors[[2]][1]
board_of_directors[2][[1]][[1]]

result = board_of_directors[[2]][[2]]
result[3,1]

board_of_directors$Exec[2]

result[result$Name=="Paramita Saha Chaudhuri",]

result[c(1,3),]
