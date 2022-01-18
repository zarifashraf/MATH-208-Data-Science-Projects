library(tidyverse)

exam_data <- dget("exam.txt")

exam_data
class(exam_data)

options(max.print=1000)

exam_data[ 1:10, 1:20, c(1,2,3)]

step1 <- apply(exam_data, c(1,3), sum)

step2 <- apply(step1, 2, mean)

step2

pip <- apply(exam_data, c(1,3), sum)
 
pip

pip1 <- pip[ 1:1000 , 1]
pip2 <- pip[ 1:1000 , 2]
pip3 <- pip[ 1:1000 , 3]

class(pip1)

sort(pip1, decreasing = TRUE)
sort(pip2, decreasing = TRUE)
sort(pip3, decreasing = TRUE)

exam_data

step3 <- apply(exam_data, c(2,3), mean)
step3


class(step3)

matplot(as.numeric(rownames(step3)), step3, type = "b" , pch = 16, lwd = 5, lty = 1, xlab = "ItemNum", ylab = "Average Overall Score", 
        ylim = c(0,5), col = c(1:3))
legend(x = 15, y = 1.5, legend = colnames(step3), lwd = 2, col = c(1:3), cex = 1.0, title = "TimePoint")
