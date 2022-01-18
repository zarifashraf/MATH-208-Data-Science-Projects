library(palmerpenguins)
library(ggplot2)
library(gridExtra)

data(penguins)
head(penguins)
names(penguins)
class(penguins)



one_plot_fun <- function(input_data, x_var, y_var) {
  
  if ((x_var) %in% colnames(input_data) && ((y_var) %in% colnames(input_data))) {
  
  answer <- ggplot(input_data, aes_string(x=x_var, y=y_var)) + geom_point()
  return(answer)
}
  
  else {
    error <- "At least one variable not contained in input_data"
    return(error)
  }

}

one_plot_fun(input_data=as_tibble(penguins),x_var="bill_length_mm",y_var="body_mass_g")
one_plot_fun(input_data=as_tibble(penguins),x_var="bill_bull_mm",y_var="body_mass_g")


many_plots_fun <- function(big_data, arg1) {
  
  P <- list()  
  
  for (xx_var in arg1) {
    
    for (yy_var in arg1) {
      
      response <- (one_plot_fun(input_data = big_data, x_var = xx_var, y_var = yy_var))
      
      if(xx_var != yy_var) {
        
        P <- c(P, list(response))  
        
      }
    }
  }
  
  return(P)
}


my_obj <- many_plots_fun(penguins, c("bill_length_mm","body_mass_g","flipper_length_mm"))
y_obj <- many_plots_fun(penguins, c("foot_length_mm","body_mass_g","flipper_length_mm"))

str(my_obj,max.level=1)
str(y_obj, max.level =1)

gridExtra::marrangeGrob(my_obj,nrow=3,ncol=2)
gridExtra::marrangeGrob(y_obj,nrow=3,ncol=2)
