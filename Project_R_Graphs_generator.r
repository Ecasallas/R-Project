#Code to generate graphical representations of the different distributions we will use.

# -----------------------------------------------------------------------------------------------------------------------
#   Project: R - The perverse procedure. 
#   Subject: Programación y Estadística en R
#   Máster en Bioinformática y Biología Computacional – UAM
#   Academic year: 2025–2026
#
#   Authors: CHEN XI YE XU, GEMMA ROZALEN, ESTEFANI CASALLAS, MARIAN MENDOZA
#   Teacher: RAMÓN DÍAZ URIARTE
#
#   Description: we want to check through a series of simulations how only using t-test, only using wilcoxon or using first a normality 
#                test such as Shapiro-Wilk to decide what kind of test to apply will affect the type I error and power of the tests. 
#                Using a normality test before choosing whether to use a parametric or non-parametric test is being called "the perverse
#                procedure" because of the problems it causes with the simplification of a choice that should be carefully pondered about by 
#                looking and understanding the data. 
#
# -----------------------------------------------------------------------------------------------------------------------

library(ggplot2)

plot_drawing <- function(size, distribution, effect = 0) {
  
  # Simulate the data. 
  if (distribution == "normal") {
    g1 <- rnorm(size, mean = 0, sd = 1)
    g2 <- rnorm(size, mean = 0, sd = 1) + effect
  } 
  else if (distribution == "exp") {
    g1 <- rexp(size, rate = 1)
    g2 <- rexp(size, rate = 1) + effect
  } 
  else if (distribution == "t3") {
    g1 <- rt(size, df = 3)
    g2 <- rt(size, df = 3) + effect
    
  } 
  else if (distribution == "lognorm") {
    g1 <- rlnorm(size, meanlog = 0, sdlog = 1)
    g2 <- rlnorm(size, meanlog = 0, sdlog = 1) + effect
    
  } 
  else if (distribution == "uniform") {
    g1 <- runif(size, min = -1, max = 1)
    g2 <- runif(size, min = -1, max = 1) + effect
    
  } 
  else if (distribution == "bimodal") {
    # Mixture of two normal distributions.
    choice <- runif(size) < 0.5 
    g1 <- rnorm(size, mean = ifelse(choice, -1, 1), sd = 1)
    g2 <- rnorm(size, mean = ifelse(choice, -1, 1), sd = 1) + effect
    
  } 
  else if (distribution == "normal_diff_var") {
    g1 <- rnorm(size, 0, 1)
    g2 <- rnorm(size, 0, 2) + effect  # different variance
  }
  else {
    stop("Invalid distribution!")
  }
  
  #We create a data.frame that ggplot2 will then use to draw the graphs.
  plot_data <- data.frame(
    values = c(g1, g2),
    labels_group = factor(c(rep("g1", size), rep("g2", size))) 
  )
  
  #The actual graphs.
  
  #The code for the canvas.
  plot_draw <- ggplot(plot_data, aes(x = values, fill = labels_group)) +
    geom_histogram(
      #The bins are the intervals
      bins = 30, 
      color = "black"
    ) +
    labs (
      title = paste("Histogramas de distribución: ", distribution),
      x = "Valores",
      y = "Frecuencia"
    )
  
  #facet_wrap helps separate the data into two graphs that we can visualize side by side. 
  final_plot <- plot_draw + facet_wrap(~labels_group, ncol = 2)
  return(final_plot)
}


#Normal
plot_drawing(size = 200, distribution = "normal", effect = 0)
plot_drawing(size = 200, distribution = "normal", effect = 5)

#Exponential
plot_drawing(size = 200, distribution = "exp", effect = 0)
plot_drawing(size = 200, distribution = "exp", effect = 5)

#t-distribution
plot_drawing(size = 200, distribution = "t3", effect = 0)
plot_drawing(size = 200, distribution = "t3", effect = 5)

#Lognormal 
plot_drawing(size = 200, distribution = "lognorm", effect = 0)
plot_drawing(size = 200, distribution = "lognorm", effect = 5)

#Uniform
plot_drawing(size = 200, distribution = "uniform", effect = 0)
plot_drawing(size = 200, distribution = "uniform", effect = 5)

#Bimodal
plot_drawing(size = 200, distribution = "bimodal", effect = 0)
plot_drawing(size = 200, distribution = "bimodal", effect = 5)

#Normal but with different variance
plot_drawing(size = 200, distribution = "normal_diff_var", effect = 0)
plot_drawing(size = 200, distribution = "normal_diff_var", effect = 5)


