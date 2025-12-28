#R project for The perverse procedure. 


#We want to study the type I error which is the probability to reject H0 even though Ho is true.
#reject <- (p_value < 0.05)
#probability <- reject/num_simulations

#There are 3 variables that are important:
#1) The size od the data: we will use 20, 50 and 200 n sizes.
#2) The type of data: 2 groups that are normally distributed, 2 non-normal distributions and then 
#1 normal and the other non-normally distributed.
#3) The escenarios: using only t-test, using only Wilcoxon and using first a Normality test and then decide. 




#1 Size = 20, 2 normally distributed groups.
num_simulations <- 1000

#We initialize the counters for rejecting H0. 
reject_1_20_00 <- 0
reject_2_20_00 <- 0
reject_3_20_00 <- 0

for (i in 1:num_simulations){
  size = 20
  
  #Creating the 2 sets of data: BOTH ARE NORMALLY DISTRIBUTED.
  x <- rnorm(size, mean = 0)
  y <- rnorm(size, mean = 0)
  
  #############################################Using only t-test!
  #Here p_value_1_20_00 indicates first the escenario, then the size of the data and finally if it is normal = 0 if not = 1. 
  p_value_1_20_00 <- t.test(x, y)$p.value
  #We check if we dismiss H0 (which means accepting H1 meaning the p-value must be below out set alpha).
  if (p_value_1_20_00 < 0.05) {
    reject_1_20_00 <- reject_1_20_00 + 1
  }
 
  #############################################Using only Wilcoxon!
  p_value_2_20_00 <- wilcox.test(x, y)$p.value
  if (p_value_2_20_00 < 0.05) {
    reject_2_20_00 <- reject_2_20_00 + 1
  }
  
  ############################################Using first a Normality test!
  p_value_normality_x <- shapiro.test(x)$p.value
  p_value_normality_y <- shapiro.test(y)$p.value
  
  #The perverse procedure
  #If the p_value is bigger than 0.05 then we accept H0 which is that the data is normally distributed.
  if (p_value_normality_x > 0.05 && p_value_normality_y > 0.05){
    p_value_3_20_00 <- t.test(x, y)$p.value
  }
  #If the Normality test fails we use Wilcoxon.
  else {
    p_value_3_20_00 <- wilcox.test(x, y)$p.value
  }
  
  if (p_value_3_20_00 < 0.05) {
    reject_3_20_00 <- reject_3_20_00 + 1
  }                                  
                                    
  

}


#Type I error:

#Always using t-test
reject_1_20_00/num_simulations 

#Always using Wilcoxon
reject_2_20_00/num_simulations 

#Using perverse procedure
reject_3_20_00/num_simulations 





#2 Size = 20, 2 non-normally distributed groups.
num_simulations <- 1000

#We initialize the counters for rejecting H0. 
reject_1_20_11 <- 0
reject_2_20_11 <- 0
reject_3_20_11 <- 0

for (i in 1:num_simulations){
  size = 10
  
  #Creating the 2 sets of data: BOTH ARE NON-NORMALLY DISTRIBUTED.
  x <- rexp(size, rate = 1)
  y <- rexp(size, rate = 1)
  
  #############################################Using only t-test!
  #Here p_value_1_20_00 indicates first the escenario, then the size of the data and finally if it is normal = 0 if not = 1. 
  p_value_1_20_11 <- t.test(x, y)$p.value
  #We check if we dismiss H0 (which means accepting H1 meaning the p-value mus be below out set alpha).
  if (p_value_1_20_11 < 0.05) {
    reject_1_20_11 <- reject_1_20_11 + 1
  }
  
  #############################################Using only Wilcoxon!
  p_value_2_20_11 <- wilcox.test(x, y)$p.value
  if (p_value_2_20_11 < 0.05) {
    reject_2_20_11 <- reject_2_20_11 + 1
  }
  
  ############################################Using first a Normality test!
  p_value_normality_x <- shapiro.test(x)$p.value
  p_value_normality_y <- shapiro.test(y)$p.value
  
  #The perverse procedure
  #If the p_value is bigger than 0.05 then we accept H0 which is that the data is normally distributed.
  if (p_value_normality_x > 0.05 && p_value_normality_y > 0.05){
    p_value_3_20_11 <- t.test(x, y)$p.value
  }
  #If the Normality test fails we use Wilcoxon.
  else {
    p_value_3_20_11 <- wilcox.test(x, y)$p.value
  }
  
  if (p_value_3_20_11 < 0.05) {
    reject_3_20_11 <- reject_3_20_11 + 1
  }                                  
  
}


#Type I error:

#Always using t-test
reject_1_20_11/num_simulations 

#Always using Wilcoxon
reject_2_20_11/num_simulations 

#Using perverse procedure
reject_3_20_11/num_simulations 





#NOTA: ESTE ESCENARIO NO FUNCIONA!!!!!!! 
#3 Size = 20, 1 group is normally distributed and the other is not distributed groups.
num_simulations <- 1000

#We initialize the counters for rejecting H0. 
reject_1_20_10 <- 0
reject_2_20_10 <- 0
reject_3_20_10 <- 0

for (i in 1:num_simulations){
  size = 20
  
  #Creating the 2 sets of data: BOTH ARE NON-NORMALLY DISTRIBUTED.
  x <- rexp(size, rate = 1)
  y <- rnorm(size, mean = 0)
  
  #############################################Using only t-test!
  #Here p_value_1_20_00 indicates first the escenario, then the size of the data and finally if it is normal = 0 if not = 1. 
  p_value_1_20_10 <- t.test(x, y)$p.value
  #We check if we dismiss H0 (which means accepting H1 meaning the p-value mus be below out set alpha).
  if (p_value_1_20_10 < 0.05) {
    reject_1_20_10 <- reject_1_20_10 + 1
  }
  
  #############################################Using only Wilcoxon!
  p_value_2_20_10 <- wilcox.test(x, y)$p.value
  if (p_value_2_20_10 < 0.05) {
    reject_2_20_10 <- reject_2_20_10 + 1
  }
  
  ############################################Using first a Normality test!
  p_value_normality_x <- shapiro.test(x)$p.value
  p_value_normality_y <- shapiro.test(y)$p.value
  
  #The perverse procedure
  #If the p_value is bigger than 0.05 then we accept H0 which is that the data is normally distributed.
  if (p_value_normality_x > 0.05 && p_value_normality_y > 0.05){
    p_value_3_20_10 <- t.test(x, y)$p.value
  }
  #If the Normality test fails we use Wilcoxon.
  else {
    p_value_3_20_10 <- wilcox.test(x, y)$p.value
  }
  
  if (p_value_3_20_10 < 0.05) {
    reject_3_20_10 <- reject_3_20_10 + 1
  }                                  
  
}


#Type I error:

#Always using t-test
reject_1_20_10/num_simulations 

#Always using Wilcoxon
reject_2_20_10/num_simulations 

#Using perverse procedure
reject_3_20_10/num_simulations 


































































