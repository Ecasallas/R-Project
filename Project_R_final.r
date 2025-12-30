# -----------------------------------------------------------------------------------------------------------------------
#   Project: R - The perverse procedure. 
#   Subject: Programación y Estadística en R
#   Máster en Bioinformática y Biología Computacional – UAM
#   Academic year: 2025–2026
#
#   Authors: CHEN XI, GEMMA ROZALEN, ESTEFANI CASALLAS, MARIAN (COMPLETAR)
#   Teacher: (COMPLETAR)
#
#   Description: we want to check through a series of simulations how only using t-test, only using wilcoxon or using first a normalisty 
#                test such as Shapiro-Wilk to decide what kind of test to apply. 
#       
# -----------------------------------------------------------------------------------------------------------------------


#################################################TYPE I ERROR#####################################################

# Type I error is defined as rejeting the null hypothesis even when the null hypothesis is TRUE.
# To demonstrate the type I error each procedure has, we simulate data that complies with the null hypothesis and see
# how each procedure handles that data. For that purporse we will use two independent groups with the same NORMAL
# distribution and then two groups with NON-NORMAL distribution (for example, exponential). 


type_1_error <- function(size, distribution, number_simulations) {
  # We initialize the counters for rejecting H0 (accepting H1). 
  reject_t_error <- 0
  reject_w_error <- 0
  reject_p_error <- 0
  
  # We create the dataframe where we store the simulation data
  results_error <- data.frame()

  # We initialize the counters for how many times the perverse procedure chooses each test. 
  perverse_counter_t_test <- 0
  perverse_counter_wilcoxon <- 0

  for (simulation in 1:number_simulations){

    # Simulate the data
    if (distribution == "normal") {
        g1 <- rnorm(size, mean = 0, sd = 1)
        g2 <- rnorm(size, mean = 0, sd = 1) 
    } 
    else if (distribution == "no") {
        g1 <- rexp(size, rate = 1)
        g2 <- rexp(size, rate = 1) 
    } 
    else {
        stop("Invalid distribution! Either 'normal' or 'no'.")
    }


    #1) Using ONLY t-test.
    p_value_t <- t.test(g1, g2)$p.value

    # We check if we dismiss H0 (which means accepting H1 meaning the p-value must be below out set alpha).
    if (p_value_t < 0.05) {
      reject_t_error <- reject_t_error + 1
    }

    #2) Using ONLY wilcoxon.
    p_value_w <- wilcox.test(g1, g2)$p.value
    if (p_value_w < 0.05) {
      reject_w_error <- reject_w_error + 1
    }
    
    # Perverse procusing first a normality test and then choosing what test. 
    p_value_normality_1 <- shapiro.test(g1)$p.value
    p_value_normality_2 <- shapiro.test(g2)$p.value

    # The perverse procedure:

    # If the p_value is bigger than 0.05 then we accept H0 which is that the data is normally distributed.
    # If we accept that both groups are normally distributed then use t-test. 
    if (p_value_normality_1 > 0.05 && p_value_normality_2 > 0.05){
      p_value_perverse <- t.test(g1, g2)$p.value
      perverse_counter_t_test <- perverse_counter_t_test + 1
    }
    # If the normality test fails we use Wilcoxon.
    else {
      p_value_perverse <- wilcox.test(g1, g2)$p.value
      perverse_counter_wilcoxon <- perverse_counter_wilcoxon +1
    }
    
    if (p_value_perverse < 0.05) {
      reject_p_error <- reject_p_error + 1
    }

                                                                         
  }

    # We calculate the type I error:
    # Always using t-test
    t_reject <- reject_t_error/number_simulations 
    
    # Always using Wilcoxon
    wilcoxon_reject <- reject_w_error/number_simulations 
    
    # Using perverse procedure
    perverse_procedure_reject <- reject_p_error/number_simulations 

    results_error <- rbind(
        results_error,
        data.frame(
        data_size = size,
        number_simulation = number_simulations,
        t_test_ERROR = t_reject,
        wilcoxon_ERROR = wilcoxon_reject,
        perverse_procedure_ERROR = perverse_procedure_reject,
        perverse_procedure_counter_t_test = perverse_counter_t_test/number_simulations,
        perverse__procedure_counter_wilcoxon = perverse_counter_wilcoxon/number_simulations
        )
    )

    
return (results_error)

}

# For small sample sizes.
results_error_type_I_NORMAL_small <- type_1_error(10, "normal", 10000)
results_error_type_I_NORMAL_small
results_error_type_I_NO_small <- type_1_error(10, "no", 10000)
results_error_type_I_NO_small

# For a medium sample size.
results_error_type_I_NORMAL_medium <- type_1_error(40, "normal", 10000)
results_error_type_I_NORMAL_medium
results_error_type_I_NO_medium <- type_1_error(40, "no", 10000)
results_error_type_I_NO_medium

# For a big sample size.
results_error_type_I_NORMAL_big <- type_1_error(200, "normal", 10000)
results_error_type_I_NORMAL_big
results_error_type_I_NO_big <- type_1_error(200, "no", 10000)
results_error_type_I_NO_big
  
########################################################POWER#########################################################

# Power is defined as the probability of correctly rejecting null hypothesis when alternative hypothesis is TRUE. 
# To demonstrate the power of each procedure, we simulate data that complies with the alternative hypothesis and see
# how each procedure handles that data. For that purporse we will use two independent groups with different NORMAL
# distributions and then two groups with different NON-NORMAL distribution (for example, exponential).

power <- function(size, distribution, number_simulations, effect) {
  # We initialize the counters for rejecting H0 (accepting H1). 
  reject_t_power <- 0
  reject_w_power <- 0
  reject_p_power <- 0

  # We initialize the counters for how many times the perverse procedure chooses each test. 
  perverse_counter_t_test <- 0
  perverse_counter_wilcoxon <- 0
  
  # We create the dataframe where we store the simulation data
  results_power <- data.frame()
  
  for (simulation in 1:number_simulations){
    
    # Simulate the data
    if (distribution == "normal") {
      g1 <- rnorm(size, mean = 0, sd = 1)
      g2 <- rnorm(size, mean = 0, sd = 1) + effect
    } 
    else if (distribution == "no") {
      g1 <- rexp(size, rate = 1)
      g2 <- rexp(size, rate = 1) + effect
    } 
    else {
      stop("Invalid distribution! Either 'normal' or 'no'.")
    }
    
    
    #1) Using ONLY t-test.
    p_value_t <- t.test(g1, g2)$p.value
    
    # We check if we dismiss H0 (which means accepting H1 meaning the p-value must be below out set alpha).
    if (p_value_t < 0.05) {
      reject_t_power <- reject_t_power + 1
    }
    
    #2) Using ONLY wilcoxon.
    p_value_w <- wilcox.test(g1, g2)$p.value
    if (p_value_w < 0.05) {
      reject_w_power <- reject_w_power + 1
    }
    
    # Perverse procusing first a normality test and then choosing what test. 
    p_value_normality_1 <- shapiro.test(g1)$p.value
    p_value_normality_2 <- shapiro.test(g2)$p.value
    
    #The perverse procedure:
    
    #If the p_value is bigger than 0.05 then we accept H0 which is that the data is normally distributed.
    #If we accept that both groups are normally distributed then use t-test. 
    if (p_value_normality_1 > 0.05 && p_value_normality_2 > 0.05){
      p_value_perverse <- t.test(g1, g2)$p.value
      perverse_counter_t_test <- perverse_counter_t_test + 1
    }
    #If the normality test fails we use Wilcoxon.
    else {
      p_value_perverse <- wilcox.test(g1, g2)$p.value
      perverse_counter_wilcoxon <- perverse_counter_wilcoxon +1
    }
    
    if (p_value_perverse < 0.05) {
      reject_p_power <- reject_p_power + 1
    }
    
  }
  
  # We calculate the power:
  # Always using t-test
  t_reject <- reject_t_power/number_simulations 
  
  # Always using Wilcoxon
  wilcoxon_reject <- reject_w_power/number_simulations 
  
  # Using perverse procedure
  perverse_procedure_reject <- reject_p_power/number_simulations 
  
  results_power <- rbind(
    results_power,
    data.frame(
      data_size = size,
      number_simulation = number_simulations,
      effect = effect,
      t_test_POWER = t_reject,
      wilcoxon_POWER = wilcoxon_reject,
      perverse_procedure_POWER = perverse_procedure_reject,
      perverse_procedure_counter_t_test = perverse_counter_t_test/number_simulations,
      perverse__procedure_counter_wilcoxon = perverse_counter_wilcoxon/number_simulations
    )
  )
  
  return (results_power)
}

# For small sample sizes.
results_power_NORMAL_small <- power(10, "normal", 10000, 2)
results_power_NORMAL_small
results_power_NO_small <- power(10, "no", 10000, 2)
results_power_NO_small

# For a medium sample size.
results_power_NORMAL_medium <- power(40, "normal", 10000, 2)
results_power_NORMAL_medium
results_power_NO_medium <- power(40, "no", 10000, 2)
results_power_NO_medium

# For a big sample size.
results_power_NORMAL_big <- power(200, "normal", 10000, 2)
results_power_NORMAL_big
results_power_NO_big <- power(200, "no", 10000, 2)
results_power_NO_big











































