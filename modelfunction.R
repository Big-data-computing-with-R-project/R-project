

#----------------------------------------------- US ----------------------------------------------------------------
seir_modelUS = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  D = state_values [5]        # deaths
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I) * (1-sigma)
      dD = (gamma * I * sigma)
      
      # combine results
      results = c (dS, dE, dI, dR, dD)
      list (results)
    }
  )
}

#Parameters
  #contact_rate = 10                # number of contacts per day
  #transmission_probability = 0.07 #24441852/328200000  # transmission probability
  infectious_period_us = 14         # infectious period 10-20
  latent_period_us = 5.2              # latent period
  deaths_period_us = 406196/24441852
  
  lambda_us = 0.8
  Ro = 2.5 #1.9-3.3
  
  beta_value_us = lambda_us * Ro * (1 / infectious_period_us)
  #beta_value = Ro * (1 / infectious_period)
  gamma_value_us = 1 / infectious_period_us
  delta_value_us = 1 / latent_period_us
  sigma_value_us = deaths_period_us
  
  #Compute Ro - Reproductive number.
  #Ro = beta_value / gamma_value
  parameterlist_us <- c(beta = beta_value_us, gamma = gamma_value_us, delta = delta_value_us, sigma = sigma_value_us)

#----------------------------------------- Create US Model ------------------------------------------------------
  # calculate for dataframe
  calculatemodel_us <- function(w, x, y, z, a, time){
    outputmodel_us <- c()
    w <- 328200000
    n <- w + x + y + z + a
    initial_values <- c(S = w/n, E = x/n, I = y/n, R = z/n, D = a/n)
    timepoints <- seq(0, time, by = 1)
    
    outputmodel_us <- lsoda(initial_values, timepoints, seir_modelUS, parameterlist_us)
    
    dfmodel_us <- as.data.frame(outputmodel_us)
    dfmodel_us[, 2:ncol(dfmodel_us)] <- (n / 1000000) * dfmodel_us[, 2:ncol(dfmodel_us)]
    dfmodel_us[, 2:ncol(dfmodel_us)] <- dfmodel_us[, 2:ncol(dfmodel_us)] %>% round(4)
    
    dfmodel_us <- as.data.frame(dfmodel_us)
    return(dfmodel_us)
  }
  
  # color for geom_line()
  colors_us <- c("S (Sensitive)" = "blue", "E (Exposed)" = "black", "I (Infected)" = "red", "R (Recovered)" = "green","D (Deaths)" = "purple")
  
    
    
# --------------------------------------------- Thai -----------------------------------------------------------

  #Function to compute derivatives of the differential equations.
  seir_modelThai = function (current_timepoint, state_values, parameters)
  {
    # create state variables (local variables)
    S = state_values [1]        # susceptibles
    E = state_values [2]        # exposed
    I = state_values [3]        # infectious
    R = state_values [4]        # recovered
    
    with ( 
      as.list (parameters),     # variable names within parameters can be used 
      {
        # compute derivatives
        dS = (-beta * S * I)
        dE = (beta * S * I) - (delta * E)
        dI = (delta * E) - (gamma * I)
        dR = (gamma * I)
        
        # combine results
        results = c (dS, dE, dI, dR)
        list (results)
      }
    )
  }
  
#Parameters
  contact_rate_th = 10834/366             # number of contacts per day
  transmission_probability_th =  (100*10834)/69630000     # transmission probability
  infectious_period_th = 5.2               # infectious period
  latent_period_th = 14

  #Compute values of beta (tranmission rate) and gamma (recovery rate).
  lambda_th = 0.65
  #beta_value =  contact_rate * transmission_probability
  beta_value_th = lambda_th * contact_rate_th * transmission_probability_th
  #beta_value = lambda * Ro * (1 / infectious_period)
  #beta_value = Ro * (1 / infectious_period)
  gamma_value_th = 1 / infectious_period_th
  delta_value_th = 1 / latent_period_th
  Ro_th = beta_value_th / gamma_value_th
  #Disease dynamics parameters.
  parameterlist_th = c (beta = beta_value_th, gamma = gamma_value_th, delta = delta_value_th)
  #Ro
  #delta_value

  #----------------------------------------- Create Thailand Model ------------------------------------------------------
  # calculate for dataframe
  calculatemodel_th <- function(w, x, y, z, time){
    outputmodel_th <- c()
    w <- 69630000 
    n <- w + x + y + z 
    initial_values <- c(S = w/n, E = x/n, I = y/n, R = z/n)
    timepoints <- seq(0, time, by = 1)
    
    outputmodel_th <- lsoda(initial_values, timepoints, seir_modelThai, parameterlist_th)
    
    dfmodel_th <- as.data.frame(outputmodel_th)
    dfmodel_th[, 2:ncol(dfmodel_th)] <- (n / 1000000) * dfmodel_th[, 2:ncol(dfmodel_th)]
    dfmodel_th[, 2:ncol(dfmodel_th)] <- dfmodel_th[, 2:ncol(dfmodel_th)] %>% round(4)
    
    dfmodel_th <- as.data.frame(dfmodel_th)
    return(dfmodel_th)
  }
  
  colors_th <- c("S (Sensitive)" = "blue", "E (Exposed)" = "black", "I (Infected)" = "red", "R (Recovered)" = "green")
  
  