### Markov model ###
###INSERT BLURB HERE TO UPLOAD TO MY GITHUB ####
#Built using tutorial from YouTube as a basis. 
#Tutorial available from : https://www.youtube.com/watch?v=xxtGOKLdQR0&t=394s
# Github for code from tutorial : https://gist.github.com/tristansnowsill/879d327b2b07adf7a1545920e343e3a1


# The Markov Model Function
state_membership_fun <- function(.params) { with(.params, {
  
  #library 
  library(dplyr)
  
  # create the transition matrix
  
  v_state_names <- c("Moderate", "Severe", "Very Severe", "Death") # names of the disease states
  
  m_P <- matrix(c(0, p_m2s, 0.00, if (sex == 0) {p_m2d_m} else if (sex == 1) {p_m2d_f} else {0}, # from moderate
                  0.00, 0, p_s2vs, if (sex == 0) {p_s2d_m} else if (sex == 1) {p_s2d_f} else {0}, # from severe
                  0.00,0.00,0, if (sex == 0) {p_vs2d_m} else if (sex == 1) {p_vs2d_f} else {0}, # from very severe 
                  0.00, 0.00, 0.00, 1.00), # from death
                nrow = 4, ncol = 4, byrow = TRUE,
                dimnames = list(from = v_state_names,
                                to = v_state_names))
  
  
  # calculates probabilities of remaining in disease state
  m_P[1,1] <- 1 - (as.numeric(m_P[1,2]) + as.numeric(m_P[1,4]))
  m_P[2,2] <- 1 - (as.numeric(m_P[2,3]) + as.numeric(m_P[2,4]))
  m_P[3,3] <- 1 - as.numeric(m_P[3,4])
  
  # create array for state membership 
  
  state_membership <- matrix(NA_real_,
                             nrow = n_t, ncol =  n_s,
                             dimnames = list(cycle = 1:n_t,
                                             state = v_state_names))
  
  # populate state membership in first cycle of the model
  
  state_membership[1, ] <- c(n_c, 0, 0, 0)
  
  # run model for the selected number of cycles using matrix multiplication
  
  for (i in 2:n_t) {
    state_membership[i, ] <- state_membership[i - 1, ] %*% m_P
  }
  
  # check statemembership:
  # matplot(1:n_t, state_membership, type = "l")

  
  return(state_membership)
  
  
}
)
}

