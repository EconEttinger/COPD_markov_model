### Markov model ###
#Built using tutorial from YouTube as a basis. 
#Tutorial available from : https://www.youtube.com/watch?v=xxtGOKLdQR0&t=394s
# Github for code from tutorial : https://gist.github.com/tristansnowsill/879d327b2b07adf7a1545920e343e3a1


# The Markov Model Function
markov_model_COPD <- function(.params) { with(.params, {
  
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

## calculate utility from SGRQ 


utility <- data.frame(
  severity = c("moderate", "severe", "very severe"),
  sgrq = c(q_mod, q_sev, q_vsev)
)


utility <- utility %>%
  mutate(eq5d = 0.9617 - 0.0013*sgrq-0.0001*(sgrq+int_sgrq)^2+0.0231*1)


# create matrix for pay offs (costs of state membership and QALYs) in states 

payoffs <- matrix(c(c_mod, c_sev, c_vsev, 0, # costs of state membership
                    utility[1,3], utility[2,3], utility[3,3] , 0), # QALYs
                  nrow = 4, ncol = 2, byrow = FALSE, # run by column 
                    dimnames = list(state = v_state_names,
                                    payoff = c("Cost", "QALY")))

payoff_trace <- array(NA_real_, 
                      dim = c(n_t, 2), 
                      dimnames = list(cycle = 1:n_t, 
                                      payoff = c("Cost", "QALY")))

# calculate costs and put into payoff array

payoff_trace[,1] <- 
  state_membership[,1] * as.numeric(payoffs[1]) + # state membership cost for moderate
  state_membership[,1] * aecopd_mod * c_aecopd + # AECOPD costs for moderate
  state_membership[,1] * hosp_mod * c_hosp + # hospital admission for moderate , 
  
  state_membership[,2] * as.numeric(payoffs[2]) + # state membership cost for severe
  state_membership[,2] * aecopd_sev * c_aecopd + # aecopd costs for severe
  state_membership[,2] * hosp_sev * c_hosp + # hospital admission costs for severe
  
  state_membership[,3] * as.numeric(payoffs[3]) + # state membership cost for severe
  state_membership[,3] * aecopd_vsev * c_aecopd + # aecopd cots for very severe
  state_membership[,3] * hosp_vsev * c_hosp # hospital admission costsfor very severe


payoff_trace[1,1] <- payoff_trace[1,1] + (sum(state_membership[1,]) * c_intervention) # cost of intervention added to first cycle

payoff_trace[,2] <- state_membership %*% as.numeric(payoffs[,2]) # qaly payoffs

## calculate discounted payoffs

d_r_payoff <- array(NA_real_, 
                    dim = c(n_t, 2), 
                    dimnames = list(cycle = 1:n_t, 
                                    payoff = c("Cost", "QALY")))
d_r_payoff[1,] <- as.numeric(payoff_trace[1,])
for (i in 2:n_t) {
  d_r_payoff[i, ] <- payoff_trace[i,] * (1/(1+d_r))^(i - 1) 
}

                    
#caculate non discounted costs and QALYS 
#colSums(payoff_trace)
#sum(payoff_trace[,1]) / sum(payoff_trace[,2])

# calculate total discounted costs and QALYs

colSums(d_r_payoff/n_c)


}
)
}





