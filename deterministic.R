### Conducting deterministic analysis on the Markov model

### Running the analysis ### FEMALES####

params_con <- data.frame( # COLD HOMES
  
  sex = 1, # 0 for males, 1 for females. affects mortality rates. Also change in transition probabilities
  
  n_t = 2, # number of cycles (years)
  n_s = 4, # number of disease states
  n_c = 1000, # cohort size
  d_r = 0.035, #discount rate
  
  ## Costs
  c_aecopd =  85.02, # AECOPD non hospitalised
  c_hosp = 1930.02, # AECOPD hospitalised
  c_mod = 209.35 , # moderate disease state
  c_sev = 1203.55, # severe disease state
  c_vsev = 2971.86, # very severe disease state
  c_intervention = 0,  #  intervention (none in comparator arm)
  
  ##SGRQ score 
  
  q_mod = 40.4,  #moderate disease state
  q_sev = 50.2, # severe disease state
  q_vsev = 58.6, #very severe disease state
  
  ## AECOPD probabilities
  #non-hospitalised
  aecopd_mod = 0.316,   #moderate disease state
  aecopd_sev = 0.393,   # severe disease state
  aecopd_vsev = 0.451,  #very severe disease state
  
  #hospitalised
  
  hosp_mod = 0.024,  #moderate disease state
  hosp_sev = 0.051,   # severe disease state
  hosp_vsev = 0.078,   #very severe disease state
  
  # transition probabilities  
  
  p_m2s = 0.043, # moderate to severe
  p_m2d_m = 0.054, #moderate to death men
  p_m2d_f = 0.041, # moderate to death women
  p_s2vs =  0.071, # severe to very severe
  p_s2d_m = 0.070, #severe to death in men
  p_s2d_f = 0.057,  #severe to death in women
  p_vs2d_m = 0.123, # very severe to death in men
  p_vs2d_f = 0.061, #very severe to death in women
  
  # treatment_effect
  
  int_sgrq = 0  # change in SGRQ score
  
)


#plot(psa_results[,2],
#    psa_results[,1],
#   type = "p", 
#  xlab = "QALY",
# ylab = "Cost £000s")


### INTERVENTION ARM

#set parameters

params_int <- data.frame(
  
  sex = 1, # 0 for males, 1 for females. affects mortality rates. Also change in transition probabilities
  
  n_t = 2, # number of cycles (years)
  n_s = 4, # number of disease states
  n_c = 1000, # cohort size
  d_r = 0.035, #discount rate
  
  ## Costs
  c_aecopd =  85.02, # AECOPD non hospitalised
  c_hosp = 1930.02, # AECOPD hospitalised
  c_mod = 209.35 , # moderate disease state
  c_sev = 1203.55, # severe disease state
  c_vsev = 2971.86, # very severe disease state
  c_intervention = 8061,  #  intervention (none in comparator arm)
  
  ##SGRQ score 
  
  q_mod = 40.4,  #moderate disease state
  q_sev = 50.2, # severe disease state
  q_vsev = 58.6, #very severe disease state
  
  ## AECOPD probabilities
  #non-hospitalised
  aecopd_mod = 0.316,   #moderate disease state
  aecopd_sev = 0.393,   # severe disease state
  aecopd_vsev = 0.451,  #very severe disease state
  
  #hospitalised
  
  hosp_mod = 0.024,  #moderate disease state
  hosp_sev = 0.051,   # severe disease state
  hosp_vsev = 0.078,   #very severe disease state
  
  
  # transition probabilities  
  
  p_m2s = 0.028, # moderate to severe
  p_m2d_m = 0.054,  #moderate to death men
  p_m2d_f = 0.041, # moderate to death women
  p_s2vs =  0.071, # severe to very severe
  p_s2d_m = 0.070, #severe to death in men
  p_s2d_f = 0.057,  #severe to death in women
  p_vs2d_m = 0.123, # very severe to death in men
  p_vs2d_f = 0.061,  # very severe to death in women
  
  # treatment_effect
  
  int_sgrq = -5.7  # change in SGRQ score
)

# RESULTS

con <- markov_model_COPD(params_con) #comparator arm
int <- markov_model_COPD(params_int) # interventio arm

#calculate ICER

icer <- (int[1]-con[1])/ (int[2]-con[2])

icer
(int[1]-con[1])
(int[2]-con[2])
con
int


# look at numbers in disease state in each model 

#control
con_sm <- data.frame(state_membership_fun(params_con)) 
con_sm <- tibble::rowid_to_column(con_sm, "year")

con_sm <- con_sm %>%
  pivot_longer(-year)
  colnames(con_sm) <- c("year","Disease State","value")
  
ggplot(con_sm, aes(x=year, y = value, group = `Disease State`, color = `Disease State`)) +
    geom_line() + 
    xlab("Time in model") +
    ylab("Number in Disease State") +
    theme_stata()

# intervention
  
int_sm <- data.frame(state_membership_fun(params_int))
int_sm <- tibble::rowid_to_column(int_sm, "year")

int_sm <- int_sm %>%
  pivot_longer(-year)
colnames(int_sm) <- c("year","Disease State","value")

ggplot(int_sm, aes(x=year, y = value, group = `Disease State`, color = `Disease State`)) +
  geom_line() + 
  xlab("Time in model") +
  ylab("Number in Disease State") +
  theme_stata()


# compare disease states between models

int_sm <- int_sm %>%
  filter(`Disease State`=="Death")

con_sm <- con_sm %>%
  filter(`Disease State`=="Death")


ggplot(int_sm, aes(x=year, y = value, group = `Disease State`, color = `Disease State`)) +
  geom_line() + 
  geom_line(data = con_sm, aes(y = value, x = year), color = "blue") +
  xlab("Time in model") +
  ylab("Number in Disease State") +
  theme_stata()



#### IN MEN ### 


### Running the analysis

params_con <- data.frame( # COLD HOMES
  
  sex = 0, # 0 for males, 1 for females. affects mortality rates. Also change in transition probabilities
  
  n_t = 40, # number of cycles (years)
  n_s = 4, # number of disease states
  n_c = 1000, # cohort size
  d_r = 0.035, #discount rate
  
  ## Costs
  c_aecopd =  85.02, # AECOPD non hospitalised
  c_hosp = 1930.02, # AECOPD hospitalised
  c_mod = 209.35 , # moderate disease state
  c_sev = 1203.55, # severe disease state
  c_vsev = 2971.86, # very severe disease state
  c_intervention = 0,  #  intervention (none in comparator arm)
  
  ##SGRQ score 
  
  q_mod = 40.4,  #moderate disease state
  q_sev = 50.2, # severe disease state
  q_vsev = 58.6, #very severe disease state
  
  ## AECOPD probabilities
  #non-hospitalised
  aecopd_mod = 0.316,   #moderate disease state
  aecopd_sev = 0.393,   # severe disease state
  aecopd_vsev = 0.451,  #very severe disease state
   
  #hospitalised
  
  hosp_mod = 0.024,  #moderate disease state
  hosp_sev = 0.051,   # severe disease state
  hosp_vsev = 0.078,   #very severe disease state
  
  # transition probabilities  
  
  p_m2s = 0.029, # moderate to severe
  p_m2d_m = 0.054, #moderate to death men
  p_m2d_f = 0.041, # moderate to death women
  p_s2vs =  0.067, # severe to very severe
  p_s2d_m = 0.070, #severe to death in men
  p_s2d_f = 0.057,  #severe to death in women
  p_vs2d_m = 0.123, # very severe to death in men
  p_vs2d_f = 0.061, #very severe to death in women
  
  # treatment_effect
  
  int_sgrq = 0  # change in SGRQ score
  
)


#plot(psa_results[,2],
#    psa_results[,1],
#   type = "p", 
#  xlab = "QALY",
# ylab = "Cost £000s")


### INTERVENTION ARM

#set parameters

params_int <- data.frame(
  
  sex = 0, # 0 for males, 1 for females. affects mortality rates. Also change in transition probabilities
  
  n_t = 40, # number of cycles (years)
  n_s = 4, # number of disease states
  n_c = 1000, # cohort size
  d_r = 0.035, #discount rate
  
  ## Costs
  c_aecopd =  85.02, # AECOPD non hospitalised
  c_hosp = 1930.02, # AECOPD hospitalised
  c_mod = 209.35 , # moderate disease state
  c_sev = 1203.55, # severe disease state
  c_vsev = 2971.86, # very severe disease state
  c_intervention = 8061,  #  intervention (none in comparator arm)
  
  ##SGRQ score 
  
  q_mod = 40.4,  #moderate disease state
  q_sev = 50.2, # severe disease state
  q_vsev = 58.6, #very severe disease state
  
  ## AECOPD probabilities
  #non-hospitalised
  aecopd_mod = 0.316*0.8,   #moderate disease state
  aecopd_sev = 0.393*0.8,   # severe disease state
  aecopd_vsev = 0.451*0.8,  #very severe disease state
  
  #hospitalised
  
  hosp_mod = 0.024*0.8,  #moderate disease state
  hosp_sev = 0.051*0.8,   # severe disease state
  hosp_vsev = 0.078*0.8,   #very severe disease state
  
  
  # transition probabilities  
  
  p_m2s = 0.025, # moderate to severe
  p_m2d_m = 0.054,  #moderate to death men
  p_m2d_f = 0.041, # moderate to death women
  p_s2vs =  0.045, # severe to very severe
  p_s2d_m = 0.070, #severe to death in men
  p_s2d_f = 0.057,  #severe to death in women
  p_vs2d_m = 0.123, # very severe to death in men
  p_vs2d_f = 0.061,  # very severe to death in women
  
  # treatment_effect
  
  int_sgrq = -5.7  # change in SGRQ score
)

# RESULTS

con <- markov_model_COPD(params_con) #comparator arm
int <- markov_model_COPD(params_int) # interventio arm

#calculate ICER

icer <- (int[1]-con[1])/ (int[2]-con[2])

icer
(int[1]-con[1])
(int[2]-con[2])
con
int

icer - icer_int
