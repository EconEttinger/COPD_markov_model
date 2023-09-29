### #LIFETIME IN WOMEN WITH PSA
# Set number of   times PSA runs

n_psa <- 1000

### CONTROL ARM 

# set parameters

params_con <- data.frame(
  
  sex = 1, # 0 for males, 1 for females. affects mortality rates. Also change in transition probabilities
  
  n_t = 40, # number of cycles (years)
  n_s = 4, # number of disease states
  n_c = 1000, # cohort size
  d_r = 0.035, #discount rate
  
  #costs
  
  c_aecopd =  85.02, # AECOPD non hospitalised
  c_hosp = 1930.02, # AECOPD hospitalised
  c_mod = 209.35 , # moderate disease state
  c_sev = 1203.55, # severe disease state
  c_vsev = 2971.86, # very severe disease state
  c_intervention = 0, #  intervention (none in comparator arm)
  
  #SGRQ score 
  
  q_mod = rnorm(n_psa, 40.4, 18.1), #moderate disease state
  q_sev = rnorm(n_psa, 50.2, 18.6), # severe disease state
  q_vsev = rnorm(n_psa, 58.6, 17.7), #very severe disease state
  
  ## AECOPD
  aecopd_mod = rbeta(n_psa,0.316, 1-0.316),   #moderate disease state
  aecopd_sev = rbeta(n_psa,0.393  , 1-0.393),   # severe disease state
  aecopd_vsev = rbeta(n_psa, 0.451, 1-0.451),   #very severe disease state
  
  hosp_mod = rbeta(n_psa, 0.024, 1-0.024),   #moderate disease state
  hosp_sev = rbeta(n_psa, 0.051, 1-0.051),   # severe disease state
  hosp_vsev = rbeta(n_psa, 0.078, 1-0.078),  #very severe disease state
  
  # transition probabilities  
  
  p_m2s = rbeta(n_psa, 0.029, 1-0.029), # moderate to severe
  p_m2d_m = rbeta(n_psa, 0.054, 1-0.054),#moderate to death men
  p_m2d_f = rbeta(n_psa, 0.041, 1-0.041),# moderate to death women
  p_s2vs = rbeta(n_psa, 0.067, 1-0.067), # severe to very severe
  p_s2d_m = rbeta(n_psa,0.070, 1-0.070), #severe to death in men
  p_s2d_f = rbeta(n_psa,0.057, 1-0.057),   #severe to death in women
  p_vs2d_m = rbeta(n_psa,0.123, 1-0.0123),  # very severe to death in men
  p_vs2d_f = rbeta(n_psa,0.061, 1 - 0.061), #very severe to death in women
  
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
  
  n_t = 40, # number of cycles (years)
  n_s = 4, # number of disease states
  n_c = 1000, # cohort size
  d_r = 0.035, #discount rate
  
  #costs
  
  c_aecopd =  85.02, # AECOPD non hospitalised
  c_hosp = 1930.02, # AECOPD hospitalised
  c_mod = 209.35 , # moderate disease state
  c_sev = 1203.55, # severe disease state
  c_vsev = 2971.86, # very severe disease state
  c_intervention = 8061, #  intervention (none in comparator arm)
  
  #SGRQ score 
  
  q_mod = rnorm(n_psa, 40.4, 18.1), #moderate disease state
  q_sev = rnorm(n_psa, 50.2, 18.6), # severe disease state
  q_vsev = rnorm(n_psa, 58.6, 17.7), #very severe disease state
  
  ## AECOPD
  aecopd_mod = rbeta(n_psa,0.316, 1-0.316),   #moderate disease state
  aecopd_sev = rbeta(n_psa,0.393  , 1-0.393),   # severe disease state
  aecopd_vsev = rbeta(n_psa, 0.451, 1-0.451),   #very severe disease state
  
  hosp_mod = rbeta(n_psa, 0.024, 1-0.024),   #moderate disease state
  hosp_sev = rbeta(n_psa, 0.051, 1-0.051),   # severe disease state
  hosp_vsev = rbeta(n_psa, 0.078, 1-0.078),  #very severe disease state
  
  
  # transition probabilities  
  
  p_m2s = rbeta(n_psa, 0.021, 1-0.021), # moderate to severe
  p_m2d_m = rbeta(n_psa, 0.054, 1-0.054),  #moderate to death men
  p_m2d_f = rbeta(n_psa, 0.041, 1-0.041),# moderate to death women
  p_s2vs = rbeta(n_psa, 0.045, 1-0.045), # severe to very severe
  p_s2d_m = rbeta(n_psa,0.070, 1-0.070),#severe to death in men
  p_s2d_f = rbeta(n_psa,0.057, 1-0.057),  #severe to death in women
  p_vs2d_m = rbeta(n_psa,0.123, 1-0.0123), # very severe to death in men
  p_vs2d_f = rbeta(n_psa,0.061, 1 - 0.061), #very severe to death in women
  # treatment_effect
  
  int_sgrq = rnorm(n_psa,-5.7, 0.037)  # change in SGRQ score
)

# run PSA for control

psa_results_con <- t(sapply(X = split(params_con, 1:n_psa),
                            FUN = markov_model_COPD,
                            simplify = TRUE))



psa_results_int <- t(sapply(X = split(params_int, 1:n_psa),
                            FUN = markov_model_COPD,
                            simplify = TRUE))

# run PSA for intervention

psa_icer <- data.frame(
  incremental_cost = psa_results_int[,1]/1000 - psa_results_con[,1]/1000, # round to £000s
  incremental_qaly = psa_results_int[,2] - psa_results_con[,2]
)

a<- data.frame(psa_icer$incremental_cost/psa_icer$incremental_qaly)


# outputs 

library(ggplot2)
library(ggthemes)

#cost effectiveness plane

ggplot(psa_icer, aes(x = incremental_qaly, y = incremental_cost)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  xlab("Incremental QALYs") + ylab("Incremental Costs (£000s)") +
  theme_stata()


#NE Quadrant

nrow(psa_icer[psa_icer$incremental_cost>0 & psa_icer$incremental_qaly>0,])

#SE Quadrant
nrow(psa_icer[psa_icer$incremental_cost<0 & psa_icer$incremental_qaly>0,])

#NW quadrant
nrow(psa_icer[psa_icer$incremental_cost>0 & psa_icer$incremental_qaly<0,])

#SW quadrant
nrow(psa_icer[psa_icer$incremental_cost<0 & psa_icer$incremental_qaly<0,])




# cost-acceptability curve 
#code sourced from:
# https://www.r-bloggers.com/2023/03/coding-the-cost-effectiveness-acceptability-curve-ceac-in-r/


#set thresholds
t_names <- c("T0","T10000","T20000","T30000","T40000","T50000","T60000","T70000","T80000","T90000","T100000")
threshold <- c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000)

# define threshold matrix
df <- matrix(NA,nrow = n_psa, ncol = length(threshold),
             dimnames = list(1:1000,WTP=t_names))

# calculate net benefit and populate DF

for (i in 1:length(threshold)){
  df[,i] <- threshold[i]*psa_icer$incremental_qaly - (psa_icer$incremental_cost*1000)
}

# calculate probability of cost-effectiveness at given thrsholds

probce_0 <- sum(df[,1]>=0)/1000

probce_10k <- sum(df[,2]>=0)/1000

probce_20k <- sum(df[,3]>=0)/1000

probce_30k <- sum(df[,4]>=0)/1000

probce_40k <- sum(df[,5]>=0)/1000

probce_50k <- sum(df[,6]>=0)/1000

probce_60k <- sum(df[,7]>=0)/1000

probce_70k <- sum(df[,8]>=0)/1000

probce_80k <- sum(df[,9]>=0)/1000

probce_90k <- sum(df[,10]>=0)/1000

probce_100k <- sum(df[,11]>=0)/1000

#put into list
prob_ce <- c(probce_0, probce_10k, probce_20k,
             probce_30k,probce_40k, probce_50k,
             probce_60k, probce_70k, probce_80k,
             probce_90k, probce_100k)

# make CEAC dataframe

CEAC_Data<-data.frame(threshold,prob_ce)

#make CEAC curve

ggplot(CEAC_Data, aes(y=prob_ce))+
  geom_line(aes(x=threshold),color="blue",size=1.0)+
  geom_point(aes(x=threshold),color="black", size=2.0)+
  ylim(0,1)+
  xlab("Willingness to Pay per QALY (£)")+
  ylab("Probability of  Cost-Effectiveness")+
  geom_vline(xintercept = 20000) + 
  geom_vline(xintercept = 40000) + 
  theme_stata() 

### Look at spread of incremental cost

ggplot(data = psa_icer, aes(x = incremental_cost )) +
  geom_histogram() +
  xlab("Incremental Cost (£000s)") +
  ylab("Number of Simulations") +
  theme_stata() 

# look at mean and median.and 95%CI

n <- nrow(psa_icer)
x_bar_cost <- mean(psa_icer$incremental_cost)  
median(psa_icer$incremental_cost)

cost_sd <- sd(psa_icer$incremental_cost)

margin_cost <- qt(0.975,df=n-1)*cost_sd/sqrt(n)

co_lower95 <- x_bar_cost - margin_cost
cost_upper95 <- x_bar_cost + margin_cost



### Look at spread of incremental QALYs

ggplot(data = psa_icer, aes(x = incremental_qaly )) +
  geom_histogram() +
  xlab("Incremental QALYs") +
  ylab("Number of Simulations") +
  theme_stata() 


x_bar_qaly <- mean(psa_icer$incremental_qaly)  
median(psa_icer$incremental_qaly)

qaly_sd <- sd(psa_icer$incremental_qaly)

SE_qaly <- qaly_sd/sqrt(n)

qaly_lower95 <- x_bar_qaly - 1.96*SE_qaly
qaly_upper95 <- x_bar_qaly + 1.96*SE_qaly


sum(psa_icer$incremental_qaly<0)

sum(psa_icer$incremental_cost<0)
