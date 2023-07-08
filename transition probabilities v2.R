
#developped from Renfrew and Paisley (MIDSPAN) Study as reported in Starkie et al. https://theses.gla.ac.uk/2154/
#NOTE STarkie et al. use mild, moderate, and severe. 
#We use the language moderate, severe and very severe in line with other studies, 
#however the lung function they represent are the same 

### variables #####


  
  ### FEV1 PREDICTED #### INTERVENTION ASSUMING WARM HOME
##estimates of lung function in a healthy population x
### Source Starkie t al. 5.2.2.


fev_trans_fun <- function(years, age, age_vec, height, fev_df_vec, sex, over_10, ex_smoker, symptoms) {

  library(dplyr)
  

    fev_1_pred <- function(years, age, height, sex) {
      age_v <- age
      fev_pred_vec <- c()
      for (i in 1:years) {
      fev_pred_vec <- c(fev_pred_vec,  
                        (if(sex == 0) {-1.859} else if (sex == 1) {-0.225})-0.029* age_v + 
                        (if(sex == 0) {0.037} else if (sex == 1) {0.024} else {NA}) *
                          height
                        )
      age_v <- age_v +1
    }
    return(fev_pred_vec)
    }
    
    
    #### FEV1
    ## prediction of lung function decline in patients with COPD
    ##SOURCE: Starkie et al. section 7.2.2 (table 7.2)
    
    
    
    
    fev_1 <- function(years, age, height,sex,ex_smoker,over_10,
                        symptoms,moderate,severe,very_severe) {
      fev_vec <- c()
      age_v <- age
      for (i in 1:years) {
      fev_vec <- c(fev_vec, -0.016
                   +0.022*height
                   -0.022*age_v
                   -0.381*sex
                   -0.005*over_10
                   +0.009*ex_smoker
                   -0.050*symptoms
                   -0.594*moderate
                   -1.173*severe
                   -1.610*very_severe)
      age_v <- age_v +1
      
      }
      return(fev_vec)
    }
    
    
    ## TRANSISTION PROBABILITILES # CONTROL

    
    ## transition for moderate - severe
      
    fev_pred_moderate <- fev_1_pred(years, age, height, sex)
      
    fev_moderate <- fev_1(years,age, height,sex,ex_smoker,over_10,
                       symptoms,1,0,0) 
    fev_moderate_pc <- fev_moderate/fev_pred_moderate
    
    ## transition from severe to very severe
    
    fev_pred_severe <- fev_1_pred(years, age, height, sex)
    
    fev_severe <- fev_1(years,age, height,sex,ex_smoker,over_10,
                            symptoms,0,1,0)
    fev_severe_pc <- fev_severe / fev_pred_severe
    
    
    fev_df <- data.frame(age_vec, fev_moderate_pc, fev_severe_pc)
    
#  ggplot(fev_df, aes(x = age_vec)) + geom_line(aes(y = fev_moderate_pc)) + geom_line(aes(y=fev_severe_pc))
    
    
    probs <- fev_df %>%
      summarise(p_mod = 1/sum(fev_moderate_pc >=0.5), # calculates amount of time (t) spent in moderate disease
                p_sev = 1/sum(fev_severe_pc >=0.3)) # calculates t in severe disease state

    return(probs)
}
# visual representation for reference 




### CONTROL ### ASSUMING COLD HOMES

fev_trans_fun_con <- function(years, age, age_vec, height, fev_df_vec, sex, over_10, ex_smoker, symptoms) {
  
  library(dplyr)
  
  
  fev_1_pred <- function(years, age, height, sex) {
    age_v <- age
    fev_pred_vec <- c()
    for (i in 1:years) {
      fev_pred_vec <- c(fev_pred_vec,  
                        (if(sex == 0) {-1.859} else if (sex == 1) {-0.225})-0.029*age_v +  
                          (if(sex == 0) {0.037} else if (sex == 1) {0.024} else {NA}) *
                          height
      )
      age_v <- age_v +1
    }
    return(fev_pred_vec)
  }
  
  
  #### FEV1
  ## prediction of lung function decline in patients with COPD
  ##SOURCE: Starkie et al. section 7.2.2 (table 7.2)
  
  
  
  
  fev_1 <- function(years, age, height,sex,ex_smoker,over_10,
                    symptoms,moderate,severe,very_severe) {
    fev_vec <- c()
    age_v <- age
    for (i in 1:years) {
      fev_vec <- c(fev_vec, (-0.016)
                   +0.022*height
                   -0.022*age_v
                   -0.381*sex
                   -0.005*over_10
                   +0.009*ex_smoker
                   -0.050*symptoms
                   -0.594*moderate
                   -1.173*severe
                   -1.610*very_severe)
      age_v <- age_v +1
      
    }
    return(fev_vec)
  }
  
  
  ## TRANSISTION PROBABILITILES # CONTROL
  
  
  ## transition for moderate - severe
  
  fev_pred_moderate <- fev_1_pred(years, age, height, sex)
  
  fev_moderate <- fev_1(years,age, height,sex,ex_smoker,over_10,
                        0,1,0,0) - 0.146
  fev_moderate_pc <- fev_moderate/fev_pred_moderate
  
  ## transition from severe to very severe
  
  fev_pred_severe <- fev_1_pred(years, age, height, sex)
  
  fev_severe <- fev_1(years,age, height,sex,ex_smoker,over_10,
                      0,0,1,0)  - 0.146
  fev_severe_pc <- fev_severe / fev_pred_severe
  
  
  fev_df <- data.frame(age_vec, fev_moderate_pc, fev_severe_pc)
  
# ggplot(fev_df, aes(x = age_vec)) + geom_line(aes(y = fev_moderate_pc)) + geom_line(aes(y=fev_severe_pc)) 
  
  probs <- fev_df %>%
    summarise(p_mod = 1/sum(fev_moderate_pc >=0.5), # calculates amount of time (t) spent in moderate disease
              p_sev = 1/sum(fev_severe_pc >=0.3)) # calculates t in severe disease state
  
  return(probs)
}



params_probs <- list(


  # demographic inputs
  years = 70,
  age = 71,
  age_vec = 71:140,
  height = 162,
  fev_df_vec = c(),
  
  #lung function input
  
  sex = 1, # 0 for males, 1 for females
  over_10 = 1, # 0 = <10, 1 = >=10 pack years
  ex_smoker = 1,  #0 = not ex-smoker, 1 = ex-smoker
  symptoms = 1# 0 = no symptoms, 1 = symptoms
  
)




probs_int <- fev_trans_fun(params_probs$years,
                       params_probs$age, 
                       params_probs$age_vec, 
                       params_probs$height,
                       params_probs$fev_df_vec,
                       params_probs$sex,
                       params_probs$over_10,
                       params_probs$ex_smoker, 
                       params_probs$symptoms)


probs_con <- fev_trans_fun_con(params_probs$years,
                   params_probs$age, 
                   params_probs$age_vec, 
                   params_probs$height,
                   params_probs$fev_df_vec,
                   params_probs$sex,
                   params_probs$over_10,
                   params_probs$ex_smoker, 
                   params_probs$symptoms)


probs_int
probs_con




