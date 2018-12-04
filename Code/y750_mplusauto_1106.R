#---------------------------------------------
# Project: Investigating Sample and Cluster 
#           Size Requirements in MLGCM
# Melissa Lee
# leemel@iu.edu
# Fall 2018

#----------------------------------------------
#Simulation
#Conditions:
# Fixed 
# (1) DIF magnitude: 0.05, 0.075, 0.10
# (2) % DIF Items: 10%, 30%

# of iterations: 500

#----------------------------------------------

#setwd("~/Documents/Box Sync/-Active/Classes Fa18/Y750 Adv SEM/ResearchProject/mplus_data_gen")

#####################################################
# Set up
#####################################################

main_dir <- setwd("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen")

rm(list=ls())
library(MplusAutomation)
library(dplyr)
library(replyr)
library(texreg)
library(tidyverse)
library(tidyr)

#####################################################
# Create & Run Models 
#####################################################
createModels("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/ml_sim_1107.txt")

runModels("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107", recursive=TRUE)

#####################################################
# Extract Parameter and Summary Stats
#####################################################

# First use readModels to extract all the output
All_Output <- readModels("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107")

# Define a table for summary stats
sum_combined <- do.call(dplyr::bind_rows, lapply(1:length(All_Output), function(m) { 
  df <- All_Output[[m]]$summaries
  df$model <- names(All_Output)[m]
  return(df)
}))

# WRite table
write.table(sum_combined, "/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107/summary_stats.txt", sep="\t")

# Define a table for undstandardized parameter estimates
par_combined <- do.call(dplyr::bind_rows, lapply(1:length(All_Output), function(m) { 
  df <- All_Output[[m]]$parameters$unstandardized
  df$model <- names(All_Output)[m]
  return(df)
}))

# New column for parameter names
par_combined$parameter = paste(par_combined$paramHeader, par_combined$param)

# List variables
model = par_combined$model
parameter = par_combined$parameter
pop_par = par_combined$population
ave_est = par_combined$average
pop_sd =  par_combined$population_sd
ave_se = par_combined$average_se
mse = par_combined$mse
cover_95 = par_combined$cover_95
pct_sig_coef = par_combined$pct_sig_coef

# New dataset for parameters
parameter <- data.frame(model, parameter, pop_par, ave_est, pop_sd, ave_se, mse, cover_95, 
                      pct_sig_coef)

# Define parameter bias
parameter$par_bias <- ((parameter$ave_est-parameter$pop_par)/parameter$pop_par)

# Define SE bias
parameter$se_bias <- ((parameter$ave_se-parameter$pop_sd)/parameter$pop_sd)

# Export as text file
write.table(parameter, "/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107/parameters.txt", sep="\t")

new_df <- read.csv("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107/parameters.csv", header=T)


# Define a table for summary stats
# sum_combined <- do.call(dplyr::bind_rows, lapply(1:length(All_Output), function(m) { 
#   df <- All_Output[[m]]$summaries
#   df$model <- names(All_Output)[m]
#   return(df)
# }))

#####################################################
# Extract ICCs 
#####################################################
#filenames <- c(list.files("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107", pattern="*.out", full.names=TRUE), "/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750_estimates.out")
filenames <- (list.files("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107", pattern="*.out", full.names=TRUE))

filenames


# Extract ICCs
row_icc = array(1:7)
row_tech9 = array(1:6)
row_savedata = array(1:6)
info_icc = array(1:7)
info_tech9 = array(1:6)

num_files = length(filenames)
# Create ICC file
for (i in 1:num_files){
  #print(i)
  trigger = 0
  row = 0
  temp1<-readLines(filenames[i])
  while(trigger == 0){
    row <- row+1
    if(temp1[row]=="     Variable  Correlation   Variable  Correlation   Variable  Correlation")
    {row_icc[i] = row
    print(temp1[(row_icc[i]+2):(row_icc[i]+3)])
    trigger=1
    }
  }
}

# Create Tech9 File
for (i in 1:num_files){
  #print(i)
  trigger = 0
  row = 0
  temp1<-readLines(filenames[i])
  while(trigger == 0){
    row <- row+1
    if(temp1[row]=="TECHNICAL 9 OUTPUT") 
    {row_tech9[i] = row}
    if(grepl("REPLICATION", temp1[row]))
     {count = count + 1}
    if(temp1[row]=="SAVEDATA INFORMATION")
     info_tech9[i] = temp1[(row_tech9[i]+2):(row_savedata[i]-1)]
    print("tech9")
    print(i)
    print(temp1[(row_tech9[i]+2):(row_savedata[i]-1)])
    trigger=1}
}

      
      
      
    {row_icc[i] = row
    print(temp1[(row_icc[i]+2):(row_icc[i]+3)])
    trigger=1
    }
  }
}
                                                               
                 
                                               
write.table(parameter, "/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project/parameters.txt", sep="\t")





for (i in 1:num_files){
  #print(i)
  count = 0
  trigger = 0
  row = 1
  temp1<-readLines(filenames[i])
  while(trigger == 0){
    #print(row)
    #print(temp1[row])
    row=row+1
    #if(temp1[row]=="     Variable  Correlation   Variable  Correlation   Variable  Correlation")
    #{row_icc[i] = row
    #info_icc[i] = temp1[(row_icc[i]+2):(row_icc[i]+3)]}
    #print("icc")
    #print(i)
    #print(temp1[(row_icc[i]+2):(row_icc[i]+3)])}
    
    # if(temp1[row]=="TECHNICAL 9 OUTPUT") 
    # {row_tech9[i] = row}
    # 
    # if(grepl("REPLICATION", temp1[row]))
    # {count = count + 1}
    
    if(temp1[row]=="SAVEDATA INFORMATION")
    # {row_savedata[i] = row
    # fileConn<-file("tech9_output.txt")
    # write(i, file = "tech9_output.txt", append=TRUE)
    for (j in (row_tech9[i]+2):(row_savedata[i]-1))
    {write(temp1[j], file = "tech9_output.txt", append=TRUE)}
    close(fileConn)
    info_tech9[i] = temp1[(row_tech9[i]+2):(row_savedata[i]-1)]
    print("tech9")
    print(i)
    print(temp1[(row_tech9[i]+2):(row_savedata[i]-1)])
    trigger=1}
  }
  print(i)
  print(count-4)
}



#####################################################
# Set up
#####################################################
new_par <-new_df %>%
  select(model, parameter, pop_par, ave_est, pop_sd, 
         ave_se, mse,	cover_95,	pct_sig_coef,	par_bias,	se_bias) %>%
  filter (parameter %in% c("SW.BY Y3",
                           "SW.BY Y4",
                           "IW.ON X",
                           "SW.ON X",
                           "IW.WITH SW",
                           "Residual.Variances Y1_w",
                           "Residual.Variances IW",
                           "Residual.Variances SW",
                           "SB.BY Y3",
                           "SB.BY Y4",
                           "IB.ON X",
                           "SB.ON X",
                           "SB.WITH IB",
                           "Intercepts IB",
                           "Intercepts SB",
                           "Residual.Variances Y1_b",
                           "Residual.Variances IB",
                           "Residual.Variances SB"))

view(new_par)
write.table(new_par, "/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107/new_par.txt", sep="\t")

save(new_df,file="new_df.Rda")
load("new_df.Rda")

parameter <- new_par$parameter
load("new_df.Rda")
view(new_df)
levels(new_df$parameter)  
new_df$newpar <- as.factor(new_df$parameter)
library(plyr)

new_par$parameter <- mapvalues(new_par$parameter,
                       c("SW.BY Y3",
                       "SW.BY Y4",
                       "IW.ON X",
                       "SW.ON X",
                       "IW.WITH SW",
                       "Residual.Variances Y1_w",
                       "Residual.Variances IW",
                       "Residual.Variances SW",
                       "SB.BY Y3",
                       "SB.BY Y4",
                       "IB.ON X",
                       "SB.ON X",
                       "SB.WITH IB",
                       "Intercepts IB",
                       "Intercepts SB",
                       "Residual.Variances Y1_b",
                       "Residual.Variances IB",
                       "Residual.Variances SB"),
          c("b1_w", "b2_w", "gamma_iw", "gamma_sw","tau_w12", "sigma_w", "tau_w11",
            "tau_w22", "b1_b", "b2_b", "gamma_ib", "gamma_sb", "tau_b12", 
            "alpha_i", "alpha_s", "sigma_b", "tau_b11", "tau_b22"))

view(new_par)

#####################################################
# SEPARATE TABLEs
#####################################################
num_models = 12
num_params = 18
num_total = num_models*num_params
b1_w = seq(1,num_total,num_params)
b2_w = seq(2,num_total,num_params)
gamma_iw = seq(3,num_total,num_params)
gamma_sw = seq(4,num_total,num_params)
tau_w12 = seq(5,num_total,num_params)
sigma_w = seq(6,num_total,num_params)
tau_w11 = seq(7,num_total,num_params)
tau_w22 = seq(8,num_total,num_params)
b1_b = seq(9,num_total,num_params)
b2_b = seq(10,num_total,num_params)
gamma_ib = seq(11,num_total,num_params)
gamma_sb = seq(12,num_total,num_params)
tau_b12= seq(13,num_total,num_params)
alpha_i= seq(14,num_total,num_params)
alpha_s = seq(15,num_total,num_params)
sigma_b = seq(16,num_total,num_params)
tau_b11 = seq(17,num_total,num_params)
tau_b22 = seq(18,num_total,num_params)

# Write tables
b1_w_table = new_par[b2_w,c(1:11)]
b2_w_table = new_par[b2_w,c(1:11)]
gamma_iw_table = new_par[b2_w,c(1:11)]
gamma_sw_table = new_par[gamma_sw,c(1:11)]
#gamma_sw_table = new_par[gamma_sw,c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11)]
tau_w12_table = new_par[tau_w12,c(1:11)]
sigma_w_table = new_par[sigma_w,c(1:11)]
tau_w11_table = new_par[tau_w11,c(1:11)]
tau_w22_table = new_par[tau_w22,c(1:11)]
b1_b_table = new_par[b1_b,c(1:11)]
b2_b_table = new_par[b2_b,c(1:11)]
gamma_ib_table = new_par[gamma_ib,c(1:11)]
gamma_sb_table = new_par[gamma_sb,c(1:11)]
tau_b12_table = new_par[tau_b12,c(1:11)]
alpha_i_table = new_par[alpha_i,c(1:11)]
alpha_s_table = new_par[alpha_s,c(1:11)]
sigma_b_table = new_par[sigma_b,c(1:11)]
tau_b11_table = new_par[tau_b11,c(1:11)]
tau_b22_table = new_par[tau_b22,c(1:11)]


dir.create("/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107/results_store")

out_dir = "/Volumes/STUFF/Y750 Project/mplus_data_gen/mplus_data_gen/y750project_1107/results_store/"

#####################################################
# Write tables to results directory
#####################################################
#tables (Need to figure out a better way to do this)
write.csv(b1_w_table, file=paste(out_dir, "b1_w_table.csv", sep=""))
write.csv(b2_w_table, file=paste(out_dir, "b2_w_table.csv", sep=""))
write.csv(gamma_iw_table, file=paste(out_dir, "gamma_iw_table.csv", sep=""))
write.csv(gamma_sw_table, file=paste(out_dir, "gamma_sw_table.csv", sep=""))
write.csv(tau_w12_table, file=paste(out_dir, "tau_w12_table.csv", sep=""))
write.csv(sigma_w_table, file=paste(out_dir, "sigma_w_table.csv", sep=""))
write.csv(tau_w11_table, file=paste(out_dir, "tau_w11_table.csv", sep=""))
write.csv(tau_w22_table, file=paste(out_dir, "tau_w22_table.csv", sep=""))
write.csv(b1_b_table, file=paste(out_dir, "b1_b_table.csv", sep=""))
write.csv(b2_b_table, file=paste(out_dir, "b2_b_table.csv", sep=""))
write.csv(gamma_ib_table, file=paste(out_dir, "gamma_ib_table.csv", sep=""))
write.csv(gamma_sb_table, file=paste(out_dir, "gamma_sb_table.csv", sep=""))
write.csv(tau_b12_table, file=paste(out_dir, "tau_b12_table.csv", sep=""))
write.csv(alpha_i_table, file=paste(out_dir, "alpha_i_table.csv", sep=""))
write.csv(alpha_s_table, file=paste(out_dir, "alpha_s_table.csv", sep=""))
write.csv(sigma_b_table, file=paste(out_dir, "sigma_b_table.csv", sep=""))
write.csv(tau_b11_table, file=paste(out_dir, "tau_b11_table.csv", sep=""))
write.csv(tau_b22_table, file=paste(out_dir, "tau_b22_table.csv", sep=""))





#####################################################
# Extra code
#####################################################
#c1_par_other = new_par[c1,c(9, 10)]
view(c1_par)
view(c1_par_other)

vector1 = rep(0,num_models)
vector2 = rep(0,num_params)

cols_desired = c(1, 2, 4, 5, 6, 7, 8)
cols_desired2 = c(9, 10)
num_desired = length(cols_desired)
num_desired2 = length(cols_desired2)
vector_desired = rep(0, num_desired)
vector_desired2 = rep(0, num_desired2)

c = array(c(vector1, vector2), dim=c(num_models, num_params))
#c_par = array(c(vector1, vector_desired), dim=c(num_models, num_desired, num_models))
#c_par_other = array(c(vector1, vector_desired2), dim=c(num_models, num_desired2, num_models))

#c_par = array(new_par[c[,1], cols_desired], dim=c(num_models, num_desired, num_models))

i=1  
c[ , i] = seq(i, num_total, num_params)
temp = new_par[c[,i], cols_desired]

#c_par[ , , i] = new_par[c[,i], cols_desired]
#c_par_other[ , , i] = new_par[c[,i], cols_desired2]


for (i in 1:num_files){
  count = 0
  trigger = 0
  row = 1
  rep0 = " "
  temp1<-readLines(filenames[i])
  while(trigger == 0){
    row=row+1
    if(temp1[row]=="     Variable  Correlation   Variable  Correlation   Variable  Correlation")
    {
      row_icc[i] = row
      print(temp1[(row_icc[i]+2):(row_icc[i]+3)])
    }
    
    if(temp1[row]=="TECHNICAL 9 OUTPUT") 
    {
      row_tech9[i] = row
    }
    
    if(grepl("REPLICATION", temp1[row]))
    { 
      rep1 = temp1[row]
      if(rep1 != rep0)
        {
          count = count + 1
      }
      rep0 = rep1
    }

    if(temp1[row]=="SAVEDATA INFORMATION")
    {
    # row_savedata[i] = row
    # fileConn<-file("tech9_output.txt")
    # write(i, file = "tech9_output.txt", append=TRUE)
    # for (j in (row_tech9[i]+2):(row_savedata[i]-1))
    # {write(temp1[j], file = "tech9_output.txt", append=TRUE)}
    # close(fileConn)
    # #info_tech9[i] = temp1[(row_tech9[i]+2):(row_savedata[i]-1)]
    # print("tech9")
    # print(i)
    # #print(temp1[(row_tech9[i]+2):(row_savedata[i]-1)])
     trigger=1
     }
  }
  print(i)
  print(count-4)
}


