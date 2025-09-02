library(tidyverse)
library(magrittr)
library(sjlabelled) #for labelled variables
library(ggpubr) #for plots
library(rstatix)
library(emmeans)
library(afex)
library(effects) #to get plots of effects of different variables
library(multcomp)
library(BayesFactor)
library(dplyr)

#create new figures folder if needed
dir.create(file.path('./', 'docs/figures'), showWarnings = FALSE)



#import csv files into R ----


getSurvey <- function (version_expe) {
  
  #set the path from version_expe
  FileName <- sprintf('./data/data_Qualtrics_%s.csv', version_expe)
  survey <- read_csv(FileName, 
                     col_types = cols())
  
  #import headers labels
  FileLblName <- sprintf('./data/data_Qualtrics_labels_%s.csv', version_expe)
  labs <- read_csv(FileLblName, 
                   col_types = cols(.default = 'c'))
  
  #set labels for the survey data frame
  survey %<>% 
    set_label(label = as.character(labs[1,])) #need to convert 1st row of labs into character
  
  return(survey)
  
}

getDistance<- function(df){
  
  df<- df%>%
    mutate(distance = sqrt(tool_x^2 + tool_z^2))
  
}


#Baseline Correct data
baselineCorrection <- function(df){
  new_ppid <- paste(df$experiment, df$ppid, sep = "_")
  df$ppid<- NULL
  df$ppid<-factor(new_ppid)
  
   df<- df%>%
     filter(distance > 0.085)

  bl_df_summary <- df %>%
    filter(expe_phase == "baseline") %>%
    group_by(ppid,tool_used, target_angle) %>%
    summarise(
      bl_deviation = median(launch_angle_err_uncorrected, na.rm = TRUE)
    )
  # convert to data.table
  bl_df_summary <- as.data.table(bl_df_summary)
  omnibus_df <- as.data.table(df)
  
  # non equi join omnibus_df and bl_df_summary
  omnibus_df <- omnibus_df[
    bl_df_summary,
    on = .(ppid, tool_used, target_angle),
    nomatch = 0
  ]
  
  # subtract the baseline from the shot deviation
  df <- omnibus_df %>%
    mutate(
      launch_angle_err_dir = launch_angle_err_uncorrected - bl_deviation
    )%>%
    arrange(ppid,trial_num)%>%
    relocate(ppid)%>%
    relocate(target_angle, .after = per_block_list_triggerType)
  
  
  
}



#calculate additional parameters ----

# 
# addBlockNtool <- function(df) {
#   
#   #extract first participant ID number
#   first_pp <- unique(df$ppid)[1]
#   
#   #define trial number cutoffs to create block number per tool used
#   cutoffs_blockNum <- df %>% 
#     filter(ppid == first_pp) %>% 
#     filter(tool_used == .$tool_used[1]) %>%
#     group_by(block_num) %>%
#     filter(row_number() == 1) %>%
#     ungroup() %>% 
#     pull(trial_num)
#   
#   #define number of blocks based on the cutoffs
#   blockNum <- seq(1:(length(cutoffs_blockNum)-1))
#   
#   #create block number per tool used
#   df <- df %>% 
#     group_by(ppid, tool_used) %>% 
#     mutate(blockN_tool = cut(trial_num, breaks = cutoffs_blockNum, labels = blockNum, 
#                              include.lowest = TRUE, right = FALSE), 
#            .before = trialN_tool) %>% 
#     mutate(blockN_tool = as.factor(blockN_tool))
#   
#   return(df)
#   
# }
# 
# 
# 

addBlockNtool <- function(df) {
  df <- df %>%
    group_by(ppid, tool_used) %>%
    # whenever block_num changes, increase block counter
    arrange(trial_num) %>%
    mutate(
      blockN_tool = cumsum(!duplicated(block_num)),
      .before = trialN_tool) %>%
    ungroup()
  
  return(df)
}


#Get data file

getDataFile <- function (version_expe) { 
  
  #set the path from version_expe
  FileName <- sprintf('./data_ToolUse_%s.csv', version_expe)
  
  #read .csv file
  data <- read_csv(FileName, 
                   col_types = cols())
  
  #define maximum error size (0.5 in Shanaa's experiment)
  max_err <- 1.0
  
  #number of observations before removing errors > max_err
  N_obs_before <- nrow(data)
  
  #process raw data
  data <- data %>% 
    #remove rows in which trial_type is 'instruction'
    filter(trial_type != 'instruction') %>% 
    #make expe_phase as levels
    mutate(expe_phase = factor(expe_phase, levels = c('practice',
                                                      'baseline',
                                                      'exposure',
                                                      'washout',
                                                      'reexposure'))) %>% 
    #calculate launch_angle_err:
    #difference between launch angle and target angle (< 0 is left; > 0 is right)
    mutate(launch_angle_err = target_angle - launch_angle) %>% 
    #replace errors > max_err with NA
    mutate(error_size = ifelse(error_size > max_err, NA, error_size),
           launch_angle = ifelse(is.na(error_size), NA, launch_angle),
           launch_angle_err = ifelse(is.na(error_size), NA, launch_angle_err)) %>%
    #calculate launch angle error direction:
    #(> 0 is the same direction as the perturbation and < 0 is opposite to the perturbation)
    mutate(launch_angle_err_uncorrected = sign_pert_tool * launch_angle_err)
  
  #calculate score_per_trial (subtract score between two successive rows after grouping by ID)
  data <- data %>% 
    group_by(ppid) %>% 
    mutate(score_per_trial = score - lag(score, default = 0), .after = score) %>% 
    ungroup()
  # #create trial number per tool used
   data <- data %>% 
     group_by(ppid, tool_used) %>% 
     mutate(trialN_tool = row_number(), .after = tool_used)
   
  # #add block number per tool used
   data <- addBlockNtool(data)
  
  #number of observations after removing errors > max_err
  N_obs_after <- data %>% 
    filter(is.na(error_size)) %>% 
    nrow()
  
  #calculate % of trials removed (errors > max_err)
  obs_rm <- N_obs_after * 100 / N_obs_before
  
  data<- getDistance(data)
  
  data<- baselineCorrection(data)
  
  
  #Remove some uncessary data for analysis
  data<-data%>%
    select(-tool_y, -ball_path_x, -ball_path_y, -ball_path_z)
  


  #print message in console
  message(
    sprintf('Maximum error size (max_err) set to %s\nPercentage of observations removed (error_size > max_err): %.2f%%', 
            max_err, obs_rm)
  )
  
  return(data)
  
}





data_FIXED<- function(df){
  # df<- df %>%
  # mutate(tool_used = case_when(tool_used == "paddle" & df$experiment == "V1" ~ "paddle_Incongruent_Dual",
  #   tool_used == "paddle" & df$experiment == "V4"~ "paddle_Congruent_Dual",
  #    tool_used == "paddle" & df$experiment == "V2"~ "paddle_Single",
  #    tool_useed == "paddle_blue" &
  #    ))
  
  df <- df %>%
    mutate(experiment = case_when(experiment == "V1"~ "Motor_Incongruent",
                                  experiment == "V2"~ "Single_Adaptation",
                                  experiment == "V3"~ "Paddle_Control",
                                  experiment == "V4" ~ "Motor_Congruent",
                                  experiment == "V5" ~ "Curling_Control",
                                  TRUE~ "no"))
  
  
  
}




