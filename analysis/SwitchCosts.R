
sort_by_first_tool <- function(df) {
  library(dplyr)
  
  df %>%
    group_by(ppid) %>%
    mutate(
      # find first tool for this participant
      first_tool = tool_used[which.min(trial_num)],
      # mark whether this row belongs to that first tool or the other
      tool_order = if_else(tool_used == first_tool, "first", "second")
    ) %>%
    ungroup()
}





CompareOrderFirstTrial <- function(df, tool_name) {
  
  # 2) Keep only the tool of interest
  df_tool <- df %>%
    filter(tool_used == tool_name)%>%
    filter(trialN_tool == 65)
  # 3) Collapse trials to participant means (per order)
  df_means <- df_tool %>%
    group_by(ppid, tool_order) %>%
    summarise(mean_val = mean(launch_angle_err_dir, na.rm = TRUE), .groups = "drop")
  

  # 5) Independent-samples t-test (different participants per group)
  t.test(mean_val ~ tool_order, data = df_means)
}

CompareOrderFirstBlock <- function(df, tool_name) {
  
  # 2) Keep only the tool of interest
  df_tool <- df %>%
    filter(tool_used == tool_name)%>%
    filter(blockN_tool == 7)
  # 3) Collapse trials to participant means (per order)
  df_means <- df_tool %>%
    group_by(ppid, blockN_tool,tool_order) %>%
    summarise(mean_val = mean(launch_angle_err_dir, na.rm = TRUE), .groups = "drop")
  
  
  # 5) Independent-samples t-test (different participants per group)
  t.test(mean_val ~ tool_order, data = df_means)
}

CompareOrderLastTrial <- function(df, tool_name) {
  

  # 2) Keep only the tool of interest
  df_tool <- df %>%
    filter(tool_used == tool_name)%>%
    filter(trialN_tool == 144)
  # 3) Collapse trials to participant means (per order)
  df_means <- df_tool %>%
    group_by(ppid, tool_order) %>%
    summarise(mean_val = mean(launch_angle_err_dir, na.rm = TRUE), .groups = "drop")
  
  
  # 5) Independent-samples t-test (different participants per group)
  t.test(mean_val ~ tool_order, data = df_means)
}

CompareOrderLastBlock <- function(df, tool_name) {
  
  
  # 2) Keep only the tool of interest
  df_tool <- df %>%
    filter(tool_used == tool_name)%>%
    filter(blockN_tool == 16)
  # 3) Collapse trials to participant means (per order)
  df_means <- df_tool %>%
    group_by(ppid, blockN_tool,tool_order) %>%
    summarise(mean_val = mean(launch_angle_err_dir, na.rm = TRUE), .groups = "drop")
  
  
  # 5) Independent-samples t-test (different participants per group)
  t.test(mean_val ~ tool_order, data = df_means)
}

CompareOrderWashoutFirstBlock <- function(df, tool_name) {
  
  
  # 2) Keep only the tool of interest
  df_tool <- df %>%
    filter(tool_used == tool_name)%>%
    filter(blockN_tool == 17)
  # 3) Collapse trials to participant means (per order)
  df_means <- df_tool %>%
    group_by(ppid, blockN_tool,tool_order) %>%
    summarise(mean_val = mean(launch_angle_err_dir, na.rm = TRUE), .groups = "drop")
  
  
  # 5) Independent-samples t-test (different participants per group)
  t.test(mean_val ~ tool_order, data = df_means)
}

#Data Motor Incongruent


dataV1_sorted<-sort_by_first_tool(dataV1)



#First trial
CompareOrderFirstTrial(dataV1_sorted, tool_name = "paddle")


#First Block
CompareOrderFirstBlock(dataV1_sorted, tool_name = "paddle")


#Last Trial
CompareOrderLastTrial(dataV1_sorted, tool_name = "paddle")


#Last Block
CompareOrderLastBlock(dataV1_sorted, tool_name = "paddle")

#Firstblock washout
CompareOrderWashoutFirstBlock(dataV1_sorted, tool_name = "paddle")



SLINGSHOT

#First Trial
CompareOrderFirstTrial(dataV1_sorted, tool_name = "slingshot")


#First Block
CompareOrderFirstBlock(dataV1_sorted, tool_name = "slingshot")


#Last trial
CompareOrderLastTrial(dataV1_sorted, tool_name = "slingshot")


#Last Block
CompareOrderLastBlock(dataV1_sorted, tool_name = "slingshot")

#Washout
CompareOrderWashoutFirstBlock(dataV1_sorted, tool_name = "slingshot")

#Single Adaptaion


dataV2_sorted<-sort_by_first_tool(dataV2)




#First trial
CompareOrderFirstTrial(dataV2_sorted, tool_name = "paddle")


#First Block
CompareOrderFirstBlock(dataV2_sorted, tool_name = "paddle")


#Last Trial
CompareOrderLastTrial(dataV2_sorted, tool_name = "paddle")


#Last Block
CompareOrderLastBlock(dataV1_sorted, tool_name = "paddle")


#Washout
CompareOrderWashoutFirstBlock(dataV2_sorted, tool_name = "paddle")
#significant

SLINGSHOT

#First Trial
CompareOrderFirstTrial(dataV2_sorted, tool_name = "slingshot")


#First Block
CompareOrderFirstBlock(dataV2_sorted, tool_name = "slingshot")


#Last trial
CompareOrderLastTrial(dataV2_sorted, tool_name = "slingshot")


#Last Block
CompareOrderLastBlock(dataV2_sorted, tool_name = "slingshot")

#Washout
CompareOrderWashoutFirstBlock(dataV2_sorted, tool_name = "slingshot")


#Paddle Control
dataV3_sorted<-sort_by_first_tool(dataV3)



#Paddle Red


#First trial
CompareOrderFirstTrial(dataV3_sorted, tool_name = "paddle_red")
#significant

#First Block
CompareOrderFirstBlock(dataV3_sorted, tool_name = "paddle_red")
#significant

#Last Trial
CompareOrderLastTrial(dataV3_sorted, tool_name = "paddle_red")
#significant (barely)

#Last Block
CompareOrderLastBlock(dataV3_sorted, tool_name = "paddle_red")

#Washout
CompareOrderWashoutFirstBlock(dataV3_sorted, tool_name = "paddle_red")


Paddle blue

#First trial
CompareOrderFirstTrial(dataV3_sorted, tool_name = "paddle_blue")


#First Block
CompareOrderFirstBlock(dataV3_sorted, tool_name = "paddle_blue")


#Last Trial
CompareOrderLastTrial(dataV3_sorted, tool_name = "paddle_blue")


#Last Block
CompareOrderLastBlock(dataV3_sorted, tool_name = "paddle_blue")
#significant

#Washout
CompareOrderWashoutFirstBlock(dataV3_sorted, tool_name = "paddle_blue")


#Motor Congruent


dataV4_sorted<-sort_by_first_tool(dataV4)



Paddle 


#First Trial
CompareOrderFirstTrial(dataV4_sorted, tool_name = "paddle")
#significant

#First Block
CompareOrderFirstBlock(dataV4_sorted, tool_name = "paddle")
#significant

#Last trial
CompareOrderLastTrial(dataV4_sorted, tool_name = "paddle")


#Last Block
CompareOrderLastBlock(dataV4_sorted, tool_name = "paddle")

#washout
CompareOrderWashoutFirstBlock(dataV4_sorted, tool_name = "paddle")



Curling rod


#First Trial
CompareOrderFirstTrial(dataV4_sorted, tool_name = "squeegee")
#significant

#First Block
CompareOrderFirstBlock(dataV4_sorted, tool_name = "squeegee")


#Last trial
CompareOrderLastTrial(dataV4_sorted, tool_name = "squeegee")


#Last Block
CompareOrderLastBlock(dataV4_sorted, tool_name = "squeegee")

#washout
CompareOrderWashoutFirstBlock(dataV4_sorted, tool_name = "squeegee")



