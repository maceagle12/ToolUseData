source('analyses/shared.R')


#plots individual data ----

plotLauchErrIndiv <- function (df, expeV) {
  
  #to save figures as a pdf doc
  fname = sprintf('./docs/LaunchAngleError_indiv_%s.pdf', expeV)
  pdf(fname, width = 12, height = 9)
  par(mfrow = c(2,2)) #2*2 grid
  
  #list all participants
  pp <- unique(df$ppid)
  
  for (i in 1:length(pp)) {
    
    #empty list to store plots
    plt_ls <- list()
    
    #get data for a single participant
    dataN <- df %>% 
      filter(ppid %in% pp[i])
    
    #get the name of the experiment
    expeV <- unique(dataN$experiment)
    
    #set title plot
    plot_ttl <- sprintf('%s - Participant %s', expeV, pp[i])
    
    
    #make plot
    plot(dataN$trial_num, dataN$launch_angle_err,
         type = 'l', col = 'gray50',
         xlab = 'Trials', ylab = 'Left <--- Angular error re: target (°) ---> Right',
         main = plot_ttl,
         ylim = c(-60, 45))
    points(dataN$trial_num, dataN$launch_angle_err,
           col = dataN$expe_phase)
    abline(h = c(-30, 0, 30))
    legend(x = 'bottomleft', col = unique(dataN$expe_phase), pch = 1,
           title = 'Experimental phase', legend = as.character(unique(dataN$expe_phase)),
           bg = 'white', horiz = TRUE, bty = 'n')
    
  }
  
  dev.off()
  
}



plotLast5Exposure_perColour <- function (df, expeV) {
  
  #to save figures as a pdf doc
  fname = sprintf('./docs/Last5TrialsExposure_perColour_%s.pdf', expeV)
  pdf(fname, width = 12, height = 9)
  par(mfrow = c(2,2)) #2*2 grid
  
  #list all participants
  pp <- unique(df$ppid)
  
  for (i in 1:length(pp)) {
    
    #empty list to store plots
    plt_ls <- list()
    
    #get data for a single participant
    dataN <- df %>% 
      filter(ppid %in% pp[i]) %>% 
      #keep exposure trials only
      filter(expe_phase == 'exposure') %>% 
      #keep the last 5 trials for each tool (df is already grouped by ppid and tool_used)
      do(tail(., 5))
    
    #get the name of the experiment
    expeV <- unique(dataN$experiment)
    
    #set title plot
    plot_ttl <- sprintf('%s - Participant %s\nMedian across 5 last trials', expeV, pp[i])
    
    
    #make plot
    boxplot(launch_angle_err_dir ~ as.factor(per_block_colour), data = dataN,
            xlab = '', ylab = 'Opposite to pert. <-- Angular error (°) --> Same side as pert.',
            main = plot_ttl,
            ylim = c(-40, 40))
    points(launch_angle_err_dir ~ as.factor(per_block_colour), data = dataN)
    abline(h = seq(-15, 15, 15), lty = 5)
    
  }
  
  dev.off()
  
}



plotLast5Exposure <- function (df, expeV) {
  
  #to save figures as a pdf doc
  fname = sprintf('./docs/Last5TrialsExposure_%s.pdf', expeV)
  pdf(fname, width = 12, height = 9)
  par(mfrow = c(2,2)) #2*2 grid
  
  #list all participants
  pp <- unique(df$ppid)
  
  for (i in 1:length(pp)) {
    
    #empty list to store plots
    plt_ls <- list()
    
    #get data for a single participant
    dataN <- df %>% 
      filter(ppid %in% pp[i]) %>% 
      #keep exposure trials only
      filter(expe_phase == 'exposure') %>% 
      #keep the last 5 trials for each tool (df is already grouped by ppid and tool_used)
      do(tail(., 5))
    
    #get the name of the experiment
    expeV <- unique(dataN$experiment)
    
    #set title plot
    plot_ttl <- sprintf('%s - Participant %s\nMedian across 5 last trials for both tools', expeV, pp[i])
    
    
    #make plot
    boxplot(dataN$launch_angle_err_dir,
            xlab = '', ylab = 'Opposite to pert. <-- Angular error (°) --> Same side as pert.',
            main = plot_ttl,
            ylim = c(-40, 40))
    points(dataN$launch_angle_err_dir)
    abline(h = seq(-15, 15, 15), lty = 5)
    
  }
  
  dev.off()
  
}



#plots averaged data ----

plotAdapt_all <- function (df) {
  
  plot <- list()
  
  for (i in 1:length(unique(df$experiment))) {
    
    expe_v <- unique(df$experiment)[i]
    
    #get the number of participants for the experiment
    Npp <- df %>%
      filter(experiment == expe_v)
    Npp <- length(unique(Npp$ppid))
    
    #calculate mean launch error angle across all participants
    df_i <- df %>% 
      filter(experiment == expe_v) %>% 
      group_by(trial_num, block_num, expe_phase) %>% 
      # summarise(mn_err = mean(launch_angle_err, na.rm = TRUE),
      #           sd_err = sd(launch_angle_err, na.rm = TRUE),
      #           .groups = 'drop')
      summarise(mn_err = mean(launch_angle_err_dir, na.rm = TRUE), #error direction instead of just error
                sd_err = sd(launch_angle_err_dir, na.rm = TRUE),
                n = n(),
                .groups = 'drop') %>% 
      #95% confidence intervals
      mutate(lower_ci = mn_err - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
             upper_ci = mn_err + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))
    
    
    #make plot
    plot[[i]] <- ggplot(data = df_i, 
                        aes(x = trial_num, y = mn_err,
                            color = expe_phase, fill = expe_phase,
                            group = interaction(expe_phase, block_num))) + 
      geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') +
      geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') +
      geom_hline(yintercept = 30, linetype = 'dashed', color = 'grey40') +
      # #sd
      # geom_ribbon(aes(ymin = mn_err - sd_err, max = mn_err + sd_err),
      #             linetype = 0, alpha = 0.5) +
      #95% CI
      geom_ribbon(aes(ymin = lower_ci, max = upper_ci),
                  linetype = 0, alpha = 0.5) +
      geom_line() +
      geom_point() + 
      
      theme_classic_article() +
      scale_color_discrete(name = 'Experimental phase') +
      scale_fill_discrete(name = 'Experimental phase') +
      labs(title = sprintf('%s (N = %s) - All trials', expe_v, Npp), 
           x = 'Trials', y = 'Side opposite to the perturbation <-- Angular error (°) --> Same side as the perturbation')
    
    # #print plot
    # print(plot[[i]])
    
    #save plot
    fname = sprintf('./docs/figures/plotAdapt_all_%s_.png', expe_v)
    ggsave(file=fname, plot=plot[[i]], width=13, height=8)
    
  }
  
}



plotAdapt_tool_work<- function (df) {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  plot <- list()
  
  for (i in 1:length(unique(df$experiment))) {
    
    expe_v <- unique(df$experiment)[i]
    
    #get the number of participants for the experiment
    Npp <- df %>%
      filter(experiment == expe_v)
    Npp <- length(unique(Npp$ppid))
    
    #calculate mean launch error angle across all participants
    df_i <- df %>% 
      filter(experiment == expe_v) %>% 
      group_by(trialN_tool, expe_phase, tool_used) %>% 
      summarise(mn_err = mean(launch_angle_err_dir, na.rm = TRUE),
                sd_err = sd(launch_angle_err_dir, na.rm = TRUE),
                n = n(),
                .groups = 'drop') %>% 
      #95% confidence intervals
     mutate(lower_ci = mn_err - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
           upper_ci = mn_err + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))
    
    
    #make plot
    plot[[i]] <- ggplot(data = df_i, 
                        aes(x = trialN_tool, y = mn_err, 
                            # color = interaction(tool_used, expe_phase), 
                            # fill = interaction(tool_used, expe_phase)
                            color = tool_used, 
                            fill = tool_used,
                            group = interaction(tool_used, expe_phase)
                            )) + 
      #margin of error (participants still get max points)
      geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
                fill = 'grey85', color = NA, alpha = 0.5) + 
      geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') +
      geom_hline(yintercept = c(-15,-10, -5,5,10, 15), linetype = 'dotted', color = 'grey40') +
      geom_hline(yintercept = 30, linetype = 'dashed', color = 'grey40') +
      # #sd
      # geom_ribbon(aes(ymin = mn_err - sd_err, max = mn_err + sd_err),
      #             linetype = 0, alpha = 0.4) +
      #95% CI
      geom_ribbon(aes(ymin = lower_ci, max = upper_ci),
                  linetype = 0, alpha = 0.5) +
      geom_line(size = 0.8) + 
      geom_point() + 
      
      theme_classic_article() + 
      scale_color_manual(name = 'Colour', values = myCols) + 
      scale_fill_manual(name = 'Colour', values = myCols) + 
      scale_y_continuous(breaks = seq(0, 30, 15), expand = c(0, 0)) + 
      labs(title = sprintf('%s (N = %s) - Per paddle', expe_v, Npp), 
           x = 'Trials per tool', y = 'Side opposite to the perturbation <-- Angular error (°) --> Same side as the perturbation') + 
      #make drawings unconfined to the plot panel
      coord_cartesian(clip = 'off') 
    
      #print plot
      print(plot[[i]])
    
    #save plot
    #fname = sprintf('./docs/figures/plotAdapt_tool_%s_.png', expe_v)
    #ggsave(file=fname, plot=plot[[i]], width=13, height=8)
    
  }
  
}



plotAdapt_tool_noPractice <- function (df, pp = 'all', save = TRUE, extension = 'png') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be')
  
  plot <- list()
  
  for (i in 1:length(unique(df$experiment))) {
    
    expe_v <- unique(df$experiment)[i]
    
    #get the number of participants for the experiment
    Npp <- df %>%
      filter(experiment == expe_v)
    Npp <- length(unique(Npp$ppid))
    
    #remove practice trials and renumber trialN_tool
    last_trialNcolour_practice <- df %>% 
      filter(expe_phase == 'practice') %>% 
      pull(trialN_colour) %>% 
      max()
    
    df <- df %>% 
      filter(expe_phase != 'practice') %>% 
      mutate(trialN_colour = trialN_colour - last_trialNcolour_practice)
    
    #calculate mean launch error angle across all participants
    df_i <- df %>% 
      filter(experiment == expe_v) %>% 
      group_by(trialN_colour, per_block_colour, expe_phase) %>% 
      summarise(mn_err = mean(launch_angle_err_dir, na.rm = TRUE), 
                sd_err = sd(launch_angle_err_dir, na.rm = TRUE),
                n = n(),
                .groups = 'drop') %>% 
      #95% confidence intervals
      mutate(lower_ci = mn_err - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
             upper_ci = mn_err + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))
    
    #plot title
    if (pp == 'all') {plot_ttl = sprintf('N = %s', Npp)}
    else {plot_ttl = sprintf('N = %s (learners only)', Npp)}
    
    
    #make plot
    plot[[i]] <- ggplot(data = df_i, 
                        aes(x = trialN_colour, y = mn_err, 
                            color = per_block_colour, 
                            fill =  per_block_colour,
                            group = interaction(per_block_colour, expe_phase)
                        )) + 
      #margin of error (participants still get max points)
      geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
                fill = 'grey85', color = NA, alpha = 0.5) + 
      geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') +
      geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') +
      geom_hline(yintercept = 30, linetype = 'dashed', color = 'grey40') +
      # #sd
      # geom_ribbon(aes(ymin = mn_err - sd_err, max = mn_err + sd_err),
      #             linetype = 0, alpha = 0.4) +
      #95% CI
      geom_ribbon(aes(ymin = lower_ci, max = upper_ci),
                  linetype = 0, alpha = 0.5) +
      geom_line(size = 0.8) + 
      geom_point() + 
      
      theme_classic_article() + 
      theme(text = element_text(size = 22)) + 
      scale_color_manual(name = 'Tool', values = myCols) + 
      scale_fill_manual(name = 'Tool', values = myCols) + 
      scale_y_continuous(breaks = seq(0, 30, 15), expand = c(0, 0)) + 
      labs(title = plot_ttl, 
           x = 'Trials per colour', y = 'Angular error at launch (°)') + 
      #make drawings unconfined to the plot panel
      coord_cartesian(clip = 'off')
    
    
    #print or save plot
    if (save == TRUE) {
      if (pp == 'all') {name_fig = 'all'}
      else {name_fig = 'learners'}
      fname = sprintf('./docs/figures/plotAdapt_tool_noPractice_%s_%s.%s', expeV, name_fig, extension)
      ggsave(file=fname, plot=plot[[i]], width=12, height=7)
      # ggsave(file=fname, plot=plot[[i]], width=19, height=7)
    } else {
      print(plot[[i]])
    }
    
  }
  
}



plotAdapt_rotation <- function (df) {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  plot <- list()
  
  for (i in 1:length(unique(df$experiment))) {
    
    expe_v <- unique(df$experiment)[i]
    
    #get the number of participants for the experiment
    Npp <- df %>%
      filter(experiment == expe_v)
    Npp <- length(unique(Npp$ppid))
    
    #calculate mean launch error angle across all participants
    df_i <- df %>% 
      filter(experiment == expe_v) %>% 
      group_by(trialN_colour, expe_phase, per_block_colour, sign_pert_tool) %>% 
      summarise(mn_err = mean(launch_angle_err, na.rm = TRUE),
                sd_err = sd(launch_angle_err, na.rm = TRUE),
                n = n(),
                .groups = 'drop') %>% 
      #95% confidence intervals
      mutate(lower_ci = mn_err - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
             upper_ci = mn_err + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))
    
    
    #make plot
    plot[[i]] <- ggplot(data = df_i, 
                        aes(x = trialN_colour, y = mn_err, 
                            # color = interaction(tool_used, sign_pert_tool, expe_phase), 
                            # fill = interaction(tool_used, sign_pert_tool, expe_phase)
                            color = per_block_colour, 
                            fill = per_block_colour,
                            group = interaction(per_block_colour, sign_pert_tool, expe_phase)
                            )) + 
      geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') +
      geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') +
      geom_hline(yintercept = c(-30, 30), linetype = 'dashed', color = 'grey40') +
      # #sd
      # geom_ribbon(aes(ymin = mn_err - sd_err, max = mn_err + sd_err),
      #             linetype = 0, alpha = 0.4) +
      #95% CI
      geom_ribbon(aes(ymin = lower_ci, max = upper_ci),
                  linetype = 0, alpha = 0.5) +
      geom_line(size = 0.8) + 
      geom_point() + 
      
      theme_classic_article() + 
      scale_color_manual(name = 'Tool', values = myCols) + 
      scale_fill_manual(name = 'Tool', values = myCols) + 
      scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0, 0)) + 
      labs(title = sprintf('%s (N = %s) - Per perturbation sign', expe_v, Npp), 
           x = 'Trials per perturbation sign', y = 'CCW <-- Angular error (°) --> CW') + 
      #make drawings unconfined to the plot panel
      coord_cartesian(clip = 'off')
    
    # #print plot
    # print(plot[[i]])
    
    #save plot
    fname = sprintf('./docs/figures/plotAdapt_rotation_%s_.png', expe_v)
    ggsave(file=fname, plot=plot[[i]], width=13, height=8)
    
  }
  
}



plotAdapt_EverySwitch <- function (df, WxL = c(10,7), 
                                    pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #get experiment version
  expeV <- unique(df$experiment)
  
  
  ##get data ----
  #get the 1st and last trial number per block_num
  trials_to_keep <- df %>% 
    filter(expe_phase != 'practice') %>% #remove practice trials
    group_by(block_num) %>% 
    filter(row_number() == 1 | row_number() == n()) %>% 
    arrange(trialN_colour) %>% 
    pull(trialN_colour) %>% 
    unique()
  
  #extract the first trials of each block_num
  #(every other element of trials_to_keep, starting from the 1st element)
  firstTr <- trials_to_keep[c(TRUE, FALSE)]
  #extract the last trials of each block_num
  #(every other element of trials_to_keep, starting from the 2nd element)
  lastTr <- trials_to_keep[c(FALSE, TRUE)]
  
  df <- df %>% 
    filter(trialN_colour %in% trials_to_keep) %>% 
    #create trialN
    mutate(trialN = case_when(trialN_colour %in% firstTr ~ 'First trial',
                              trialN_colour %in% lastTr ~ 'Last trial',
                              TRUE ~ 'NA')) %>%
    # #rearrange otherwise geom_line and geom_point for individual 
    # #data points don't have the same jitter and aren't aligned
    # arrange(trialN_tool, ppid, tool_used) %>% 
    group_by(trialN_colour, trialN, per_block_colour, expe_phase) %>% 
    summarise(mn_err = mean(launch_angle_err_dir, na.rm = TRUE),
              sd_err = sd(launch_angle_err_dir, na.rm = TRUE),
              n = n(),
              .groups = 'drop') %>% 
    mutate(blockN_colour = rep(1:(length(unique(trialN_colour))/2), each = 4),
           lower_ci = mn_err - qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)),
           upper_ci = mn_err + qt(1 - (0.05/2), n - 1) * (sd_err / sqrt(n)))
  
  
  #set plot title
  if (pp == 'learners') {ttl = 'First and last trials of every switch (learners only)'}
  else {ttl = 'First and last trials of every switch (all participants)'}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(color = interaction(per_block_colour, trialN),
                    fill = interaction(per_block_colour, trialN))) + 
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(30), linetype = 'dashed', color = 'grey40') + 
    # #lines between individual data points
    # geom_line(aes(group = ppid),
    #           color = 'grey50', alpha = 0.4,
    #           position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    # #boxplots for each tool used
    # geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) + 
    # #individual data points
    # geom_point(size = 2, alpha = 0.8,
    #            position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    # #add mean to boxplots
    # stat_summary(fun = 'mean', geom = 'line', size = 1, group = trialN,
    #              position = position_dodge(width = 0.9), show.legend = FALSE) + 
  #95% CI
  geom_ribbon(aes(x = trialN_colour, y = mn_err,
                  ymin = lower_ci, max = upper_ci,
                  group = interaction(per_block_colour, trialN, expe_phase)),
              linetype = 0, alpha = 0.5) +
  geom_line(aes(x = trialN_colour, y = mn_err, group = interaction(per_block_colour, trialN, expe_phase)),
                size = 1) +
    
    theme_classic_article() + 
    # scale_color_manual(name = 'Tool', values = myCols) + 
    # scale_fill_manual(name = 'Tool', values = myCols) + 
    scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0.02, 0)) + 
    labs(title = ttl, x = '', 
         y = 'Opposite to perturbation <-- Angular error (°) --> Same as perturbation') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/Adaptation_perSwitch_%s_%s.%s', expeV, name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])
  
}



#plots individual and averaged data ----

plotAngErr_FirstLast_Trial <- function (df, WxL = c(10,7), 
                                        pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #get experiment version
  expeV <- unique(df$experiment)
  
  #set plot title
  if (pp == 'learners') {ttl = 'Errors on single trials (learners only)'}
  else {ttl = 'Errors on single trials (all participants)'}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = per_block_colour, y = launch_angle_err_dir, 
                    color = per_block_colour, fill = per_block_colour)) + 
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(-30, 30), linetype = 'dashed', color = 'grey40') + 
    facet_grid(. ~ expe_phase + trialN) + 
    # #lines between individual data points
    # geom_line(aes(group = ppid),
    #           color = 'grey50', alpha = 0.4,
    #           position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #boxplots for each tool used
    geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) + 
    #individual data points
    geom_point(size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #add mean to boxplots
    stat_summary(fun = 'mean', geom = 'point', size = 4.5, shape = 18, 
                 color = 'white', show.legend = FALSE) + 
    
    theme_classic_article() +
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols) + 
    scale_fill_manual(name = 'Tool', values = myCols) + 
    scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0.02, 0)) + 
    labs(title = ttl, x = '', 
         y = 'Opposite to perturbation <-- Angular error (°) --> Same as perturbation') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/AngError_perTrial_%s_%s.%s', expeV, name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])
  
}


plotAngErr_FirstLast_Trial_pres <- function (df, WxL = c(12,7), 
                                        pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #get experiment version
  expeV <- unique(df$experiment)
  
  #get the number of participants
  npp <- length(unique(df$ppid))
  
  #set plot title
  if (pp == 'all') {ttl = sprintf('N = %s', npp)}
  else {ttl = sprintf('N = %s (learners only)', npp)}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = per_block_colour, y = launch_angle_err_dir, 
                    color = per_block_colour, fill = per_block_colour)) + 
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(-30, 30), linetype = 'dashed', color = 'grey40') + 
    facet_grid(. ~ expe_phase + trialN) + 
    #boxplots for each tool used
    geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) + 
    #individual data points
    geom_point(size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #add mean to boxplots
    stat_summary(fun = 'mean', geom = 'point', size = 4.5, shape = 18, 
                 color = 'white', show.legend = FALSE) + 
    
    theme_classic_article() +
    theme(text = element_text(size = 15)) + 
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols) + 
    scale_fill_manual(name = 'Tool', values = myCols) + 
    scale_y_continuous(limits = c(-35, 73), breaks = seq(-30, 30, 15), expand = c(0.02, 0)) +
    labs(title = ttl, x = '', 
         y = 'Angular error at launch (°)') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/AngError_perTrial_Pres_%s_%s.%s', expeV, name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])
  
}



plotAngErr_FirstLast_Block <- function (df, WxL = c(10,7), 
                                        pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #get experiment version
  expeV <- unique(df$experiment)
  
  #set plot title
  if (pp == 'learners') {ttl = 'Errors averaged across 1 block (learners only)'}
  else {ttl = 'Errors averaged across 1 block (all participants)'}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = per_block_colour, y = mn_launch_angle_err_dir, 
                    color = per_block_colour, fill = per_block_colour)) + 
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(-30, 30), linetype = 'dashed', color = 'grey40') + 
    facet_grid(. ~ expe_phase + blockN) + 
    # #lines between individual data points
    # geom_line(aes(group = ppid),
    #           color = 'grey50', alpha = 0.4,
    #           position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #boxplots for each tool used
    geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) + 
    #individual data points
    geom_point(size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #add mean to boxplots
    stat_summary(fun = 'mean', geom = 'point', size = 4.5, shape = 18, 
                 color = 'white', show.legend = FALSE) + 
    
    theme_classic_article() +
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols) +
    scale_fill_manual(name = 'Tool', values = myCols) +
    scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0.02, 0)) + 
    labs(title = ttl, x = '', 
         y = 'Opposite to perturbation <-- Angular error (°) --> Same as perturbation') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/AngError_perBlock_%s_%s.%s', expeV, name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])

}


plotAngErr_FirstLast_Block_pres <- function (df, WxL = c(12,7), 
                                        pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #get experiment version
  expeV <- unique(df$experiment)
  
  #get the number of participants
  npp <- length(unique(df$ppid))
  
  #set plot title
  if (pp == 'all') {ttl = sprintf('N = %s', npp)}
  else {ttl = sprintf('N = %s (learners only)', npp)}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = per_block_colour, y = mn_launch_angle_err_dir, 
                    color = per_block_colour, fill = per_block_colour)) + 
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = 30, linetype = 'dashed', color = 'grey40') + 
    facet_grid(. ~ expe_phase + blockN) + 
    #boxplots for each tool used
    geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) + 
    #individual data points
    geom_point(size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #add mean to boxplots
    stat_summary(fun = 'mean', geom = 'point', size = 4.5, shape = 18, 
                 color = 'white', show.legend = FALSE) + 
    
    theme_classic_article() +
    theme(text = element_text(size = 20)) + 
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols) +
    scale_fill_manual(name = 'Tool', values = myCols) + 
    scale_y_continuous(limits = c(-16, 45), breaks = seq(-30, 30, 15), expand = c(0.02, 0)) +
    labs(title = ttl, x = '', 
         y = 'Angular error at launch (°)') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/Pres_AngError_perBlock_%s_%s.%s', expeV, name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])
  
}



plotAngErr_FirstLast_4Trial_pres <- function (df, WxL = c(12,7), 
                                             pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #get experiment version
  expeV <- unique(df$experiment)
  
  #get the number of participants
  npp <- length(unique(df$ppid))
  
  #set plot title
  if (pp == 'all') {ttl = sprintf('N = %s', npp)}
  else {ttl = sprintf('N = %s (learners only)', npp)}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = per_block_colour, y = mn_launch_angle_err_dir,
                    color = per_block_colour, fill = per_block_colour)) +
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(30), linetype = 'dashed', color = 'grey40') + 
    facet_grid(. ~ expe_phase + trialN) + 
    #boxplots for each tool used
    geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) + 
    #individual data points
    geom_point(size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #add mean to boxplots
    stat_summary(fun = 'mean', geom = 'point', size = 4.5, shape = 18, 
                 color = 'white', show.legend = FALSE) + 
    
    theme_classic_article() +
    theme(text = element_text(size = 15)) + 
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols) + 
    scale_fill_manual(name = 'Tool', values = myCols) + 
    scale_y_continuous(limits = c(-20, 45), breaks = seq(-30, 30, 15), expand = c(0.02, 0)) +
    labs(title = ttl, x = '', 
         y = 'Angular error at launch (°)') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/Pres_AngError_per4Trials_%s_%s.%s', expeV, name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])
  
}



plotAngErr_FirstLast_4Trial_V1V2_pres <- function (df, WxL = c(12,7), 
                                              pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#dd571c', '#1338be', '#1338be')
  myFills <- c('NA', '#dd571c', '#1338be', 'NA')
  
  # #get experiment version
  # expeV <- unique(df$experiment)
  # 
  # #get the number of participants
  # npp <- length(unique(df$ppid))
  # 
  # #set plot title
  # if (pp == 'all') {ttl = sprintf('N = %s', npp)}
  # else {ttl = sprintf('N = %s (learners only)', npp)}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = tool_expe, y = mn_launch_angle_err_dir)) +
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(30), linetype = 'dashed', color = 'grey40') + 
    facet_grid(. ~ expe_phase + trialN) + 
    #boxplots for each tool used
    geom_boxplot(aes(color = tool_expe, fill = tool_expe), 
                 outlier.shape = NA, alpha = 0.6) + 
    #individual data points
    geom_point(aes(color = tool_expe, fill = tool_expe), shape = 21, stroke = 0.7,
               size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #add mean to boxplots
    stat_summary(fun = 'mean', geom = 'point', size = 3.5, shape = 23, 
                 color = 'black', fill = 'white', show.legend = FALSE) + 
    
    theme_classic_article() +
    theme(text = element_text(size = 18)) + 
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols, 
                       labels = c('paddle (single)', 'paddle (dual)', 'slingshot (dual)', 'slingshot (single)')) + 
    scale_fill_manual(name = 'Tool', values = myFills, 
                      labels = c('paddle (single)', 'paddle (dual)', 'slingshot (dual)', 'slingshot (single)')) + 
    scale_y_continuous(limits = c(-20, 45), breaks = seq(-30, 30, 15), expand = c(0.02, 0)) +
    labs(x = '', 
         y = 'Angular error at launch (°)') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/Pres_V1V2_AngError_per4Trials_%s.%s', name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])

}



plotAngErr_EverySwitch <- function (df, WxL = c(10,7), 
                                        pp = 'all', extension = 'svg') {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #get experiment version
  expeV <- unique(df$experiment)
  
  
  ##get data ----
  #get the 1st and last trial number per block_num
  trials_to_keep <- df %>% 
    filter(expe_phase != 'practice') %>% #remove practice trials
    group_by(block_num) %>% 
    filter(row_number() == 1 | row_number() == n()) %>% 
    arrange(trialN_colour) %>% 
    pull(trialN_colour)
  
  #extract the first trials of each block_num
  #(every other element of trials_to_keep, starting from the 1st element)
  firstTr <- trials_to_keep[c(TRUE, FALSE)]
  #extract the last trials of each block_num
  #(every other element of trials_to_keep, starting from the 2nd element)
  lastTr <- trials_to_keep[c(FALSE, TRUE)]
  
  df <- df %>% 
    filter(trialN_colour %in% trials_to_keep) %>% 
    #create trialN
    mutate(trialN = case_when(trialN_colour %in% firstTr ~ 'First trial',
                              trialN_colour %in% lastTr ~ 'Last trial',
                              TRUE ~ 'NA')) %>%
    #rearrange otherwise geom_line and geom_point for individual 
    #data points don't have the same jitter and aren't aligned
    arrange(trialN_colour, ppid, per_block_colour)
  
  
  #set plot title
  if (pp == 'learners') {ttl = 'Errors on single trials (learners only)'}
  else {ttl = 'Errors on single trials (all participants)'}
  
  #make plot
  plt <- ggplot(data = df, 
                aes(x = per_block_colour, y = launch_angle_err_dir, 
                    color = per_block_colour, fill = per_block_colour)) + 
    #margin of error (participants still get max points)
    geom_rect(mapping = aes(xmin = -Inf, xmax = +Inf, ymin = -3, ymax = +3), 
              fill = 'grey85', color = NA, alpha = 0.5) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-15, 15), linetype = 'dotted', color = 'grey40') + 
    geom_hline(yintercept = c(-30, 30), linetype = 'dashed', color = 'grey40') + 
    facet_grid(. ~ trialN_colour) + 
    # #lines between individual data points
    # geom_line(aes(group = ppid),
    #           color = 'grey50', alpha = 0.4,
    #           position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #boxplots for each tool used
    geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) + 
    #individual data points
    geom_point(size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #add mean to boxplots
    stat_summary(fun = 'mean', geom = 'point', size = 4.5, shape = 18, 
                 color = 'white', show.legend = FALSE) + 
    
    theme_classic_article() +
    #remove x axis elements
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) + 
    scale_color_manual(name = 'Tool', values = myCols) + 
    scale_fill_manual(name = 'Tool', values = myCols) + 
    scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0.02, 0)) + 
    labs(title = ttl, x = '', 
         y = 'Opposite to perturbation <-- Angular error (°) --> Same as perturbation') 
  
  
  #save plot
  if (pp == 'all') {name_fig = 'all'}
  else {name_fig = 'learners'}
  fname = sprintf('./docs/figures/AngError_perSwitch_%s_%s.%s', expeV, name_fig, extension)
  ggsave(file=fname, plot=plt, width=WxL[1], height=WxL[2])
  
}



plotImprove_First_Last <- function (df) {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #calculate improvement between last and first trials
  #(> 0 is more errors during last trial, < 0 is less errors during last trial)
  df_wide <- df %>% 
    dplyr::select(experiment, first_pert_cond, ppid, expe_phase, 
                  per_block_colour, launch_angle_err_dir, trialN) %>% 
    #transform into wide format
    spread(trialN, launch_angle_err_dir) %>% 
    arrange(ppid, expe_phase) %>% 
    mutate(percent_improv = (`Last trial` - `First trial`) / `First trial` * 100) %>% 
    filter(percent_improv > -1000 & percent_improv < 1000)
  
  
  #make plot
  plt <- ggplot(data = df_wide, 
                aes(x = expe_phase, y = percent_improv, 
                    color = per_block_colour, fill = per_block_colour)) + 
    geom_hline(yintercept = 0, linetype = 'solid', color = 'grey40') + 
    geom_hline(yintercept = c(-100, 100), linetype = 'dashed', color = 'grey40') +
    # #lines between individual data points
    # geom_line(aes(group = ppid),
    #           color = 'grey50', alpha = 0.4,
    #           position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    #boxplots for each tool used
    geom_boxplot(outlier.shape = NA, color = 'black', alpha = 0.6) +
    #individual data points
    geom_point(size = 2, alpha = 0.8,
               position = position_jitter(width = 0.1, seed = 1)) + #set seed to make jitter reproducible
    
    theme_classic_article() + 
    scale_color_manual(name = 'Tool', values = myCols) +
    scale_fill_manual(name = 'Tool', values = myCols) +
    # scale_y_continuous(breaks = seq(-30, 30, 15), expand = c(0.02, 0)) + 
    labs(x = '', y = 'Improvement between last and first trial') 
  
  print(plt)
  
}



#plots for methods

plotPertSchedule_V1 <- function () {
  
  #custom colors
  myCols <- c('#dd571c', '#1338be') 
  
  #create a data frame
  df <- data.frame(
    trialNo = seq(1:336),
    size_pert = rep(c(-30, 30), times = 21, each = 8)
  )
  
  #add size of the perturbation
  size_pert_is0 <- c(c(1:80), c(241:288))
  df <- df %>% 
    mutate(size_pert = ifelse(trialNo %in% size_pert_is0, 0, size_pert))
  
  #set phase labels
  PhLbl <- data.frame(
    label = c('Baseline', 'Exposure', 'Washout', 'Reexposure'),
    xpos = c(mean(c(1, 81)), 
             mean(c(81, 241)), 
             mean(c(241, 289)), 
             mean(c(289, 336))),
    ypos = rep(38, 4)
  )
  
  #set ticks to show tool switching during baseline and exposure
  ticks_tool <- data.frame(
    x = c(seq(1, 80, 8), seq(241, 288, 8)),
    y = c(rep(-1, 16)),
    yend = c(rep(1, 16))
  )
  
  #make plot
  plt <- ggplot(data = df,
                 aes(x = trialNo, y = size_pert)) +
    geom_vline(xintercept = c(81, 241, 289), linetype = 'dashed', color = 'grey40') + 
    geom_step() + #use geom_step to create a staircase plot
    geom_segment(data = ticks_tool, 
                 aes(x = x, y = y, xend = x, yend = yend)) +  
    geom_text(data = PhLbl, aes(x = xpos, y = ypos, label = label), size = 6) + 
    
    theme_classic() + 
    theme(text = element_text(size = 17)) + 
    scale_x_continuous(breaks = c(1, 81, 241, 289, 336), expand = c(0.01, 0.01)) + 
    scale_y_continuous(breaks = c(-30, 0, 30), labels = c('30 CW', '0', '30 CCW')) + 
    labs(x = 'Trial', y = 'Perturbation size (°)') 
  
  
  #save plot
  fname = sprintf('./docs/figures/perturbSchedule_V1.svg')
  ggsave(file=fname, plot=plt, width=12, height=6)
  
}



#custom theme for ggplot ----

theme_classic_article <- function(){
  
  theme_classic() %+replace%
    
    theme(
      
      #text elements
      text = element_text(size = 12),
      
      #legend elements
      legend.position = 'top',
      
      # #margins around plot (top, right, bottom, left)
      # plot.margin = unit(c(10,15,0,15), 'pt'),
      
      #axes elements
      # axis.title.x.bottom = element_text(margin = margin(10,0,0,0, 'pt')), #add top margin to x label
      # axis.title.y.left = element_text(margin = margin(0,10,0,0, 'pt')), #add right margin to y label
      axis.text   = element_text(color = 'grey40'),
      axis.line   = element_line(color = 'grey40'),
      axis.ticks  = element_line(color = 'grey40'),
      axis.ticks.length = unit(5, 'pt')
      
    )
  
}

