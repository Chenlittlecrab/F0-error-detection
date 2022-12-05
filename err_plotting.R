##############################################################
# this script plots F0 values with different error taggings.

# load library
library(tidyverse)
library(ggplot2)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### Load and prepare dfs to be used for plotting #####################

# set the directory to the folder that contains the three .txt files
setwd("/Users/chenz/Documents/CUNY/QP1/Stats/modeling/pitch_range_data/dataset")

# load two files marked with sd method (one marked errors with 2sd and the other 2.5 sd)
# one with 2sd
df_1 <- read_delim("all_speaker_sd_2.txt")

# one with 2.5 sd
df_2 <- read_delim("all_speaker_sd_2_point_5.txt")

# load the file marked with Steffman & Cole's method
df_3 <- read_delim("all_speaker_annotated_all_steffman_cole.txt")


# select the columns needed and rename some
df_3 <- df_3 %>% 
  select(Filename, t_ms, strF0, flagged_samples, speaker) %>% 
  mutate(speaker = paste("gd", as.character(speaker), sep = ""))

# change error marking of all dfs into the same name: errors
df_3$errors <- df_3$flagged_samples
df_2$errors <- df_2$err
df_1$errors <- df_1$err

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### Plot by speaker ##################################################
# plot by speaker, get a rough picture of how each method of error does
# this function iteratively plot each speaker's all F0 values
# input: df (df_1, df_2, or df_3); methods_str: string that specifies different methods of error taggings
plot_speaker <- function(df, methods_str){
  # methods_str can be either "2sd" or "2.5sd", or "Steffman_cole_method"
  speaker_ls <- unique(df$speaker)
  for (i_speaker in 1:length(speaker_ls)){
    df_speaker <- df %>% 
      subset(df$speaker == speaker_ls[i_speaker]) %>% 
      mutate(errors = as.factor(errors))
    print(
      ggplot(data = df_speaker, aes(x = t_ms, y = strF0, color = errors))+
        geom_point(aes(color=errors, shape = errors))+
        theme_minimal()+
        labs(title = paste(speaker_ls[i_speaker], "_", methods_str, sep = ""))
    )
  } # end of the loop
} # end of the function

# Call function and plot
# example 1: plot each speaker's all F0 values with Steffman & Cole's method
plot_speaker(df_3, "Steffman_cole_method")
# plots can be seen on the Plots panel of RStudio,
# all plots are printed out till the last plot
# click <- button to view previous one

# example 2: plot each speaker's all F0 values with 2sd method
plot_speaker(df_1, "2sd")

# example 3: plot each speaker's all F0 values with 2.5sd method
plot_speaker(df_2, "2.5sd")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### Plot by each F0 trajectory (i.e. each sound file) ################
# Use this function of plotting to look closely into each of the 96 sound files
# for each speaker
# function
plot_traj <- function(df, methods_str){
  # methods_str can be either "2sd" or "2.5sd", or "Steffman_cole_method"
  file_ls <- unique(df$Filename)
  for (i_file in 1:length(file_ls)){
    df_file <- df %>% 
      subset(df$Filename == file_ls[i_file]) %>% 
      mutate(errors = as.factor(errors))
    print(
      ggplot(data = df_file, aes(x = t_ms, y = strF0, color = errors))+
        geom_point(aes(color=errors, shape = errors))+
        theme_minimal()+
        labs(title = paste(str_sub(file_ls[i_file], end = -5), 
                           "_", methods_str, sep = ""))
    )
  }
}

# How to use this function:
# First, subset one speaker's data from either df_1, df_2 or df_3
# This example get a df for speaker gd11 ONLY
# get df for gd11 with 2sd of error marking
gd_11_sd <- df_1 %>% 
  subset(df_1$speaker == "gd11")

# get df for gd11 with Steffman and Cole's method or error marking
gd_11_steff <- df_3 %>% 
  subset(df_3$speaker == "gd11")

# plot each F0's trajectory for speaker gd11 with 2sd method
plot_traj(gd_11_sd, "2sd")

# plot each F0's trajectory for speaker gd11 with Steffman & Cole's method
plot_traj(gd_11_steff, "Steffman")


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# error category interpretations #
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

## for steffman and Cole's: errors = 0: not an F0 error; errors = 1: an F0 error
## for sd method: errors = 0: not an F0 error; errors = 1: F0 error detected in the initial filtering;errrors = 2: F0 error detected in the second filtering



