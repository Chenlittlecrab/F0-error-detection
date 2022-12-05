library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr) # for using function join_all

# set the directory to the folder that contains the file
setwd("/Users/chenz/Documents/CUNY/QP1/Stats/modeling/pitch_range_data")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#### preliminary data cleaning, only run ONCE ########################
##### clean up some strange characters in sound files ################
##### for speaker 59, 60, and 61 #####################################
# original gd59  file
# df_gd59_or <- read_delim("gd59_output.txt") %>% 
#   mutate(filename = ifelse(startsWith(Filename, "gd59"), 
#                            Filename, 
#                            str_sub(Filename, start = 2))) %>%
#   select(-Filename) %>% 
#   rename(Filename = filename)
# write.table(df_gd59_or,file = "gd59_output.txt", sep="\t", row.names=FALSE)
# 
# # original gd60
# df_gd60_or <- read_delim("gd60_output.txt") %>% 
#   mutate(filename = ifelse(startsWith(Filename, "gd60"), 
#                            Filename, 
#                            str_sub(Filename, start = 2))) %>%
#   select(-Filename) %>% 
#   rename(Filename = filename)
# write.table(df_gd60_or,file = "gd60_output.txt", sep="\t", row.names=FALSE)
# 
# # original gd61
# df_gd61_or <- read_delim("gd61_output.txt") %>% 
#   mutate(filename = ifelse(startsWith(Filename, "gd61"), 
#                            Filename, 
#                            str_sub(Filename, start = 2))) %>%
#   select(-Filename) %>% 
#   rename(Filename = filename)
# write.table(df_gd61_or,file = "gd61_output.txt", sep="\t", row.names=FALSE)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#### Iteratively read each speaker's file into a df ###################
all_files <- list.files(pattern = "*.txt", full.names = FALSE)
all_files

# iteratively plot all speakers' pooled f0 values
# for (i in 1: length(all_files)){
#   df_tmp <- read_delim(all_files[i])
#   print(ggplot(data = df_tmp, aes(x = t_ms, y = strF0))+
#           geom_point() +
#           theme_classic() +
#           labs(title = sprintf("%s", str_sub(all_files[i], 1, -12))
#                )
#         )
#   name <- paste("df_", str_sub(all_files[i], 1, -12), sep="")
#   assign(name,df_tmp)
# }

# iteratively read all speakers' file into seperate df
# and save their original plots for future reference (run only ONCE)
for (i in 1: length(all_files)){
  df_tmp <- read_delim(all_files[i]) %>% 
    filter(strF0 != 0,
           Label == "V") 
  # %>% select(-...7) some file does not have this nonsense column
  # plot part only needs to be run ONCE
  # ggplot(data = df_tmp, aes(x = t_ms, y = strF0))+
  #         geom_point() +
  #         theme_classic() +
  #         labs(title = sprintf("%s", str_sub(all_files[i], 1, -12))
  #              )
  # ggsave(filename = sprintf("%s.png", str_sub(all_files[i], 1, -12)))
  name <- paste("df_", str_sub(all_files[i], 1, -12), sep="")
  assign(name,df_tmp)
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### initial filtering and plotting function ##########################
# filtering out too high and too low values
# variabe: df, filter high, filter low, title for the plot
initial_filter_plot_df <- function(df,filter_high, filter_low, speaker){
  df <- df %>% 
    filter(strF0 < filter_high,
           strF0 > filter_low,
           strF0 != 0,
           Label == "V") %>% 
    select(-...7)
  print(ggplot(data = df, aes(x = t_ms, y = strF0))+
          geom_point() +
          theme_classic() + 
          labs(title = sprintf("%s", speaker))
  )
  return(df)
}

#### initial filtering without plotting nor error label #######################
initial_filter_no_err <- function(df,filter_high, filter_low, speaker){
  df <- df %>% 
    filter(strF0 < filter_high,
           strF0 > filter_low,
           strF0 != 0,
           Label == "V")
  return(df)
}

#### initial filtering with error label #######################################
initial_filter<- function(df,filter_high, filter_low, speaker){
  df <- df %>% 
    filter(strF0 != 0,
           Label == "V") %>% 
    mutate(err = case_when(strF0 >= filter_high ~ 1,
                           strF0 <= filter_low ~ 1,
                           strF0 < filter_high&strF0 > filter_low ~ 0)) %>% 
    select(-...7) # delete nonsense column
  return(df)
}

#### initial filtering for speaker 59, 60, 61, since they do not have nonsense column
initial_filter_spec<- function(df,filter_high, filter_low, speaker){
  df <- df %>% 
    filter(strF0 != 0,
           Label == "V") %>% 
    mutate(err = case_when(strF0 >= filter_high ~ 1,
                           strF0 <= filter_low ~ 1,
                           strF0 < filter_high&strF0 > filter_low ~ 0))
  return(df)
}

# plot function
initial_plot <- function(df, speaker){
  df <- df %>% mutate(err = as.factor(err))
  ggplot(data = df, aes(x = t_ms, y = strF0, color = err))+
    geom_point(aes(color = err))+
    theme_minimal()+
    labs(title = sprintf("%s", speaker))
}

# apply function to each speaker depending on the plot
# for speaker whose files do not need first filter,
# set high filter value as 800 and low as 0
df_gd82 <- initial_filter(df_gd82, 240, 70, "gd82")
initial_plot(df_gd82, "gd82")
df_gd8 <- initial_filter(df_gd8, 540, 70, "gd8")
initial_plot(df_gd8, "gd8")


df_gd79 <- initial_filter(df_gd79, 300, 100, "gd79")
initial_plot(df_gd79, "gd79")
df_gd77 <- initial_filter(df_gd77, 240, 0, "gd77")
initial_plot(df_gd77, "gd77")
df_gd76 <- initial_filter(df_gd76, 230, 0, "gd76")
df_gd75 <- initial_filter(df_gd75, 160, 0, "gd75") 
df_gd74 <- initial_filter(df_gd74, 270, 110, "gd74") 
df_gd73 <- initial_filter(df_gd73, 220, 0, "gd73") 
df_gd72 <- initial_filter(df_gd72, 250, 40, "gd72") 
df_gd71 <- initial_filter(df_gd71, 800, 0, "gd71") # not sure if the second filter is needed
df_gd70 <- initial_filter(df_gd70, 360, 150, "gd70")  
df_gd7 <- initial_filter(df_gd7, 220, 0, "gd7")

df_gd69 <- initial_filter(df_gd69, 800, 0, "gd69") 
df_gd68 <- initial_filter(df_gd68, 225, 60, "gd68")
df_gd67 <- initial_filter(df_gd67, 200, 70, "gd67")
df_gd66 <- initial_filter(df_gd66, 250, 50, "gd66") # throw out anything later than 4500 ms
df_gd65 <- initial_filter(df_gd65, 280, 0, "gd65")
df_gd64 <- initial_filter(df_gd64, 180, 0, "gd64")
df_gd63 <- initial_filter(df_gd63, 225, 80, "gd63") 
df_gd61 <- initial_filter(df_gd61, 210, 0, "gd61")
df_gd60 <- initial_filter(df_gd60, 400, 0, "gd60")
df_gd6 <- initial_filter(df_gd6, 150, 60, "gd6") 

df_gd59 <- initial_filter(df_gd59, 400, 100, "gd59") # not sure about low
df_gd58 <- initial_filter(df_gd58, 200, 0, "gd58") 
df_gd57 <- initial_filter(df_gd57, 240, 0, "gd57") 
df_gd56 <- initial_filter(df_gd56, 200, 0, "gd56") 
df_gd55 <- initial_filter(df_gd55, 380, 100, "gd55") 
df_gd54 <- initial_filter(df_gd54, 500, 75, "gd54") 
df_gd53 <- initial_filter(df_gd53, 320, 0, "gd53") # not sure about low 
df_gd52 <- initial_filter(df_gd52, 320, 0, "gd52")
df_gd51 <- initial_filter(df_gd51, 250, 150, "gd51") 
df_gd50 <- initial_filter(df_gd50, 350, 150, "gd50") 
df_gd5 <- initial_filter(df_gd5, 320, 90, "gd5") 

df_gd49 <- initial_filter(df_gd49, 275, 140, "gd49") 
df_gd48 <- initial_filter(df_gd48, 240, 0, "gd48")
df_gd47 <- initial_filter(df_gd47, 350, 0, "gd47") # need to check all plots
df_gd46 <- initial_filter(df_gd46, 350, 0, "gd46") 
df_gd45 <- initial_filter(df_gd45, 420, 120, "gd45") 
df_gd44 <- initial_filter(df_gd44, 330, 160, "gd44") 
df_gd43 <- initial_filter(df_gd43, 350, 0, "gd43") 
df_gd40 <- initial_filter(df_gd40, 800, 0, "gd40")
df_gd4 <- initial_filter(df_gd4, 800, 85, "gd4")

df_gd39 <- initial_filter(df_gd39, 400, 0, "gd39") 
df_gd38 <- initial_filter(df_gd38, 300, 90, "gd38") 
df_gd37 <- initial_filter(df_gd37, 300, 0, "gd37") 
df_gd36 <- initial_filter(df_gd36, 320, 50, "gd36") 
df_gd35 <- initial_filter(df_gd35, 330, 150, "gd35") 
df_gd34 <- initial_filter(df_gd34, 210, 50, "gd34") 
df_gd33 <- initial_filter(df_gd33, 320, 80, "gd33") 
df_gd32 <- initial_filter(df_gd32, 275, 0, "gd32") 
df_gd31 <- initial_filter(df_gd31, 200, 0, "gd31")
df_gd30 <- initial_filter(df_gd30, 175, 60, "gd30")  
df_gd3 <- initial_filter(df_gd3, 350, 150, "gd3") 

df_gd29 <- initial_filter(df_gd29, 300, 70, "gd29") # (plot ones below 100 ms),any data points after 5000 ms
df_gd28 <- initial_filter(df_gd28, 190, 75, "gd28") 
df_gd27 <- initial_filter(df_gd27, 350, 0, "gd27") # check the plots for those that are below 175 Hz
head(df_gd27)
df_gd26 <- initial_filter_spec(df_gd26, 400, 75, "gd26")
head(df_gd26)
df_gd25 <- initial_filter(df_gd25, 380, 0, "gd25") 
head(df_gd25)
df_gd24 <- initial_filter(df_gd24, 250, 75, "gd24") 
df_gd23 <- initial_filter(df_gd23, 375, 100, "gd23") 
df_gd22 <- initial_filter_spec(df_gd22, 300, 50, "gd22")
df_gd21 <- initial_filter(df_gd21, 300, 80, "gd21") 
df_gd20 <- initial_filter(df_gd20, 170, 0, "gd20") # throw away time beyond 6000 ms?
df_gd2 <- initial_filter(df_gd2, 310, 0, "gd2") 

df_gd19 <- initial_filter(df_gd19, 410, 90, "gd19") 
df_gd18 <- initial_filter(df_gd18, 190, 0, "gd18") 
df_gd17 <- initial_filter(df_gd17, 200, 75, "gd17") 
df_gd16 <- initial_filter(df_gd16, 225, 0, "gd16") 
df_gd15 <- initial_filter(df_gd15, 200, 75, "gd15") 
df_gd14 <- initial_filter(df_gd14, 300, 50, "gd14")
df_gd13 <- initial_filter(df_gd13, 250, 60, "gd13")  
df_gd12 <- initial_filter(df_gd12, 300, 0, "gd12") 
df_gd11 <- initial_filter_spec(df_gd11, 275, 75, "gd11") # rerun without select
df_gd1 <- initial_filter(df_gd1, 275, 70, "gd1") # check ones below 120



# put all filtered files into the new list
# to create the input of the second filter function
# all_files_new <- bind_rows(df_gd1, df_gd11, df_gd12, df_gd13, df_gd14, df_gd15, df_gd16, df_gd17, df_gd18, df_gd19,
#                       df_gd2, df_gd20, df_gd21, df_gd22, df_gd23, df_gd24, df_gd25, df_gd26, df_gd27, df_gd28, df_gd29,
#                       df_gd3, df_gd30, df_gd31, df_gd32, df_gd33, df_gd34, df_gd35, df_gd36, df_gd37, df_gd38, df_gd39,
#                       df_gd4, df_gd40, df_gd43, df_gd44, df_gd45, df_gd46, df_gd47, df_gd48, df_gd49,
#                       df_gd5, df_gd50, df_gd51, df_gd52, df_gd53, df_gd54, df_gd55, df_gd56, df_gd57, df_gd58, df_gd59,
#                       df_gd6, df_gd60, df_gd61, df_gd63, df_gd64, df_gd65, df_gd66, df_gd67, df_gd68, df_gd69,
#                       df_gd7, df_gd70, df_gd71, df_gd72, df_gd73, df_gd74, df_gd75, df_gd76, df_gd77, df_gd79,
#                       df_gd8, df_gd82
# )

# write.table(all_files_new, file = "all_speaker_pitch_initial_filtered_info_needed.txt", sep = "\t", row.names = FALSE)


df_all_files <- rbind(df_gd1, df_gd11, df_gd12, df_gd13, df_gd14, df_gd15, df_gd16, df_gd17, df_gd18, df_gd19,
                      df_gd2, df_gd20, df_gd21, df_gd22, df_gd23, df_gd24, df_gd25, df_gd26, df_gd27, df_gd28, df_gd29,
                      df_gd3, df_gd30, df_gd31, df_gd32, df_gd33, df_gd34, df_gd35, df_gd36, df_gd37, df_gd38, df_gd39,
                      df_gd4, df_gd40, df_gd43, df_gd44, df_gd45, df_gd46, df_gd47, df_gd48, df_gd49,
                      df_gd5, df_gd50, df_gd51, df_gd52, df_gd53, df_gd54, df_gd55, df_gd56, df_gd57, df_gd58, df_gd59,
                      df_gd6, df_gd60, df_gd61, df_gd63, df_gd64, df_gd65, df_gd66, df_gd67, df_gd68, df_gd69,
                      df_gd7, df_gd70, df_gd71, df_gd72, df_gd73, df_gd74, df_gd75, df_gd76, df_gd77, df_gd79,
                      df_gd8, df_gd82
)

# create speaker column for the big file
df_all_files <- df_all_files %>% 
  mutate(speakerID = str_sub(Filename, 1, 4)) %>% 
  # eliminate _ in the string for speaker ID
  mutate(speaker = ifelse(endsWith(speakerID, "_"), 
                          str_sub(speakerID, end = -2), speakerID)) %>% 
  select(-speakerID)

# test how to combine dfs in a list into a large df
some_list <- list(df_gd1, df_gd2)
df_list <- some_list %>% 
  reduce(full_join)
df_list


# second filtering based on initial_filter function (with err labeled!!!) 
second_filter <- function(df, sd_value){
  df_ls <- list()
  speaker_list = unique(df$speaker)
  for(i in 1: length(speaker_list)){
    # create df for each speaker
    df_tmp <- df %>% subset(df$speaker == speaker_list[i])
    # seperating the df into two: 
    # one with err = 0, 
    df_err_0 <- df_tmp %>% filter(err == 0)
    # the err = 1
    df_err_1 <- df_tmp %>% filter(err == 1)
    
    # get sound file name for each speaker
    sound_file <- unique(df_err_0$Filename)
    
    # loop over df to filter out values above and below 2.5 sd of the mean
    # create the first cleaned subset of df
    df_err_2 <- df_err_0 %>% subset(df_err_0$Filename == sound_file[1]) %>%
      mutate(err = case_when(strF0 >= mean(strF0) + sd_value*sd(strF0) ~ 2,
                             strF0 <= mean(strF0) - sd_value*sd(strF0) ~ 2,
                             strF0 < mean(strF0) + sd_value*sd(strF0)&strF0>mean(strF0) - sd_value*sd(strF0) ~ 0))
    
    # loop over the rest of the subset (i.e. each sound file)
    for(j in 2: length(sound_file)){
      df_err_2_tmp <-  df_err_0 %>% subset(df_err_0$Filename == sound_file[j]) %>% 
        mutate(err = case_when(strF0 >= mean(strF0) + sd_value*sd(strF0) ~ 2,
                               strF0 <= mean(strF0) - sd_value*sd(strF0) ~ 2,
                               strF0 < mean(strF0) + sd_value*sd(strF0)&strF0>mean(strF0) - sd_value*sd(strF0) ~ 0))
      df_err_2 <- rbind(df_err_2, df_err_2_tmp)
    } # end of sound file loop for one speaker
    
    # put data points that marked as err = 1 back
    df_clean <- rbind(df_err_1, df_err_2)
    
    # problem unsolved: how to combine each df_clean into df_final!!!
    df_ls[[i]] <- df_clean
  } # end of the entire loop
  df_final <- df_ls %>% reduce(full_join)
  return(df_final)
} # end of the function

df_all <- second_filter(df_all_files, 2.5)
df_all_2 <- second_filter(df_all_files, 2)
head(df_all)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
######## save plotting ################################################


