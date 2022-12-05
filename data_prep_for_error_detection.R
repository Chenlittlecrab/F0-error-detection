###############################################
# this script combines each speakers' F0 file into one file 
# and add additional info to get the required columns for F0 error detection
# added columns include: gender info, F0 semitones, speaker ID (seperated from Filename column)

### required output: 
# long format data with the following columns 
# -F0 in semitones (can be converted from Hz in R using this function): 
# https://search.r-project.org/CRAN/refmans/hqmisc/html/f2st.html
# -time in ms (can be at any interval, e.g., 1ms, 10ms, 100ms)
# -a column which uniquely identifies each trajectory
# -an optional column that contains speaker information. 

# import library
library(tidyverse)
library(dplyr)# dealing with issue that some speakers' files have an additional column
# install.packages("hqmisc")
library(hqmisc) # for converting F0 scale from Hz to semitones

# set the directory to the folder that contain speakers' files
setwd("/Users/chenz/Documents/CUNY/QP1/Stats/modeling/pitch_range_data")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
#### Combine all .txt files into one ##################################
#### Only need to run ONCE ##################################
# create a vector containing the names of all speakers' files
all_files <- list.files(pattern = "*.txt", full.names = FALSE)
all_files

# read in the first file, filter out non-V labels and 0 F0 values
df <- read_delim(all_files[1]) %>% 
  filter(Label == "V", 
         strF0 != 0)
head(df)

# loop over the rest and combine them into one file
for(i in 2: length(all_files)){
  df_tmp <- read_delim(all_files[i]) %>% 
    filter(Label == "V",
           strF0 != 0)
  # combine the df_tmp of each iteration with df
  df = bind_rows(df, df_tmp)
}

# eliminate the extra column
df <- df %>% select(-...7)
head(df)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#### alternative: loading the one with initial filtering applied ######
df <- read_delim("all_speaker_pitch_initial_filtered_info_needed.txt") %>% 
  select(-...7)
head(df)

# create a column for speakerID
df <- df %>% 
  mutate(speakerID = str_sub(Filename, 1, 4)) %>% 
  # eliminate _ in the string for speaker ID
  mutate(speaker = ifelse(endsWith(speakerID, "_"), 
                          str_sub(speakerID, end = -2), speakerID)) %>% 
  select(-speakerID)

head(df)

# Create column for semitone
# semitone was calculated with the mean F0 value of each speaker as base

# first, create a vector with speakerID
all_speaker <- unique(df$speaker)
all_speaker
length(all_speaker)

# create the first subset of df with semitone values
df_semi <- df %>% subset(df$speaker == all_speaker[1]) %>% 
  mutate(F0_semitones = f2st(strF0, mean(strF0)))

# loop over the rest and combine with the first subset
for(i in 2:length(all_speaker)){
  df_tmp <- df %>% subset(df$speaker == all_speaker[i]) %>% 
    mutate(F0_semitones = f2st(strF0, mean(strF0)))
  df_semi = rbind(df_semi, df_tmp)
}

head(df_semi)

summary(df_semi)


# import the file with speaker info and get gender column added to the current file
setwd("/Users/chenz/Documents/CUNY/QP1/Stats/modeling/pitch_range_data/dataset")

# read the file that contains speaker info
df_speakerInfo <- read_delim("all_gender_data_grand_ave_with_fricatives.txt")

# select only the columns that contain info needed
# make speaker column as factor for inner_join two files
df_speakerInfo <- df_speakerInfo %>% 
  select(speaker, sex) %>% 
  mutate(speaker = as.factor(speaker))
head(df_speakerInfo)

# change the value for speaker the same with speaker value in df_speakerInfo
# make speaker column as factor too
df_semi <- df_semi %>% 
  mutate(speaker = str_sub(speaker, start = 3)) %>% 
  mutate(speaker = as.factor(speaker))

head(df_semi)

# combine two files
df_final <- df_semi %>% 
  inner_join(df_speakerInfo, by = "speaker")
head(df_final)

# save the file for error detection
write.table(df_final, file = "all_speaker_pitch_initial_filtered.txt", sep = "\t", row.names = FALSE)
