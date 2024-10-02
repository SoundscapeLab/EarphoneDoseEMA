###################################################################################
# This script uses dB SPL values extracted from Audioscan Verifit to calculate overall SPLs
# with A-weighting and diffuse or free field corrections as well as uncorrected values.
#
# This script assumes the first two tests in Speechmap correspond to measured music levels 
# from an earphone and the second two tests in Speechmap correspond to measured speech levels. 
# The code can be easily modified to accomodate other measurements.
# 
# The input file should be a csv file with columns for stimuli (speech or music), Ear (left or right) 
# Freq (frequency value in 1/12th octave center frequencies, the code will subset to 1/3rd octaves), 
# value (dB SPL), and name (test name). 
#
# Data extraction from the Verifit is provided by an additional script in this repository: extractVerifitData.R
#
# Disclaimer stuff:
# Code is provided "as-is" without any warranties or guarantees of any kind. 
# The user assumes full responsibility for using this code and any outcomes resulting from its use.
# The code is provided without any form of support maintenance, or updates. Use at your own risk.
#
# Soundscape & Audiology Research Lab
# UW-Madison
# audlab@csd.wisc.edu
# 2024
#
###################################################################################

# Required libraries
library(dplyr)

# Set WD
setwd("WD")

# Read in the extracted Verifit data
fullData <- read.csv('FILENAME')

# Subset 1/3 octaves
freq_values <- c(200, 250, 315, 400, 500, 630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000, 12500)
data <- fullData %>% filter(Freq %in% freq_values)

# Apply corrections and A-weighting
# These values are from ISO 11904-1:2002 and ANSI/ASA 21.4/IEC61672:2014
FF_Corrections <- c(0.6, 0.8, 1.1, 1.5, 2.0, 2.3, 3.1, 2.7, 2.9, 5.8, 12.4, 15.7, 14.9, 13.2, 8.9, 3.1, -1.4, -3.8, -0.1 ); #SUBTRACT
Aweighting <- c(-10.9, -8.6, -6.6, -4.8, -3.2, -1.9, -0.8, 0, 0.6, 1.0, 1.2, 1.3, 1.2, 1.0, 0.5, -0.1, -1.1, -2.5, -4.3); #ADD
DF_Corrections <- c(0.6, 0.8, 1.1, 1.5, 2.1, 2.8, 3.3, 4.1, 5.5, 7.7, 11.0, 15.3, 15.7, 12.9, 10.6, 9.4, 9.5, 6.8, 3.8); #SUBTRACT

## MUSIC

music_data <- subset(data, stimuli == "music")

music_data <- music_data %>%
  # First, average across tests (Test 1 and 2) within each ID, Freq, and Ear
  group_by(ID, Freq, Ear) %>%
  summarise(Average_within_test = mean(Average, na.rm = TRUE)) %>%
  ungroup() %>%
  # Then, average across ears within each ID and Freq
  group_by(ID, Freq) %>%
  summarise(Average = mean(Average_within_test, na.rm = TRUE)) %>%
  ungroup()

# View the resulting data
print(music_data)

#UNCORRECTED VALUES
# Summarize the decibel values within each ID
music_data_uncorrected_summed <- music_data %>%
  group_by(ID) %>%
  summarise(Total = 10 * log10(sum(10^(Average / 10)))) %>%
  ungroup()

# View the resulting data
print(music_data_uncorrected_summed)


#CORRECTED VALUES
music_data_corrected <- music_data %>%
  mutate(Corrected_Average = Average - DF_Corrections + Aweighting)

# View the resulting data
print(music_data_corrected)

# Summarize the decibel values within each ID
music_data_corrected_summed <- music_data_corrected %>%
  group_by(ID) %>%
  summarise(Total = 10 * log10(sum(10^(Corrected_Average / 10)))) %>%
  ungroup()

# View the resulting data
print(music_data_corrected_summed)


## SPEECH

speech_data <- subset(data, stimuli == "speech")

speech_data <- speech_data %>%
  # First, average across tests (Test 1 and 2) within each ID, Freq, and Ear
  group_by(ID, Freq, Ear) %>%
  summarise(Average_within_test = mean(Average, na.rm = TRUE)) %>%
  ungroup() %>%
  # Then, average across ears within each ID and Freq
  group_by(ID, Freq) %>%
  summarise(Average = mean(Average_within_test, na.rm = TRUE)) %>%
  ungroup()

# View the resulting data
print(speech_data)

#UNCORRECTED VALUES
# Summarize the decibel values within each ID
speech_data_uncorrected_summed <- speech_data %>%
  group_by(ID) %>%
  summarise(Total = 10 * log10(sum(10^(Average / 10)))) %>%
  ungroup()

# View the resulting data
print(speech_data_uncorrected_summed)


#CORRECTED VALUES
speech_data_corrected <- speech_data %>%
  mutate(Corrected_Average = Average - DF_Corrections + Aweighting)

# View the resulting data
print(speech_data_corrected)

# Summarize the decibel values within each ID
speech_data_corrected_summed <- speech_data_corrected %>%
  group_by(ID) %>%
  summarise(Total = 10 * log10(sum(10^(Corrected_Average / 10)))) %>%
  ungroup()

# View the resulting data
print(speech_data_corrected_summed)

#########
# Rename the Total columns in each dataset
speech_data_corrected_summed <- speech_data_corrected_summed %>%
  rename(speech_data_corrected_summed = Total)

speech_data_uncorrected_summed <- speech_data_uncorrected_summed %>%
  rename(speech_data_uncorrected_summed = Total)

music_data_corrected_summed <- music_data_corrected_summed %>%
  rename(music_data_corrected_summed = Total)

music_data_uncorrected_summed <- music_data_uncorrected_summed %>%
  rename(music_data_uncorrected_summed = Total)

# Combine the datasets by ID
combined_data <- speech_data_corrected_summed %>%
  left_join(speech_data_uncorrected_summed, by = "ID") %>%
  left_join(music_data_corrected_summed, by = "ID") %>%
  left_join(music_data_uncorrected_summed, by = "ID")

# View the resulting combined data
print(combined_data)

################################################################################