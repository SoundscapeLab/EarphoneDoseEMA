###################################################################################
# This script calculates noise exposure doses using EMA responses for minutes of earphone use every 2 hours.
# Dose is calculated using the reported exposure time and the average earphone listening levels measured with real-ear measures.
# Earphone level calculations are performed using other code provided in this repository: 
# extractVerifitData.R and
# calcOveralEarphoneLevel.R
#
# This script requires csv files with EMA responses and earphone levels for each ID.
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

# Read in EMA data
# This should have columns for EMA responses of minutes of music or speech listened to
# on earphones in the prior 2 hours. This can be easily modified depending on the EMA method.
data <- read.csv('FILENAME')

# Aggreate total minutes listened to speech and music on earphones for each ID

# Calculate total minutes earphone speech
# Subset speech
speechListening <- subset(data, Q2 == 'Speech')

#count number of data points
counts <- speechListening %>%
  count(ID)

# Make numeric
speechListening$Q15 <- as.numeric(speechListening$Q15)
# Check
is.numeric(speechListening$Q15)

# Sum minutes earphone within each ID
sumSpeechListening <- aggregate(Q15 ~ ID, data = speechListening, FUN = sum)

# Calculate total minutes earphone music
# Subset music
musicListening <- subset(data, Q2 == 'Music')

# Count number of data points
counts <- musicListening %>%
  count(ID)

# Make numeric
musicListening$Q15 <- as.numeric(musicListening$Q15)
# CHeck
is.numeric(musicListening$Q15)

# Sum minutes earphone within each ID
sumMusicListening <- aggregate(Q15 ~ ID, data = musicListening, FUN = sum)

###############################################################################

# EMA data needs to be binded with average earphone level data before dose calculations
# Earphone listening levels are calculated with another script in this repository: calcOverallEarphoneLevel.R

# This is the reference dose according to WHO-ITU 2019
# 80 dBA for 40 hrs/week with a 3 dB exchange rate
# C = 40
# D = 100(C1/T1 + C2/T2)
# T = 40 / (2^((80-80)/3))

#SPEECH DOSE

# Calculate reference durations
SpeechT <- 40 / (2^((data$AvgSpeech-80)/3))

# Make numeric
data$MinutesSpeechEstimate <- as.numeric(data$MinutesSpeechEstimate)
# Check
is.numeric(data$MinutesSpeechEstimate)

# Convert minutes to hours
HoursSpeech <- data$MinutesSpeechEstimate / 60

# Calculate speech exposure
SpeechCT <- HoursSpeech/SpeechT

# MUSIC DOSE

# Calculate reference durations
MusicT <- 40 / (2^((data$AvgMusic-80)/3))

# Make numeric
data$MinutesMusicEstimate <- as.numeric(data$MinutesMusicEstimate)
# Check
is.numeric(data$MinutesMusicEstimate)

# Convert minutes to hours
HoursMusic <- data$MinutesMusicEstimate / 60

# Calculate speech exposure
MusicCT <- HoursMusic/MusicT

## TOTAL DOSE
Dose <- 100 * (SpeechCT + MusicCT)

# Bind
data$Dose <- Dose

################################################################################

