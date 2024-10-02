###################################################################################
# This script extracts average dB SPL values from Speechmap module tests 
# from an xml session file from Audioscan Verifit2. It is designed to extract data 
# from all 4 tests allowed on a single Speechmap. It returns the dB SPL value in 
# 1/12th octave bands. Note, however, that the level in each 1/12th octave band is
# the level in 1/3rd octave with a 1/12th octave center frequency. To calculate the
# overall level, only 1/3rd octave bands should be extracted and summed.
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
library(xml2)
library(tidyverse)

# Set WD
setwd("WD")

# Get a list of all XML files in the directory
xml_files <- list.files(pattern = "*.xml")

# Frequency values
freq_values <- c(
  200, 210, 225, 235, 250, 265, 280, 300, 315, 335, 355, 375, 400, 420, 
  450, 470, 500, 530, 560, 595, 630, 670, 710, 750, 800, 840, 900, 945, 
  1000, 1060, 1120, 1190, 1250, 1335, 1400, 1500, 1600, 1680, 1800, 1890, 
  2000, 2120, 2240, 2380, 2500, 2670, 2800, 3000, 3150, 3365, 3550, 3775, 
  4000, 4240, 4500, 4760, 5000, 5340, 5600, 6000, 6300, 6725, 7100, 7550, 
  8000, 8500, 9000, 9500, 10000, 10600, 11200, 11800, 12500
)

# Function to process a XML file and extract data
process_xml_file <- function(xml_file_path) {
  file_name <- tools::file_path_sans_ext(basename(xml_file_path))
  xml_data <- read_xml(xml_file_path)
  
  # Extract <data> nodes
  data_nodes <- xml_find_all(xml_data, "//data")
  
  # Convert nodes to a data frame
  data_df <- map_df(data_nodes, function(node) {
    attrs <- xml_attrs(node)
    tibble(
      name = attrs["name"] %>% as.character(),
      yunit = attrs["yunit"] %>% as.character(),
      xscale = attrs["xscale"] %>% as.character(),
      xunit = attrs["xunit"] %>% as.character(),
      internal_alt = attrs["internal_alt"] %>% as.character(),
      display = attrs["display"] %>% as.character(),
      envdisplay = attrs["envdisplay"] %>% as.character(),
      stim_type = attrs["stim_type"] %>% as.character(),
      source = attrs["source"] %>% as.character(),
      bandwidth = attrs["bandwidth"] %>% as.character(),
      internal = attrs["internal"] %>% as.character(),
      value = xml_text(node) %>% as.character()
    )
  })
  
  # Remove rows where internal_alt or display is NA
  data_df <- data_df %>% filter(!is.na(internal_alt) & !is.na(display))
  
  # Split the `value` column into individual numbers and create a new data frame
  split_values_df <- data_df %>%
    select(name, value) %>%
    separate_rows(value, sep = "\\s+") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(
      Ear = case_when(
        row_number() <= 292 ~ "Right",
        row_number() > 292 & row_number() <= 584 ~ "Left",
        TRUE ~ NA_character_
      ),
      stimuli = case_when(
        row_number() <= 146 ~ "music",
        row_number() > 146 & row_number() <= 292 ~ "speech",
        row_number() > 292 & row_number() <= 438 ~ "music",
        row_number() > 438 & row_number() <= 584 ~ "speech",
        TRUE ~ NA_character_
      ),
      Test = case_when(
        row_number() <= 73 ~ 1,
        row_number() > 73 & row_number() <= 146 ~ 2,
        row_number() > 146 & row_number() <= 219 ~ 3,
        row_number() > 219 & row_number() <= 292 ~ 4,
        row_number() > 292 & row_number() <= 365 ~ 1,
        row_number() > 365 & row_number() <= 438 ~ 2,
        row_number() > 438 & row_number() <= 511 ~ 3,
        row_number() > 511 & row_number() <= 584 ~ 4,
        TRUE ~ NA_integer_
      ),
      ID = file_name
    )
  
  # Add the Freq column with repeating frequency values
  split_values_df <- split_values_df %>%
    mutate(Freq = rep(freq_values, length.out = n()))
  
  return(split_values_df)
}

# Process each XML file and combine the results into a single data frame
combined_df <- map_df(xml_files, process_xml_file)

# Print the combined data frame to verify
print(combined_df)

# Write the combined data frame to a CSV file
write_csv(combined_df, "FILENAME")

##################################################################################

