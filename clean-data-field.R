############################################################
#                                                          #
#                        Clean data                        #
#                                                          #
############################################################

# NOTE: All animals were given supplementary oxygen

# Load packages
library(dplyr)
library(readr)
library(tidyr)

############################################################
#                                                          #
#                 Field: respiratory rate                  #
#                                                          #
############################################################
field_RR <- read_csv('./original-data/RR-field.csv') %>%
    # Remove blank columns
    select(-c(10:15)) %>%
    # Convert to long format
    gather(key = intervention, value = resp_rate, -X1) %>%
    # Fix column class
    mutate(resp_rate = as.integer(resp_rate)) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    mutate(id = rep(seq(from = 1, 
                        to = nrow(.) / 5, 
                        by = 1), 
                    each = 5)) %>%
    # Clean-up intervention
    mutate(intervention = stringr::str_replace_all(intervention, 
                                                   pattern = '\\+O2 [fF]ield', 
                                                   replacement = '')) %>%
    # Convert ID to character
    mutate(id = paste0('ID', id)) %>%
    # Order interventions
    mutate(intervention = case_when(
        .$intervention == 'But' ~ 'Butorphanol',
        .$intervention == 'But+M5050' ~ 'Butorphanol+M5050'
    ),
    intervention = factor(intervention),
    intervention = forcats::fct_relevel(intervention,
                                        'Butorphanol', 
                                        'Butorphanol+M5050'))

# Save and clean-up
write_rds(x = field_RR,
          path = './data/RR_field.rds')
write_csv(x = field_RR,
          path = './data/RR_field.csv')
rm(field_RR)

############################################################
#                                                          #
#    Field: Arterial partial pressure of oxygen (PO2)      #
#                                                          #
############################################################
field_PO2 <- read_csv('./original-data/PO2-field.csv') %>%
    # Remove blank columns
    select(-c(10:15)) %>%
    # Convert to long format
    gather(key = intervention, value = PO2, -X1) %>%
    # Fix column class
    mutate(PO2 = round(PO2)) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    mutate(id = rep(seq(from = 1, 
                        to = nrow(.) / 5, 
                        by = 1), 
                    each = 5)) %>%
    # Clean-up intervention
    mutate(intervention = stringr::str_replace_all(intervention, 
                                                   pattern = '\\+O2 [fF]ield', 
                                                   replacement = '')) %>%
    # Convert ID to character
    mutate(id = paste0('ID', id)) %>%
    # Order interventions
    mutate(intervention = case_when(
        .$intervention == 'But' ~ 'Butorphanol',
        .$intervention == 'But+M5050' ~ 'Butorphanol+M5050'
    ),
    intervention = factor(intervention),
    intervention = forcats::fct_relevel(intervention,
                                        'Butorphanol', 
                                        'Butorphanol+M5050'))

# Save and clean-up
write_rds(x = field_PO2,
          path = './data/PO2_field.rds')
write_csv(x = field_PO2,
          path = './data/PO2_field.csv')
rm(field_PO2)

############################################################
#                                                          #
#            Field: Arterial partial pressure              #
#                 of carbon dioxide (PCO2)                 #
#                                                          #
############################################################
field_PCO2 <- read_csv('./original-data/PCO2-field.csv') %>%
    # Remove blank columns
    select(-c(10:15)) %>%
    # Convert to long format
    gather(key = intervention, value = PCO2, -X1) %>%
    # Fix column class
    mutate(PCO2 = round(PCO2)) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    mutate(id = rep(seq(from = 1, 
                        to = nrow(.) / 5, 
                        by = 1), 
                    each = 5)) %>%
    # Clean-up intervention
    mutate(intervention = stringr::str_replace_all(intervention, 
                                                   pattern = '\\+O2 [fF]ield', 
                                                   replacement = '')) %>%
    # Convert ID to character
    mutate(id = paste0('ID', id)) %>%
    # Order interventions
    mutate(intervention = case_when(
        .$intervention == 'But' ~ 'Butorphanol',
        .$intervention == 'But+M5050' ~ 'Butorphanol+M5050'
    ),
    intervention = factor(intervention),
    intervention = forcats::fct_relevel(intervention,
                                        'Butorphanol', 
                                        'Butorphanol+M5050'))

# Save and clean-up
write_rds(x = field_PCO2,
          path = './data/PCO2_field.rds')
write_csv(x = field_PCO2,
          path = './data/PCO2_field.csv')
rm(field_PCO2)
