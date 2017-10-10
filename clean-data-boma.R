############################################################
#                                                          #
#                        Clean data                        #
#                                                          #
############################################################
# Load packages
library(dplyr)
library(readr)
library(tidyr)

############################################################
#                                                          #
#                  Boma: respiratory rate                  #
#                                                          #
############################################################
# With oxygen
boma_O2 <- read_csv('./original-data/RR-boma.csv') %>%
    # Convert to long format
    gather(key = intervention, value = resp_rate, -X1) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    group_by(time_min, intervention) %>%
    mutate(id = row_number()) %>%
    # Add identifier
    ungroup() %>%
    mutate(supplement = rep('oxygen', nrow(.))) %>%
    # Clean-up intervention
    mutate(intervention = stringr::str_replace_all(intervention, 
                                                   pattern = '\\+O2', 
                                                   replacement = ''))

# Without oxygen
boma <- read_csv('./original-data/RR-boma-no-oxygen.csv') %>%
    # Convert to long format
    gather(key = intervention, value = resp_rate, -X1) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    group_by(time_min, intervention) %>%
    mutate(id = row_number()) %>%
    # Add identifier
    ungroup() %>%
    mutate(supplement = rep('nothing', nrow(.)))

# Bind the two together
resp_rate <- bind_rows(boma, boma_O2) %>%
    # Ungroup
    ungroup() %>%
    # Convert to integer
    mutate(resp_rate = as.integer(resp_rate)) %>%
    # Convert ID to character
    mutate(id = paste0('ID', id)) %>%
    # Order interventions
    mutate(intervention = case_when(
        .$intervention == 'Control' ~ 'Saline',
        .$intervention == 'But' ~ 'Butorphanol',
        .$intervention == 'But+M5050' ~ 'Butorphanol+M5050'
    ),
    intervention = factor(intervention),
    intervention = forcats::fct_relevel(intervention,
                                        'Butorphanol', 
                                        'Butorphanol+M5050',
                                        'Saline'))

# Save and clean-up
write_rds(x = resp_rate,
          path = './data/RR_boma.rds')
write_csv(x = resp_rate,
          path = './data/RR_boma.csv')
rm(boma, boma_O2, resp_rate)

############################################################
#                                                          #
#     Boma: Arterial partial pressure of oxygen (PO2)      #
#                                                          #
############################################################
# With oxygen
boma_O2 <- read_csv('./original-data/PO2-boma.csv') %>%
    # Convert to long format
    gather(key = intervention, value = PO2, -X1) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    group_by(time_min, intervention) %>%
    mutate(id = row_number()) %>%
    # Add identifier
    ungroup() %>%
    mutate(supplement = rep('oxygen', nrow(.))) %>%
    # Clean-up intervention
    mutate(intervention = stringr::str_replace_all(intervention, 
                                                   pattern = '\\+O2', 
                                                   replacement = ''))

# Without oxygen
boma <- read_csv('./original-data/PO2-boma-no-oxygen.csv') %>%
    # Convert to long format
    gather(key = intervention, value = PO2, -X1) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    group_by(time_min, intervention) %>%
    mutate(id = row_number()) %>%
    # Add identifier
    ungroup() %>%
    mutate(supplement = rep('nothing', nrow(.)))

# Bind the two together
PO2 <- bind_rows(boma, boma_O2) %>%
    # Ungroup
    ungroup() %>%
    # Convert ID to character
    mutate(id = paste0('ID', id)) %>%
    # Order interventions
    mutate(intervention = case_when(
        .$intervention == 'Control' ~ 'Saline',
        .$intervention == 'But' ~ 'Butorphanol',
        .$intervention == 'But+M5050' ~ 'Butorphanol+M5050'
    ),
    intervention = factor(intervention),
    intervention = forcats::fct_relevel(intervention,
                                        'Butorphanol', 
                                        'Butorphanol+M5050',
                                        'Saline'))

# Save and clean-up
write_rds(x = PO2,
          path = './data/PO2_boma.rds')
write_csv(x = PO2,
          path = './data/PO2_boma.csv')
rm(boma, boma_O2, PO2)

############################################################
#                                                          #
#             Boma: Arterial partial pressure              #
#                 of carbon dioxide (PCO2)                 #
#                                                          #
############################################################
# With oxygen
boma_O2 <- read_csv('./original-data/PCO2-boma.csv') %>%
    # Convert to long format
    gather(key = intervention, value = PCO2, -X1) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    group_by(time_min, intervention) %>%
    mutate(id = row_number()) %>%
    # Add identifier
    ungroup() %>%
    mutate(supplement = rep('oxygen', nrow(.))) %>%
    # Clean-up intervention
    mutate(intervention = stringr::str_replace_all(intervention, 
                                                   pattern = '\\+O2', 
                                                   replacement = ''))

# Without oxygen
boma <- read_csv('./original-data/PCO2-boma-no-oxygen.csv') %>%
    # Convert to long format
    gather(key = intervention, value = PCO2, -X1) %>%
    # Rename columns
    rename(time_min = X1) %>%
    # Clean-up intervention names
    separate(col = intervention,
             into = c('intervention', 'new'),
             sep = '_') %>%
    select(-new) %>%
    # Add id column
    group_by(time_min, intervention) %>%
    mutate(id = row_number()) %>%
    # Add identifier
    ungroup() %>%
    mutate(supplement = rep('nothing', nrow(.)))

# Bind the two together
PCO2 <- bind_rows(boma, boma_O2) %>%
    # Ungroup
    ungroup() %>%
    # Convert ID to character
    mutate(id = paste0('ID', id)) %>%
    # Order interventions
    mutate(intervention = case_when(
        .$intervention == 'Control' ~ 'Saline',
        .$intervention == 'But' ~ 'Butorphanol',
        .$intervention == 'But+M5050' ~ 'Butorphanol+M5050'
    ),
           intervention = factor(intervention),
           intervention = forcats::fct_relevel(intervention,
                                               'Butorphanol', 
                                               'Butorphanol+M5050',
                                               'Saline'))

# Save and clean-up
write_rds(x = PCO2,
          path = './data/PCO2_boma.rds')
write_csv(x = PCO2,
          path = './data/PCO2_boma.csv')
rm(boma, boma_O2, PCO2)
