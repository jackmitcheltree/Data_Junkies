install.packages("readr", "dplyr", "tidyr") #script for installing necessary code

library(readr) #activating that code if not already active
library(dplyr)
library(tidyr)

DRUGS <- read_csv("DRUGS.csv") #importing the dataset if it is in the project file

#What is going to be the final code for the analysis:
Drugs_btr <- 
  DRUGS %>%
  mutate(Sex = ifelse(Sex == "Male", 1, 0)) %>%
  mutate(Heroin = ifelse(Heroin == "Y", 1, 0)) %>%
  mutate(Cocaine = ifelse(Cocaine == "Y", 1, 0)) %>%
  mutate(Fentanyl = ifelse(Fentanyl == "Y", 1, 0)) %>%
  mutate(FentanylAnalogue = ifelse(FentanylAnalogue == "Y", 1, 0)) %>%
  mutate(Oxycodone = ifelse(Oxycodone == "Y", 1, 0)) %>%
  mutate(Oxymorphone = ifelse(Oxymorphone == "Y", 1, 0)) %>%
  mutate(Ethanol = ifelse(Ethanol == "Y", 1, 0)) %>%
  mutate(Hydrocodone = ifelse(Hydrocodone == "Y", 1, 0)) %>%
  mutate(Benzodiazepine = ifelse(Benzodiazepine == "Y", 1, 0)) %>%
  mutate(Methadone = ifelse(Methadone == "Y", 1, 0)) %>%
  mutate(Amphet = ifelse(Amphet == "Y", 1, 0)) %>%
  mutate(Tramad = ifelse(Tramad == "Y", 1, 0)) %>%
  mutate(Morphine_NotHeroin = ifelse(Morphine_NotHeroin == "Y", 1, 0)) %>%
  mutate(Hydromorphone = ifelse(Hydromorphone == "Y", 1, 0)) %>%
  mutate(Other = ifelse(Other == "Y", 1, 0)) %>%
  mutate(OpiateNOS = ifelse(OpiateNOS == "Y", 1, 0)) %>%
  mutate(AnyOpioid = ifelse(AnyOpioid == "Y", 1, 0)) %>%

#Rabbit - Hole, no longer relevant code.
DRUGS %>%
  transmute(Heroin_B = ifelse(Heroin == "Y", 1, 0)) %>%
  mutate(Heroin_BB = ifelse(is.na(Heroin_B), 0, Heroin_B)) %>%
  select(Heroin_B, Heroin_BB)

#Single variable test of the below
DRUGS %>%
  transmute(Heroin_B = ifelse(Heroin == "Y", 1, 0)) %>%
  mutate_if(is.numeric, funs(replace_na(., 0)))

#A test to see if the code at the bottom functioned properly and
#if you can mutate right onto the same variable and it won't double 
#it into a new vairable of the same name

drugs_dummies <- DRUGS %>% 
  mutate(Heroin = ifelse(Heroin == "Y", 1, 0)) %>%
  mutate(Cocaine = ifelse(Cocaine == "Y", 1, 0)) %>%
  mutate(Fentanyl = ifelse(Fentanyl == "Y", 1, 0)) %>%
  mutate(FentanylAnalogue = ifelse(FentanylAnalogue == "Y", 1, 0)) %>%
  mutate(Oxycodone = ifelse(Oxycodone == "Y", 1, 0)) %>%
  mutate(Oxymorphone = ifelse(Oxymorphone == "Y", 1, 0)) %>%
  mutate(Ethanol = ifelse(Ethanol == "Y", 1, 0)) %>%
  mutate(Hydrocodone = ifelse(Hydrocodone == "Y", 1, 0)) %>%
  mutate(Benzodiazepine = ifelse(Benzodiazepine == "Y", 1, 0)) %>%
  mutate(Methadone = ifelse(Methadone == "Y", 1, 0)) %>%
  mutate(Amphet = ifelse(Amphet == "Y", 1, 0)) %>%
  mutate(Tramad = ifelse(Tramad == "Y", 1, 0)) %>%
  mutate(Morphine_NotHeroin = ifelse(Morphine_NotHeroin == "Y", 1, 0)) %>%
  mutate(Hydromorphone = ifelse(Hydromorphone == "Y", 1, 0)) %>%
  mutate(Other = ifelse(Other == "Y", 1, 0)) %>%
  mutate(OpiateNOS = ifelse(OpiateNOS == "Y", 1, 0)) %>%
  mutate(AnyOpioid = ifelse(AnyOpioid == "Y", 1, 0)) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) %>%
  select(Heroin, Cocaine, Ethanol, Other)
