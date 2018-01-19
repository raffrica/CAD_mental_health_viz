library(tidyverse)
library(plyr)

mental_health <- read_csv("data/mental_health.csv")

glimpse(mental_health)

# Vector of Provinces to keep
provinces <- mental_health$GEO %>% factor() %>% levels()
provinces <- provinces[!provinces %in% c("Canada", "Atlantic Provinces", "Prairie Provinces")]

# Vector of Ages to keep
ages <- mental_health$AGE %>% factor() %>% levels()
ages <- ages[c(1,2,4,5, 6)]
ages_clean <- c("Total", "15 to 24", "25 to 44", "45 to 64", "65 +")

stress_level <- c("Self-rated work stress, days not at all or not very stressful", "Self-rated work stress, days quite a bit or extremely stressful")
stress_level_clean <- c("Low Stress", "High Stress")

perceived_need <- c("Perceived need for mental health care, need not met", 
                    "Perceived need for mental health care, all needs met")
perceived_need_clean <- c("Needs not Met", "Needs Met")


perceived_mental_health <- c("Perceived mental health, fair or poor", "Perceived mental health, very good or excellent")
perceived_mental_health_clean <- c("Poor Mental Health", "Good Mental Health")


general_disorders <- c("Any mood disorder, life", "Major depressive episode, life" , 
                       "Bipolar disorder, life", "Generalized anxiety disorder, life",
                       "Suicidal thoughts, life", "Schizophrenia or psychosis, ever received diagnosis", "Post-traumatic stress disorder, current diagnosed condition")

general_disorders_clean <- c("Any Disorder", "Depression", "Bipolar", "Anxiety", "Suicidal", "Psychosis", "PTSD")

substance_disorders  <- c("Any substance use disorder (alcohol or drug), life", 
                          "Alcohol abuse or dependence, life" ,
                          "Cannabis abuse or dependence, life", 
                          "Other drug abuse or dependence (excluding cannabis), life")
substance_disorders_clean <- c("Any Substance", "Alcohol", "Cannabis", "Other")



(mental_health_reduced <- mental_health %>% 
    # Filter to only have Number of Persons and Percent, and only from year 2012
    filter(UNIT %in% c("Number of persons","Percent"), Ref_Date == 2012) %>% 
    # Filter to only have provinces 
    filter(GEO %in% provinces) %>% 
    # Filter to only have unique ages (no overlap ranges)
    filter(AGE %in% ages) %>% 
    # Change Units
    mutate(Value = as.double(Value)) %>% 
    # Select only variables that are actually being used later
    dplyr::select(-c(Ref_Date, Vector, Coordinate)) %>% 
    
    # Filter HRPROF (i.e. variables) by category
    filter(HRPROF %in% stress_level |
             HRPROF %in% perceived_need |
             HRPROF %in% perceived_mental_health |
             HRPROF %in% general_disorders |
             HRPROF %in% substance_disorders) %>% 
    
    # Change Variables so cleaner names
    mutate(HRPROF = mapvalues(HRPROF, stress_level, stress_level_clean)) %>% 
    mutate(HRPROF = mapvalues(HRPROF, perceived_need, perceived_need_clean)) %>% 
    mutate(HRPROF = mapvalues(HRPROF, perceived_mental_health, perceived_mental_health_clean)) %>% 
    mutate(HRPROF = mapvalues(HRPROF, general_disorders, general_disorders_clean)) %>% 
    mutate(HRPROF = mapvalues(HRPROF, substance_disorders, substance_disorders_clean)) %>% 
    mutate(SEX = mapvalues(SEX, c("Both sexes"), c("Both"))) %>% 
    mutate(UNIT = mapvalues(UNIT, c("Number of persons"), c("Number"))) %>% 
    mutate(AGE = mapvalues(AGE, ages, ages_clean)) %>% 
    
    
    # Mutate values that are percent so that can be rendered effectively later  
    mutate(Value = ifelse(UNIT == "Percent", Value/100, Value))
  
)

write_csv(mental_health_reduced, path = "data/mh_clean.csv") 