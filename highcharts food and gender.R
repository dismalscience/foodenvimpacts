# Script to generate high-quality graphs for the food and gender analysis (Dismal Science) - focus on high charts


######### 1. Loading libraries ###############

library(tidyverse)
library(janitor)
library(writexl)
library(readxl)
library(pals)
library(highcharter)
library(htmlwidgets)
library(jsonlite)
library(purrr)

########### 2. Loading the source files ###############

data_adults <- read_excel("Source data/synthesis_env_impacts_adults.xlsx", sheet = "environmental_data_adults") # import the files with consumption data by adult groups and environmental impacts (specific foodstuff level)


data_children <- read_excel("Source data/synthesis_env_impacts_children.xlsx",  sheet = "environmental_data_children") # same process for children

ind_caracs_impacts <- read_excel("Source data/individual_caracs_and_impacts.xlsx")

lca_cum <- read_excel("Source data/computations_life analysis by demography.xlsx", sheet = "graphs")

########## 3. Processing the data ##########

# Cleaning names of the dataframes for easier manipualtion

data_adults <- data_adults %>% clean_names() 
data_children <- data_children %>% clean_names() 
ind_caracs_impacts <- ind_caracs_impacts  %>% clean_names()
lca_cum <- lca_cum %>% clean_names()

# Formatting variables (e.g., factors, table formatting)

data_adults$demographic_group <- as.factor(data_adults$demographic_group)
data_adults$macro_group <- as.factor(data_adults$macro_group)
data_adults$sex <- as.factor(data_adults$sex)
data_adults$age_group <- as.factor(data_adults$age_group)
data_adults$demographic_group <- fct_reorder(data_adults$demographic_group, data_adults$order_group)
data_adults$age_group <- fct_reorder(data_adults$age_group, data_adults$order_group)


data_children$demographic_group <- as.factor(data_children$demographic_group)
data_children$macro_group <- as.factor(data_children$macro_group)
data_children$sex <- as.factor(data_children$sex)
data_children$age_group <- as.factor(data_children$age_group)
data_children$demographic_group <- fct_reorder(data_children$demographic_group, data_children$order_group)
data_children$age_group <- fct_reorder(data_children$age_group, data_children$order_group)


lca_cum$male_cc <- NULL
lca_cum$female_cc <- NULL
lca_cum <- lca_cum %>% pivot_longer(!age, names_to = "Sex", values_to = "EFP")
lca_cum$Sex <- ifelse(lca_cum$Sex == "male_efp", "Male", "Female")


########## 4. Building dataframes for graphs ##########

########## 4.1 Total food quantities consumed by age group ##########

########## a. Adults ##########


adults_conso_quant <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                  group_by(demographic_group, sex, age_group) %>% 
                                  summarise(daily_conso_kg = round(sum(daily_conso_kg),digits = 2))  # average daily consumption of adults by sex and age groups


########## b. Children ##########

children_conso_quant <- data_children %>% group_by(demographic_group, sex, age_group) %>% 
                                          summarise(daily_conso_kg = sum(round(daily_conso_kg, digits = 2)))                                           


########## 4.2 Diet composition ##########

########## a. Adults share by age group ##########

adults_diet_macro_comp <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                          group_by(demographic_group, sex, age_group, macro_group) %>% 
                                          summarise(daily_conso_kg = sum(daily_conso_kg)) %>% 
                                          mutate(percent = 100 * daily_conso_kg/sum(daily_conso_kg)) %>% 
                                          mutate(reord_age_group = factor(age_group, levels = rev(levels(age_group)))) # composition of diet by age group and macro group for adults


########## b. Adults diet difference for all adults between males and females ##########

adults_diet_difference <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                          filter(demographic_group %in% c("All adult females", "All adult males")) %>% 
                                          group_by(sex, macro_group, specific_group) %>% 
                                          summarise(daily_conso_kg = sum(daily_conso_kg)) %>% 
                                          group_by(sex) %>% 
                                          mutate(percent = 100 * daily_conso_kg/sum(daily_conso_kg)) %>% 
                                          group_by(specific_group) %>% 
                                          mutate(percent_males_vs_females = percent-lag(percent,default=first(percent))) %>% 
                                          group_by(specific_group) %>% 
                                          mutate(quant_males_vs_females = daily_conso_kg-lag(daily_conso_kg,default=first(daily_conso_kg))) %>% 
                                          filter(sex == "Males") 

macro_adults_diet_difference <- adults_diet_difference %>% group_by(macro_group) %>% 
                                                            summarise(quant_males_vs_females = sum(round(365.25 * quant_males_vs_females, digits = 2))) # yearly consumption difference

macro_adults_diet_difference$macro_group <- factor(macro_adults_diet_difference$macro_group)

macro_adults_diet_difference$macro_group <- reorder(macro_adults_diet_difference$macro_group, macro_adults_diet_difference$quant_males_vs_females, FUN = mean)



specific_adults_diet_difference <- adults_diet_difference %>% 
                                  select(c("macro_group" = macro_group, "specific_group" = specific_group,  "quant_males_vs_females" = quant_males_vs_females)) %>% 
                                  as.data.frame() # yearly consumption difference

specific_adults_diet_difference$quant_males_vs_females <- round(365.25 * specific_adults_diet_difference$quant_males_vs_females, digits = 2)
specific_adults_diet_difference$specific_group <- as.factor(specific_adults_diet_difference$specific_group)
specific_adults_diet_difference$specific_group <- reorder(specific_adults_diet_difference$specific_group, specific_adults_diet_difference$quant_males_vs_females, FUN = mean)


specific_diet_drilldown <- specific_adults_diet_difference  %>% 
                            group_nest(macro_group) %>% 
                            mutate(id = macro_group, type = "bar", data = purrr::map(data, mutate, name = specific_group, y  = (quant_males_vs_females)), data = purrr::map(data, list_parse))


########## c. Children diet difference for all kids between males and females ##########

children_diet_difference <- data_children %>% 
                            filter(demographic_group %in% c("All female children", "All male children")) %>% 
                            group_by(sex, macro_group, specific_group) %>% 
                            summarise(daily_conso_kg = sum(daily_conso_kg)) %>% 
                            group_by(sex) %>% 
                            mutate(percent = 100 * daily_conso_kg/sum(daily_conso_kg)) %>% group_by(specific_group) %>% 
                            mutate(percent_males_vs_females = percent-lag(percent,default=first(percent))) %>% 
                            group_by(specific_group) %>% 
                            mutate(quant_males_vs_females = daily_conso_kg-lag(daily_conso_kg,default=first(daily_conso_kg))) %>% 
                            filter(sex == "Males") 

macro_children_diet_difference <- children_diet_difference %>% group_by(macro_group) %>% 
                                                              summarise(quant_males_vs_females = sum(round(365.25 * quant_males_vs_females, digits = 2))) # yearly consumption difference

macro_children_diet_difference$macro_group <- factor(macro_children_diet_difference$macro_group)
macro_children_diet_difference$macro_group <- reorder(macro_children_diet_difference$macro_group, macro_children_diet_difference$quant_males_vs_females, FUN = mean)


specific_children_diet_difference <- children_diet_difference %>% select(c("macro_group" = macro_group, "specific_group" = specific_group,  "quant_males_vs_females" = quant_males_vs_females)) %>% 
                                                                  as.data.frame() # yearly consumption difference
                                                                  
specific_children_diet_difference$quant_males_vs_females <- round(365.25 * specific_children_diet_difference$quant_males_vs_females, digits = 2)
specific_children_diet_difference$specific_group <- as.factor(specific_children_diet_difference$specific_group)
specific_children_diet_difference$specific_group <- reorder(specific_children_diet_difference$specific_group, specific_children_diet_difference$quant_males_vs_females, FUN = mean)

specific_diet_children_drilldown <- specific_children_diet_difference  %>% group_nest(macro_group) %>% 
                                                                          mutate(id = macro_group, type = "bar", data = purrr::map(data, mutate, name = specific_group, y  = (quant_males_vs_females)), data = purrr::map(data, list_parse))


########## d. diet consumption ratio compared to all adults - males ##########

consumption_ratio_adults_specific_groups_male <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                                                  group_by(demographic_group, specific_group) %>% 
                                                                  summarise(spec_group_conso = sum(daily_conso_kg)) %>% 
                                                                  mutate(share_conso = (spec_group_conso / sum(spec_group_conso)))

# compute consumption by specific group of foodstuff and by demographic group

consumption_ratio_adults_specific_groups_male <- left_join(consumption_ratio_adults_specific_groups_male, consumption_ratio_adults_specific_groups_male %>% 
                                                  filter(demographic_group %in% c("All adult males", "All adult females")) %>% 
                                                  group_by(specific_group) %>% 
                                                  summarise(mean_conso = mean(spec_group_conso)) %>% 
                                                  mutate(mean_share = (mean_conso / sum(mean_conso)))) # add the mean consumption for all adults

consumption_ratio_adults_specific_groups_male <- consumption_ratio_adults_specific_groups_male %>% filter(demographic_group %in% c("All adult males", "All adult females"))

consumption_ratio_adults_specific_groups_male$ratio_conso_vs_all_adults <- consumption_ratio_adults_specific_groups_male$spec_group_conso / consumption_ratio_adults_specific_groups_male$mean_conso # representation ratio comparing the consumption of the group to the average for all adults
consumption_ratio_adults_specific_groups_male$ratio_share_vs_all_adults <- consumption_ratio_adults_specific_groups_male$share_conso / consumption_ratio_adults_specific_groups_male$mean_share # representation ratio comparing the share of the consumption of the group to the average for all adults

consumption_ratio_adults_specific_groups_male$macro_group <- data_adults$macro_group[match(consumption_ratio_adults_specific_groups_male$specific_group,data_adults$specific_group)] # adding corresponding macro group

consumption_ratio_adults_specific_groups_male <- consumption_ratio_adults_specific_groups_male %>% filter(demographic_group == "All adult males") %>% 
                                                                                                    select("macro_group" = macro_group, "specific_group" = specific_group, "ratio_share_vs_all_adults" = ratio_share_vs_all_adults)

consumption_ratio_adults_specific_groups_male$ratio_share_vs_all_adults <- round(consumption_ratio_adults_specific_groups_male$ratio_share_vs_all_adults, digits = 2)


consumption_ratio_adults_macro_groups_male <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                                              group_by(demographic_group, macro_group) %>% 
                                                              summarise(macro_group_conso = sum(daily_conso_kg)) %>% 
                                                              mutate(share_conso = (macro_group_conso / sum(macro_group_conso)))

# compute consumption by macro group of foodstuff and by demographic group

consumption_ratio_adults_macro_groups_male <- left_join(consumption_ratio_adults_macro_groups_male, consumption_ratio_adults_macro_groups_male %>% filter(demographic_group %in% c("All adult males", "All adult females")) %>% group_by(macro_group) %>% summarise(mean_conso = mean(macro_group_conso)) %>% mutate(mean_share = (mean_conso / sum(mean_conso)))) # add the mean consumption for all adults

consumption_ratio_adults_macro_groups_male <- consumption_ratio_adults_macro_groups_male %>% filter(demographic_group %in% c("All adult males", "All adult females"))

consumption_ratio_adults_macro_groups_male$ratio_conso_vs_all_adults <- consumption_ratio_adults_macro_groups_male$macro_group_conso / consumption_ratio_adults_macro_groups_male$mean_conso # representation ratio comparing the consumption of the group to the average for all adults
consumption_ratio_adults_macro_groups_male$ratio_share_vs_all_adults <- consumption_ratio_adults_macro_groups_male$share_conso / consumption_ratio_adults_macro_groups_male$mean_share # representation ratio comparing the share of the consumption of the group to the average for all adults

consumption_ratio_adults_macro_groups_male <- consumption_ratio_adults_macro_groups_male %>% filter(demographic_group == "All adult males") %>% 
                                                                                              select("macro_group" = macro_group, "ratio_share_vs_all_adults" = ratio_share_vs_all_adults)

consumption_ratio_adults_macro_groups_male$ratio_share_vs_all_adults <- round(consumption_ratio_adults_macro_groups_male$ratio_share_vs_all_adults, digits = 2)

consumption_ratio_adults_specific_groups_drilldown_male <- consumption_ratio_adults_specific_groups_male %>% 
                                                            as.data.frame()  %>% 
                                                            group_nest(macro_group) %>% 
                                                            mutate(id = macro_group, type = "bar", data = purrr::map(data, mutate, name = specific_group, y  = (ratio_share_vs_all_adults)), data = purrr::map(data, list_parse))


########## e. diet consumption ratio compared to all adults - females ##########

consumption_ratio_adults_specific_groups_female <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                                                    group_by(demographic_group, specific_group) %>% 
                                                                    summarise(spec_group_conso = sum(daily_conso_kg)) %>% 
                                                                    mutate(share_conso = (spec_group_conso / sum(spec_group_conso)))

# compute consumption by specific group of foodstuff and by demographic group

consumption_ratio_adults_specific_groups_female <- left_join(consumption_ratio_adults_specific_groups_female, consumption_ratio_adults_specific_groups_female %>% 
                                                    filter(demographic_group %in% c("All adult males", "All adult females")) %>% 
                                                    group_by(specific_group) %>% summarise(mean_conso = mean(spec_group_conso)) %>%
                                                    mutate(mean_share = (mean_conso / sum(mean_conso)))) # add the mean consumption for all adults

consumption_ratio_adults_specific_groups_female <- consumption_ratio_adults_specific_groups_female %>% filter(demographic_group %in% c("All adult males", "All adult females"))

consumption_ratio_adults_specific_groups_female$ratio_conso_vs_all_adults <- consumption_ratio_adults_specific_groups_female$spec_group_conso / consumption_ratio_adults_specific_groups_female$mean_conso # representation ratio comparing the consumption of the group to the average for all adults
consumption_ratio_adults_specific_groups_female$ratio_share_vs_all_adults <- consumption_ratio_adults_specific_groups_female$share_conso / consumption_ratio_adults_specific_groups_female$mean_share # representation ratio comparing the share of the consumption of the group to the average for all adults

consumption_ratio_adults_specific_groups_female$macro_group <- data_adults$macro_group[match(consumption_ratio_adults_specific_groups_female$specific_group,data_adults$specific_group)] # adding corresponding macro group

consumption_ratio_adults_specific_groups_female <- consumption_ratio_adults_specific_groups_female %>% filter(demographic_group == "All adult females") %>% 
                                                                                                        select("macro_group" = macro_group, "specific_group" = specific_group, "ratio_share_vs_all_adults" = ratio_share_vs_all_adults)

consumption_ratio_adults_specific_groups_female$ratio_share_vs_all_adults <- round(consumption_ratio_adults_specific_groups_female$ratio_share_vs_all_adults, digits = 2)


consumption_ratio_adults_macro_groups_female <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                                                group_by(demographic_group, macro_group) %>% 
                                                                summarise(macro_group_conso = sum(daily_conso_kg)) %>% 
                                                                mutate(share_conso = (macro_group_conso / sum(macro_group_conso)))

# compute consumption by macro group of foodstuff and by demographic group

consumption_ratio_adults_macro_groups_female <- left_join(consumption_ratio_adults_macro_groups_female, consumption_ratio_adults_macro_groups_female %>% 
                                                  filter(demographic_group %in% c("All adult males", "All adult females")) %>% 
                                                  group_by(macro_group) %>% summarise(mean_conso = mean(macro_group_conso)) %>% 
                                                  mutate(mean_share = (mean_conso / sum(mean_conso)))) # add the mean consumption for all adults

consumption_ratio_adults_macro_groups_female <- consumption_ratio_adults_macro_groups_female %>% filter(demographic_group %in% c("All adult males", "All adult females"))

consumption_ratio_adults_macro_groups_female$ratio_conso_vs_all_adults <- consumption_ratio_adults_macro_groups_female$macro_group_conso / consumption_ratio_adults_macro_groups_female$mean_conso # representation ratio comparing the consumption of the group to the average for all adults
consumption_ratio_adults_macro_groups_female$ratio_share_vs_all_adults <- consumption_ratio_adults_macro_groups_female$share_conso / consumption_ratio_adults_macro_groups_female$mean_share # representation ratio comparing the share of the consumption of the group to the average for all adults

consumption_ratio_adults_macro_groups_female <- consumption_ratio_adults_macro_groups_female %>% filter(demographic_group == "All adult females") %>% 
                                                                                                  select("macro_group" = macro_group, "ratio_share_vs_all_adults" = ratio_share_vs_all_adults)

consumption_ratio_adults_macro_groups_female$ratio_share_vs_all_adults <- round(consumption_ratio_adults_macro_groups_female$ratio_share_vs_all_adults, digits = 2)

consumption_ratio_adults_specific_groups_drilldown_female <- consumption_ratio_adults_specific_groups_female %>% as.data.frame()  %>% 
                                                                                                                group_nest(macro_group) %>% mutate(id = macro_group, type = "bar", data = purrr::map(data, mutate, name = specific_group, y  = (ratio_share_vs_all_adults)), data = purrr::map(data, list_parse))


########## 4.3 Total environmental impacts by groups ##########

########## a. Adults ##########

adults_impacts_group <- data_adults %>% pivot_longer(cols = environmental_footprint_score:mineral_depletion, names_to = "env_impacts", values_to = "value") %>% 
                                        group_by(demographic_group, age_group, sex, env_impacts) %>% 
                                        summarise(value = sum(value * 365.25)) # consolidate impact score by sex (yearly values)


########## b. Children ##########

children_impacts_group <- data_children %>% pivot_longer(cols = environmental_footprint_score:mineral_depletion, names_to = "env_impacts", values_to = "value") %>% 
                                            group_by(demographic_group, age_group, sex, env_impacts) %>% 
                                            summarise(value = sum(value * 365.25)) # consolidate impact score by sex (yearly values)


########## c. Percentage differences for adults (synthesis)   ##########

synthesis_difference_adults_impacts <- adults_impacts_group %>% filter(age_group == "18-79")
synthesis_difference_adults_impacts <- synthesis_difference_adults_impacts %>% pivot_wider(names_from = "env_impacts", values_from = "value")
synthesis_difference_adults_impacts$demographic_group <- NULL
synthesis_difference_adults_impacts$age_group <- NULL

synthesis_difference_adults_impacts <- as.data.frame( ( ( ( synthesis_difference_adults_impacts[2,2:ncol(synthesis_difference_adults_impacts)] - synthesis_difference_adults_impacts[1,2:ncol(synthesis_difference_adults_impacts ) ] ) / ( synthesis_difference_adults_impacts[1,2:ncol(synthesis_difference_adults_impacts)] ) ) * 100) ) %>% 
                                        pivot_longer(cols = acidification:water_depletion, values_to = "value", names_to = "env_impacts")

synthesis_difference_adults_impacts$value <- round(synthesis_difference_adults_impacts$value, digits = 2)


########## d. Common variables   ##########

# related names of impacts and associated units 

list_impacts <- sort(unique(adults_impacts_group$env_impacts))
names_impacts <- c("Soil & Water Acidification", "Anthropogenic Climate Change", "Resource Depletion - Energy", "Environmental Footprint Score", "Freshwater Ecotoxicity", "Freshwater eutrophication", "Ionizing Radiation", "Marine Eutrophication", "Resource Depletion - Minerals, Metals", "Ozone Layer Depletion", "Photochemical Ozone Formation", "Particulate Matter", "Land Use", "Terrestrial Eutrophication", "Resource Depletion - Water")
units_impacts <- c("mol H+ eq.", "kg CO2 eq.", "MJ", "mPt", "CTUe", "E-03 kg P eq.", "kBq U-235 eq.", "E-03 kg N eq.", "E-06 kg Sb eq.", "E-06 kg CFC11 eq.", "E-03 kg NMVOC eq.", "E-06 disease inc.", "Pt", "mol N eq.", "m3 depriv.")

synthesis_difference_adults_impacts$env_impacts <- names_impacts # adjust names of variables for easier visualisation 



########## 4.4. Correlations between types of impacts ##########

########## a. Adults correlations ##########

cor_impacts_adults_ind <- ind_caracs_impacts %>% select(c(sex_ps, environmental_footprint_score:mineral_resources_depletion))


names(cor_impacts_adults_ind) <- c("Sex", "Environmental Footprint Score",  "Anthropogenic Climate Change", "Ozone Layer Depletion", "Ionizing Radiation", "Photochemical Ozone Formation", "Particulate Matter", "Soil & Water Acidification", "Terrestrial Eutrophication", "Freshwater eutrophication", "Marine Eutrophication","Land Use","Freshwater Ecotoxicity", "Resource Depletion - Water", "Resource Depletion - Energy", "Resource Depletion - Minerals, Metals")

short_names_cor_impacts_adults_ind <- c("Sex", "EFS",  "CC", "OLD", "IR", "POF", "PM", "SWA", "TE", "FEut", "ME","LU","FEtox", "WRD", "FRD", "MRD")

units_cor_impacts <- c("mPt", "kg CO2 eq.", "E-06 kg CFC11 eq.", "kBq U-235 eq.", "E-03 kg NMVOC eq.", "E-06 disease inc.", "mol H+ eq.", "mol N eq.", "E-03 kg P eq.", "E-03 kg N eq.", "Pt", "CTUe", "m3 depriv.", "MJ", "E-06 kg Sb eq.")
  


cor_impacts_adults_ind$Sex <- ifelse(cor_impacts_adults_ind$Sex == 1, "Male", "Female")

cor_impacts_adults_ind <- cor_impacts_adults_ind %>% mutate_if(is.numeric, round, digits = 2)



########## 4.5. Distribution of impacts ##########

########## a. Adults deciles of EFS ##########


adults_deciles_efs <- cor_impacts_adults_ind[,1:2]
adults_deciles_efs$deciles <- ntile(adults_deciles_efs$`Environmental Footprint Score`, 10)

adults_deciles_efs <- adults_deciles_efs %>% group_by(deciles, Sex) %>% 
                                              summarise(count = n()) %>% 
                                              mutate(freq = 100 * count / sum(count))



########## 4.6. Breakdown of male vs female differences in impacts ##########

########## a. Adults EFS difference ##########

efs_breakdown_adults <- adults_impacts_group %>% filter(demographic_group %in% c("All adult females", "All adult males")) %>% 
                                                  filter(env_impacts == "environmental_footprint_score") # value in yearly impacts

efs_breakdown_adults <- left_join(efs_breakdown_adults, adults_conso_quant)
efs_breakdown_adults$yearly_conso_kg <- efs_breakdown_adults$daily_conso_kg * 365.25 # harmonise to yearly value
efs_breakdown_adults$efs_intensity <- efs_breakdown_adults$value / efs_breakdown_adults$yearly_conso_kg
  
efs_breakdown_adults_waterfall <- data.frame("Variable" = c("Female Environmental Footprint Score", "Dietary choices", "Consumed quantities", "Interactions", "Male Environmental Footprint Score"), 

                                              "Value" = c( efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult females",]$value, 
                                              (efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult males",]$efs_intensity - efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult females",]$efs_intensity) * efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult females",]$yearly_conso_kg, 
                                              (efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult males",]$yearly_conso_kg - efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult females",]$yearly_conso_kg) * efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult females",]$efs_intensity, 
                                              (efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult males",]$efs_intensity - efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult females",]$efs_intensity) * (efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult males",]$yearly_conso_kg - efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult females",]$yearly_conso_kg), 
                                              - efs_breakdown_adults[efs_breakdown_adults$demographic_group == "All adult males",]$value), 
                                              "Type" = c("Average Impact Female", "Causes of differences", "Causes of differences", "Causes of differences", "Average Impact Male")) %>% 
                                              
                                              mutate(shade = colorize(Type, c(sex_colors[1], sex_colors[2],"#de2d26")))


########## 4.7. Analysis of contribution of macro groups to impact differences between males and females ##########


adults_decomposition_macro_group <- data_adults %>% filter(macro_group != "Baby food") %>% 
                                                    filter(demographic_group %in% c("All adult females", "All adult males")) %>% 
                                                    group_by(sex, macro_group) %>% 
                                                    summarise(quantities = 365.25 * sum(daily_conso_kg), efs = 365.25 * sum(environmental_footprint_score)) %>% 
                                                    mutate(intensity = efs/quantities) # overall table of impacts by groups

# impacts by group and by sex

adults_decomposition_macro_group_males <- adults_decomposition_macro_group %>% filter(sex == "Males")
adults_decomposition_macro_group_females <- adults_decomposition_macro_group %>% filter(sex == "Females")

# Decomposition of the impacts by group

adults_decomposition_macro_group_sex_difference <- data.frame(macro_group = adults_decomposition_macro_group_females$macro_group, quantities = adults_decomposition_macro_group_males$quantities - adults_decomposition_macro_group_females$quantities, efs = adults_decomposition_macro_group_males$efs - adults_decomposition_macro_group_females$efs, intensity = adults_decomposition_macro_group_males$intensity - adults_decomposition_macro_group_females$intensity) # decomposition summary

adults_decomposition_macro_group_sex_difference$diff_caused_by_dietary_choices <- adults_decomposition_macro_group_sex_difference$intensity * adults_decomposition_macro_group_females$quantities # difference caused by choices in foodstuffs within the group (intensity)

adults_decomposition_macro_group_sex_difference$diff_caused_by_consumed_quantities <- adults_decomposition_macro_group_sex_difference$quantities * adults_decomposition_macro_group_females$intensity # difference caused by quantities consumed within the group (quantity)

adults_decomposition_macro_group_sex_difference$diff_caused_by_interactions <- adults_decomposition_macro_group_sex_difference$intensity * adults_decomposition_macro_group_sex_difference$quantities # difference caused by interactions

adults_decomposition_macro_group_sex_difference$sh_diet <- adults_decomposition_macro_group_sex_difference$diff_caused_by_dietary_choices / (adults_decomposition_macro_group_sex_difference$diff_caused_by_dietary_choices + adults_decomposition_macro_group_sex_difference$diff_caused_by_consumed_quantities + adults_decomposition_macro_group_sex_difference$diff_caused_by_interactions)

adults_decomposition_macro_group_sex_difference$sh_quant <- adults_decomposition_macro_group_sex_difference$diff_caused_by_consumed_quantities / (adults_decomposition_macro_group_sex_difference$diff_caused_by_dietary_choices + adults_decomposition_macro_group_sex_difference$diff_caused_by_consumed_quantities + adults_decomposition_macro_group_sex_difference$diff_caused_by_interactions)

adults_decomposition_macro_group_sex_difference$sh_interact <- adults_decomposition_macro_group_sex_difference$diff_caused_by_interactions / (adults_decomposition_macro_group_sex_difference$diff_caused_by_dietary_choices + adults_decomposition_macro_group_sex_difference$diff_caused_by_consumed_quantities + adults_decomposition_macro_group_sex_difference$diff_caused_by_interactions)


adults_decomposition_macro_group_sex_difference_waterfall <- adults_decomposition_macro_group_sex_difference %>% 
                                                              select(-quantities, -sh_diet, -sh_quant, - sh_interact, - intensity)

colnames(adults_decomposition_macro_group_sex_difference_waterfall) <- c("Macro Group", "Net difference",  "Dietary choices", "Consumed quantities", "Interactions")

adults_decomposition_macro_group_sex_difference_waterfall_perc <- adults_decomposition_macro_group_sex_difference_waterfall %>% 
                                                                    mutate_at(vars(-`Macro Group`),  ~ 100 * . / adults_decomposition_macro_group_females$efs) # percentage difference compared to female EFS


########## 4.8 Environmental impacts by occupation and gender ##########


occupation_impacts_gender <- ind_caracs_impacts %>% group_by(sex_ps, pcs_8cl_interv) %>% 
                                                    summarise(avg = mean(as.numeric(environmental_footprint_score), na.rm = T))

occupation_impacts_gender$sex_ps <- ifelse(occupation_impacts_gender$sex_ps == "1", "Males", "Females")
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 1] <- "Office Worker"
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 2] <- "Manual Worker"
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 3] <- "Farmer"
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 4] <- "Craftsman, shopkeeper or company director/owner"
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 5] <- "Middle manager or other intermediate profession"
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 6] <- "Executive, self-employed professional"
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 7] <- "Retired, used to work"
occupation_impacts_gender$pcs_8cl_interv[occupation_impacts_gender$pcs_8cl_interv == 8] <- "Other non-working"

occupation_impacts_gender$avg <- occupation_impacts_gender$avg * 365.25

########## 5. creating graphs ##########

########## 5.1 Formatting of graphs ##########


# Font Sizes

title_size <- 22
axis_size <- 16
caption_size <- 14

# Colors and theming

sex_colors <- c("#9970ab","#5aae61")

decompo_colors <- c("#66c2a5", "#fc8d62", "#8da0cb")

foodgroups_colors <- c("#e65e62", "#0b0e0a", "#F3C300", "#9e7587", "#6b3241", "#A1CAF1", "#C2B280","#9b3950", "#BE0032", "#4C8659","#f79b57", "#0067A5")

foodgroups_children_colors <- c("#848482", "#C2B280", "#0b0e0a", "#A1CAF1", "#F3C300", "#e65e62", "#9e7587", "#4C8659","#BE0032", "#9b3950", "#f79b57","#6b3241", "#0067A5")

male_adults_ratio_fg_colors <- c("#0b0e0a", "#BE0032", "#C2B280", "#f79b57", "#9e7587", "#A1CAF1", "#F3C300","#4C8659", "#0067A5", "#9b3950","#6b3241", "#e65e62")

female_adults_ratio_fg_colors <- c("#BE0032", "#C2B280", "#0b0e0a", "#f79b57", "#9e7587", "#A1CAF1", "#F3C300","#4C8659", "#0067A5", "#9b3950","#6b3241", "#e65e62")

impacts_colors <- c("#654522", "#C2B280", "#BE0032", "#DCD300", "#B2533E", "#2f4b7c", "#008856","#DE8344", "#665191","#d45087", "#0570b0", "#F5C342", "#9ADE7B","#A1CAF1", "#003f5c")

methodo_colors <- c("#a6d96a", "#1a9641", "#ffffbf", "#d7191c")

selected_theme <- hc_theme_smpl()



########## 5.2 Quantities graphs ##########

males_females_daily_conso <- hchart(adults_conso_quant, "column", hcaes(x = age_group, y = daily_conso_kg, group = sex), color = sex_colors) %>% 
                              
                              hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Male adults consume higher quantities of food than females across all age groups") %>% 
                              
                              hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                              hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015)", href = "https://www.anses.fr/fr/content/inca-3-evolution-des-habitudes-et-modes-de-consommation-de-nouveaux-enjeux-en-mati%C3%A8re-de") %>% 

                              hc_xAxis(title = list(text = "Age group", style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                              hc_yAxis(title = list(text = "Daily food consumption (kg)", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                              hc_yAxis(labels = list(format = "{value} kg", style = list(useHTML = TRUE, fontSize = axis_size))) %>%

                              hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), headerFormat = "<b>{point.name}</b>", valueSuffix = " kg")   %>%

                              hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                              
                              hc_add_theme(selected_theme) %>% 
                              
                              hc_exporting(enabled = TRUE, filename = "males_females_adult_daily_conso")
                              
                               

males_females_daily_conso$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("males_females_daily_conso.js")


# Total food quantities kids

males_females_kids_daily_conso <- hchart(children_conso_quant, "column", hcaes(x = age_group, y = daily_conso_kg, group = sex), color = sex_colors) %>% 

                                  hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Except for babies below 1, this pattern is already present for children and adolescents") %>% 
                                  
                                  hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                                  hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015)", href = "https://www.anses.fr/fr/content/inca-3-evolution-des-habitudes-et-modes-de-consommation-de-nouveaux-enjeux-en-mati%C3%A8re-de") %>% 

                                  hc_xAxis(title = list(text = "Age group", style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                  hc_yAxis(title = list(text = "Daily food consumption", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                  hc_yAxis(labels = list(format = "{value} kg", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                  
                                  hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), headerFormat = "<b>{point.name}</b>", valueSuffix = " kg")  %>% 

                                  hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>%
                                  
                                  hc_add_theme(selected_theme) %>% 
                                  
                                  hc_exporting(enabled = TRUE, filename = "males_females_children_daily_conso") 
                                  
                                  
                                  

males_females_kids_daily_conso$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("males_females_kids_daily_conso.js")




# Differences in consumption by type of foodstuff adults


tooltip_category_text_conso_diff_adults <- c("Male - Female cons. (kg):")
tooltip_formatted_values_conso_diff_adults <- c("{point.quant_males_vs_females} kg")
my_tooltips_conso_diff_adults <- tooltip_table(tooltip_category_text_conso_diff_adults, tooltip_formatted_values_conso_diff_adults)


adult_quantities_differences <- highchart() %>% 

                              hc_xAxis(type = "category") %>% 
                              hc_legend(enabled = FALSE) %>% 
                              
                              hc_add_series(
                                data = macro_adults_diet_difference %>% arrange(quant_males_vs_females) %>% mutate(shade = colorize(macro_group, foodgroups_colors)), 
                                type = "bar", hcaes(name = macro_group, y = quant_males_vs_females, drilldown = macro_group, color = shade), name = "Macro-groups") %>%
                                
                                 hc_drilldown(series = list_parse(specific_diet_drilldown), colorByPoint=T, allowPointDrilldown = TRUE,  activeDataLabelStyle = list(textDecoration="none", color="black")) %>% 

                                  hc_title(text = "Male adults consume much more alcohol, red meat (esp. beef) and various junk food than females", style = list(useHTML = TRUE, fontSize = title_size)) %>%

                                 hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                                 hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015)", href = "https://www.anses.fr/fr/content/inca-3-evolution-des-habitudes-et-modes-de-consommation-de-nouveaux-enjeux-en-mati%C3%A8re-de") %>% 

                                hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                hc_yAxis(title = list(text = "Male yearly overconsumption compared to females", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                hc_yAxis(labels = list(format = "{value} kg", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                            
                                 hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), pointFormat = my_tooltips_conso_diff_adults) %>% 
                                 
                                 hc_add_theme(selected_theme)
                                 

adult_quantities_differences$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("adult_quantities_differences.js")



# Differences in consumption by type of foodstuff children


children_quantities_differences <- highchart() %>% 

                                  hc_xAxis(type = "category") %>% 
                                  hc_legend(enabled = FALSE) %>% 
                                  
                                  hc_add_series(data = macro_children_diet_difference %>% arrange(quant_males_vs_females) %>% mutate(shade = colorize(macro_group, foodgroups_children_colors)), 
                                  type = "bar", hcaes(name = macro_group, y = quant_males_vs_females, drilldown = macro_group, color = shade), name = "Macro-groups", colorByPoint = TRUE) %>% 
                                  hc_drilldown(series = list_parse(specific_diet_children_drilldown), colorByPoint=T, allowPointDrilldown = TRUE,  activeDataLabelStyle = list(textDecoration="none", color="black")) %>% 
                                  
                                  hc_title(text = "Male children consume more food than females, with the notable exception of the earliest period (esp. baby food)", style = list(useHTML = TRUE, fontSize = title_size)) %>%

                                  hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 0-17") %>% 
                                  hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015)", href = "https://www.anses.fr/fr/content/inca-3-evolution-des-habitudes-et-modes-de-consommation-de-nouveaux-enjeux-en-mati%C3%A8re-de")  %>%

                                  hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                  hc_yAxis(title = list(text = "Male yearly overconsumption compared to females"), style = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                                  hc_yAxis(labels = list(format = "{value} kg", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 

                                  hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), pointFormat = my_tooltips_conso_diff_adults)  %>% 
                                  
                                  hc_add_theme(selected_theme) 


children_quantities_differences$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("children_quantities_differences.js")


# Consumption ratio specific groups - adults males

tooltip_category_text_share_ratio_adults_male <- c("Ratio male vs all adults (share in diet):")
tooltip_formatted_values_share_ratio_adults_male <- c("{point.ratio_share_vs_all_adults}")
my_tooltips_share_ratio_adults_male <- tooltip_table(tooltip_category_text_share_ratio_adults_male, tooltip_formatted_values_share_ratio_adults_male)


adult_share_ratio_male <- highchart() %>% 

                          hc_xAxis(type = "category") %>% 
                          hc_legend(enabled = FALSE) %>% 
                          
                          hc_add_series(data = consumption_ratio_adults_macro_groups_male %>% arrange(-ratio_share_vs_all_adults) %>% mutate(shade = male_adults_ratio_fg_colors),
                           type = "bar", hcaes(name = macro_group, y = round(ratio_share_vs_all_adults, digits = 2), 
                           drilldown = macro_group, color = shade), name = "Macro-groups") %>% 
                           hc_drilldown(series = list_parse(consumption_ratio_adults_specific_groups_drilldown_male), colorByPoint=T, allowPointDrilldown = TRUE,  activeDataLabelStyle = list(textDecoration="none", color="black")) %>% 

                           hc_title(text = "Male diets are richer in meat, ice creams, sweet and cereal when compared to all adults", style = list(useHTML = TRUE, fontSize = title_size)) %>% 
                           
                           hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                           hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015)", href = "https://www.anses.fr/fr/content/inca-3-evolution-des-habitudes-et-modes-de-consommation-de-nouveaux-enjeux-en-mati%C3%A8re-de")   %>% 
                           
                           hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                           hc_yAxis(title = list(text = "Share of consumption as compared to all adults", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                           hc_yAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size)), plotLines = list(list(label = list(text = ""), dashStyle = "dash", color = "black", width = 2, value = 1, zIndex = 5))) %>% 

                           hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), pointFormat = my_tooltips_share_ratio_adults_male) %>% 
                           
                           hc_add_theme(selected_theme)


# Consumption ratio specific groups - adults females

tooltip_category_text_share_ratio_adults_female <- c("Ratio female vs all adults (share in diet):")
tooltip_formatted_values_share_ratio_adults_female <- c("{point.ratio_share_vs_all_adults}")
my_tooltips_share_ratio_adults_female <- tooltip_table(tooltip_category_text_share_ratio_adults_female, tooltip_formatted_values_share_ratio_adults_female)


adult_share_ratio_female <- highchart() %>% 
                            hc_xAxis(type = "category") %>% 
                            hc_legend(enabled = FALSE) %>% 
                            
                            hc_add_series(data = consumption_ratio_adults_macro_groups_female %>% arrange(-ratio_share_vs_all_adults) %>% mutate(shade = rev(female_adults_ratio_fg_colors)), 
                            type = "bar", hcaes(name = macro_group, y = round(ratio_share_vs_all_adults, digits = 2), 
                            drilldown = macro_group, color = shade), name = "Macro-groups") %>% 
                            hc_drilldown(series = list_parse(consumption_ratio_adults_specific_groups_drilldown_female), colorByPoint=T, allowPointDrilldown = TRUE,  activeDataLabelStyle = list(textDecoration="none", color="black")) %>% 
                            
                            hc_title(text = "Female diets are richer in eggs, starters/mixes dishes and dairy when compared to all adults", style = list(useHTML = TRUE, fontSize = title_size)) %>% 
                            
                            hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                            hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015)", href = "https://www.anses.fr/fr/content/inca-3-evolution-des-habitudes-et-modes-de-consommation-de-nouveaux-enjeux-en-mati%C3%A8re-de") %>% 

                            hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                            hc_yAxis(title = list(text = "Share of consumption as compared to all adults"), style = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                            hc_yAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size)), title = list(style = list(useHTML = TRUE, fontSize = axis_size)), plotLines = list(list(label = list(text = ""), dashStyle = "dash", color = "black", width = 2, value = 1, zIndex = 5))) %>% 

                            hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), pointFormat = my_tooltips_share_ratio_adults_female) %>% 
                            
                            hc_add_theme(selected_theme)
                            



# Grid combining both males and females consumption ratios

adult_share_ratio <- hw_grid(adult_share_ratio_male, adult_share_ratio_female, ncol = 2)

adult_share_ratio$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("adult_share_ratio.js")


# Diet composition

# For reference only, do a pictorial graph directly using highcharter js 

hchart(diet_macro_comp %>% filter(age_group == "18-79"), "bar", hcaes(x = sex, y = percent, group = macro_group)) %>%  hc_plotOptions(bar = list(stacking = "normal"))




########## 5.3 Overview of Impacts graphs ##########


# Adult impact scores
 

for (i in 1:length(list_impacts)) { # graphs for all types of impacts
  
  assign(paste0("adults_", list_impacts[i]), 
  
  hchart(adults_impacts_group %>% 
        
        filter(env_impacts == list_impacts[i]), 
        "column", hcaes(x = age_group, y = round(value, digits = 2), group = sex), color = sex_colors) %>% 
        
        hc_title(text = names_impacts[i], style = list(useHTML = TRUE, fontSize = title_size)) %>% 

        hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size)), title = list(text = "Age group", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
        hc_yAxis(title = list(text = names_impacts[i], style = list(useHTML = TRUE, fontSize = 0))) %>% 
        hc_yAxis(labels = list(format = paste0("{value} ",units_impacts[i]), style = list(useHTML = TRUE, fontSize = axis_size)))) %>% 

        hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), headerFormat = "<b>{point.name}</b>", valueSuffix = paste0(" ",units_impacts[i]))  %>% 
         
        hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 

        hc_add_theme(selected_theme)
  
  
}


adults_impacts <- hw_grid(adults_environmental_footprint_score, adults_climate_change, adults_ion_radiation, adults_ozone_depletion, adults_ozone_formation, adults_particles, adults_soil_use, adults_acidification, adults_terrestrial_eutrophisation, adults_marine_eutrophisation, adults_fresh_water_eutrophisation, adults_fresh_water_ecotoxicity, adults_water_depletion, adults_energy_depletion, adults_mineral_depletion, ncol = 3)

adults_impacts$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("adults_impacts.js")




# Children impact scores


for (i in 1:length(list_impacts)) { # graphs for all types of impacts
  
  assign(paste0("children_", list_impacts[i]), 
  
  hchart(children_impacts_group %>% 
          
          filter(env_impacts == list_impacts[i]), 
          "column", hcaes(x = age_group, y = round(value, digits = 2), group = sex), color = sex_colors) %>% 
          
          hc_title(text = names_impacts[i], style = list(useHTML = TRUE, fontSize = title_size)) %>% 
          
          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size)), title = list(text = "Age group", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
          hc_yAxis(title = list(text = names_impacts[i], style = list(useHTML = TRUE, fontSize = 0))) %>% 
          hc_yAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size), format = paste0("{value} ",units_impacts[i])))) %>% 

          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), headerFormat = "<b>{point.name}</b>", valueSuffix = paste0(" ",units_impacts[i])) %>% 

          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 

          hc_add_theme(selected_theme)
  
  
}


children_impacts <- hw_grid(children_environmental_footprint_score, children_climate_change, children_ion_radiation, children_ozone_depletion, children_ozone_formation, children_particles, children_soil_use, children_acidification, children_terrestrial_eutrophisation, children_marine_eutrophisation, children_fresh_water_eutrophisation, children_fresh_water_ecotoxicity, children_water_depletion, children_energy_depletion, children_mineral_depletion, ncol = 3)


children_impacts$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("children_impacts.js")






# Scatter plot for correlations between EFS and other impacts
# use individual level data


# simplify: show correlations only between enviornmental footprint and rest of indicators > can do highchart scatterplot (15), with correlation coefficients shown 



for (i in 3:ncol(cor_impacts_adults_ind)) { # graphs for all types of impacts
  
  assign(paste0("adults_correl_EFS_", short_names_cor_impacts_adults_ind[i]), 
  
  hchart(cor_impacts_adults_ind, "scatter", hcaes(x = round(`Environmental Footprint Score` * 365.25, digits = 0), y = round(365.25 * get(colnames(cor_impacts_adults_ind)[i]), digits = 0), group = Sex), 

  regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 3, name = "%eq | r2: %r", hideInLegend = FALSE), color = sex_colors)  %>% 
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_colors(sex_colors) %>% 
  
  hc_title(text = paste0("Correlation between Environmental Footprint Score and ", colnames(cor_impacts_adults_ind)[i]), style = list(useHTML = TRUE, fontSize = title_size)) %>% 
  
  hc_xAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
  hc_xAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) )
  hc_xAxis(type = "logarithmic") %>% 
  hc_yAxis(title = list(text = paste0(colnames(cor_impacts_adults_ind)[i]), style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
  hc_yAxis(labels = list(format = paste0("{value} ",units_cor_impacts[i-1]), style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
  hc_yAxis(type = "logarithmic") %>% 

  hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + 'EFS: ' + this.point.x + ' (Pt) <br>' + 'Correlate: ' + this.point.y + ' (see unit lhs)' )}")) %>% 

  hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
  
  hc_add_theme(selected_theme) 
  
  
}


overview_correlations_EFS_adults <- hw_grid(adults_correl_EFS_CC, adults_correl_EFS_OLD, adults_correl_EFS_IR,  adults_correl_EFS_POF, adults_correl_EFS_PM, adults_correl_EFS_SWA, adults_correl_EFS_TE, adults_correl_EFS_FEut, adults_correl_EFS_ME, adults_correl_EFS_LU, adults_correl_EFS_FEtox, adults_correl_EFS_WRD, adults_correl_EFS_FRD, adults_correl_EFS_MRD, ncol = 3)

overview_correlations_EFS_adults$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("overview_correlations_EFS_adults.js")


# put a note saying that the axes are logarithmic 



########## 5.4 Comparison of Impacts between Groups graphs ##########



# Synthesis graph: difference between impacts in percentage for all adults (synthesis of the difference basically)


synthesis_adults_impacts <- hchart(synthesis_difference_adults_impacts %>% arrange(value) %>% mutate(shade = rev(impacts_colors)), 

                            "column", hcaes(color = shade, x = env_impacts, y = round(value, digits = 2))) %>% 
                            
                            hc_title(text = "Adult males pollute more for food consumption than females alongside all environmental indicators", style = list(useHTML = TRUE, fontSize = title_size)) %>% 

                            hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                            hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>% 
                            
                            hc_xAxis(title = list(text = "Environmental Impact", style  = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                            hc_yAxis(title = list(text = "Percentage (males compared to females)", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                            hc_yAxis(plotLines = list(list(label = list(text = "Overall footprint", style = list(useHTML = TRUE, fontSize = axis_size)), dashStyle = "dash", color = "black", width = 2, value = (synthesis_difference_adults_impacts[synthesis_difference_adults_impacts$env_impacts == "Environmental Footprint Score",]$value), zIndex = 5))) %>% 
                            
                            hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), pointFormat = paste0("<b>{point.key}</b> Males pollute ", "{point.value}", "% more than Females")) %>% 
                            
                            hc_add_theme(selected_theme)  %>% 

                            hc_exporting(enabled = TRUE, filename = "synthesis_male_food_impacts_above_female_adult") %>% hc_yAxis(labels = list(format = "{value}%", style = list(useHTML = TRUE, fontSize = axis_size)))



synthesis_adults_impacts$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("synthesis_adults_impacts.js")




# Distribution of EFS by sex

distrib_efs_by_sex <- hchart(density(cor_impacts_adults_ind[cor_impacts_adults_ind$Sex == "Male",]$`Environmental Footprint Score` * 365.25), type = "area", name = "Males", color = sex_colors[2]) %>%  

hc_add_series(density(cor_impacts_adults_ind[cor_impacts_adults_ind$Sex == "Female",]$`Environmental Footprint Score` * 365.25), name = "Females", type = "area", color = sex_colors[1]) %>% 

hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = paste0("Females have lower and less dispersed environmental impacts than males"))  %>% 

hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>% 

hc_xAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
hc_xAxis(plotLines = list(list(color = "black", width = 2, zIndex = 5,  dashStyle = "dash", label = list(text = "Male average", style = list(useHTML = TRUE, fontSize = axis_size)), value = (adults_impacts_group %>% filter(demographic_group == "All adult males") %>% filter(env_impacts == "environmental_footprint_score"))$value), list(color = "black", width = 2, zIndex = 5,  dashStyle = "dash",  label = list(text = "Female average", style = list(useHTML = TRUE, fontSize = axis_size)), value = (adults_impacts_group %>% filter(demographic_group == "All adult females") %>% filter(env_impacts == "environmental_footprint_score"))$value))) %>% 
hc_xAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
hc_yAxis(title = list(text = "Density", style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>% 

hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' Individuals <br>' + 'EFS: ' + Highcharts.numberFormat(this.point.x,0) + ' (mPt) <br>' + 'Density: ' + Highcharts.numberFormat(this.point.y,5) )}")) %>% 

hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 

hc_add_theme(selected_theme) %>% 

hc_exporting(enabled = TRUE, filename = "individual_efs_density")


distrib_efs_by_sex$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T, force = TRUE) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("distrib_efs_by_sex.js")




# Composition of deciles of EFS by sex 

compo_deciles_efs_sex <- hchart(adults_deciles_efs, 

        "column", hcaes(x = deciles, y = freq, group = Sex), color = sex_colors) %>%  
        hc_plotOptions(column = list(stacking = "normal")) %>% 
                
        hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Males are strongly over-represented in the top polluters from food consumption") %>% 
        
        hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79. Black horizontal line represents the equal gender split") %>% 
        hc_credits( style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>%

        hc_xAxis(title = list(text = "Environmental Footprint Score Deciles (from least to most polluting)",  style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
        hc_yAxis(title = list(text = "Share of Decile", style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size)), min = 0, max = 105) %>% 
        hc_yAxis(plotLines = list(list(color = "black", width = 2, zIndex = 5,  dashStyle = "dash", value = 50))) %>% 
        hc_yAxis(labels = list(format = "{value}%")) %>%
        
        hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (Highcharts.numberFormat(this.point.y,2) + '% of people in the D' + this.point.x + ' of Environmental Footprint Score are ' + this.series.name + 's' )}"))   %>% 

        hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
        
        hc_add_theme(selected_theme) %>% 
        
        hc_exporting(enabled = TRUE, filename = "deciles_efs_sex_adults") 
        

compo_deciles_efs_sex$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T, force = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("compo_deciles_efs_sex.js")
    



# Waterfall chart overall impact decomposition males vs females

waterfall_decomposition <- hchart(efs_breakdown_adults_waterfall, 

          "waterfall")  %>% 
          hc_xAxis(type = "category") %>% 
          
          hc_add_series(efs_breakdown_adults_waterfall, "waterfall", hcaes(x = Variable, y = round(Value, digits = 0), color = shade)) %>% 
          
          hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Males pollute more than females primarily because they eat more, but also due to their dietary choices") %>% 

          hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
          hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79. Differences computed based on a decomposition approach distinguishing quantities and environmental impact intensity (by kg)") %>% 

          hc_xAxis(title = list(text = ""), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_yAxis(labels = list(format = "{value} mPt" , style = list(useHTML = TRUE, fontSize = axis_size))) %>%

          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.point.name + '' + ': ' + Math.abs(this.point.y) )}")) %>% 
          
          hc_add_theme(selected_theme) %>% 
          
          hc_exporting(enabled = TRUE, filename = "waterfall_efs_adults")
          
          
waterfall_decomposition$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T, force = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("waterfall_decomposition.js")             
                  



########## 5.5 Lifecycle graphs ##########



# Life cycle analysis (cumulative by age)

lca_impacts_efs <- hchart(lca_cum, "line") %>% 

                  hc_add_series(lca_cum, "line", 
                  hcaes(x = age, y = round(EFP, digits = 0), group = Sex),  color = sex_colors) %>% 
                  hc_plotOptions(series = list(animation = list(duration = 6000)))  %>% 
                  
                  hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Over their lifetime, males pollute about 20% more than females for their food consumption")  %>% 

                  hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
                  hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79. Impacts based on smoothened values by age groups") %>% 

                  hc_xAxis(title = list(text = "Age",  style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                  hc_xAxis(max = 85)  %>% 
                  hc_yAxis(title = list(text = "Cumulative Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                  hc_yAxis(labels = list(format = "{value} mPt")) %>% 

                  hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), shared = TRUE, crosshairs = T) %>% 

                  hc_legend(align = "center", verticalAlign = "top") %>% 
                  
                  hc_add_theme(selected_theme) %>% 
                  
                  hc_exporting(enabled = TRUE, filename = "life_cycle_cumulative_efp")
                  
                  
lca_impacts_efs$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("lca_impacts_efs.js")             
                  
                  

########## 5.6 Decomposition by food group impact graphs ##########



# macrogroup contribution to impacts 

group_contrib_impacts <- hchart(adults_decomposition_macro_group  %>% arrange(-efs), "lollipop", 
                          hcaes(name = macro_group, low = round(efs, digits = 1), group = sex), name = c("Females", "Males"),   color = sex_colors, stacking = "normal") %>%
                           hc_chart(inverted=T) %>% 
                           
                           hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Meat, mixed dishes and beverages are the leading causes of environmental impacts for both genders") %>% 
                           
                           hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                            hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>% 

                           hc_xAxis(type = "category", labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                           hc_xAxis(title = list(text = "Food Macro Groups", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                           hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                           hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size)))  %>% 
                           hc_yAxis(labels = list(format = "{value} mPt")) %>% 

                           hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), valueSuffix = " mPt") %>%

                           hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                           
                           hc_add_theme(selected_theme) %>% hc_exporting(enabled = TRUE, filename = "efs_difference_macro_groups")
                           
                           
group_contrib_impacts$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("group_contrib_impacts.js")                         


# Decomposition of impacts between quantities and intensity

decompos_waterfall_pts <- hchart(adults_decomposition_macro_group_sex_difference_waterfall %>% 
                          pivot_longer(!`Macro Group`, names_to = "type", values_to = "value") %>% 
                          filter(type != "Net difference") %>% arrange(-value), 
                          "bar", hcaes(x = `Macro Group`, y = round(value, digits = 1), group = type), stacking = "normal", color = decompo_colors) %>% 

                           hc_add_series(adults_decomposition_macro_group_sex_difference_waterfall %>% pivot_longer(!`Macro Group`, names_to = "type", values_to = "value") %>% filter(type == "Net difference") , "scatter", hcaes(`Macro Group`, round(value, digits = 1)), name = "Net Difference", color = "black") %>% 

                          hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Gender differences in environmental impacts have origins that strongly depend on the food group") %>% 
                          
                          hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                          hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>% 

                          hc_xAxis(title = list(text = "Food Macro Groups", style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                          hc_yAxis(title = list(text = "Gender difference in Environmental Footprint Score (Male - Female)")) %>% 
                          hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size)), title = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%

                          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return ('For ' + this.point.name + ', males pollute ' + (this.point.y) + ' mPt more than females due to this factor' )}")) %>%

                          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% hc_add_theme(selected_theme) %>% 
                          
                          hc_exporting(enabled = TRUE, filename = "efs_difference_macro_groups_decomposition")
                          
                          
decompos_waterfall_pts$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("decompos_waterfall_pts.js")                         
     
                          


decompo_waterfall_pc <- hchart(adults_decomposition_macro_group_sex_difference_waterfall_perc %>% 
                        pivot_longer(!`Macro Group`, names_to = "type", values_to = "value") %>% 
                        filter(type != "Net difference") %>% arrange(-value), 
                        "bar", hcaes(x = `Macro Group`, y = round(value, digits = 1), group = type), stacking = "normal", color = decompo_colors) %>% 
                        
                       hc_add_series(adults_decomposition_macro_group_sex_difference_waterfall_perc %>% pivot_longer(!`Macro Group`, names_to = "type", values_to = "value") %>% filter(type == "Net difference") , "scatter", hcaes(`Macro Group`, round(value, digits = 1)), name = "Net Difference", color = "black") %>% 

                        hc_title(style = list(useHTML = TRUE, fontSize = title_size), text = "Gender differences in environmental impacts have origins that strongly depend on the food group") %>% 
                        
                        hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
                        hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>% 

                        hc_xAxis(title = list(text = "Food Macro Groups", style = list(useHTML = TRUE, fontSize = axis_size)), labels = list(style = list(useHTML = TRUE, fontSize = axis_size)))   %>% 
                        hc_yAxis(title = list(text = "Additional male pollution compared to female (%)")) %>% 
                        hc_yAxis(labels = list(format = "{value}%", style = list(useHTML = TRUE, fontSize = axis_size)), title = list(style = list(useHTML = TRUE, fontSize = axis_size))) %>%

                        hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return ('For ' + this.point.name + ', males pollute ' + (this.point.y) + '% more than females due to this factor' )}")) %>% 

                        hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                        
                        hc_add_theme(selected_theme) %>% 
                        
                        hc_exporting(enabled = TRUE, filename = "efs_difference_macro_groups_decomposition_perc")
                        
                        
                        
decompo_waterfall_pc$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T, force = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("decompo_waterfall_pc.js")                         
                         



########## 5.7. Anthropometrics graphs ##########


# Correlation BMR and EFS 


correl_bmr_efp_bothsexes_pooled <- hchart(ind_caracs_impacts %>% mutate(sex = ifelse(sex_ps == 1, "Males", "Females")), 
                                  "scatter", hcaes(x = round(bmr_kcal, digits = 0), y = round(environmental_footprint_score * 365.25, digits = 0), group = sex)) %>% 
                                  
                                 hc_add_series(data.frame("bmr_kcal" = c(977,3022), "environmental_footprint_score" = c(0.11*977+86.55, 0.11*3022+86.55)), "line", hcaes(bmr_kcal, environmental_footprint_score), name = "Trend line (both sexes pooled): 0.11 * BMR + 86.55 (r2 = 0.27)", color = "black", dashStyle = "ShortDash",  lineWidth = 3) %>% 

                                  hc_title(text = paste0("BMR strongly correlates with Environmental Footprint Score across sexes"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 

                                  hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
                                  hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79")  %>%

                                  hc_xAxis(title = list(text = "Basal Metabolic Rate", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                  hc_xAxis(labels = list(format = "{value} kcal", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                  hc_xAxis(type = "logarithmic") %>%                                 
                                  hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                  hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                  hc_yAxis(type = "logarithmic") %>% 
                                  
                                  hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + 'BMR: ' + this.point.x + ' (kcal) <br>' + 'EFS: ' + this.point.y + ' (mPt)' )}")) %>% 

                                  hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                                  
                                  hc_colors(sex_colors) %>% 

                                  hc_add_theme(selected_theme) %>% 
                                  
                                  hc_exporting(enabled = TRUE, filename = "efs_bmr_correlation_sex") 
                                  

correl_bmr_efp_bothsexes_pooled$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T, force = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("correl_bmr_efp_bothsexes_pooled.js")                         
                                                   
                                  
                                  


correl_bmr_efp_individual_sexes <- hchart(ind_caracs_impacts %>% mutate(sex = ifelse(sex_ps == 1, "Males", "Females")), 
                                    "scatter", hcaes(x = round(bmr_kcal, digits = 0), y = round(environmental_footprint_score * 365.25, digits = 0), group = sex), 
                                    
                                    regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 3, name = "%eq | r2: %r", hideInLegend = FALSE), useAllSeries = TRUE, color = sex_colors)  %>% 
                                    hc_add_dependency("plugins/highcharts-regression.js") %>% 
                                    
                                    hc_title(text = paste0("Correlation between BMR and Environmental Footprint Score is weak"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 

                                    hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
                                    hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 

                                    hc_xAxis(title = list(text = "Basal Metabolic Rate", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                    hc_xAxis(labels = list(format = "{value} kcal", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                    hc_xAxis(type = "logarithmic") %>% 
                                    hc_yAxis(type = "logarithmic") %>% 
                                    hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                    hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 

                                    hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + 'BMR: ' + this.point.x + ' (kcal) <br>' + 'EFS: ' + this.point.y + ' (mPt)' )}")) %>% 

                                    hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                                    
                                    hc_add_theme(selected_theme) %>% 

                                    hc_colors(sex_colors) %>% 

                                    hc_exporting(enabled = TRUE, filename = "efs_bmr_correlation_sex") 
                                    

                                    

correl_bmr_efp_individual_sexes$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("correl_bmr_efp_individual_sexes.js")  




# Correlation heigth and EFS


correl_height_efp_bothsexes_pooled <- hchart(ind_caracs_impacts %>% mutate(sex = ifelse(sex_ps == 1, "Males", "Females")), 
          "scatter", hcaes(x = taille, y = round(environmental_footprint_score * 365.25, digits = 0), group = sex)) %>% 

          hc_add_series(data.frame("height" = c(143,196), "environmental_footprint_score" = c(3.03*143-258.99,3.03*196-258.99)), "line", hcaes(height, environmental_footprint_score), name = "Trend line (both sexes pooled): 3.03 * Height -258.99 (r2 = 0.28)", color = "black", dashStyle = "ShortDash",  lineWidth = 3) %>% 
          
          hc_title(text = paste0("Correlation between height and Environmental Footprint Score is also high when considering the entire sample of adults"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 

          hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
          hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 
          
          hc_xAxis(title = list(text = "Height", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_xAxis(labels = list(format = "{value} cm", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
          hc_xAxis(type = "logarithmic") %>% 
          hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_yAxis(type = "logarithmic") %>% 
          
          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + 'Height: ' + this.point.x + ' (cm) <br>' + 'EFS: ' + this.point.y + ' (mPt)' )}")) %>% 
          
          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 

          hc_colors(sex_colors) %>% 

          hc_add_theme(selected_theme) %>% 

          hc_exporting(enabled = TRUE, filename = "efs_height_correlation_sex")
          

correl_height_efp_bothsexes_pooled$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("correl_height_efp_bothsexes_pooled.js")      
          



correl_height_efp_individual_sexes <- hchart(ind_caracs_impacts %>% mutate(sex = ifelse(sex_ps == 1, "Males", "Females")), 
                                      "scatter", hcaes(x = taille, y = round(environmental_footprint_score * 365.25, digits = 0), group = sex), 
                                      
                                      regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 3, name = "%eq | r2: %r", hideInLegend = FALSE), color = sex_colors)  %>% 
                                      hc_add_dependency("plugins/highcharts-regression.js") %>% 
                                      
                                      hc_title(text = paste0("Correlation between height and Environmental Footprint Score is positive but weak"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 

                                      hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
                                      hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 

                                      hc_xAxis(title = list(text = "Height", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                      hc_xAxis(labels = list(format = "{value} cm", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                      hc_xAxis(type = "logarithmic") %>% 
                                      hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                      hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                      hc_yAxis(type = "logarithmic") %>% 

                                      hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + 'Height: ' + this.point.x + ' (cm) <br>' + 'EFS: ' + this.point.y + ' (mPt)' )}")) %>% 

                                      hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                                      
                                      hc_colors(sex_colors) %>% 

                                      hc_add_theme(selected_theme) %>% 
                                      
                                      hc_exporting(enabled = TRUE, filename = "efs_height_correlation")
                                                                       


correl_height_efp_individual_sexes$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("correl_height_efp_individual_sexes.js")   



# Correlation weight and EFS by sex


correl_weight_efp_bothsexes_pooled <- hchart(ind_caracs_impacts %>% mutate(sex = ifelse(sex_ps == 1, "Males", "Females")), 
          "scatter", hcaes(x = round(as.numeric(poids), digits = 2), y = round(environmental_footprint_score * 365.25, digits = 0), group = sex)) %>% 

          hc_add_series(data.frame("weight" = c(40,161.1), "environmental_footprint_score" = c(1.07*40+170.49,1.07*161.1+170.49)), "line", hcaes(weight, environmental_footprint_score), name = "Trend line (both sexes pooled): 1.07 * Weight + 170.49 (r2 = 0.17)", color = "black", dashStyle = "ShortDash",  lineWidth = 3) %>% 

          hc_title(text = paste0("The association between weight and Environmental Footprint Score is weaker but still positive"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 

          hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
          hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>%
          
          hc_xAxis(title = list(text = "Weight", style = list(useHTML = TRUE, fontSize = axis_size)), min = 40, max = 162) %>% 
          hc_xAxis(labels = list(format = "{value} kg", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
          hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
          hc_yAxis(type = "logarithmic") %>% 

          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + 'Weight: ' + this.point.x + ' (kg) <br>' + 'EFS: ' + this.point.y + ' (mPt)' )}")) %>% 

          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 

          hc_colors(sex_colors) %>% 

          hc_add_theme(selected_theme) %>% 

          hc_exporting(enabled = TRUE, filename = "efs_weight_correlation_sex") 
          

correl_weight_efp_bothsexes_pooled$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("correl_weight_efp_bothsexes_pooled.js")          
          



correl_weight_efp_individual_sexes <- hchart(ind_caracs_impacts %>% mutate(sex = ifelse(sex_ps == 1, "Males", "Females")), 
                                      "scatter", hcaes(x = round(as.numeric(poids), digits = 2), y = round(environmental_footprint_score * 365.25, digits = 0), group = sex),
                                      
                                       regression = TRUE, regressionSettings = list(dashStyle = "ShortDash",  lineWidth = 3, name = "%eq | r2: %r", hideInLegend = FALSE), color = sex_colors)  %>% 
                                       hc_add_dependency("plugins/highcharts-regression.js") %>% 
                                                                              
                                       hc_title(text = paste0("Correlation between weight and Environmental Footprint Score is basically inexistent for men"), style = list(useHTML = TRUE, fontSize = title_size)) %>% 

                                       hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)")  %>% 
                                       hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 

                                       hc_xAxis(title = list(text = "Weight", style = list(useHTML = TRUE, fontSize = axis_size)), min = 40, max = 162) %>% 
                                       hc_xAxis(labels = list(format = "{value} kg", style = list(useHTML = TRUE, fontSize = axis_size))) %>%
                                       hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                       hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                                       hc_yAxis(type = "logarithmic") %>% 

                                       hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + 'Weight: ' + this.point.x + ' (kg) <br>' + 'EFS: ' + this.point.y + ' (mPt)' )}")) %>% 

                                       hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                                       
                                       hc_colors(sex_colors) %>% 

                                       hc_add_theme(selected_theme) %>% 
                                       
                                       hc_exporting(enabled = TRUE, filename = "efs_weight_correlation")
                                       


correl_weight_efp_individual_sexes$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("correl_weight_efp_individual_sexes.js")  



########## 5.8. Other caracteristics graphs ##########


# Occupation and impacts

occupation_impacts <- hchart(occupation_impacts_gender %>% arrange(-avg), 
                          "column", hcaes(x = pcs_8cl_interv, y = round(avg, digits = 0), group = sex_ps), color = sex_colors) %>% 
                          
                          hc_title(text = "Adults with physically-intensive occupations tend to pollute more via food consumption", style = list(useHTML = TRUE, fontSize = title_size)) %>% 

                          hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>% 
                          hc_caption(style = list(useHTML = TRUE, fontSize = caption_size), text = "Note: Analysis based on a representative sample of the French population aged 18-79") %>% 

                          hc_xAxis(labels = list(style = list(useHTML = TRUE, fontSize = axis_size)), title = list(text = "Occupation", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                          hc_yAxis(title = list(text = "Environmental Footprint Score", style = list(useHTML = TRUE, fontSize = axis_size))) %>% 
                          hc_yAxis(labels = list(format = "{value} mPt", style = list(useHTML = TRUE, fontSize = axis_size)))  %>% 

                          hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + ' <br>' + this.point.name + '<br>' + 'EFS: ' + this.point.y + ' (mPt)' )}")) %>%

                          hc_legend(align = "center", verticalAlign = "top",  itemStyle = list(useHTML = TRUE, fontSize = axis_size)) %>% 
                          
                          hc_add_theme(selected_theme) %>% 
                          
                          hc_exporting(enabled = TRUE, filename = "occupation_gender_efs")
                          


occupation_impacts$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T, force = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("occupation_impacts.js")  


########## 5.9. Methodological graphs ##########


# Methodological quality rating of food matching


methodological_quality <- hchart(data_adults %>% filter(demographic_group == "All adult males") %>% count(rating_matching) %>% arrange(-n), 
                              "pie", hcaes(x = rating_matching, y = round(100*n/2857, digits = 2)), 
                              name = "Share of products", 
                              innerSize="70%", size = "80%",) %>% 

                              hc_plotOptions(pie =list(dataLabels = list(enabled = TRUE, style = list(useHTML = TRUE, fontSize = axis_size)))) %>%

                              hc_title(text = "Quality of the matching between INCA3 and AGRIBALYSE products", style = list(useHTML = TRUE, fontSize = title_size))  %>% 

                              hc_credits(style = list(useHTML = TRUE, fontSize = caption_size), enabled = T, text = "Source: Dismal Science based on INCA3 study (2015) and Agribalyse (2022)") %>% 

                              hc_tooltip(style = list(useHTML = TRUE, fontSize = axis_size), formatter = JS("function(){ return (this.series.name + '<br> ' + this.point.name + ' matching: ' + this.point.y + '%' )}")) %>% 

                              hc_colors(methodo_colors) %>% 

                              hc_add_theme(selected_theme) %>% 

                              hc_exporting(enabled = TRUE, filename = "matching_quality")
                              
                              
methodological_quality$x$hc_opts %>% # export strategy based on https://github.com/jbkunst/highcharter/issues/371 
  toJSON(pretty = T, auto_unbox = T, force = T) %>% 
  str_replace_all('"(\\w+)":', "\\1:") %>% 
  paste0("Highcharts.chart('container', ", ., ");") %>% 
  writeLines("methodological_quality.js")  
                         
                              
                              









