rm(list = ls()) # nettoyage de l'environnement de travail

setwd("C:/Users/odinr/Desktop/Stage LBBE MMIP/LBBEAntlerSenescence")

source("1_utils_packages.R")
source("2_script_merge_dataset_telomere_epi.R")
source("3_script_standardisation_antler.R")
source("5_script_epi_metrics.R")

ggplot(dantler,
       aes(x = AgeAccelResiduals,
           y = RTL,
           color=Population)) +
  geom_point()

dantler_lm = subset(dantler, Age>1) %>% 
  dplyr::select(RTL, AgeAccelResiduals, 
                           Cohort, Population, Weight , Antler_std) %>% 
  na.omit()



reg_lm_full = lm(AgeAccelResiduals ~ RTL  + 
                   Cohort + Population + Antler_std + Weight, 
                 data = dantler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)


reg_lm_full = lm(RTL ~ AgeAccelResiduals + 
                   Cohort + Population + Antler_std + Weight, 
                 data = dantler_lm) 

options(na.action = "na.fail")

fm_full <- reg_lm_full
ms_full <- dredge(fm_full)

head(ms_full)

ggplot(dantler,
       aes(x = Weight,
           y = RTL,
           color=Population)) +
  geom_point()
