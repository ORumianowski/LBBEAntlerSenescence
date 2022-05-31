rm(list = ls()) # nettoyage de l'environnement de travail

setwd("C:/Users/odinr/Desktop/Stage LBBE MMIP/LBBEAntlerSenescence")

source("1_utils_packages.R")
source("2_script_merge_dataset_telomere_epi.R")
source("3_script_standardisation_antler.R")

source("5_script_epi_metrics.R")

vis_miss(dantler)


# avec log ----------------------------------------------------------------

dantler_1A = dantler %>% 
  subset(Age==1) %>% 
  dplyr::select(Id, Year, Day, 
                               Cohort, Population, 
                               Weight_log, AntlerLength, 
                               RTL,
                               EpiSimpleDiff)

# deux ind. retirés: AntlerLength==0
dantler_1A[which(dantler_1A$AntlerLength==0),]

dantler_1A = dantler %>% 
  subset(Antler_std>=0.1) 

vis_miss(dantler_1A)

# afin de conserver un maximum de données, les analyses se feront avec et sans RTL
# avec RTL

dantler_1A_with_RTL = dantler %>% 
  dplyr::select(Id, Year, Day, 
                Cohort, Population, 
                Weight_log, AntlerLength_log, AntlerLength, 
                RTL,
                EpiSimpleDiff) %>% 
  na.omit()


reg_lm_1A <- lm(EpiSimpleDiff ~ Antler_std_log + Population + Weight_log +
                  Antler_std_log:Population + Antler_std_log:Weight_log,  
                data=data_antler_1A)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

par(mar = c(3,5,6,4))
plot(ms1A, labAsExpr = TRUE)

