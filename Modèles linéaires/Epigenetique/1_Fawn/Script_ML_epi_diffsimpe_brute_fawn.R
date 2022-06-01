rm(list = ls()) # nettoyage de l'environnement de travail

setwd("C:/Users/odinr/Desktop/Stage LBBE MMIP/LBBEAntlerSenescence")

source("1_utils_packages.R")
source("2_script_merge_dataset_telomere_epi.R")
source("3_script_standardisation_antler.R")
source("4_script_standardisation_weight.R")
source("5_script_epi_metrics.R")

###
# Ce script a  pour objectif d'appliquer des modèles linéaires avec:
# Population: les individus avec un âge de première année

# L'analyse de la localisation des données manquantes nous incite
# à procéder en deux étapes avec et sans RTL en variables explicatives.

# Afin de tenir en compte de la relation allométrique entre
#la taille des bois et la masse, la sélection de modèle sera en deux étapes:
#(i) la taille des bois et la masse sont des variables explicatives exclusives
#(ii) la taille des bois et la masse sont log-transformées et 
# sont obligatoirement intégrés aux différents modèles testés.

# Dans les analyses avec la log-transformation, les bois de taille nulle sont retirés



## Sélection de la population

dantler_1A = dantler %>% 
  subset(Age==1) %>% 
  mutate(Weight_std_log=log(Weight_std)) %>% 
  dplyr::select(Id, Year, Day, Age,
                               Cohort, Population, 
                               Weight_std, Antler_std, 
                               RTL,
                               EpiSimpleDiff) 
vis_miss(dantler_1A)

dantler_1A = dantler_1A[complete.cases(dantler_1A$EpiSimpleDiff), ]

vis_miss(dantler_1A)


# Sans la variable RTL ----------------------------------------------------

## Bois et masse exclusifs -------------------------------------------------

dantler_1A_without_RTL = dantler_1A %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_std, Antler_std,
                EpiSimpleDiff) %>% 
  na.omit()


reg_lm_1A <- lm(EpiSimpleDiff ~ Cohort + Population + 
                  Weight_std + Antler_std+
                  Antler_std:Cohort + Antler_std:Population + 
                  Weight_std:Cohort  + Weight_std:Population +
                  Cohort:Population
                ,  
                data=dantler_1A_without_RTL)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

reg <- lm(EpiSimpleDiff ~ Antler_std + Cohort + Population + 
            Antler_std:Cohort + Antler_std:Population + Cohort:Population,  
          data=dantler_1A_without_RTL)
reg %>% 
  summary()

reg %>% 
  car::Anova()

ggplot(dantler_1A_without_RTL,
       aes(x = Antler_std,
           y = EpiSimpleDiff,
           color = Cohort,
           shape = Population)) +
  geom_point() 

ggplot(dantler_1A_without_RTL,
       aes(x = Cohort,
           y = EpiSimpleDiff,
           color=Population)) +
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.05))


# deux ind. retirés: AntlerLength==0
dantler_1A[which(dantler_1A$Antler_std==0),]

dantler_1A = dantler_1A %>% 
  subset(Antler_std>=0.1) %>% 
  mutate(Antler_std_log = log(Antler_std))

vis_miss(dantler_1A)


# sans RTL

dantler_1A_without_RTL = dantler_1A %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_std_log, Antler_std_log, Antler_std,
                EpiSimpleDiff) %>% 
  na.omit()


reg_lm_1A <- lm(EpiSimpleDiff ~ Cohort + Population + 
                Weight_std_log + Antler_std_log+
                Antler_std_log:Weight_std_log + Antler_std_log:Cohort + Antler_std_log:Population + 
                Weight_std_log:Cohort  + Weight_std_log:Population +
                Cohort:Population
                ,  
                data=dantler_1A_without_RTL)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)


reg <- lm(EpiSimpleDiff ~ Antler_std + Cohort + Population + 
            Antler_std:Cohort + Cohort:Population,  
                data=dantler_1A_without_RTL)
reg %>% 
  summary()

reg %>% 
  car::Anova()
plot(reg)

ggplot(dantler_1A_without_RTL,
       aes(x = Antler_std,
           y = EpiSimpleDiff,
           color = Cohort,
           shape = Population)) +
  geom_point() 


ggplot(dantler_1A_without_RTL,
       aes(x = Antler_std,
           y = EpiSimpleDiff,
           color =Population ,
           shape =Cohort )) +
  geom_point() 


# avec RTL

dantler_1A_with_RTL = dantler_1A %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_std_log, Antler_std_log, Antler_std,
                RTL,
                EpiSimpleDiff) %>% 
  na.omit()


reg_lm_1A <- lm(EpiSimpleDiff ~ Cohort + Population + 
                  Weight_std_log + Antler_std_log + 
                  RTL,  
                data=dantler_1A_with_RTL)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)



