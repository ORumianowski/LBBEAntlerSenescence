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
# à procéder en deux étapes avec et sans AgeAccelResiduals en variables explicatives.

# Afin de tenir en compte de la relation allométrique entre
#la taille des bois et la masse, la sélection de modèle sera en deux étapes:
#(i) la taille des bois et la masse sont des variables explicatives exclusives
#(ii) la taille des bois et la masse sont log-transformées et 
# sont obligatoirement intégrés aux différents modèles testés.

# Dans les analyses avec la log-transformation, les bois de taille nulle sont retirés



## Sélection de la population

dantler_1A = dantler %>% 
  subset(Age==1) %>% 
  dplyr::select(Id, Year, Day, Age,
                               Cohort, Population, 
                               Weight_std, Antler_std, 
                               AgeAccelResiduals,
                               RTL) 
vis_miss(dantler_1A)

dantler_1A = dantler_1A[complete.cases(dantler_1A$RTL), ]

vis_miss(dantler_1A)


# Sans la variable AgeAccelResiduals ----------------------------------------------------

## Bois et masse exclusifs -------------------------------------------------

dantler_1A_without_AgeAccelResiduals = dantler_1A %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_std, Antler_std,
                RTL) %>% 
  na.omit()


reg_lm_1A <- lm(RTL ~ Cohort + Population + 
                  Weight_std + Antler_std+
                  Antler_std:Cohort + Antler_std:Population + 
                  Weight_std:Cohort  + Weight_std:Population +
                  Cohort:Population
                ,  
                data=dantler_1A_without_AgeAccelResiduals)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

reg <- lm(RTL ~ Cohort + Population + 
                  Antler_std +
                  Antler_std:Cohort + Antler_std:Population + 
                  Cohort:Population
                ,  
                data=dantler_1A_without_AgeAccelResiduals)
reg %>% 
  summary()

reg %>% 
  car::Anova()

ggplot(dantler_1A_without_AgeAccelResiduals,
       aes(x = Antler_std,
           y = RTL,
           color = Cohort)) +
  geom_point()

ggplot(dantler_1A_without_AgeAccelResiduals,
       aes(x = Cohort,
           y = RTL,
           color=Population)) +
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.05))

## Bois et masse obligatoires -------------------------------------------------

# un ind. retirés: AntlerLength==0
dantler_1A[which(dantler_1A$Antler_std==0),]

dantler_1A_without_AgeAccelResiduals = dantler_1A %>% 
  subset(Antler_std!=0) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_std_log = log(Weight_std)) %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_std_log, Antler_std_log,
                RTL) %>% 
  na.omit()


reg_lm_1A <- lm(RTL ~ Weight_std_log + Antler_std_log + Cohort + Population + 
                Antler_std_log:Weight_std_log + Antler_std_log:Cohort + Antler_std_log:Population + 
                Weight_std_log:Cohort  + Weight_std_log:Population +
                Cohort:Population
                ,  
                data=dantler_1A_without_AgeAccelResiduals)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

# Avec la variable AgeAccelResiduals ----------------------------------------------------

## Bois et masse exclusifs -------------------------------------------------

dantler_1A_with_AgeAccelResiduals = dantler_1A %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_std, Antler_std, AgeAccelResiduals,
                RTL) %>% 
  na.omit()


reg_lm_1A <- lm(RTL ~ Cohort + Population + 
                  Weight_std + Antler_std + AgeAccelResiduals +
                  Antler_std:Cohort + Antler_std:Population + 
                  Weight_std:Cohort  + Weight_std:Population +
                  Cohort:Population
                ,  
                data=dantler_1A_with_AgeAccelResiduals)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

## Bois et masse obligatoires -------------------------------------------------

# deux ind. retirés: AntlerLength==0
dantler_1A[which(dantler_1A$Antler_std==0),]

dantler_1A_with_AgeAccelResiduals = dantler_1A %>% 
  subset(Antler_std!=0) %>% 
  mutate(Antler_std_log = log(Antler_std),
         Weight_std_log = log(Weight_std)) %>% 
  dplyr::select(Id, Year, Day, Age,
                Cohort, Population, 
                Weight_std_log, Antler_std_log, AgeAccelResiduals,
                RTL) %>% 
  na.omit()


reg_lm_1A <- lm(RTL ~ Weight_std_log + Antler_std_log + Cohort + Population + AgeAccelResiduals +
                  Antler_std_log:Weight_std_log + Antler_std_log:Cohort + Antler_std_log:Population + 
                  Weight_std_log:Cohort  + Weight_std_log:Population +
                  Cohort:Population
                ,  
                data=dantler_1A_with_AgeAccelResiduals)


options(na.action = "na.fail")

fm1A <- reg_lm_1A
ms1A <- dredge(fm1A)

head(ms1A)

