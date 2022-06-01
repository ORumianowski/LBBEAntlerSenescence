rm(list = ls()) # nettoyage de l'environnement de travail

setwd("C:/Users/odinr/Desktop/Stage LBBE MMIP/LBBEAntlerSenescence")

source("1_utils_packages.R")
source("2_script_merge_dataset_telomere_epi.R")
source("3_script_standardisation_antler.R")
source("5_script_epi_metrics.R")




# Détermination de la variation télomérique --------------------------------------------


data_difference_telomere = dplyr::select(dantler, Id, Year, RTL) %>% 
  na.omit() %>% 
  group_by(Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    
    else
      tibble( Difference_telo = arrange(tableau, Year) %>% 
                  pull(RTL) %>%  
                  diff(),
             Id = groupe$Id) %>% 
      return() 
  }) %>% 
  bind_rows() 

data_difference_antler = dplyr::select(dantler, Id, Year, AntlerLength) %>% 
  na.omit() %>% 
  group_by(Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    
    else
      tibble(  Difference_antler = arrange(tableau, Year) %>% 
                 pull(AntlerLength) %>%  
                 diff(),
               Id = groupe$Id) %>% 
      return() 
  }) %>% 
  bind_rows() 

data_difference_weight = dplyr::select(dantler, Id, Year, Weight) %>% 
  na.omit() %>% 
  group_by(Id) %>% 
  group_map(.f = function(tableau, groupe){
    if(nrow(tableau) == 1){
      return(NULL)
    }
    
    else
      tibble( Difference_weight = arrange(tableau, Year) %>% 
                 pull(Weight) %>%  
                 diff(),
               Id = groupe$Id) %>% 
      return() 
  }) %>% 
  bind_rows() 



dantler_diff = left_join(data_difference_weight, dplyr::select(filter(dantler, Year == 2016), 
                          Antler_std, Id, Tbars, AgeAccelResiduals,
                          Weight, AgeClass, Age,  Population, Age, Cohort)) %>% 
  left_join(data_difference_telomere) %>% 
  left_join(data_difference_antler)



ggplot(dantler_diff,
       aes(x = Tbars,
           y = Difference_telo,
           color = AgeClass,
           shape=Population)) +
  geom_point()


ggplot(dantler_diff,
       aes(x = Weight,
           y = Difference_telo,
           shape=Population,
           color = AgeClass)) +
  geom_point()


ggplot(dantler_diff,
       aes(x = Antler_std,
           y = Difference_telo,
           shape=Population,
           color = AgeClass)) +
  geom_point()

ggplot(dantler_diff,
       aes(x = Difference_antler,
           y = Difference_telo,
           shape=Population,
           color = AgeClass)) +
  geom_point()


ggplot(dantler_diff,
       aes(x = Difference_weight,
           y = Difference_telo,
           shape=Population,
           color = AgeClass)) +
  geom_point()


ggplot(dantler_diff,
       aes(x = Difference_weight,
           y = Difference_antler,
           shape=Population,
           color = AgeClass)) +
  geom_point()



dantler_diff_lm = dplyr::select(dantler_diff, Difference_telo, Difference_weight,Difference_antler,
                                  Weight , Antler_std , Id,
                                  AgeClass , Age ,  Population , Age , Cohort)%>% 
  na.omit()

vis_miss(dantler_diff_lm)

reg_lm <- lm(Difference_telo ~ Difference_weight + Difference_antler +
             Weight + Antler_std + 
             AgeClass + Age +  Population + Age + Cohort
             ,  
             data=dantler_diff_lm)


options(na.action = "na.fail")

fm <- reg_lm
ms <- dredge(fm)

head(ms)

