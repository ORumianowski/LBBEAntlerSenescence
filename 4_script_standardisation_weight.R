#Script utilisé pour standardiser les masses des 1A

#Les fonctions utilisées proviennent de Douhard et al., 2017.


# "Throughout the capture period, fawns gained
# an average of 12 g day–10.005 SE and 24 g day–10.008 SE at CH and TF, respectively, 
# without detectablesex-differences."


dantler_1A = subset(dantler, Age==1) %>% 
  dplyr::select(Day,
         Weight,
         Population,
         Id,Year) 


# Unmeasured Antler Length: 0 individual
dantler_1A[which(is.na(dantler_1A$Weight)) , ]

## Fonction de standardisation de la masse des 1A (population-dependante)

std_weight = function(Weight, Day, Day_ref=60, Population, gain_C= 0.012, gain_TF=0.024){
  if (Population=="C"){
    return (Weight + (Day_ref-Day)*gain_C)
  }
  if (Population=="TF"){
    return (Weight + (Day_ref-Day)*gain_TF)
  }
}



##Standardisation
dantler_1A_std =   dplyr::select(dantler_1A, Day,
                                 Weight,
                                 Population) 

dantler_1A = mutate(dantler_1A,
                   Weight_std = purrr::pmap_dbl(dantler_1A_std, std_weight)) %>% 
  dplyr::select(Id, Year, Weight_std)



dantler <-  merge(dantler, dantler_1A, by=c("Id", "Year"), all.x = TRUE)

rm(dantler_1A_std)
rm(dantler_1A)
rm(std_weight)

