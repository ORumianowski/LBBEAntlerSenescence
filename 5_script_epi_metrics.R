
#Script utilisé pour obtnir les indices de senescence épigénétique

# Trois indices sont obtenus:
# (i) la différence simple i.e. (l'âge épi - l'âge chrnologique): "EpiSimpleDiff"
# (ii) l'indice de Horvath: "AgeAccelLOO"
# (iii) l'indice via les residus de la regression 
# de la fonction sélectionnée par cross-validation: "AgeAccelResiduals"

# Via les résidus (fct log) -----------------------------------------------

dantler_accel = dplyr::select(dantler, DNAmAge, Age, Id, Year) %>% 
  na.omit

regaccel_Age_log = lm(DNAmAge ~ Age + log(Age), dantler_accel)

dantler_accel = dantler_accel %>% 
  mutate(AgeAccelResiduals = regaccel_Age_log$residuals) %>% 
  dplyr::select(Id, Year, AgeAccelResiduals)
  

dantler =  merge(dantler, dantler_accel, by=c("Id", "Year"), all.x = TRUE)

# Différence simple -------------------------------------------------------

dantler = dantler %>% 
  mutate(EpiSimpleDiff = DNAmAge - Age)


# Equivalence des trois métriques rapportée à l'âge -----------------------

dantler = dantler %>% 
  mutate(EpiSimpleDiffRap = EpiSimpleDiff / Age,
         AgeAccelResidualsRap = AgeAccelResiduals / Age,
         AgeAccelLOORap = AgeAccelLOO / Age)

rm(regaccel_Age_log)
rm(dantler_accel)

