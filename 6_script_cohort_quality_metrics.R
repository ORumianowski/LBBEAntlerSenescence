
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

