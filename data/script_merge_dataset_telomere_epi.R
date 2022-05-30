rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")

data_antler_telo = read_excel("data/Dataset_Telomere_Bois040522.xlsx", skip = 0, na = "NA") %>% 
  rename(Day = DateCapture,
         RTL = QC_RTL,
         Population = population,
         Tbars = tbars,
         Cohort = Cohorte,
         AntlerType = State,
         Weight = Masse,
         AntlerLeft = Antler_left,
         AntlerRight = Antler_right) %>% 
  dplyr::select(Year, Day, 
                Population, Id_JM,
                Cohort, 
                AntlerLeft, AntlerRight, AntlerType, Weight,
                Tbars, RTL) %>% 
  unite("Id", 3:4, sep="", remove=FALSE) %>% 
  dplyr::select(Year, Day, Id,  
                Cohort, Population, AntlerLeft, AntlerRight, AntlerType, Weight,
                Tbars, RTL) %>% 
  mutate(Id = as.factor(Id),
         Year = as.factor(Year),
         Cohort = as.factor(Cohort),
         Population = as.factor(Population),
         AntlerType = as.factor(AntlerType))


#convertir la date
#recyclage lors du merge



data_antler_epi = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  mutate(RightAntlerLength    = str_remove(RightAntlerLength, "_broken") %>% 
           as.numeric()) %>% 
  rename(Year = YearCapture,
         Day = JulianCaptureDate,
         DNAmAge = DNAmAgeLOO,
         Weight = WeightAnimal.kg,
         ProblemDNA  = ProblemDNA_Concentration,
         AgeAccelLOO = `AgeAccelLOO(ComputedUCLA)`, 
         AntlerLeft = Left_AntlerLength,
         AntlerRight = RightAntlerLength)%>% 
  dplyr::select(Year, Day, 
                Population, Id,
                Cohort, Population,
                AntlerLeft, AntlerRight, AntlerType, Weight,
                DNAmAge, AgeAccelLOO)%>% 
  unite("Id", 3:4, sep="", remove=FALSE) %>% 
  dplyr::select(Year, Day, Id,  
                Cohort, Population, AntlerLeft, AntlerRight, AntlerType, Weight,
                DNAmAge, AgeAccelLOO)




data_antler <- merge(data_antler_epi, data_antler_telo, 
                     by=c("Id", "Year", "Population", "AgeClass", "RTL", "Weight", "AntlerLength", "Cohort", "AntlerType" ), all=TRUE )
