rm(list = ls()) # nettoyage de l'environnement de travail

# Chargement du jeu de données --------------------------------------------

source("utils_packages.R")


date_day = function(date){
  
  if (format(date, format = "%Y")=="2016"){
    startdate <- as.Date("01/12/2015","%d/%m/%Y")
    new_date = difftime(date,startdate ,units="days") %>% 
      as.integer()
    return(new_date)
  }
  
  if (format(date, format = "%Y")=="2017"){
    startdate <- as.Date("01/12/2016","%d/%m/%Y")
    new_date = difftime(date,startdate ,units="days")%>% 
      as.integer()
    return(new_date)
  }
  
  if (format(date, format = "%Y")=="2018"){
    startdate <- as.Date("01/12/2017","%d/%m/%Y")
    new_date = difftime(date,startdate ,units="days")%>% 
      as.integer()
    return(new_date)
  }
}

data_antler_telo = read_excel("Dataset_Telomere_Bois040522.xlsx", skip = 0, na = "NA") %>% 
  rename(RTL = QC_RTL,
         Population = population,
         Tbars = tbars,
         Cohort = Cohorte,
         AntlerType = State,
         Weight = Masse,
         AntlerLeft = Antler_left,
         AntlerRight = Antler_right) %>% 
  dplyr::select(Year, DateCapture, 
                Population, Id_JM,
                Cohort, 
                AntlerLeft, AntlerRight, AntlerType, Weight,
                Tbars, RTL) %>% 
  unite("Id", 3:4, sep="", remove=FALSE) %>% 
  mutate(Day = map_dbl(DateCapture, date_day)) %>% 
  dplyr::select(Year, Day, Id,  
                Cohort, Population, AntlerLeft, AntlerRight, AntlerType, Weight,
                Tbars, RTL) %>% 
  mutate(Id = as.factor(Id),
         Year = as.factor(Year),
         Cohort = as.factor(Cohort),
         Population = as.factor(Population),
         AntlerType = as.factor(AntlerType))



data_antler_epi = read_excel("Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
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
