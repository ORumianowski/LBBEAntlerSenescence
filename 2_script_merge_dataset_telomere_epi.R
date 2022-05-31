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

data_antler_telo = read_excel("data/Dataset_Telomere_Bois040522.xlsx", skip = 0, na = "NA") %>% 
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
                AntlerLeft, AntlerRight, AntlerType, Weight, Age,
                Tbars, RTL) %>% 
  unite("Id", 3:4, sep="", remove=FALSE) %>% 
  mutate(Day = map_dbl(DateCapture, date_day)) %>% 
  dplyr::select(Year, Day, Id,  
                Cohort, Population, AntlerLeft, AntlerRight, AntlerType, Weight, Age,
                Tbars, RTL)



data_antler_epi = read_excel("data/Dataset_ODIN_160422.xlsx", skip = 0, na = "NA") %>% 
  mutate(RightAntlerLength    = str_remove(RightAntlerLength, "_broken") %>% 
           as.numeric(),
         Age = round(Age)) %>% 
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
                AntlerLeft, AntlerRight, AntlerType, Weight, Age,
                DNAmAge, AgeAccelLOO)%>% 
  unite("Id", 3:4, sep="", remove=FALSE) %>% 
  dplyr::select(Year, Day, Id,  
                Cohort, Population, AntlerLeft, AntlerRight, Weight, Age,
                DNAmAge, AgeAccelLOO)





dantler <- merge(data_antler_epi, data_antler_telo, 
                     by=c("Year", "Day", "Id", 
                          "Cohort", "Population", "AntlerLeft", "AntlerRight", "Weight", "Age"), all=TRUE ) %>% 
  mutate(AntlerLength = map2_dbl(.x = AntlerLeft, 
                                 .y = AntlerRight,
                                         .f = function(x, y){
                                            if(is.na(x))
                                              return(y)
                                            else if(is.na(y))
                                              return(x)
                                            else
                                              return((x+y)/2)
                                          })) %>% 
  dplyr::select(Year, Day, Id,  
                Cohort, Population, AntlerLength, AntlerType, Weight, Age,
                Tbars, RTL,
                DNAmAge, AgeAccelLOO)  %>% 
  mutate(Year = as.factor(Year),
         Id = as.factor(Id),
         Cohort = as.factor(Cohort),
         Population = as.factor(Population),
         AntlerLength = as.numeric(AntlerLength), 
         AntlerType = as.factor(AntlerType),
         Weight = as.numeric(Weight), 
         Age = as.numeric(Age),
         Tbars = as.numeric(Tbars), 
         RTL = as.numeric(RTL),
         DNAmAge = as.numeric(DNAmAge),
         AgeAccelLOO = as.numeric(AgeAccelLOO)) %>% 
  mutate(Age_2 = Age^2,
         AgeClass = cut(Age, breaks = c(0,1,4,8,25)) %>% 
           as.character() %>% 
           as.factor(),
         Weight_log = log(Weight),
         AntlerLength_log = log(AntlerLength))


# Problem with the sample
dantler = dantler[-which(dantler$Id=="CM2301" & dantler$Year=="2016") , ]
# Problem with the sample (Female)
dantler = dantler[-which(dantler$Id=="TFM368" & dantler$Year=="2018") , ]


rm(data_antler_telo)
rm(data_antler_epi)
rm(date_day)

