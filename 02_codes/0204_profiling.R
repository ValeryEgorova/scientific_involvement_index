#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: Profiling 
# Author: Valeria Egorova
# Date: 19 Nov 2023
#-------

profile_sex <- 
  data_for_glm%>%
  group_by(sex)%>%
  summarise(index = mean(group, na.rm = T))%>%
  as.data.frame()%>%
  write_xlsx(file.path(outData,"profile_sex.xlsx"))

profile_sctype <- 
  data_for_glm%>%
  group_by(sc_type)%>%
  summarise(index = mean(group, na.rm = T))%>%
  as.data.frame()%>%
  write_xlsx(file.path(outData,"profile_sctype.xlsx"))

profile_grade <- 
  data_for_glm%>%
  group_by(grade)%>%
  summarise(index = mean(group, na.rm = T))%>%
  as.data.frame()%>%
  write_xlsx(file.path(outData,"profile_grade.xlsx"))
