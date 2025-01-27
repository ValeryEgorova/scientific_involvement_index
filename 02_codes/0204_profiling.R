#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: Profiling 
# Author: Valeria Egorova
# Date: 19 Nov 2023
#-------

profile_sex_res <- 
  data_for_glm %>%
  filter(junior_res == 1) %>%
  group_by(sex) %>%
  summarise(index = round(mean(index, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_sex_res.xlsx"))

profile_sex_gen <- 
  data_for_glm %>%
  filter(junior_res == 0) %>%
  group_by(sex) %>%
  summarise(index = round(mean(index, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_sex_gen.xlsx"))

profile_sctype_res <- 
  data_for_glm %>%
  filter(junior_res == 1) %>%
  group_by(sc_type) %>%
  summarise(index = round(mean(index, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_sctype_res.xlsx"))

profile_sctype_gen <- 
  data_for_glm %>%
  filter(junior_res == 0) %>%
  group_by(sc_type) %>%
  summarise(index = round(mean(index, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_sctype_gen.xlsx"))

profile_grade_res <- 
  data_for_glm %>%
  filter(junior_res == 1) %>%
  group_by(grade) %>%
  summarise(index = round(mean(index, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_grade_res.xlsx"))

profile_grade_gen <- 
  data_for_glm %>%
  filter(junior_res == 0) %>%
  group_by(grade) %>%
  summarise(index = round(mean(index, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_grade_gen.xlsx"))

profile_psy <- 
  data_for_glm %>%
  group_by(junior_res) %>%
  summarise(self_control = round(mean(self_control, na.rm = T), 2),
            self_efficacy = round(mean(self_efficacy, na.rm = T), 2),
            educational = round(mean(educational, na.rm = T), 2),
            communicative = round(mean(communicative, na.rm = T), 2),
            emotional = round(mean(emotional, na.rm = T), 2),
            growth = round(mean(growth, na.rm = T), 2),
            position = round(mean(position, na.rm = T), 2),
            achievment = round(mean(achievment, na.rm = T), 2),
            external = round(mean(external, na.rm = T), 2),
            self_respect = round(mean(self_respect, na.rm = T), 2),
            motivational = round(mean(motivational, na.rm = T), 2),
            cognitive = round(mean(cognitive, na.rm = T), 2),
            behavioural = round(mean(behavioural, na.rm = T), 2),
            research_potential = round(mean(research_potential, na.rm = T), 2),
            em_knowledge = round(mean(em_knowledge, na.rm = T), 2),
            em_control = round(mean(em_control, na.rm = T), 2),
            self_mot = round(mean(self_mot, na.rm = T), 2),
            empaty = round(mean(empaty, na.rm = T), 2),
            em_identification = round(mean(em_identification, na.rm = T), 2),
            O = round(mean(O, na.rm = T), 2),
            C = round(mean(C, na.rm = T), 2),
            E = round(mean(E, na.rm = T), 2),
            A = round(mean(A, na.rm = T), 2),
            N = round(mean(N, na.rm = T), 2),
            gm_intelligence = round(mean(gm_intelligence, na.rm = T), 2),
            gm_personality = round(mean(gm_personality, na.rm = T), 2),
            externalism = round(mean(externalism, na.rm = T), 2),
            commitment = round(mean(commitment, na.rm = T), 2),
            consistency = round(mean(consistency, na.rm = T), 2),
            grit = round(mean(grit, na.rm = T), 2),
            creativity = round(mean(creativity, na.rm = T), 2)) %>%
  as.data.frame() %>%
  write_xlsx(file.path(outData,"profile_psy.xlsx"))


data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta"))

lapply(data_for_glm[var], function(x) t.test(x ~ data_for_glm$junior_res, data = data_for_glm))

  