#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: Conducting comparison analysis
# Author: Valeria Egorova
# Date: 03 Dec 2023
#-------

data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta"))

data_high <-
  data_for_glm %>%
  #filter(group == 1) %>%
  #mutate(sc_type = as.factor(sc_type)  %>%
  mutate(ac_ach = as.factor(ac_ach))  %>%
  select(sc_type, group, self_control, self_efficacy, educational,
         communicative, emotional, growth, creativity,
         position, achievment, external, self_respect, motivational, 
         cognitive, behavioural, research_potential, em_knowledge,
         em_control, self_mot, empaty, em_identification,
         O, C, E, A, N, gm_intelligence, gm_personality, 
         externalism,  commitment, consistency, grit, ac_ach) %>%
  drop_na()

#####---------ANOVA_Schools--------######
var2 <- c("self_control", "self_efficacy",  "creativity", "em_knowledge",
          "em_control", "self_mot", "empaty", "em_identification",
          "O", "C", "E", "A", "N", "gm_intelligence", "gm_personality", 
           "commitment", "consistency", "grit") 

sum_aov <- list()
Tukey <- list()

for (i in 1:length(var2)) {
  
  formula <- as.formula(paste0(var2[i], "  ~  as.factor(data_high$ac_ach)"))
  sum_aov[[i]] <- aov(formula, data = data_high)
  Tukey[[i]] <- TukeyHSD(sum_aov[[i]])
}  


data_mean_bg <- 
  data_high %>%
  group_by(ac_ach) %>%
  summarise_at(.vars = var2, .fun = ~ mean(., na.rm = T)) %>%
  write_xlsx(file.path(outData,"means.xlsx"))

lapply(data_for_glm[c(var2)], function(x) psych::describe(x))


anova1 <- aov(self_control ~ as.factor(ac_ach), data = data_high)
summary(anova1)
TukeyHSD(anova1)



