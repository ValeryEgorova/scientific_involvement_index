data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta"))

mod <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + AP02 +  sex 
           + as.factor(grade) + as.factor(sc_type), data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod)

margin <- margins(mod)
margin

mod2 <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + sex  + AP02 +
           + as.factor(grade) + as.factor(sc_type) + research_potential + grit + gm_intelligence, 
           data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod2)

margin <- margins(mod2)
margin

vif(mod2)

mod2_1 <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + sex  + AP02 +
              + as.factor(grade) + as.factor(sc_type) 
              + as.factor(sc_type)*research_potential + as.factor(sc_type)*grit 
              + as.factor(sc_type)*gm_intelligence, 
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod2_1)

margin <- margins(mod2_1)
margin

mod3 <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + sex  + AP02 +
              + as.factor(grade) + as.factor(sc_type) + research_potential  + gm_intelligence + O + C + 
              E + A + N, 
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod3)

margin <- margins(mod3)
margin
vif(mod3)

mod3_1 <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + sex  + AP02 +
                + as.factor(grade) + as.factor(sc_type)  + as.factor(sc_type)*research_potential 
              + as.factor(sc_type)*gm_intelligence + as.factor(sc_type)*O + as.factor(sc_type)*C + 
                as.factor(sc_type)*E + as.factor(sc_type)*A + as.factor(sc_type)*N, 
              data = data_for_glm, 
              family = binomial(link = "logit"), x = TRUE)
summary(mod3_1)

margin <- margins(mod3_1)
margin
