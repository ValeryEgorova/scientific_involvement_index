data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta")) %>%
  filter(grade > 1)

mod <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + AP02 +  sex 
           + as.factor(grade) + as.factor(sc_type), data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod)

margin <- margins(mod)
margin

vif(mod)

dat <- 
  data_for_glm %>%
  select(group, PI , sci_env, ac_ach, AP02, sex, grade, sc_type) %>%
  drop_na()

predicts <- as.numeric(mod$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$group))


lm1 <- read_excel("03_outputs/0302_tables/lm1.xlsx") %>%
  mutate(value = value * 100)

#ggplot(lm1, aes(x = name, y = value)) +
  #geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  #geom_point( color = "slateblue2", size = 4, alpha = 1) +
  #geom_label(aes(name, value + 0,5, label = signif(value,2)), colour = "black", nudge_x = 0.35, size = 4) +
  #coord_flip() + 
  #theme_bw() +
  #labs(y = "Предельные эффекты", x = "")

ggplot(lm1, aes(x = name, y = value)) +
  geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  geom_point( color = "slateblue2", size = 4, alpha = 1) +
  geom_label(aes(name, value + 0.5*value, label = paste0(signif(value,2), "%")), colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() +
  theme_bw() +
  labs(y = "Предельные эффекты", x = "")

mod2 <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + sex  + AP02 +
           + as.factor(grade) + as.factor(sc_type) + research_potential + grit + gm_intelligence, 
           data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod2)

margin <- margins(mod2)
margin

vif(mod2)

dat <- 
  data_for_glm %>%
  select(group, PI , sci_env, ac_ach, AP02, sex, grade, sc_type, research_potential ,
         grit , gm_intelligence) %>%
  drop_na()

predicts <- as.numeric(mod2$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$group))

lm3 <- read_excel("03_outputs/0302_tables/lm3.xlsx") %>%
  mutate(value = value * 100)

ggplot(lm3, aes(x = name, y = value)) +
  geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  geom_point( color = "slateblue2", size = 4, alpha = 1) +
  geom_label(aes(name, value + 0.5*value, label = paste0(signif(value,2), "%")), colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() +
  theme_bw() +
  labs(y = "Предельные эффекты", x = "")

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

dat <- 
  data_for_glm %>%
  select(group, PI , sci_env, ac_ach, AP02, sex, grade, sc_type, research_potential ,
          gm_intelligence, O, C, E, A, N) %>%
  drop_na()

predicts <- as.numeric(mod3$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$group))

lm2 <- read_excel("03_outputs/0302_tables/lm2.xlsx") %>%
  mutate(value = value * 100)

ggplot(lm2, aes(x = name, y = value)) +
  geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  geom_point( color = "slateblue2", size = 4, alpha = 1) +
  geom_label(aes(name, value + 0.5*value, label = paste0(signif(value,2), "%")), colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() +
  theme_bw() +
  labs(y = "Предельные эффекты", x = "")



mod6 <- glm(group ~ research_potential  + gm_intelligence + O + C + 
              E + A + N, 
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod6)

margin <- margins(mod6)
margin
vif(mod6)





mod3_1 <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + sex  + AP02 +
                + as.factor(grade) + as.factor(sc_type)  + as.factor(sc_type)*research_potential 
              + as.factor(sc_type)*gm_intelligence + as.factor(sc_type)*O + as.factor(sc_type)*C + 
                as.factor(sc_type)*E + as.factor(sc_type)*A + as.factor(sc_type)*N, 
              data = data_for_glm, 
              family = binomial(link = "logit"), x = TRUE)
summary(mod3_1)

margin <- margins(mod3_1)
margin
