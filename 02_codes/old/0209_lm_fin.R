# Social factors

data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta")) %>%
  filter(grade > 1)

mod <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + AP02 +  sex 
           + as.factor(grade) + as.factor(sc_type), data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod)

margin <- margins(mod)
margin

vif(mod)

PseudoR2(mod, which = "all")

dat <- 
  data_for_glm %>%
  select(group, PI , sci_env, ac_ach, AP02, sex, grade, sc_type) %>%
  drop_na()

predicts <- as.numeric(mod$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$group))


lm1 <- read_excel("03_outputs/0302_tables/lm1.xlsx") %>%
  mutate(value = value * 100)

ggplot(lm1, aes(x = name, y = value)) +
  geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  geom_point( color = "slateblue2", size = 4, alpha = 1) +
  geom_label(aes(name, value + 0.5*value, label = paste0(signif(value,2), "%")), colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() +
  theme_bw() +
  labs(y = "Предельные эффекты", x = "")

# Social & psychological

mod2 <- glm(group ~ PI + sci_env + as.factor(ac_ach)  + AP02 +  sex 
           + as.factor(grade) + as.factor(sc_type) + 
              growth +  #research_potential + 
             + cognitive + self_mot +  grit + gm_intelligence, 
           data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod2)

margin <- margins(mod2)
margin

vif(mod2)

PseudoR2(mod2, which = "all")

dat2 <- 
  data_for_glm %>%
  select(group, PI , sci_env, ac_ach, AP02, sex, grade, sc_type, growth ,
         research_potential , self_mot ,  grit , gm_intelligence,) %>%
  drop_na()

predicts2 <- as.numeric(mod2$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts2), as.factor(dat2$group))


lm2 <- read_excel("03_outputs/0302_tables/lm4.xlsx") %>%
  mutate(value = value * 100)

ggplot(lm1, aes(x = name, y = value)) +
  geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  geom_point( color = "slateblue2", size = 4, alpha = 1) +
  geom_label(aes(name, value + 0.5*value, label = paste0(signif(value,2), "%")), colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() +
  theme_bw() +
  labs(y = "Предельные эффекты", x = "")