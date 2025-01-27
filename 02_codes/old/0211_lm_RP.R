data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta")) %>%
  filter(grade > 1) %>%
  mutate(grade7 = ifelse(grade == 2,1,0),
         grade8 = ifelse(grade == 3,1,0),
         grade9 = ifelse(grade == 4,1,0),
         grade10 = ifelse(grade == 5,1,0),
         grade11 = ifelse(grade == 6,1,0))

# Social factors RP

mod <- glm(group_res ~ PI + sci_env  +  sex 
           +  grade8 + grade9 + grade10 + grade11 + as.factor(sc_type), data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod)

margin <- margins(mod)
margin

vif(mod)

PseudoR2(mod, which = "all")

dat <- 
  data_for_glm %>%
  select(group_res, PI , sci_env, sex,  grade8, grade9,  grade10,  grade11, sc_type) %>%
  drop_na()

predicts <- as.numeric(mod$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$group_res))

to_check <- 
  data_for_glm %>%
  select(group_res, PI , sci_env, sex, grade8, grade9,  grade10,  grade11)

corr_matrix1 <- cor(to_check)
p.mat1 = cor_pmat(to_check)
ggcorrplot(corr_matrix1,  type = "lower",
           lab = TRUE, p.mat = p.mat1, tl.cex = 10, lab_size = 2.5)

# Social & psychological factors RP

mod2 <- glm(group_res ~ PI + sci_env  +  as.factor(sex) + as.factor(ac_ach) 
            +  as.factor(grade8) + as.factor(grade9) + as.factor(grade10) 
            + as.factor(grade11) + as.factor(sc_type) + grit 
            + gm_intelligence + gm_personality + self_mot + self_efficacy, 
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod2)

margin <- margins(mod2)
margin

vif(mod2)

PseudoR2(mod2, which = "all")

dat2 <- 
  data_for_glm %>%
  select(group_res, PI,  sci_env,  sex, grade8, grade9, grade10,  grade11,  sc_type,
         growth,  grit,  gm_intelligence, ac_ach) %>%
  drop_na() # check variables as finished

predicts2 <- as.numeric(mod2$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts2), as.factor(dat2$group_res))

to_check2 <- 
  data_for_glm %>%
  select(group_res, PI,  sci_env,  sex, grade8, grade9, grade10,  grade11,
         growth,  grit,  gm_intelligence, ac_ach, self_efficacy)

corr_matrix2 <- cor(to_check2)
p.mat2 = cor_pmat(to_check2)
ggcorrplot(corr_matrix2,  type = "lower",
           lab = TRUE, p.mat = p.mat2, tl.cex = 10, lab_size = 2.5)



data_desc <- 
  data_for_glm %>%
  select(growth,  grit,  PI,  sci_env, 
         gm_intelligence, ac_ach, research_potential, gm_personality, self_mot,
         self_efficacy)

psych::describe(data_desc$growth)
psych::describe(data_desc$grit)
psych::describe(data_desc$gm_intelligence)
psych::describe(data_desc$gm_personality)
psych::describe(data_desc$research_potential)


psych::describe(data_desc$PI)
psych::describe(data_desc$gm_personality)
psych::describe(data_desc$self_mot)
psych::describe(data_desc$self_efficacy)

a <- density(data_desc$self_efficacy)
plot(a)

dh <- 
  data_for_glm %>%
  group_by(group_res) %>%
  summarise(growth = mean( growth, na.rm =  T),  
          grit = mean(grit , na.rm =  T),  
          PI = mean(PI , na.rm =  T),  
          sci_env = mean(sci_env , na.rm =  T), 
          gm_intelligence = mean(gm_intelligence , na.rm =  T), 
          research_potential = mean(research_potential , na.rm =  T), 
          gm_personality = mean(gm_personality , na.rm =  T))

varsss <- c("growth",  "grit",  "PI",  "sci_env", 
            "gm_intelligence", "ac_ach", "gm_personality")
lapply(data_for_glm[c(varsss)], function(x) t.test(x ~ data_for_glm$group_res))


lm3 <- read_excel("03_outputs/0302_tables/for_pic.xlsx") %>%
  mutate(value = value * 100)

ggplot(lm3, aes(x = name, y = value)) +
  geom_segment( aes(x = name, xend = name, y = 0, yend = value), color = "black") +
  geom_point( color = "slateblue2", size = 4, alpha = 1) +
  geom_label(aes(name, value + 0.5*value, label = paste0(signif(value,2), "%")), colour = "black", nudge_x = 0.35, size = 4) +
  coord_flip() +
  theme_bw() +
  ylim(-20, 30) +
  labs(y = "Предельные эффекты", x = "")
