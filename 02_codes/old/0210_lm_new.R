data_for_glm <- read_dta(file.path(outData,"data_for_glm.dta")) %>%
  filter(grade > 1) %>%
  mutate(grade7 = ifelse(grade == 2,1,0),
         grade8 = ifelse(grade == 3,1,0),
         grade9 = ifelse(grade == 4,1,0),
         grade10 = ifelse(grade == 5,1,0),
         grade11 = ifelse(grade == 6,1,0))

# Social factors corrected

mod <- glm(group ~ PI + sci_env  +  sex 
           +  grade8 + grade9 + grade10 + grade11 + as.factor(sc_type), data = data_for_glm, 
           family = binomial(link = "logit"), x = TRUE)
summary(mod)

margin <- margins(mod)
margin

vif(mod)

PseudoR2(mod, which = "all")

dat <- 
  data_for_glm %>%
  select(group, PI , sci_env, sex,  grade8, grade9,  grade10,  grade11, sc_type) %>%
  drop_na()

predicts <- as.numeric(mod$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts), as.factor(dat$group))


to_check <- 
  data_for_glm %>%
  select(group, PI , sci_env, sex, grade8, grade9,  grade10,  grade11)

corr_matrix1 <- cor(to_check)
p.mat1 = cor_pmat(to_check)
ggcorrplot(corr_matrix1,  type = "lower",
           lab = TRUE, p.mat = p.mat1, tl.cex = 10, lab_size = 2.5)

# Social & psychological factors corrected

mod2 <- glm(group ~ PI + sci_env  +  sex + as.factor(ac_ach) 
            +  grade8 + grade9 + grade10 + grade11 + as.factor(sc_type) +
              growth + grit + gm_intelligence + self_mot, 
            data = data_for_glm, 
            family = binomial(link = "logit"), x = TRUE)
summary(mod2)

margin <- margins(mod2)
margin

vif(mod2)

PseudoR2(mod2, which = "all")

dat2 <- 
  data_for_glm %>%
  select(group, PI,  sci_env,  sex, grade8, grade9, grade10,  grade11,  sc_type,
           growth,  grit,  gm_intelligence, ac_ach) %>%
  drop_na() # check variables as finished

predicts2 <- as.numeric(mod2$fitted.values >= 0.5)
confusionMatrix(as.factor(predicts2), as.factor(dat2$group))

to_check2 <- 
  data_for_glm %>%
  select(group, PI,  sci_env,  sex, grade8, grade9, grade10,  grade11,
         growth, cognitive,  grit,  gm_intelligence, ac_ach)


corr_matrix2 <- cor(to_check2)
p.mat2 = cor_pmat(to_check2)
ggcorrplot(corr_matrix2,  type = "lower",
           lab = TRUE, p.mat = p.mat2, tl.cex = 10, lab_size = 2.5)
