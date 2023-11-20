#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: Conducting logistic regression
# Author: Valeria Egorova
# Date: 19 Nov 2023
#-------

# Logistic regression version 1 

mod <- glm(group ~ PI + sci_env + ac_ach  + AP02 +  sex + grade + sc_type + #AP01
              research_potential, data = data_for_glm, family = binomial(link="logit"), x=TRUE)
summary(mod)

data_for_glm$group_yhat = predict(mod, newdata = data_for_glm)

#Scatter plot

ggplot(data_for_glm, aes(x=group, y=group_yhat)) +
  geom_point(shape=1)+
  xlab("Уровень выраженности индекса")+
  ylab("Предсказанные значения")+
  theme_bw()

#VIF

vif(mod)%>%
  as.data.frame()%>%
  write_xlsx(file.path(outData,"vif.xlsx"))

#Goodness of fit

maBina(mod)

exp(mod$coefficients)
margin <- margins(mod)
margin

## Logistic regression version 2 

mod2 <- glm(group2 ~ PI + sci_env + AP01 + AP02 +  sex + grade + sc_type + #ac_ach 
             research_potential, data = data_for_glm, family = binomial(link="logit"), x=TRUE)
summary(mod2)

margin2 <- margins(mod2)
margin2