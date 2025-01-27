
data_cronbach <- 
  data_for_glm %>%
  dplyr::select(motivational, cognitive, behavioural) #, research_potential)

library(ltm)
cronbach.alpha(data_cronbach, CI=TRUE)

alpha(data_cronbach)

data_cognit <- 
  data%>%
  select(PRP03 , PRP04 , PRP06 , PRP08 , PRP10 , PRP11 , PRP12 ,
         PRP14 , PRP15 , PRP16 , PRP20 , PRP23 , PRP25 , PRP28 , PRP36 ,
         PRP40 , PRP43 , PRP47 , PRP49 , PRP50)
alpha(data_cognit)  
  
data_beh <- 
  data%>%
  select(PRP01 , PRP02 , PRP05 , PRP07 , PRP18 , PRP19 ,
         PRP21 , PRP24 , PRP26 , PRP30 , PRP31 , PRP34 , PRP35 , PRP38 ,
         PRP39 , PRP44 , PRP45 , PRP46 , PRP48 , PRP51)
alpha(data_beh)  

data_mot <- 
  data%>%
  select(PRP09 , PRP13 , PRP17 , PRP22 , PRP27 , PRP29 ,
         PRP32 , PRP33 , PRP37 , PRP41 , PRP42)
alpha(data_mot)  





corr_matrix <- cor(data_cronbach)
fa.eigen <- eigen(corr_matrix)
fa.eigen$values

plot(fa.eigen$values, type='b', ylab='Eigenvalues', xlab='Factor')

fa.res<-factanal(x=data_cronbach, factors=1, rotation='promax')
print(fa.res, cut=0.3)

EFA_model <- fa(data_cronbach, nfactors = 1)
fa.diagram(EFA_model)
EFA_model$loadings

#########################################################################
corr_matrix <- cor(data_cognit)
fa.eigen <- eigen(corr_matrix)
fa.eigen$values

plot(fa.eigen$values, type='b', ylab='Eigenvalues', xlab='Factor')

fa.res<-factanal(x=data_cognit, factors=1, rotation='promax')
print(fa.res, cut=0.3)

EFA_model <- fa(data_cognit, nfactors = 1)
fa.diagram(EFA_model)
EFA_model$loadings


#########################################################################
corr_matrix <- cor(data_beh)
fa.eigen <- eigen(corr_matrix)
fa.eigen$values

plot(fa.eigen$values, type='b', ylab='Eigenvalues', xlab='Factor')

fa.res<-factanal(x=data_beh, factors=1, rotation='promax')
print(fa.res, cut=0.3)

EFA_model <- fa(data_beh, nfactors = 1)
fa.diagram(EFA_model)
EFA_model$loadings

#########################################################################
corr_matrix <- cor(data_mot)
fa.eigen <- eigen(corr_matrix)
fa.eigen$values

plot(fa.eigen$values, type='b', ylab='Eigenvalues', xlab='Factor')

fa.res<-factanal(x=data_mot, factors=1, rotation='promax')
print(fa.res, cut=0.3)

EFA_model <- fa(data_mot, nfactors = 1)
fa.diagram(EFA_model)
EFA_model$loadings



