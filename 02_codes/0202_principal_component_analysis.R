#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: Creating the indicator showing students` involvement in research activities
# Author: Valeria Egorova
# Date: 19 Nov 2023
#-------

#Grouping similar variables

scales <- read_dta(file.path(outData,"scales.dta"))

data <- 
  scales%>%
  mutate(events = rowMeans(select(.,c(SC009, SC010, SC011, SC012, SC013, SC014, SC015, SC016)),
                           na.rm = T),
         attract = rowMeans(select(.,c(SC026, SC027, SC028, SC029, SC030, SC031)),
                            na.rm = T))
# PCA
for_pca <- 
  data%>%
  select(events, attract, SC001, SC003, SC004, SC008, SC025, SC033, SC034)

pca <- prcomp(for_pca, center = T,scale. = TRUE)

summary(pca)

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50))

pca$sdev ^ 2 #Kaiser-Guttman Criterion - higher than 1

print(pca$rotation) # understand this staff

pca %>% biplot(cex = .5)

table <- as.table(pca$rotation)

rotation_table <- as.data.frame(pca$rotation)

rotation_table <- 
  rotation_table%>%
  write_xlsx(file.path(outData,"rotation_table.xlsx"))

#Indicator version with the wights for every variable 

# Variation for every variable in the first component

variance_explained <- (pca$rotation[,1]^2) / sum(pca$rotation[,1]^2)
variance_explained


#Indicator version with two components 

explained_variance <- pca$sdev^2
total_variance <- sum(explained_variance)
prop_var_explained <- explained_variance / total_variance

# Weight for PC1 and PC2
weight_PC1 <- prop_var_explained[1]
weight_PC2 <- prop_var_explained[2]

#Including indicators to dataset 

data_index <- 
  data%>%
  mutate(index_v2 = weight_PC1 * pca$x[, 1] + weight_PC2 * pca$x[, 2],
         index_v1 = events*0.14 + attract*0.11 + SC001*0.13 + SC003*0.05 + SC004*0.12 + SC008*0.09
         + SC025*0.10 + SC033*0.15 + SC034*0.10,
         index_v3 = (events + attract + SC001 + SC003 + SC004 + SC008 + SC025 + SC033 + SC034)/ 9)%>%
  write_dta(file.path(outData, "data_index.dta"))


# Goodness of fit for PCA

fit_1 <- lm(events ~ ., data = data)

components <- cbind(events = data[, "events"], pca$x[, 1:2]) %>%
  as.data.frame()

fit_2 <- lm(events ~ ., data = components)

components2 <- cbind(events = data[, "events"], pca$x[, 1]) %>%
  as.data.frame()

fit_3 <- lm(events ~ ., data = components2)

summary(fit_1)$adj.r.squared

summary(fit_2)$adj.r.squared

summary(fit_3)$adj.r.squared

#Checking correlation between two versions of variables

data_cor <- 
  data_index%>%
  select(index_v2, index_v1, index_v3)

corr_matrix <- cor(data_cor)
p.mat = cor_pmat(data_cor)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE, p.mat = p.mat, tl.cex = 10, lab_size = 2.5)

describe(data_index$index_v2)

a <- density(data_index$index_v2)
plot(a)
