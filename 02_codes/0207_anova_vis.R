#-------------------------------------------------------------------
# Project: Scientific Involvement Indicator
# Organization: SFedU Future Skills Research Lab
# Objective: Visualizations for ANOVA
# Author: Valeria Egorova
# Date: 03 Dec 2023
#-------

B5 <- read_excel(file.path(outTables,"b5.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a <- ggplot(B5, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  xlab("") +
  ylab("Большая пятерка") +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())

#########################################################

EI <- read_excel(file.path(outTables,"ei.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a1 <- ggplot(EI, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Эмоциональный интеллект") +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())

#########################################################

gm <- read_excel(file.path(outTables,"gm.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a2 <- ggplot(gm, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Мышление роста") +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())

#########################################################

grit <- read_excel(file.path(outTables,"grit.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a3 <- ggplot(grit, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Упорство") +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())

#########################################################

motives <- read_excel(file.path(outTables,"motives.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a4 <- ggplot(motives, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Учебная мотивация школьников") +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())

#########################################################

others <- read_excel(file.path(outTables,"others.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a5 <- ggplot(others, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())

#########################################################

RP <- read_excel(file.path(outTables,"rp.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a6 <- ggplot(RP, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("Исследовательский потенциал") +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())

#########################################################

self <- read_excel(file.path(outTables,"self.xlsx")) %>%
  mutate(name = as.factor(name),
         group = factor(group))

a7 <- ggplot(self, aes(x = name, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = signif(round(value, 2))), position = position_dodge(width = 1), vjust = 1, hjust = 0, size = 3.5) +
  theme_bw() +
  coord_flip() +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("#8DB6CD", "#1874CD", "#79CDCD") ) +
  # scale_fill_manual(values = var)+
  theme(legend.title = element_blank())


ggarrange(a, a1, a2, a3, a4, a5, a6, a7,
          ncol = 3, nrow = 3)