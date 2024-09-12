# Notenauswertungen
# Juergen Degenfellner, 21.12.23

# From: https://github.com/jdegenfellner/MC_exam_Reader_public_version

library(pacman)
p_load(readxl, writexl, tidyverse, data.table, fancycut)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read_excel("./Namen_Punkte.xlsx", sheet = 1)

df <- as.data.table(df)
# 60% hard cutoff for positive
intervals <- c('[60.00,64.44]', '(64.44,68.89]', '(68.89,73.33]', 
               '(73.33,77.78]', '(77.78,82.22]', '(82.22,86.67]', 
               '(86.67,91.11]', '(91.11,95.56]', '(95.56,100]')
grades <- seq(4.0, 6.0, by = 0.25)

df$Note <- wafflecut(df$Punkte, intervals, grades)

df %>% filter(Punkte > 0)  %>% ggplot(aes(x = Note)) + 
  geom_bar() + 
  geom_text(stat='count', aes(label = after_stat(count)), vjust=-0.3) +
  ggtitle("Notenübersicht") + 
  theme(plot.title = element_text(hjust = 0.5))#

# Example
write_xlsx(df, "Noten_calculated_2.5.24.xlsx")


df

summary(df$Punkte[df$Punkte>0])
hist(df$Punkte[df$Punkte>0])

