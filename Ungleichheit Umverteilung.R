
install.packages("corrplot")
library(haven)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(car)
library(dplyr)
install.packages("carData")
library(haven)

##LOG Einwohnerzahl

Daten_USA$log_Einwohnerzahl2020 <- log(Daten_USA$Einwohnerzahl2020)

##Log Ärzte
Daten_USA$log_Aerzte2024 <- log(Daten_USA$Aerzte2024)

##Abbildung Ärzte und Einwohnerzahl 

ggplot(data=Daten_USA, aes(x = log_Aerzte2024, y = log_Einwohnerzahl2020)) +
  geom_point(color = "black", size = 3) + # Scatter points
  geom_smooth(method = "lm", color = "blue", se = TRUE) + # Regression line with confidence interval
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1) + 
  ggtitle("Linear Regression with ggplot2") +
  xlab("Anzahl Ärzte 2024") +
  ylab("Log Einwohnerzahl") +
  theme_minimal()


##Regression AV Aerztedichte pro KM

##Einwohnerzahl
Einwohnerzahl2020<- lm(data = Daten_USA, AerztedichteProKM  ~ log_Einwohnerzahl2020)
summary(Einwohnerzahl2020)

##Armut
Armut <- lm(data = Daten_USA, AerztedichteProKM  ~ Armut2023 )
summary(Armut)

## Per Capita Personal Income/ Persönliches pro Kopf Einkommen
PCPI <- lm(data = Daten_USA, AerztedichteProKM  ~ PCPI2023 )
summary(PCPI)

##Capita Personal Income log

Daten_USA$log_PCPI2023 <- log(Daten_USA$PCPI2023)

##Visualize Capita Personal Income
ggplot(data=Daten_USA, aes(x = log_PCPI2023, y = AerztedichteProKM)) +
  geom_point(color = "black", size = 3) + # Scatter points
  geom_smooth(method = "lm", color = "blue", se = TRUE) + # Regression line with confidence interval
  ggtitle("Linear Regression with ggplot2") +
  xlab("Persönliches pro Kopf Einkommen") +
  ylab("Ärztedichte pro km2") +
  theme_minimal()

##Versicherungsstatus --> Unversichert
Unversicherte <- lm(data = Daten_USA, AerztedichteProKM  ~ Unversicherte )
summary(Unversicherte)

##Visualize Versicherungsstatus
ggplot(data=Daten_USA, aes(x = Unversicherte, y = AerztedichteProKM)) +
  geom_point(color = "black", size = 3) + # Scatter points
  geom_smooth(method = "lm", color = "blue", se = TRUE) + # Regression line with confidence interval
  ggtitle("Linear Regression with ggplot2") +
  xlab("Versicherungsstatus (Unversicherte)") +
  ylab("Ärztedichte pro km2") +
  theme_minimal()

##Alter 65+

AlterÜ65 <- lm(data = Daten_USA, AerztedichteProKM  ~ AlterÜ65 )
summary(AlterÜ65)

##Alter Median

AlterMedian <- lm(data = Daten_USA, AerztedichteProKM  ~ AlterMedian )
summary(AlterMedian)

##Bildung
Bildung <- lm(data = Daten_USA, AerztedichteProKM  ~ BildungLess9thGrade )
summary(Bildung)

##Prüfen der Korrelationen zwischen den Variablen 

corr_selected <- Daten_USA %>% 
  select(AerztedichteProKM, Armut2023, PCPI2023, Unversicherte, AlterÜ65, AlterMedian, BildungLess9thGrade) %>% 
  cor(use = "pairwise") %>% 
  round(1)

ggcorrplot(corr_selected, type = "lower", lab = T, show.legend = F)

##Tabelle

install.packages("knitr")
library("knitr")
install.packages("kableExtra")
library("kableExtra")

correlation_summary <- data.frame( Variable = c("AlterMedian", "ProKopfEinkommen", "Bildung", "Armut", "VersicherungsstatusUnversicherte"), Estimate = c("44.66", "1.906e-02", "6022.01", "-2587.1", "-4920.1"), StdError = c("24.43", "4.935e-03", "3850.18", "1918.0", "1835.5"),tvalue = c("1.828", "3.862", "1.564", "-1.349", "-2.681"),pvalue = c("0.0741", "0.000342***", "0,125", "0.1840", "0.0101*"))

kable(correlation_summary, caption = "Table1")

table <- kable(correlation_summary, format = "html", caption = "Table 1 - Summary")

styled_table <- table %>% 
  kable_styling(bootstrap_options = "basic") %>% 
  column_spec(1, bold = TRUE) %>% 
  column_spec(3, bold = TRUE) %>% 
  column_spec(4, bold = TRUE) %>% 
  column_spec(5, color = "blue", bold = TRUE) %>% 
  row_spec(0, background = "#D3D3D3")

styled_table

