# Load relevant libraries
library(dplyr)
library(ggplot2)

# Load data
data <- read.csv('/home/bdiethelmv/sync/unasalud_uc/iaap/iaap_consolidada_29mar2023.csv')
data$infection <- ifelse(data$Clasificación=='Positivo',1,0)

# Get total infection cases per regions
# To-do: the code chunk below could be improved with a for loop
arica <- sum(subset(data, Región=='Arica' & infection==1)$infection)
tarapaca <- sum(subset(data, Región=='Tarapacá' & infection==1)$infection)
antofagasta <- sum(subset(data, Región=='Antofagasta' & infection==1)$infection)
atacama <- sum(subset(data, Región=='Atacama' & infection==1)$infection)
coquimbo <- sum(subset(data, Región=='Coquimbo' & infection==1)$infection)
valparaiso <- sum(subset(data, Región=='Valparaíso' & infection==1)$infection)
metropolitana <- sum(subset(data, Región=='Metropolitana' & infection==1)$infection)
ohiggins <- sum(subset(data, Región=="O'Higgins" & infection==1)$infection)
maule <- sum(subset(data, Región=="Maule" & infection==1)$infection)
nuble <- sum(subset(data, Región=="Ñuble" & infection==1)$infection)
biobio <- sum(subset(data, Región=="Bío Bío" & infection==1)$infection)
araucanía1 <- sum(subset(data, Región=="Araucanía" & infection==1)$infection)
araucanía2 <- sum(subset(data, Región=="Araucanía " & infection==1)$infection)
rios <- sum(subset(data, Región=="Los Ríos" & infection==1)$infection)
lagos <- sum(subset(data, Región=="Los Lagos" & infection==1)$infection)
aysen <- sum(subset(data, Región=="Aysén" & infection==1)$infection)
magallanes <- sum(subset(data, Región=="Magallanes" & infection==1)$infection)


# Paste the cases by region data into a data frame
regiones <- c(arica, tarapaca, antofagasta, atacama, coquimbo, valparaiso, metropolitana,
              ohiggins, maule, nuble, biobio, araucanía1, rios, lagos, aysen, magallanes)

region_nombre <- c("Arica","Tarapacá", "Antofagasta", "Atacama", "Coquimbo", 
                   "Valparaíso", 
                   "Metropolitana",
                   "O'Higgins", "Maule", "Ñuble", 
                   "Bío Bío", "Araucanía","Los Ríos", "Los Lagos",
                   "Aysén", "Magallanes")

summary_frame1 <- cbind(region_nombre,regiones)
summary_frame1 <- data.frame(summary_frame1)
summary_frame1$region_nombre <- factor(summary_frame1$region_nombre, 
                               levels=c("Arica","Tarapacá", "Antofagasta", 
                                        "Atacama", "Coquimbo", "Valparaíso", 
                                        "Metropolitana", "O'Higgins", "Maule", 
                                        "Ñuble", "Bío Bío", "Araucanía",
                                        "Los Ríos", "Los Lagos", "Aysén", 
                                        "Magallanes"))

summary_frame1$regiones <- as.double(summary_frame1$regiones)

# Create a new column for datetime info, and create new data frame for
# cumulative cases
data <- mutate(data, Fecha.Toma.Muestra=as.Date(Fecha.Toma.Muestra, format="%m/%d/%Y"))
data <- subset(data, infection==1)

summary_frame2 <- data %>%
  group_by(Fecha.Toma.Muestra) %>%
  summarise(inf=sum(infection))

summary_frame2$cumsum <- cumsum(summary_frame2$inf)

# Graph time
ggplot(data=summary_frame1, aes(x=region_nombre, y=regiones)) + 
  geom_bar(stat='identity', fill='darkred') +
  theme_bw() + 
  scale_y_continuous(breaks =seq(0, 60, by=10)) +
  labs(x='Chile Administrative Region', 
       y='Total Reported Avian Influenza \nCases from December 2022 to March 2023') +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position="none")

ggplot(data=summary_frame2) + geom_line(aes(x=Fecha.Toma.Muestra, y=cumsum,
                                    linewidth=I(2.5)), color='darkred')+
  labs(x='Sample collection date', 
       y='Cumulative Reported Avian Influenza \nCases from December 2022 to March 2023') +
  theme_bw() + 
  scale_y_continuous(breaks =seq(0, 350, by=50)) +
  scale_x_date(date_breaks = "15 day", date_labels = "%d %b %Y") +
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.9),
        legend.position="none")

# Pending: logistic regression models to assess risk of infection
mod1 <- glm(data$infection~data$Región, family='binomial')
summary(mod1)
