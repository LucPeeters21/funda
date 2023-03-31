library(haven)
library(dplyr)
library(car)
library(emmeans)
library(effectsize)
library(dataPreparation)
library(readxl)
library(stringr)
library(tidyr)
library(data.table)
library(tidyverse)
library(scales) 
library(stringr) 
library(Hmisc) 
library(forcats) 
library(ggthemes)

options(scipen = 999)
#################
## data import ##
#################

# set working directory
setwd("C:/Users/LPEE/OneDrive - Hoppenbrouwers Techniek B.V/Documenten/Projects/funda/data")

# import funda and city data
df <- read_excel("raw_data_for_excel_analysis.xlsx")
df_places <- read_excel("plaatsnamen_nederland.xlsx")

# remove na's, duplicates
df_places <- na.omit(df_places)
df_places <- df_places[!duplicated(df_places$City), ]

# merge funda and city data
df_full <- merge(df, df_places, by = 'City')

######################
## data preparation ##
######################

# identify duplicated adresses
df <- unique(df_full)

# convert to correct data types
df$Price <- as.numeric(df$Price)
df$`Build year` <- as.character(df$`Build year`)

# separate rooms into 'rooms' and 'bed_rooms'
df <- separate(df, col=Rooms, into =c("rooms", "bed_rooms"), sep=" kamers ") 
df$bed_rooms <- gsub('[(slaapkamers)]','',df$bed_rooms)
df$bed_rooms <- gsub('[(]','',df$bed_rooms)
df$rooms <- as.numeric(df$rooms)
df$bed_rooms <- as.numeric(df$bed_rooms)
df$rooms <- (df$rooms - df$bed_rooms)

# separate Toilet into 'bath_rooms' and 'toilets'
df <- separate(df, col=Toilet, into =c("bath_rooms", "toilets"), sep=" en ") 
df$bath_rooms <- gsub('[(badkamers)]','',df$bath_rooms)
df$toilets <- gsub('[(aparte toiletten)]','',df$toilets) 
df$bath_rooms <- as.numeric(df$bath_rooms)
df$toilets <- as.numeric(df$toilets)

# separate Floors into 'floors' and 'basement/attic' 
df <- separate(df, col=Floors, into =c("living_floors", "basement"), sep=" woonla")
df$`basement` <- gsub(',','',df$`basement`) 

# duplicate 'basement' column and name 'attic' and 'vliering'
df$attic = df$basement 
df <- df %>% relocate(attic, .after = basement)

# keep correct part of each string
df$basement <- str_sub(df$basement, -6, -1)
df$basement <- gsub('kelder','1',df$basement) 
df$basement <- gsub('ag','0',df$basement)
df$basement <- gsub('gen','0',df$basement)
df$basement <- gsub('iering','0',df$basement)
df$basement <- gsub('zolder','0',df$basement)
df$basement <- as.numeric(df$basement)

############################
##   data visualization   ##
############################

ggplot(df, 
       aes(x = `Living space size (m2)`, 
           y = Price)) +
  geom_point(stat = "identity", color = "black", position = position_dodge()) + ggtitle("Funda") + 
  xlab("m2") + ylab("Prijs") +  theme_linedraw() + coord_cartesian(ylim = c(149000, 2000000), xlim = c(50, 400)) + 
  geom_smooth(method='lm')

ggplot(data = df, aes(x = Provincie, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 800000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

df_spec


############################
## descriptive statistics ##
############################

# correlation matrix
cor <- as.data.frame(as.table(cor(use = "complete.obs", df[, unlist(lapply(df, is.numeric))])))
cor <- cor[!duplicated(cor$Freq), ]

############################
##                      ##

# regression model
model <- lm(Price ~ `Lot size (m2)`+`Living space size (m2)`+as.factor(rooms)+as.factor(bath_rooms)+
              as.factor(bed_rooms)+as.factor(toilets)+as.factor(living_floors)+`Estimated neighbourhood price per m2`, data = df)
summary(model)
