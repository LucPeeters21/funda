library(olsrr)
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
library(pals)
library(maps)
library(shiny)
library(shinythemes)
library(sf)
library(mapview)
library(leaflet)
library(plotly)
library(tm)
library(wordcloud)
library(gridExtra)
library(ggplot2)
library(scales)
library(mice)
library(randomForest) 
library(data.table)
library(corrplot) 
library(GGally)
library(e1071)

options(scipen = 999)
#################
## data import ##
#################

# set working directory
setwd("C:/Users/LPEE/OneDrive - Hoppenbrouwers Techniek B.V/Documenten/Projects/funda/data")

# import funda
df_funda <- read_excel("raw_data_for_excel_analysis.xlsx")

# import city data
df_places <- read_excel("plaatsnamen_nederland.xlsx")

# import spatial data (optionally) https://public.opendatasoft.com/explore/dataset/georef-netherlands-postcode-pc4/table/
coord <- read_xlsx("georef-netherlands-postcode-pc4.xlsx")

######################
## data preparation ##
######################

# remove NA's and exact duplicates
df_places <- na.omit(df_places)
df_places <- df_places[!duplicated(df_places$City), ]

# merge funda and city data
df <- merge(df_funda, df_places, by = 'City')

# subset only unique adresses after merge
df <- unique(df)

# remove 'build type' variable since it has only one value expect for two observations
df <- df %>% select(-`Build type`)

# relocate 'Gemeente' and 'Provincie' variables
df <- df %>% relocate(Gemeente, .after = Address)
df <- df %>% relocate(Provincie, .after = Gemeente)

# remove 'Address'
df <- df %>% select(-Address)

# convert to correct data types
df$Price <- as.numeric(df$Price)
df$`Build year` <- as.character(df$`Build year`)

# separate rooms into 'rooms' and 'bed_rooms' to create two separate variables
df <- separate(df, col=Rooms, into =c("rooms", "bedrooms"), sep=" kamers ") 
df$bedrooms <- gsub('[(slaapkamers)]','',df$bedrooms)
df$bedrooms <- gsub('[(]','',df$bedrooms)
df$rooms <- as.numeric(df$rooms)
df$bedrooms <- as.numeric(df$bedrooms)

# separate Toilet into 'bathrooms' and 'toilets'
df <- separate(df, col=Toilet, into =c("bathrooms", "toilets"), sep=" en ") 
df$bathrooms <- gsub('[(badkamers)]','',df$bathrooms)
df$toilets <- gsub('[(aparte toiletten)]','',df$toilets) 
df$bathrooms <- as.numeric(df$bathrooms)
df$toilets <- as.numeric(df$toilets)

# separate Floors into 'floors' and 'basement/attic' 
df <- separate(df, col=Floors, into =c("living_floors", "basement"), sep=" woonla")
df$`basement` <- gsub(',','',df$`basement`) 

# duplicate 'basement' column and name 'attic'
df$attic = df$basement 
df <- df %>% relocate(attic, .after = basement)

# turn 'basement' into 0's and 1's 
mapping <- c("ag" = 0, 
             "ag een vliering en een kelder" = 1, 
             "ag een zolder en een kelder" = 1, 
             "ag en een kelder" = 1,
             "ag en een vliering" = 0,
             "ag en een zolder" = 0,
             "ag en een zolder met vliering" = 0,
             "gen" = 0,
             "gen een vliering en een kelder" = 1,
             "gen een zolder en een kelder" = 1,
             "gen een zolder met vliering en een kelder" = 1,
             "gen en een kelder" = 1,
             "gen en een vliering" = 0,
             "gen en een zolder" = 0,
             "gen en een zolder met vliering" = 1)

df$basement <- mapping[df$basement]

# turn 'attic' into 0's and 1's
mapping <- c("ag" = 0, 
             "ag een vliering en een kelder" = 1, 
             "ag een zolder en een kelder" = 1, 
             "ag en een kelder" = 0,
             "ag en een vliering" = 1,
             "ag en een zolder" = 1,
             "ag en een zolder met vliering" = 1,
             "gen" = 0,
             "gen een vliering en een kelder" = 1,
             "gen een zolder en een kelder" = 1,
             "gen een zolder met vliering en een kelder" = 1,
             "gen en een kelder" = 0,
             "gen en een vliering" = 1,
             "gen en een zolder" = 1,
             "gen en een zolder met vliering" = 1)

df$attic <- mapping[df$attic]

# create new variable 'Randstad'
df <- df %>%
  mutate(Randstad = case_when(Provincie == 'Drenthe' ~ 0,
                              Provincie == 'Flevoland' ~ 0,
                              Provincie == 'Fryslân' ~ 0,
                              Provincie == 'Gelderland' ~ 0,
                              Provincie == 'Groningen' ~ 0,
                              Provincie == 'Limburg' ~ 0,
                              Provincie == 'Noord-Brabant' ~ 0,
                              Provincie == 'Noord-Holland' ~ 1,
                              Provincie == 'Overijssel' ~ 0,
                              Provincie == 'Utrecht' ~ 1,
                              Provincie == 'Zeeland' ~ 0,
                              Provincie == 'Zuid-Holland' ~ 1))

# create groups for energy labels
df <- df %>%
  mutate(label_group = case_when(`Energy label` == 'G' ~ 'very low',
                                  `Energy label` == 'F' ~ 'very low',
                                  `Energy label` == 'E' ~ 'low',
                                  `Energy label` == 'D' ~ 'low',
                                  `Energy label` == 'C' ~ 'medior',
                                  `Energy label` == 'B' ~ 'medior',
                                  `Energy label` == 'A' ~ 'high',
                                  `Energy label` == 'A+' ~ 'high',
                                  `Energy label` == 'A++' ~ 'very high',
                                  `Energy label` == 'A+++' ~ 'very high',
                                  `Energy label` == 'A++++' ~ 'very high',
                                  `Energy label` == 'Niet verplicht' ~ 'unknown'))

# create new variable called 'age' 
x <- Sys.Date()
x <- year(x)
df$age <- x - as.numeric(df$`Build year`)

# create age factors
df$agecategory <- with(df, ifelse(age <= 10, '0-10',
                                ifelse(age <= 50, '11-50', 
                                       ifelse(age <= 100, '51-100', '100<' ))))

# create rooftype variable
df <- df %>%
  mutate(Roof = case_when(Roof == 'Dwarskap' ~ 'transverse',
                          Roof == 'Dwarskap bedekt met bitumineuze dakbedekking' ~ 'transverse',
                          Roof == 'Dwarskap bedekt met bitumineuze dakbedekking en pannen' ~ 'transverse',
                          Roof == 'Dwarskap bedekt met metaal' ~ 'transverse',
                          Roof == 'Dwarskap bedekt met overig' ~ 'transverse',
                          Roof == 'Dwarskap bedekt met pannen' ~ 'transverse',
                          Roof == 'Lessenaardak' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met asbest' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met bitumineuze dakbedekking' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met bitumineuze dakbedekking en pannen' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met kunststof' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met leisteen' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met metaal' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met overig' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met overig en pannen' ~ 'shed',
                          Roof == 'Lessenaardak bedekt met pannen' ~ 'shed',
                          Roof == 'Mansarde dak' ~ 'mansard',
                          Roof == 'Mansarde dak bedekt met bitumineuze dakbedekking' ~ 'mansard',
                          Roof == 'Mansarde dak bedekt met bitumineuze dakbedekking en leisteen' ~ 'mansard',
                          Roof == 'Mansarde dak bedekt met bitumineuze dakbedekking en pannen' ~ 'mansard',
                          Roof == 'Mansarde dak bedekt met overig' ~ 'mansard',
                          Roof == 'Mansarde dak bedekt met pannen' ~ 'mansard',
                          Roof == 'Mansarde dak bedekt met riet' ~ 'mansard',
                          Roof == 'Plat dak' ~ 'flat',
                          Roof == 'Plat dak bedekt met bitumineuze dakbedekking' ~ 'flat',
                          Roof == 'Plat dak bedekt met bitumineuze dakbedekking en overig' ~ 'flat',
                          Roof == 'Plat dak bedekt met bitumineuze dakbedekking en pannen' ~ 'flat',
                          Roof == 'Plat dak bedekt met kunststof' ~ 'flat',
                          Roof == 'Plat dak bedekt met leisteen' ~ 'flat',
                          Roof == 'Plat dak bedekt met overig' ~ 'flat',
                          Roof == 'Plat dak bedekt met pannen' ~ 'flat',
                          Roof == 'Samengesteld dak' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met asbest en bitumineuze dakbedekking' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met asbest en pannen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking en kunststof' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking en leisteen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking en metaal' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking en overig' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking en pannen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking en riet' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking, leisteen en metaal' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met bitumineuze dakbedekking, pannen en riet' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met kunststof en leisteen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met kunststof en pannen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met leisteen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met metaal' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met overig' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met overig en pannen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met pannen' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met pannen en riet' ~ 'composite',
                          Roof == 'Samengesteld dak bedekt met riet' ~ 'composite',
                          Roof == 'Schilddak' ~ 'hip',
                          Roof == 'Schilddak bedekt met asbest en pannen' ~ 'hip',
                          Roof == 'Schilddak bedekt met bitumineuze dakbedekking en pannen' ~ 'hip',
                          Roof == 'Schilddak bedekt met bitumineuze dakbedekking, overig en pannen' ~ 'hip',
                          Roof == 'Schilddak bedekt met leisteen' ~ 'hip',
                          Roof == 'Schilddak bedekt met metaal' ~ 'hip',
                          Roof == 'Schilddak bedekt met overig en pannen' ~ 'hip',
                          Roof == 'Schilddak bedekt met pannen' ~ 'hip',
                          Roof == 'Schilddak bedekt met pannen en riet' ~ 'hip',
                          Roof == 'Schilddak bedekt met riet' ~ 'hip',
                          Roof == 'Tentdak' ~ 'tented',
                          Roof == 'Tentdak bedekt met bitumineuze dakbedekking' ~ 'tented',
                          Roof == 'Tentdak bedekt met bitumineuze dakbedekking en pannen' ~ 'tented',
                          Roof == 'Tentdak bedekt met pannen' ~ 'tented',
                          Roof == 'Tentdak bedekt met riet' ~ 'tented',
                          Roof == 'Zadeldak' ~ 'gable',
                          Roof == 'Zadeldak bedekt met asbest' ~ 'gable',
                          Roof == 'Zadeldak bedekt met asbest en pannen' ~ 'gable',
                          Roof == 'Zadeldak bedekt met bitumineuze dakbedekking' ~ 'gable',
                          Roof == 'Zadeldak bedekt met bitumineuze dakbedekking en kunststof' ~ 'gable',
                          Roof == 'Zadeldak bedekt met bitumineuze dakbedekking en metaal' ~ 'gable',
                          Roof == 'Zadeldak bedekt met bitumineuze dakbedekking en overig' ~ 'gable',
                          Roof == 'Zadeldak bedekt met bitumineuze dakbedekking en pannen' ~ 'gable',
                          Roof == 'Zadeldak bedekt met bitumineuze dakbedekking, overig en pannen' ~ 'gable',
                          Roof == 'Zadeldak bedekt met kunststof' ~ 'gable',
                          Roof == 'Zadeldak bedekt met kunststof en pannen' ~ 'gable',
                          Roof == 'Zadeldak bedekt met leisteen' ~ 'gable',
                          Roof == 'Zadeldak bedekt met metaal' ~ 'gable',
                          Roof == 'Zadeldak bedekt met metaal en pannen' ~ 'gable',
                          Roof == 'Zadeldak bedekt met pannen' ~ 'gable',
                          Roof == 'Zadeldak bedekt met pannen en riet' ~ 'gable',
                          Roof == 'Zadeldak bedekt met riet' ~ 'gable',
                          Roof == 'Zadeldak bedekt met overig' ~ 'gable',
                          Roof == 'Zadeldak bedekt met overig en pannen' ~ 'gable'))

# create new house type variable
df <- separate(df, col=`House type`, into =c("House type", "placement"), sep=", ") 
df <- separate(df, col=placement, into =c("placement", "rest"), sep=" ")
df <- df %>% select(-rest)

# turn variables into factors
df$basement <- as.factor(df$basement)
df$attic <- as.factor(df$attic)
df$rooms <- as.factor(df$rooms)
df$bedrooms <- as.factor(df$bedrooms)
df$bathrooms <- as.factor(df$bathrooms)
df$toilets <- as.factor(df$toilets)
df$Randstad <- as.factor(df$Randstad)
df$living_floors <- as.factor(df$living_floors)

# order variables
df <- df %>% relocate(Randstad, .after = Provincie)
df <- df %>% relocate(`House type`, .after = Garden)
df <- df %>% relocate(Roof, .after = `House type`)
df <- df %>% relocate(placement, .after = `House type`)
df <- df %>% relocate(age, .after = `Build year`)
df <- df %>% relocate(label_group, .after = `Energy label`)


# connect geo points to gemeente names and create new frame for map
coord <- coord %>% rename(Gemeente = `Gemeente name`)
coord <- coord %>% select(Gemeente, `Geo Point`) 
coord <- separate(coord, col= `Geo Point`, into =c("latitude", "longitude"), sep=", ") 
coord <-  coord[!duplicated(coord$Gemeente),]
topo <- merge(df, coord, by = "Gemeente")
topo <- topo %>% select(Gemeente, City, Provincie, Price, placement, longitude, latitude)
avg <- mean(df$Price, na.rm = TRUE)
topo$above_avg <- ifelse(topo$Price>avg,"1","0")

# impute missing numeric data by mean centering and remove rows with missing data
colSums(sapply(df, is.na))

df$Price[which(is.na(df$Price))] <- mean(df$Price,na.rm = T)
df$age[which(is.na(df$age))] <- mean(df$age,na.rm = T)
df$`Estimated neighbourhood price per m2`[which(is.na(df$`Estimated neighbourhood price per m2`))] <- mean(df$`Estimated neighbourhood price per m2`,na.rm = T)

# remove outliers

# boxplot method (IQR) 
summary(df$Price)
outliers <- boxplot.stats(df$Price)$out
outliers
out_ind <- which(df$Price %in% c(outliers))
out_ind
out <- df[out_ind, ]

df <- anti_join(df, out)
summary(df$Price)

write.csv(df,
          "C:/Users/LPEE/OneDrive - Hoppenbrouwers Techniek B.V/Documenten/Projects/funda/data/df.csv", fileEncoding = "UTF-8")

############################
##   data visualization   ##
############################

# distribution of house prices
ggplot(df, aes(x = Price, fill = ..count..)) + geom_histogram(binwidth = 5000) + coord_cartesian(xlim = c(0, 2000000))

# map
topo$longitude <- as.numeric(topo$longitude)
topo$latitude <- as.numeric(topo$latitude)

# above average selection
pal0 <- colorFactor(topo.colors(8), topo$placement)
map0 <- leaflet(topo) %>% addTiles %>% 
  addCircleMarkers(clusterOptions = markerClusterOptions(), color = ~pal0(topo$above_avg), lat = ~latitude, lng = ~longitude, label = topo$placement) %>%  addLegend(pal = pal0, values = ~placement)
map0

# house category selection
pal1 <- colorFactor(topo.colors(2), topo$above_avg)
map1 <- leaflet(topo) %>% addTiles %>% 
  addCircleMarkers(clusterOptions = markerClusterOptions(), color = ~pal1(topo$above_avg), lat = ~latitude, lng = ~longitude, label = topo$above_avg) %>%  addLegend(pal = pal1, values = ~above_avg)
map1


# living space and price
ggplot(df, 
       aes(x = `Living space size (m2)`, 
           y = Price)) +
  geom_point(stat = "identity", color = "black", position = position_dodge()) + ggtitle("Funda") + 
  xlab("m2") + ylab("price") +  theme_linedraw() + coord_cartesian(ylim = c(149000, 2000000), xlim = c(50, 400)) + 
  geom_smooth(method='lm')

# lot size and price
ggplot(df, 
       aes(x = `Lot size (m2)`, 
           y = Price)) +
  geom_point(stat = "identity", color = "black", position = position_dodge()) + ggtitle("Funda") + 
  xlab("m2") + ylab("price") +  theme_linedraw() + coord_cartesian(ylim = c(149000, 2000000), xlim = c(50, 1000)) + 
  geom_smooth(method='lm')

# age and price
ggplot(df, 
       aes(x = age, 
           y = Price)) +
  geom_point(stat = "identity", color = "black", position = position_dodge()) + ggtitle("Funda") + 
  xlab("age") + ylab("price") +  theme_linedraw() + coord_cartesian(ylim = c(200000, 800000), xlim = c(0, 250)) +
  geom_smooth(method='lm')

# build year and price
ggplot(df, 
       aes(x = `Build year`, 
           y = Price)) +
  geom_point(stat = "identity", color = "black", position = position_dodge()) + ggtitle("Funda") + 
  xlab("age") + ylab("price") +  theme_linedraw() + coord_cartesian(ylim = c(200000, 800000)) +
  geom_smooth(method='lm') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# randstad and price
ggplot(data = df, aes(x = Randstad, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 800000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

# province and price
ggplot(data = df, aes(x = Provincie, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 800000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

# energy label and price
ggplot(data = df, aes(x = label_group, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 1500000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

# basement and price
ggplot(data = df, aes(x = basement, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 1500000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

# attic and price
ggplot(data = df, aes(x = attic, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 1500000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

# roof and price
ggplot(data = df, aes(x = Roof, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 1500000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

# House type and price
ggplot(data = df, aes(x = `House type`, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 2000000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")

# placement and price
ggplot(data = df, aes(x = placement, y = Price)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  coord_cartesian(ylim = c(200000, 1500000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")


# 
# ###########################################################
# ##      training and test set creation and statistics    ##
# ###########################################################
# 
# # create train and test sets
# set.seed(1)
# sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
# train  <- df[sample, ]
# test   <- df[!sample, ]
# 
# # data structure
# str(train)
# str(test)
# 
# # summary statistics
# summary(train[,sapply(train[,1:24], typeof) == "integer"])
# summary(test[,sapply(train[,1:24], typeof) == "integer"])
# 
# # train datasets from numeric and categorical/factor
# cat_var <- names(train)[which(sapply(train, is.character))]
# fac_var <- names(train)[which(sapply(train, is.factor))]
# num_var <- names(train)[which(sapply(train, is.numeric))]
# 
# train_cat <- train[cat_var]
# train_fac <- train[fac_var]
# train_num <- train[num_var]
# 
# # create barplot function
# plotHist <- function(data_in, i) 
# {
#   data <- data.frame(x=data_in[[i]])
#   p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
#     theme(axis.text.x = element_text(angle = 90, hjust =1))
#   return (p)
# }
# 
# # create density plot function
# plotDen <- function(data_in, i){
#   data <- data.frame(x=data_in[[i]], Price = data_in$Price)
#   p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
#     xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
#   return(p)
#   
# }
# 
# # create function to call both types of plots
# doPlots <- function(data_in, fun, ii, ncol=3) 
# {
#   pp <- list()
#   for (i in ii) {
#     p <- fun(data_in=data_in, i=i)
#     pp <- c(pp, list(p))
#   }
#   do.call("grid.arrange", c(pp, ncol=ncol))
# }
# 
# # call barplots
# doPlots(train_cat, fun = plotHist, ii = 1:4, ncol = 2)
# doPlots(train_cat, fun = plotHist, ii = 5:8, ncol = 2)
# doPlots(train_cat, fun = plotHist, ii = 9:11, ncol = 2)
# 
# doPlots(train_fac, fun = plotHist, ii = 1:4, ncol = 2)
# doPlots(train_fac, fun = plotHist, ii = 5:8, ncol = 2)
# 
# # call density plots
# doPlots(train_num, fun = plotDen, ii = 1:5, ncol = 2)
# 
# # explore correlations
# correlations <- cor(na.omit(train_num))
# row_indic <- apply(correlations, 1, function(x) sum(x > 0 | x < 0) > 1)
# correlations<- correlations[row_indic ,row_indic ]
# corrplot(correlations, method="square")
# 
# # find missing values in data
# colSums(sapply(train, is.na))
# colSums(sapply(test, is.na))
# 
# # mark rows of test and train data
# train$isTrain <- 1
# test$isTrain <- 0
# 
# # add log of house price
# #df$log_price <- log(df$Price)


df <- df %>% rename(energy = `Energy label`)



#############################
####      modelling      ####
#############################

# define model
newPredict <- df %>% select(Provincie, Gemeente, energy, `Lot size (m2)`, `Living space size (m2)`, rooms, bedrooms, bathrooms, toilets, living_floors, `House type`, placement, Roof, Garden, Price)
model <-  lm(Price ~ Provincie + Gemeente + energy + `Lot size (m2)`+`Living space size (m2)` + rooms * bedrooms * bathrooms + living_floors + `House type` + placement + Roof, data = df)
newPredict$pred <-  predict(model, newPredict) 
newPredict$error <- newPredict$Price - newPredict$pred

# define RMSE
newPredict$error_sqr <- newPredict$error * newPredict$error
n <- nrow(newPredict)
MSE <- sum(newPredict$error_sqr, na.rm = TRUE)/n
RMSE <- sqrt(MSE)

# visualise error distribution
hist(newPredict$error, breaks=25)



#############################
#### shiny application 1 ####
#############################

# define ui
ui <- fluidPage(theme = shinytheme("cyborg"),
  
  
  headerPanel("Huizenprijs Generator"),
  
  sidebarPanel(
    
    selectInput(inputId = "city", label = "Provincie",
                choices = df$Provincie,
                selected = df$Provincie[1]),
    
    selectInput(inputId = "city", label = "Energie label",
                choices = df$energy,
                selected = df$energy[1]),
    
    actionButton("go", "Voorspel")
  ),
  
  mainPanel(
    sidebarPanel( width = 25,
                  
                  headerPanel("Prijs: "),
                  
                  textOutput("value")
    )
  )
  
)



# define server
server <- function(input, output) {
  
  observeEvent(input$go,{
    
  })
    
    output$value <- renderText({newPredict$pred})
  
  
}

# run app
shinyApp(ui, server)








#############################
#### shiny application 2 ####
#############################

# Define UI for application that draws a histogram 
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title
                titlePanel("Nederlandse huizenmarkt"),
                
                # Navigation bar - Klantenkaart
                navbarPage("Huizenkaart", 
                           
                           # Show output
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Type woning", leafletOutput("map0", width = "100%", height = 800)),
                               tabPanel("Prijscategorie", leafletOutput("map1", width = "100%", height = 800)),
                             ))
                ),
                
                # Navigation bar - Product
                navbarPage("Boxplots", 
                           
                           # Show output
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Provincie", plotlyOutput("provincie")),
                               tabPanel("Type woning", plotlyOutput("typewoning")),

                             ))
                ),
                
                # Navigation bar - Project
                navbarPage("Scatterplots", 
                           
                           # Show output
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Woonruimte", plotlyOutput("woonruimte")),

                             ))
                ),
                
                # Navigation bar - Evaluatie
                navbarPage("Barplots", 
                           
                           # Show output
                           mainPanel(
                             tabsetPanel(

                             ))
                ),
                
                # Navigation bar - Ontwikkeling
                navbarPage("Regressie", 
                           
                           # Show output
                           mainPanel(
                             tabsetPanel(

                             ))
                ),
                
                # Navigation bar - Data
                navbarPage("Data"),
                
                # Show output
                mainPanel(
                  dataTableOutput("table")
                )
)

# Define server logic required to draw output
server <- function(input, output) {
  
  output$provincie <- renderPlotly({
    
    ggplot(data = df, aes(x = Provincie, y = Price)) +
      geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
      coord_cartesian(ylim = c(200000, 800000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")
    
  })
  
  output$typewoning <- renderPlotly({
    
    ggplot(data = df, aes(x = `House type`, y = Price)) +
      geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
      coord_cartesian(ylim = c(200000, 2000000)) + stat_summary(fun.y=mean, geom="point", shape=1, size=5, color="red", fill="red")
    
  })
  
  output$woonruimte <- renderPlotly({
    
    ggplot(df, 
           aes(x = `Living space size (m2)`, 
               y = Price)) +
      geom_point(stat = "identity", color = "black", position = position_dodge()) + ggtitle("Funda") + 
      xlab("m2") + ylab("price") +  theme_linedraw() + coord_cartesian(ylim = c(149000, 2000000), xlim = c(50, 400)) + 
      geom_smooth(method='lm')
  })
  
  output$map0 <- renderLeaflet({
    
    map0
    
  })
  
  output$map1 <- renderLeaflet({
    
    map1
    
  })
  
  output$table <- renderDataTable({
    
    df <- df
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)





