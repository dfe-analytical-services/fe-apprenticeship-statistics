#### 
# 1. Load packages ----

#library (dplyr) -- Ricks packages
#library (ggplot2)
#library(stringr)
#library(reshape2)
library(leaflet)
library(geojsonio)
library(rgdal)
library(sp)
library(data.table)
library(RColorBrewer)
library(raster)

library(pander)
library(tidyverse)
library(shinycssloaders)
library(plotly)

library(DT)
library(ggalt)
library(magrittr)

library (rpivotTable)
library (reshape2)
library(grid)
library(packrat)

##
#Ricks script
##setwd("~/Nov app sandbox")
data_ini <- read.csv("appagelevel.csv")
options(scipen = 999)

startsbyage <- data_ini %>% 
  group_by(academic_year,Age) %>% 
  summarise(Starts=round(sum(Starts),-1))# %>% mutate_each(funs(prettyNum(., big.mark=",")))
#startsbyage <- startsbyage%>%  select (Starts)%>% mutate_each(funs(prettyNum(., big.mark=","))) ## add big.mark for comma separated

startsbyage$Age <- str_wrap(startsbyage$Age, width = 5)
startsbyage$Age <- factor(startsbyage$Age, levels = c("Under\n19","19-24","25+"))
startsbyage$academic_year <- paste0(substr(as.character(startsbyage$academic_year),1,4),"/",
                                    substr(as.character(startsbyage$academic_year),5,6))
#startsbyage <- startsbyage%>%  select (Starts)%>% mutate_each(funs(prettyNum(., big.mark=",")))

age_plot <- ggplot(data=startsbyage) +
    geom_col(mapping = aes(x=Age, y=Starts, fill=Age))+
    facet_grid(.~academic_year, switch = "x") +
    labs(x="Age-group by academic year", y="Starts", fill="Age-group")+
  scale_fill_manual(values=c("lightblue3","blue","navy"))+
  scale_y_continuous(breaks = seq(0,250000,50000),expand = c(0, 0), 
                     limits = c(0,250000), 
                     labels = function(d){paste0(format(d,big.mark=",",trim=TRUE))}) +
  theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour="grey80"), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        strip.background.x = element_rect(linetype = 0),
        strip.text.x = element_text(size = 9),
        axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8))

startsbylev <- data_ini %>% 
  group_by(academic_year,Level) %>% 
  summarise(Starts=round(sum(Starts),-1))

startsbylev <- startsbylev %>% filter(Level !='NA')
startsbylev$Level <- factor(startsbylev$Level, levels = 
                              c("Intermediate Apprenticeship",
                                "Advanced Apprenticeship",
                                "Higher Apprenticeship"))
startsbylev$academic_year <- paste0(substr(as.character(startsbylev$academic_year),1,4),"/",
                                    substr(as.character(startsbylev$academic_year),5,6))

level_plot <- ggplot(data=startsbylev) +
  geom_col(mapping = aes(x=Level, y=Starts, fill=Level))+
  facet_grid(.~academic_year,switch = "x")+
  labs(x="Level by academic year", y="Starts", fill="Level")+
  scale_fill_manual(values=c("lightblue3","blue","navy")) +
  scale_y_continuous(breaks = seq(0,300000,50000),expand = c(0, 0), 
                     limits = c(0,300000), 
                     labels = function(d){paste0(format(d,big.mark=",",trim=TRUE))}) +
  theme_classic() +
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(colour="grey80"), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        strip.background.x = element_rect(linetype = 0),
        strip.text.x = element_text(size = 9),
        axis.line = element_line(colour = "grey80"),
        axis.ticks = element_line(colour = "grey80"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=8),
        legend.text = element_text(size=8))
########
#Jon's script
#######
####
# 2. General functions, formatting years and rounding ----

# Change the year variable into xxxx/xx format

#formatyr <- function(refyear) {

#  sub("(.{4})(.*)", "\\1/\\2", refyear)

#}

# example
# formatyr(201213)
# = 2012/13



####
# 3. Load the data required ----

# load main_ud file
# includes main measures at school, la, region and national level for 2006/07 to 2015/16

# 1415 - historic info
main_ud <- read_csv('data/poc_2_1415.csv', col_types = cols(.default = "c"))
poc3<- read_csv('data/poc3.csv', col_types = cols(.default = "c"))

#glimpse(main_ud)

# load reason_ud file
# includes la, region and national level for 2006/07 to 2015/16

#POC Output - 1617
#poc_ud <- read_csv('data/poc3.csv', col_types = cols(.default = "c")) # Old script
#poc_ud <- read_csv('data/PT3_2.csv', col_types = cols(.default = "c")) # New script
poc_ud <- read_csv('data/PT3_2_num_2.csv', col_types = cols(.default = "c")) # New script


#SFR Output
SFR_ud <- read_csv('data/sfr.csv', col_types = cols(.default = "c"))
# head(poc_ud)

#Commit Output
nat_commit <- read_csv('data/commit_ud.csv', col_types = cols(.default = "c"))

# characteristics UD

#char_ud <- read_csv('data/SFR35_2017_National_characteristics.csv', col_types = cols(.default = "c"))

####
# 4. Front page ----

## national plots

# create a national summary table (used for plots): -- Based code - FE_Trends.R

nat_summary <-
  dplyr::select(main_ud, 
                Academic_Year,
                SSA_T1,
                # Age,
                # Level,
                # Gender,
                #Framework, 
                Starts , 
                Achievements
                
  ) %>%
  arrange(Academic_Year)

nat_summary[nat_summary=="-"]<-0

nat_summary$Starts <- as.numeric(nat_summary$Starts)
nat_summary$Achievements <- as.numeric(nat_summary$Achievements)

# Aggregate
nat_summary<-group_by(nat_summary, Academic_Year,SSA_T1) %>% summarise_all(sum) #%>% mutate(Starts = sum(Starts), Achievements = sum(Achievements)) 




## Quarterly Commitments
nat_summary <- # eror agge not found
  dplyr::select(
    filter(SFR_ud, level == 'All Apprenticeships'), #,Age=="Total"
    Quarter, Age,Value) %>%
  arrange(Quarter)

nat_commit <- # eror agge not found
  dplyr::select(nat_commit, Date,AllAges_P,AllAges_F,AllAges_T)%>%
  arrange(Date)

# National bar chart (front page)

national_bars <- function(category) { # was x
  if (category== 'P') {
    data <- filter(nat_summary, Age == 'Total') %>%
      mutate(Quarter = as.factor(Quarter),
             Value = as.numeric(Value))
    
    p_bar_g <- 
      ggplot(data, aes(x = Quarter, y = Value)) + #formatyr(Quarter)
      geom_bar(fill = 'dodgerblue4', stat = "identity") +
      theme_classic() +
      ylab("Apprenticeships") +
      xlab("Academic Quarters") +
      #scale_y_continuous(breaks = seq(0, max(data$Value + 0.01), 0.02)) + generates 1m line
      theme(axis.title.x = element_blank())
    #plot(p_bar_g) 
    
    return(ggplotly(p_bar_g))
  }
  if (category == 'F') { # update with stacked plot
    
    data2 <- select(nat_commit, Date,AllAges_T)
    #%>% mutate(Date=as.Date(Date), AllAges_T = as.numeric(AllAges_T))
    
    f_bar_g <- 
      ggplot(data2, aes(x = Date, y = AllAges_T)) +
      geom_bar(fill = 'red', stat = "identity") +
      theme_classic() +
      ylab("Commitments") +
      xlab("Monthly Figures") +
      #scale_y_continuous(breaks = seq(0, max(data$value + 0.5), 0.50)) +
      theme(axis.title.x = element_blank())
    
    return(ggplotly(f_bar_g))
  }
}


####
# 4. LA trends ----
#Starts & Achievements
#la_plot_data <-
#  dplyr::select(poc_ud, 
#                level,la_name,SSA,Age,Starts,Achievements) # Old script
#filter(data, Level=='Totals', `SSA T1`=='Totals',Region=='Totals',PCON=='Totals',`PCON Code`=='Totals',LAD != 'Totals',`LAD Code` != 'Totals',Age=='Totals')

la_plot_data <-
  dplyr::filter(poc_ud, Region=='Totals',PCON=='Totals',`PCON Code`=='Totals',LAD != 'Totals',`LAD Code` != 'Totals')#%>%
  la_plot_data <-
  dplyr::  select(la_plot_data, Level,LAD,`SSA T1`,Age,`1617_Starts`,`1617_Achievements`) 
  
  
colnames(la_plot_data) <- c("level","la_name","SSA","Age","Starts","Achievements")
# Sort SSA small to large
la_plot_data <- within(la_plot_data, 
                       SSA <- factor(SSA, 
                                     levels=names(sort(table(SSA), 
                                                       decreasing=FALSE))))
## Order two
#la_plot_data$SSA <- factor(la_plot_data$SSA, levels="Health, Public Services and Care","Science and Mathematics","Agriculture, Horticulture and Animal Care","Engineering and Manufacturing Technologies","Construction, Planning and the Built Environment","Information and Communication Technology (ICT)","Retail and Commercial Enterprise","Leisure, Travel and Tourism", "Arts, Media and Publishing","Education and Training","Business, Administration, Finance and Law","Unknown")

la_plot_data$Starts <- as.numeric(la_plot_data$Starts)
la_plot_data$Achievements <- as.numeric(la_plot_data$Achievements) # New scipt - Error : object 'SSA' not found


### LA time series tables

la_table_num <- function(la, age, level) {
  
  d <- dplyr::select(la_plot_data, level:Starts)
  
  return(d)
}  







####
# 5. MAP ----

ukLocalAuthoritises <- shapefile("data/shapefiles/England_LAUA_2016.shp")
ukLocalAuthoritises <- spTransform(ukLocalAuthoritises, CRS("+proj=longlat +ellps=GRS80"))

# Filter for England with %like% "E"
englishLocalAuthorities = subset(ukLocalAuthoritises, LAD16CD %like% "E") # Code begins with E - reference is case sensitive.

# English Data
#data <- read_csv("data/POC3_tom.csv") # old script
data <- read_csv("data/PT3_2.csv") # new script
data <- dplyr::filter(data, Level=='Totals', `SSA T1`=='Totals',Region=='Totals',PCON=='Totals',`PCON Code`=='Totals',LAD != 'Totals',`LAD Code` != 'Totals',Age=='Totals') %>%
    select(`LAD Code`,LAD,`1617_Starts`,`1617_Achievements`) 
colnames(data)[1] <- "new_lad_code"
colnames(data)[2] <- "la_name"
colnames(data)[3] <- "starts"
colnames(data)[4] <- "achievements"
data$starts <- as.numeric(data$starts)
data$achievements <- as.numeric(data$achievements)

# Merge data together by LAD to get map data
mapData <- merge(englishLocalAuthorities, 
                 data, 
                 by.x = 'LAD16CD', 
                 by.y = 'new_lad_code',
                 all.y = TRUE)

####
# 3. Create mapping components ----

# Generate the bins of the data you want to have as colours
binsStart <- c(0, 500, 15000, 3000, 4500, 6000,7500) # Note - extended bin range and elements 5 to 6

# Tell R what you want to map by variable wise
palStart <- colorBin("YlOrRd", domain = data$starts, bins = binsStart)



# Add a label for tooltip (bit of html to add variables and data in)
start_labels <- sprintf("<strong>%s</strong><br/>Starts <strong>%s</strong>",
                        mapData$LAD16NM, paste(as.character(mapData$starts), "")) %>%
  
  
  lapply(htmltools::HTML)



# Generate the bins of the data you want to have as colours
binsAchievements <- c(0, 1500, 3000, 4500, 6000, 11000)

# Tell R what you want to map by variable wise
palAchievements <- colorBin("YlOrRd", domain = data$achievements, bins = binsAchievements)



# Add a label for tooltip (bit of html to add variables and data in)
achievement_labels <- sprintf("<strong>%s</strong><br/>Achievements <strong>%s</strong>",
                              mapData$LAD16NM, paste(as.character(mapData$achievements), "")) %>%
  
  
  lapply(htmltools::HTML)



map <- function(measure) {
  
  if(measure == 'starts') {
    
    
    ####
    # 4. Generate map ----
    
    map <- leaflet(mapData) %>% 
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 7, maxZoom = 10)) %>%
      addPolygons(fillColor = ~palStart(starts),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  label = start_labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto",
                    opacity = 1))  %>%
      addLegend(colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#C74375", "#808080"), #https://en.wikipedia.org/wiki/Pantone
                opacity = 0.7, 
                title = NULL,
                position = "topright",
                labels= c("Lowest starts", "","","","","","Highest starts")) %>%
      setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95)
    
    
    
    
    
    
  }
  
  if(measure == 'achievements') {
    
    map <-leaflet(mapData) %>% 
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 7, maxZoom = 10)) %>%
      addPolygons(fillColor = ~palAchievements(achievements),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  label = achievement_labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto",
                    opacity = 1))  %>%
      addLegend(colors = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026", "#808080"), 
                opacity = 0.7, 
                title = NULL,
                position = "topright",
                labels= c("Lowest achievements", "","","","Highest achievements", "No data")) %>%
      setMaxBounds(lat1 = 55.5, lng1 = -6.8, lat2 = 49.99, lng2 = 1.95)
    
  }
  
  return(map)
  
}

####
# 4. LA Breakdown  ----
################
# Testing
#################
# Starts & Achievements bar charts - testing scenarios
#la <- "Wigan"
#age <- "19-24"
#age<-'Under 19'
#level <- "Totals"
#level<-'Intermediate Apprenticeship'
#level<-"Higher Apprenticeship" # Note only four rows

#la_plot_data_wigan<-dplyr::filter(la_plot_data,level=='Higher Apprenticeship',la_name=='Wigan',Age=='Under 19')
#################





###########
#Deve Work
## Note - need to summarise!!!! %>% 
#group_by(academic_year,Age) %>% 
 # summarise(Starts=round(sum(Starts),-1))

#  data <- filter(reason_ud,
#                 level == 'National',
#                 school_type == schtype,
#                 year >= 201112) %>%
#    select(year, perm_physical_pupils:perm_other)

#  data_long <- data %>% gather(key = reason,
#                               value = exc,
#                               perm_physical_pupils:perm_other)

#  return(data_long %>% spread(key = year, value =  exc))
perm_reason_table  <- function(la,age, level_select) {
  #d <- filter(main_ud, level == "School",la_name == la) %>% 
  d <- filter(la_plot_data, la_name==la,Age==age,level==level_select,SSA!='Totals') %>%
    select(
      #     year,
      SSA,
      la_name,
      level,
      Age,
      Starts
      #Achievements
      # perm_excl_rate,
      # fixed_excl,
      # fixed_excl_rate,
      #  one_plus_fixed,
      #  one_or_more_fixed_excl_rate
    )
  
  #return(d %>% select(SSA,Achievements))
  return(d %>% select(SSA,Starts)%>%mutate_each(funs(prettyNum(., big.mark=","))))
}


fixed_reason_table  <- function(la,age, level_select) {
  #d <- filter(main_ud, level == "School",la_name == la) %>% 
  d <- filter(la_plot_data, la_name==la,Age==age,level==level_select,SSA!='Totals') %>%
    select(
      #     year,
      SSA,
      la_name,
      level,
      Age,
      # Starts
      Achievements
      # perm_excl_rate,
      # fixed_excl,
      # fixed_excl_rate,
      #  one_plus_fixed,
      #  one_or_more_fixed_excl_rate
    )
  
  #return(d %>% select(SSA,Achievements))
  return(d %>% select(SSA,Achievements)%>%mutate_each(funs(prettyNum(., big.mark=","))))
}


perm_reason_bar <- function(la,age, level_select){
  
  data <- filter(la_plot_data, la_name==la,Age==age,level==level_select,SSA!='Totals') %>% # try Level - doesn't change
    select(
      #     year,
      SSA,
      la_name,
      level,
      Age,
      Starts
      #      Achievements
      # perm_excl_rate,
      # fixed_excl,
      # fixed_excl_rate,
      #  one_plus_fixed,
      #  one_or_more_fixed_excl_rate
    )%>% arrange(SSA)
  
  return(ggplot(data=data, aes(x=SSA,y=as.numeric(Starts))) +
           geom_bar(fill = 'steelblue4',stat="identity")+
           theme_classic() +
           coord_flip() +
           ylab("Number of Starts"))
  
}

fixed_reason_bar <- function(la,age, level_select){
  
  data <- filter(la_plot_data, la_name==la,Age==age,level==level_select,SSA!='Totals') %>%
    select(
      #     year,
      SSA,
      la_name,
      level,
      Age,
      # Starts
      Achievements
    )%>% arrange(SSA)
  
  
  return(ggplot(data=data, aes(x = SSA,y=as.numeric(Achievements))) +
           geom_bar(fill = 'steelblue4',stat="identity")+
           theme_classic() +
           coord_flip() +
           ylab("Number of Achievements"))
  
}

########
#Clear Data objects
########
#rm(list=ls())
