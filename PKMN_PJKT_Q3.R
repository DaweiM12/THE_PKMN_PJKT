install.packages('rvest')
install.packages('tibble')
install.packages('dplyr')
install.packages('httr')
install.packages('tidyr')
install.packages('tidyverse')

library('rvest')
library('tibble')
library('dplyr')
library('httr')
library('tidyr')
library('tidyverse')


##Include only primary type in the table

PKMN_STAT_CLEAN <- cbind(PKMN_STAT[ ,1:3], PKMN_STAT[ ,5:11])

##Retrieves a list of the different types

TYPE_LIST <- PKMN_STAT_CLEAN$type_1

TYPE_LIST <- unique(TYPE_LIST)

numberOfTypes <- length(TYPE_LIST)

##Creates a list of stats

statsList <- c("HP", "Attack", "Defense", "Sp.Atk", "Sp.Def", "Speed")

numberOfStats <- length(statsList)

##Creates a new data frame to store the average stats of each type

averageStats <- data.frame("Type" = TYPE_LIST, "HP" = NA, "Attack" = NA, "Defense" = NA, "Sp.Atk" = NA, "Sp.Def" = NA, "Speed" = NA, "StatTotal" = NA)

##For each type, filter the full list of Pokemon for just Pokemon with the primary type.

for(i in 1:numberOfTypes){
  
  type <- TYPE_LIST[i]
  
  eval(parse(text = paste("'", type, "'", '<- PKMN_STAT_CLEAN %>% filter(type_1 ==', "'",type,"'", ')', sep = '')))
 
  eval(paste(text = paste(type, '<- data.frame(',type,')', sep = '')))
   
}

##For each type, run the mean() function on each of the stats and then put it into a 
##data frame. 

for(i in 1:numberOfTypes){
  
  typeName <- TYPE_LIST[i]
  
  for(j in 1:numberOfStats){
    
    eval(parse(text = paste('averageStats[i, j+1] <- mean(',typeName,'[ ,j+4])', sep = '')))
    
  }
  
  averageStats[i, 8] <- sum(averageStats[i, 2:7])
  
}

### Calculate the correlation coefficients of each of the scatter plots 

AtkDefCorr <- cor(PKMN_STAT$Attack, PKMN_STAT$Defense)

SpAtkSpDefCorr <- cor(PKMN_STAT$`Sp. Atk`, PKMN_STAT$`Sp. Def`)

AtkDefCorr

SpAtkSpDefCorr

# Writes the PKMN_STAT and averageStats data franes to separate CSV files for visualization

write.csv(PKMN_STAT, "PKMN_STAT.csv")

write.csv(averageStats, "average_stats.csv")
