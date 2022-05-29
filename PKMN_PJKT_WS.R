library(rvest)
library(dplyr)
library(tidyr)


#PKMN Statistics Table
link <- "https://pokemondb.net/pokedex/all"
page <- read_html(link)

title <- page %>% html_nodes(".sortwrap") %>% html_text()

PKMN_No <- page %>% html_nodes(".infocard-cell-data") %>% html_text() %>% as.numeric()

Name <- page %>% html_nodes(".cell-name") %>% html_text()
Name <- gsub("([a-z])([A-Z])","\\1 \\2",Name)

type <- page %>% html_nodes(".cell-icon") %>% html_text()
type <- gsub("([a-z])([A-Z])","\\1 \\2",type)

total_rate <- page %>% html_nodes(".cell-total") %>% html_text() %>% as.numeric()

HP <- page %>% html_nodes(".cell-total+ .cell-num") %>% html_text() %>% as.numeric()

Attack <- page %>% html_nodes(".cell-num:nth-child(6)") %>% html_text() %>% as.numeric()

Defense <-page %>% html_nodes(".cell-num:nth-child(7)") %>% html_text() %>% as.numeric()

Sp.Atk <- page %>% html_nodes(".cell-num:nth-child(8)") %>% html_text() %>% as.numeric()

Sp.Def <- page %>% html_nodes(".cell-num:nth-child(9)") %>% html_text() %>% as.numeric()

Speed <-  page %>% html_nodes(".cell-num:nth-child(10)") %>% html_text() %>% as.numeric()

PKMN_STAT <- data.frame(PKMN_No, Name, type, total_rate, HP, Attack, Defense, Sp.Atk, Sp.Def, Speed)
names(PKMN_STAT) <- title
PKMN_STAT <- PKMN_STAT %>% separate(Type, c("type_1", "type_2"), " ") 

write.csv(PKMN_STAT, "/Users/daweimouland//PKMN_STAT.csv")

#PKMN Attack table
link <- "https://pokemondb.net/move/all"
page <- read_html(link)

title <- page %>% html_nodes(".sortwrap") %>% html_text()

atk_name <- page %>% html_nodes(".cell-name") %>% html_text()

atk_type <- page %>% html_nodes(".cell-name+ .cell-icon") %>% html_text()

atk_power <- page %>% html_nodes(".text-center+ .cell-num") %>% html_text() %>% as.numeric()

atk_acc <- page %>% html_nodes(".cell-num:nth-child(5)") %>% html_text() %>% as.numeric()

atk_effect <- page %>% html_nodes(".cell-long-text") %>% html_text()

atk_miss_prob <- page %>% html_nodes(".cell-long-text+ .cell-num") %>% html_text() %>% as.numeric()

PKMN_ATK <- data.frame(atk_name,atk_type,atk_power,atk_acc,atk_effect,atk_miss_prob)
names(PKMN_ATK) <- title

write.csv(PKMN_STAT)

###### REF ######
#https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr
#https://www.youtube.com/watch?v=v8Yh_4oE-Fs&t=170s
#https://stackoverflow.com/questions/37425019/gsub-only-part-of-pattern

