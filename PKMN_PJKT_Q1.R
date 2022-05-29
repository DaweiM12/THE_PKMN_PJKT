### packages ###
library(dplyr)
library(httr)
library(rvest)
library(stringr)

### scraping Pokedex ###
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

### legendaries, sublegendaries, and mythicals by pokedex number ###
gen1_dex <- c("144","145","146","150","151")
gen2_dex <- c("243","244","245","249","250","251")
gen3_dex <- c("377","378","379","380","381","382","383","384","385","386")
gen4_dex <- c("480","481","482","483","484","485","486","487","488","489","490","491","492","493")
gen5_dex <- c("494","638","639","640","641","642","643","644","645","646","647","648","649")
gen6_dex <- c("716","717","718","719","720")
gen7_dex <- c("772","773","785","789","790","791","792","800","802","807","808","809")
gen8_dex <- c("888","890","891","892","893","894","895","896", "897","898")

legendaries <- c(gen1_dex,
                 gen2_dex,
                 gen3_dex,
                 gen4_dex,
                 gen5_dex,
                 gen6_dex,
                 gen7_dex,
                 gen8_dex)

all_leg <- PKMN_STAT %>%
  filter(PKMN_No %in% legendaries)

#inputting generation
all_df <- data.frame("Generation"= c(replicate(5,1),
                                     replicate(6,2),
                                     replicate(10,3),
                                     replicate(14,4),
                                     replicate(13,5),
                                     replicate(5,6),
                                     replicate(12,7),
                                     replicate(10,8)),
                     "."=all_leg[!duplicated(all_leg$`#`),])

##average stats per generation (only including original form)
gen1 <- all_leg[c(1,3,5,7,10),]
gen2 <- all_leg[11:16,]
gen3 <- all_leg[c(18,19,20,22,24,26,28,30,31),]
gen4 <- all_leg[c(35,36,37,38,39,40,41,42,44,45,46,47,48,50),]
gen5 <- all_leg[c(51,52,53,54,55,56,57,59,60,61,63,66,68,70),]
gen6 <- all_leg[c(71,72,73,76,78),]
gen7 <- all_leg[c(80,81,82,83,84,85,86,87,91,92,93,94),]
gen8 <- all_leg[c(97,99,100,102,103,104,105,106,107),]

og_avgstats <- data.frame("Gen 1"=round(colMeans(gen1[5:10]),3),
                           "Gen 2"=round(colMeans(gen2[5:10]),3),
                           "Gen 3"=round(colMeans(gen3[5:10]),3),
                           "Gen 4"=round(colMeans(gen4[5:10]),3),
                           "Gen 5"=round(colMeans(gen5[5:10]),3),
                           "Gen 6"=round(colMeans(gen6[5:10]),3),
                           "Gen 7"=round(colMeans(gen7[5:10]),3),
                           "Gen 8"=round(colMeans(gen8[5:10]),3))

## comparing the original legendaries to subsequent generations
  #https://stackoverflow.com/questions/14614710/calculate-percentage-change-in-an-r-data-frame
  #https://www.datasciencemadesimple.com/row-wise-maximum-row-maximum-in-dataframe-r-2/
best_hp <- which.max(og_avgstats[1,])
  print(best_hp)
best_atk <- which.max(og_avgstats[2,])
  print(best_atk)
best_def <- which.max(og_avgstats[3,])
  print(best_def)
best_spatk <- which.max(og_avgstats[4,])
  print(best_spatk)
best_spdef <- which.max(og_avgstats[5,])
  print(best_spdef)
best_speed <- which.max(og_avgstats[6,])
  print(best_speed)

comparison <- round((og_avgstats/stats::lag(og_avgstats$Gen.1,-1) - 1),3)
  comparison$max_change = apply(comparison[,-1], 1, max)
View(comparison)


## all re-done legendary PKMNS
  #https://stackoverflow.com/questions/21946201/remove-all-unique-rows
  #https://stackoverflow.com/questions/52117192/find-the-complement-with-the-subset-function-in-r
  #https://stackoverflow.com/questions/28680994/converting-rows-into-columns-and-columns-into-rows-using-r 
classics <- all_leg[match(unique(all_leg$`#`), all_leg$`#`), ]
  classics_avgs <- round(colMeans(classics[6:10]),3)

redone <- subset(all_leg,duplicated(`#`)|duplicated(`#`, fromLast=TRUE))
  split_redone <- subset(redone, !row.names(redone) %in% row.names(classics))
  redone_avgs <- round(colMeans(split_redone[6:10]),3)
  
comparison2 <- data.frame("classics"=classics_avgs,
                          "redone"=redone_avgs)
comparison2 <- as.data.frame(t(comparison2))

#test to compare means of classics vs redone PKMN with 95% confidence level
  #http://www.sthda.com/english/wiki/paired-samples-t-test-in-r 
results <- t.test(classics_avgs, redone_avgs, paired = TRUE, alternative = "two.sided")
  ifelse((results$p.value < 0.05), "significant difference, reject the null hypothesis",
         "no significant difference, keep the null hypothesis")
  