library(dplyr)
library(stats)
library(ggplot2)
library(car)


###### My part ########

#question: Can you use the first evolution stats to predict the second evolution's stats

#can you use the second evolution individual stats to predict the third evolution's stats

###### REF ######
#https://www.youtube.com/watch?v=v8Yh_4oE-Fs&t=170s
#https://stackoverflow.com/questions/37425019/gsub-only-part-of-pattern

###########################################################################################################

### Set up###

Start_PKMN_first <- data.frame()

Start_PKMN_second <- data.frame()

Start_PKMN_third <- data.frame()

starts_first <- c("Bulbasaur", "Charmander","Squirtle","Chikorita","Cyndaquil","Totodile","Treecko","Torchic","Mudkip"
                  ,"Turtwig","Chimchar","Piplup","Snivy","Tepig","Oshawott","Chespin","Fennekin","Froakie","Rowlet","Litten",
                  "Popplio","Grookey","Scorbunny","Sobble")

starts_second <- c("Ivysaur" , "Charmeleon" , "Wartortle" , "Bayleef" , "Quilava" , "Croconaw" , "Grovyle" , "Combusken" , "Marshtomp" ,
                   "Grotle" , "Monferno" , "Prinplup" , "Servine" , "Pignite" , "Dewott" , "Quilladin" , "Braixen" , "Frogadier" ,
                   "Dartrix" , "Torracat" , "Brionne", "Thwackey" , "Raboot" , "Drizzile"  )

starts_third <- c("Venusaur" , "Charizard" , "Blastoise" , "Meganium" , "Typhlosion" , "Feraligatr", "Sceptile" , "Blaziken" , "Swampert" ,
                  "Torterra" , "Infernape" , "Empoleon" , "Serperior" , "Emboar" , "Samurott" , "Chesnaught" , "Delphox" , "Greninja" , 
                  "Decidueye" , "Incineroar" , "Primarina", "Rillaboom" , "Cinderace" , "Inteleon" )

Start_PKMN_first <- filter(PKMN_STAT, Name %in% starts_first)

Start_PKMN_second <- filter(PKMN_STAT, Name %in% starts_second)

Start_PKMN_third <- filter(PKMN_STAT, Name %in% starts_third)

###########################################################################################################
###########################################################################################################

#question:
starter_1 <- lm(Start_PKMN_second$Total~Start_PKMN_first$HP
                +Start_PKMN_first$Attack
                +Start_PKMN_first$Defense
                +Start_PKMN_first$`Sp. Atk`
                +Start_PKMN_first$`Sp. Def`
                +Start_PKMN_first$Speed)

summary(starter_1)

starter_1$coefficients[1]
starter_1$coefficients[2]
starter_1$coefficients[3]
starter_1$coefficients[4]
starter_1$coefficients[5]
starter_1$coefficients[6]
starter_1$coefficients[7]

mean(Start_PKMN_first$Total)
sd(Start_PKMN_first$Total)

tot <- plot(Start_PKMN_first$Total,Start_PKMN_second$Total)

abline(tot) #add line of fit to graph

FUNC2 <- function(PK) {
  (starter_1$coefficients[2]*Start_PKMN_first[Start_PKMN_second$Name == PK,6])+
    (starter_1$coefficients[3]*Start_PKMN_first[Start_PKMN_second$Name == PK,7])+
    (starter_1$coefficients[4]*Start_PKMN_first[Start_PKMN_second$Name == PK,8])+
    (starter_1$coefficients[5]*Start_PKMN_first[Start_PKMN_second$Name == PK,9])+
    (starter_1$coefficients[6]*Start_PKMN_first[Start_PKMN_second$Name == PK,10])+
    (starter_1$coefficients[7]*Start_PKMN_first[Start_PKMN_second$Name == PK,11])+ 
    starter_1$coefficients[1]
} #much more efficient function

Pred.2.STAT <- data.frame(sapply(starts_second,FUNC2))

Actual.2.STAT <- Start_PKMN_second[,5]

Diff.2.STAT <-  Actual.2.STAT - Pred.2.STAT # neg numbers mean they are underpowered, pos means overpowered


#################### Manual verification #######################
PKMN_2nd_Ev_func <- function(HP, ATK, DEF, SPA, SPB, SPD) {
  (starter_1$coefficients[2]*HP)+
    (starter_1$coefficients[3]*ATK)+
    (starter_1$coefficients[4]*DEF)+
    (starter_1$coefficients[5]*SPA)+
    (starter_1$coefficients[6]*SPB)+
    (starter_1$coefficients[7]*SPD)+ starter_1$coefficients[1]
}

x <- c(((Start_PKMN_second[Start_PKMN_second$Name == "Ivysaur",5]) - PKMN_2nd_Ev_func(45,49,49,65,65,45)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Charmeleon",5]) - PKMN_2nd_Ev_func(39,52,43,60,50,65)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Wartortle",5]) - PKMN_2nd_Ev_func(44,48,65,50,64,43)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Bayleef",5]) - PKMN_2nd_Ev_func(45,49,65,49,65,45)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Quilava",5]) - PKMN_2nd_Ev_func(39,52,43,60,50,65)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Croconaw",5]) - PKMN_2nd_Ev_func(50,65,64,44,48,43)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Grovyle",5]) - PKMN_2nd_Ev_func(40,45,35,65,55,70)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Combusken",5]) - PKMN_2nd_Ev_func(45,60,40,70,50,45)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Marshtomp",5]) - PKMN_2nd_Ev_func(50,70,50,50,50,40)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Grotle",5]) - PKMN_2nd_Ev_func(55,68,64,45,55,31)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Monferno",5]) - PKMN_2nd_Ev_func(44,58,44,58,44,61)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Prinplup",5]) - PKMN_2nd_Ev_func(53,51,53,61,56,40)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Servine",5]) - PKMN_2nd_Ev_func(45,45,55,45,55,63)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Pignite",5]) - PKMN_2nd_Ev_func(65,63,45,45,45,45)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Dewott",5]) - PKMN_2nd_Ev_func(55,55,45,63,45,45)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Quilladin",5]) - PKMN_2nd_Ev_func(56,61,65,48,45,38)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Braixen",5]) - PKMN_2nd_Ev_func(40,45,40,62,60,60)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Frogadier",5]) - PKMN_2nd_Ev_func(41,56,40,62,44,71)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Dartrix",5]) - PKMN_2nd_Ev_func(68,55,55,50,50,42)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Torracat",5]) - PKMN_2nd_Ev_func(45,65,40,60,40,70)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Brionne",5]) - PKMN_2nd_Ev_func(45,54,54,66,56,40)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Thwackey",5]) - PKMN_2nd_Ev_func(50,65,50,40,40,65)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Raboot",5]) - PKMN_2nd_Ev_func(50,71,40,40,40,69)),
       
       ((Start_PKMN_second[Start_PKMN_second$Name == "Drizzile",5]) - PKMN_2nd_Ev_func(50,40,40,70,40,70))) 

x <- data.frame(x, starts_second)

###########################################################################################################

starter_2 <- lm(Start_PKMN_third$Total~Start_PKMN_second$HP
                +Start_PKMN_second$Attack
                +Start_PKMN_second$Defense
                +Start_PKMN_second$`Sp. Atk`
                +Start_PKMN_second$`Sp. Def`
                +Start_PKMN_second$Speed)

summary(starter_2)

starter_2$coefficients[1]
starter_2$coefficients[2]
starter_2$coefficients[3]
starter_2$coefficients[4]
starter_2$coefficients[5]
starter_2$coefficients[6]
starter_2$coefficients[7]


FUNC3 <- function(PK) {
  (starter_2$coefficients[2]*Start_PKMN_second[Start_PKMN_third$Name == PK,6])+
    (starter_2$coefficients[3]*Start_PKMN_second[Start_PKMN_third$Name == PK,7])+
    (starter_2$coefficients[4]*Start_PKMN_second[Start_PKMN_third$Name == PK,8])+
    (starter_2$coefficients[5]*Start_PKMN_second[Start_PKMN_third$Name == PK,9])+
    (starter_2$coefficients[6]*Start_PKMN_second[Start_PKMN_third$Name == PK,10])+
    (starter_2$coefficients[7]*Start_PKMN_second[Start_PKMN_third$Name == PK,11])+ 
    starter_2$coefficients[1]
}

Pred.3.STAT <- data.frame(sapply(starts_third,FUNC3))

Actual.3.STAT <- Start_PKMN_third[,5]

Diff.3.STAT <-  Actual.3.STAT - Pred.3.STAT # neg numbers mean they are underpowered, pos means overpowered

###########################################################################################################
mean(Diff.2.STAT$sapply.starts_second..FUNC2.)

mean(Diff.3.STAT$sapply.starts_third..FUNC3.)
###########################################################################################################

###########################

mean(Start_PKMN_second$Total) - mean(Start_PKMN_first$Total) 


# Ho: Pokemon evolve in a predictable manner # Actual = Predicted 

# Ha: Pokemon do not evolve in a predictable manner # Actual =/= Predicted 

#alpha = 0.05, on a 0.95 confidence interval

qplot(Start_PKMN_second$Total)

shapiro.test(Start_PKMN_second$Total) #not normally distributed
mean(Start_PKMN_second$Total) #actual 2nd
sd(Start_PKMN_second$Total)

#--------------------------------------

qplot(Pred.2.STAT$sapply.starts_second..FUNC2.)

shapiro.test((Pred.2.STAT$sapply.starts_second..FUNC2.)) #not normally distributed
mean(Pred.2.STAT$sapply.starts_second..FUNC2.) #predicted 2nd
sd(Pred.2.STAT$sapply.starts_second..FUNC2.)


#--------------------------------------

wilcox.test(Start_PKMN_second$Total,Pred.2.STAT$sapply.starts_second..FUNC2., 
            alternative = "two.sided", mu = 0, paired = TRUE, exact = FALSE, conf.int = 0.95) #Fail to Reject null hypothesis

#--------------------------------------

mean(Diff.2.STAT$sapply.starts_second..FUNC2.) # Evolution are on average overpowered. 

###########################

mean(Start_PKMN_third$Total) - mean(Start_PKMN_second$Total) 

# Ho: Pokemon evolve in a predictable manner # Actual = Predicted 

# Ha: Pokemon do not evolve in a predictable manner # Actual =/= Predicted 

#alpha = 0.05 on a 0.95 confidence interval

qplot(Start_PKMN_third$Total)

shapiro.test(Start_PKMN_third$Total) #not normally distributed
mean(Start_PKMN_third$Total) #actual 3rd
sd(Start_PKMN_third$Total)

#--------------------------------------

qplot(Pred.3.STAT$sapply.starts_third..FUNC3.)

shapiro.test(Pred.3.STAT$sapply.starts_third..FUNC3.) #only one that is normally distributed 
mean(Pred.3.STAT$sapply.starts_third..FUNC3.) #predicted 3rd
sd(Pred.3.STAT$sapply.starts_third..FUNC3.)


wilcox.test(Start_PKMN_third$Total,Pred.3.STAT$sapply.starts_third..FUNC3., 
            alternative = "two.sided", mu = 0, paired = TRUE, exact = FALSE, conf.int = 0.95) #Fail to Reject null hypothesis.

#--------------------------------------

mean(Diff.3.STAT$sapply.starts_third..FUNC3.) #Positive mean indicates that third evolution PKMN are overpowered 

#Start pokemon do evolve in a predictable fashion. 

#using a 95% conf interval with an alpha of 0.05

###### OKay Recomendations ######################

#If someone wanted to rebalance the first evolution stats to be closer to the 
#actual stats they could use the below function to do so. 

##### i made a balancer #####

Balancer <- function(PKMN){
  
  actual.temp <- Start_PKMN_first[Start_PKMN_first$Name == PKMN, 5] 
  
  actual.temp.2 <- Start_PKMN_second[Start_PKMN_first$Name == PKMN, 5] 
  
  HP.pred <- (20:75)
  ATK.pred <- (20:75)
  DEF.pred <- (20:75)
  Sp.Atk.pred <- (20:75)
  Sp.Def.pred <- (20:75)
  SPD.pred <- (20:75)
  
  total.bal <- data.frame("Dif" = as.numeric(),
                          "Total" = as.numeric(), 
                          "HP" = as.numeric(),
                          "ATK" = as.numeric(),
                          "DEF" = as.numeric(),
                          "Sp.ATK" = as.numeric(),
                          "Sp.DEF" = as.numeric(),
                          "SPD" = as.numeric())
  
  
  for(i in 1:10000) {
    total.bal[i,3] <- sample(HP.pred,1)
    total.bal[i,4] <- sample(ATK.pred,1)
    total.bal[i,5] <- sample(DEF.pred,1)
    total.bal[i,6] <- sample(Sp.Atk.pred,1)
    total.bal[i,7] <- sample(Sp.Def.pred,1)
    total.bal[i,8] <- sample(SPD.pred,1)
    
    total.bal[i,2] <- (total.bal[i,3]+
                         total.bal[i,4]+
                         total.bal[i,5]+
                         total.bal[i,6]+
                         total.bal[i,7]+
                         total.bal[i,8])
    
    total.bal[i,1] <- actual.temp - total.bal[i,2]
  }
  
  
  rebal <- subset(total.bal[(total.bal$Dif<1 & total.bal$Dif>-1),])
  
  plz <- data.frame("Dif" = as.numeric(),
                    "HP" = as.numeric(),
                    "ATK" = as.numeric(),
                    "DEF" = as.numeric(),
                    "Sp.ATK" = as.numeric(),
                    "Sp.DEF" = as.numeric(),
                    "SPD" = as.numeric(),
                    "TOTAL.2nd.PRED" = as.numeric(),
                    "TOTAL.2nd.ACTUAL" = as.numeric())
  
  
  for(j in 1:nrow(rebal)) {
    
    plz[j,2] <- rebal[j,3]
    plz[j,3] <- rebal[j,4]
    plz[j,4] <- rebal[j,5]
    plz[j,5] <- rebal[j,6]
    plz[j,6] <- rebal[j,7]
    plz[j,7] <- rebal[j,8]
    plz[j,8] <- PKMN_2nd_Ev_func((rebal[j,3]),
                                 (rebal[j,4]),
                                 (rebal[j,5]),
                                 (rebal[j,6]),
                                 (rebal[j,7]),
                                 (rebal[j,8]))
    plz[j,9] <- actual.temp.2
    
    plz[j,1] <- data.frame(actual.temp.2 - PKMN_2nd_Ev_func((rebal[j,3]),
                                                            (rebal[j,4]),
                                                            (rebal[j,5]),
                                                            (rebal[j,6]),
                                                            (rebal[j,7]),
                                                            (rebal[j,8])))
    
  }
  
  return(plz[which.min(abs(plz$Dif)),])
}

results <- data.frame(sapply(starts_first,Balancer)) %>% t() 

###################adjust second evolution stats to meet third

PKMN_3rd_Ev_func <- function(HP, ATK, DEF, SPA, SPB, SPD) {
  (starter_2$coefficients[2]*HP)+
    (starter_2$coefficients[3]*ATK)+
    (starter_2$coefficients[4]*DEF)+
    (starter_2$coefficients[5]*SPA)+
    (starter_2$coefficients[6]*SPB)+
    (starter_2$coefficients[7]*SPD)+ starter_2$coefficients[1]
}


Balancer.3 <- function(PKMN){
  
  actual.temp <- Start_PKMN_second[Start_PKMN_second$Name == PKMN, 5] 
  
  actual.temp.2 <- Start_PKMN_third[Start_PKMN_second$Name == PKMN, 5] 
  
  HP.pred <- (36:93)
  ATK.pred <- (36:93)
  DEF.pred <- (36:93)
  Sp.Atk.pred <- (36:93)
  Sp.Def.pred <- (36:93)
  SPD.pred <- (36:93)
  
  total.bal <- data.frame("Dif" = as.numeric(),
                          "Total" = as.numeric(), 
                          "HP" = as.numeric(),
                          "ATK" = as.numeric(),
                          "DEF" = as.numeric(),
                          "Sp.ATK" = as.numeric(),
                          "Sp.DEF" = as.numeric(),
                          "SPD" = as.numeric())
  
  
  for(i in 1:10000) {
    total.bal[i,3] <- sample(HP.pred,1)
    total.bal[i,4] <- sample(ATK.pred,1)
    total.bal[i,5] <- sample(DEF.pred,1)
    total.bal[i,6] <- sample(Sp.Atk.pred,1)
    total.bal[i,7] <- sample(Sp.Def.pred,1)
    total.bal[i,8] <- sample(SPD.pred,1)
    
    total.bal[i,2] <- (total.bal[i,3]+
                         total.bal[i,4]+
                         total.bal[i,5]+
                         total.bal[i,6]+
                         total.bal[i,7]+
                         total.bal[i,8])
    
    total.bal[i,1] <- actual.temp - total.bal[i,2]
  }
  
  
  rebal <- subset(total.bal[(total.bal$Dif<1 & total.bal$Dif>-1),])
  
  plz <- data.frame("Dif" = as.numeric(),
                    "HP" = as.numeric(),
                    "ATK" = as.numeric(),
                    "DEF" = as.numeric(),
                    "Sp.ATK" = as.numeric(),
                    "Sp.DEF" = as.numeric(),
                    "SPD" = as.numeric(),
                    "TOTAL.2nd.PRED" = as.numeric(),
                    "TOTAL.2nd.ACTUAL" = as.numeric())
  
  
  for(j in 1:nrow(rebal)) {
    
    plz[j,2] <- rebal[j,3]
    plz[j,3] <- rebal[j,4]
    plz[j,4] <- rebal[j,5]
    plz[j,5] <- rebal[j,6]
    plz[j,6] <- rebal[j,7]
    plz[j,7] <- rebal[j,8]
    plz[j,8] <- PKMN_2nd_Ev_func((rebal[j,3]),
                                 (rebal[j,4]),
                                 (rebal[j,5]),
                                 (rebal[j,6]),
                                 (rebal[j,7]),
                                 (rebal[j,8]))
    plz[j,9] <- actual.temp.2
    
    plz[j,1] <- data.frame(actual.temp.2 - PKMN_3rd_Ev_func((rebal[j,3]),
                                                            (rebal[j,4]),
                                                            (rebal[j,5]),
                                                            (rebal[j,6]),
                                                            (rebal[j,7]),
                                                            (rebal[j,8])))
    
  }
  
  return(plz[which.min(abs(plz$Dif)),])
}


results.2 <- data.frame(sapply(starts_second,Balancer.3)) %>% t() 


##### EXPORT TO EXCEL #####

write.csv(Diff.2.STAT, file = "Diff2STAT.cvs")

write.csv(Diff.3.STAT, file = "Diff3STAT.cvs")

write.csv(starts_first, file = "first.cvs")

write.csv(Start_PKMN_first, file = "Starter_First")

write.csv(Start_PKMN_second, file = "Starter_Second")

write.csv(Start_PKMN_third, file = "Starter_Third")

write.csv(results, file = "Balance_Test")

write.csv(results.2, file = "Balance_Test2")






