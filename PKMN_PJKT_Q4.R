setwd('C:/Users/Levi Dvorak/Documents/New Computer/R')

library(rvest)
library(tibble)
library(dplyr)
library(httr)
library(tidyr)
library(tidyverse)
library(ggplot2)


# here are the observation of elemental types 

PKMN_TYPE_TOTAL <- PKMN_STAT %>% count(PKMN_STAT$Type)

# water seems to be the most common. excluding the PKMN that have 
# two elemental types



# -----------------------------------------------------------------------------



# What is the most common primary type?

primary.type.df <- data.frame(sum(PKMN_STAT$type_1 == 'Normal', na.rm = TRUE), 
                              sum(PKMN_STAT$type_1 == 'Fire', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Water', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Grass', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Flying', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Fighting', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Poison', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Electric', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Ground', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Rock', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Psychic', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Ice', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Bug', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Ghost', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Steel', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Dragon', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Dark', na.rm = TRUE),
                              sum(PKMN_STAT$type_1 == 'Fairy', na.rm = TRUE))
names(primary.type.df) <- c('Normal', 'Fire', 'Water', 'Grass', 'Flying', 
                            'Fighting', 'Poison', 'Electric', 'Ground',
                            'Rock', 'Psychic', 'Ice', 'Bug', 'Ghost',
                            'Steel', 'Dragon', 'Dark', 'Fairy')
which.max(primary.type.df)
primary.type.df$Water

new.primary.type.df <- rbind(primary.type.df$Normal, primary.type.df$Fire, 
                             primary.type.df$Water, primary.type.df$Grass,
                             primary.type.df$Flying, primary.type.df$Fighting,
                             primary.type.df$Poison, primary.type.df$Electric,
                             primary.type.df$Ground, primary.type.df$Rock, 
                             primary.type.df$Psychic, primary.type.df$Ice,
                             primary.type.df$Bug, primary.type.df$Ghost, 
                             primary.type.df$Steel, primary.type.df$Dragon,
                             primary.type.df$Dark, primary.type.df$Fairy)
mean(new.primary.type.df)
sd(new.primary.type.df)
type.mean <- mean(new.primary.type.df)
type.sd <- sd(new.primary.type.df)
y <- dnorm(new.primary.type.df, type.mean, type.sd)
plot(new.primary.type.df, y, xlab = "Number of Pokemon")
plot(new.primary.type.df, y, type = 'h', xlab = "Number of Pokemon")

# As we can see from the which.max() function, the most common primary type is 
# Water. The significance of this is clear in 2 different ways.
# 1) As a Pokemon player, statistically speaking, it would be wise to have a 
# Grass or Electric Pokemon in your party. This is due to the fact that:
options(digits = 4)
(primary.type.df$Water / PKMN_TYPE_TOTAL$n) * 100
# as the code above shows, 12.82% of all Pokemon are Water. The odds of you 
# coming across an opponents with a Water is higher than any other type. Having
# a Grass or Electric Pokemon in your party immediately puts you at an 
# advantage if this assumption holds true as they are both super effective 
# against Water.
# 2) Again from the perspective of a Pokemon player, if statistically, the odds 
# of you battling someone with a Water Pokemon is high, it is best to try and
# avoid having any Fire, Ground, or Rock type Pokemon in your party. 
((primary.type.df$Ground + primary.type.df$Rock + primary.type.df$Fire) / 
  PKMN_TYPE_TOTAL$n) * 100
# As the above code shows, 15.89% of all Pokemon are Ground, Rock, and Fire. 
# IN SUMMARY: With Water as the most common type, statistically it would make
# sense for every Pokemon player to:
# 1) Have a Grass or Electric type Pokemon in your party
# 2) Do not have a Fire, Ground, or Rock type Pokemon in your party.



# -----------------------------------------------------------------------------



# From the above summary point #1, should you choose Grass or Electric?

# First we need to define 2 things:
# 1) What is Electric super effective against? - Flying, Water
# 2) What is Grass super effective against? - Ground, Rock, Water

# For this analysis we will 
# 1) take the average atk_power for Electric and Grass type Pokemon 
# respectively. 
# 2) Then compare them to the average HP for each type they are super 
# effective against. 
# Finally, we will compare the atk_power for Electric and Grass type Pokemon
# with the average HP for the types they are super effective against. z

# 1) Average attack power for Electric moves, and for Grass moves.
avg.atk.elec <- na.omit(subset(PKMN_ATK, atk_type == 'Electric'))
avg.atk.elec <- mean(avg.atk.elec$atk_power)
avg.atk.grass <- na.omit(subset(PKMN_ATK, atk_type == 'Grass'))
avg.atk.grass <- mean(avg.atk.grass$atk_power)

# 2) a) Health points for every Flying and Water type, and then the average.
avg.fly.hp <- na.omit(subset(PKMN_STAT, type_1 == 'Flying'))
avg.water.hp <- na.omit(subset(PKMN_STAT, type_1 == 'Water'))
avg.water.fly.hp <- (mean(avg.fly.hp$HP) + mean(avg.water.hp$HP)) / 2

# b) Health points for every Ground, Rock, and Water type, and then the average.
avg.ground.hp <- na.omit(subset(PKMN_STAT, type_1 == 'Ground'))
avg.rock.hp <- na.omit(subset(PKMN_STAT, type_1 == 'Rock'))
avg.water.hp <- na.omit(subset(PKMN_STAT, type_1 == 'Water'))
avg.g.r.w.hp <- (mean(avg.ground.hp$HP) + mean(avg.rock.hp$HP) + 
                   mean(avg.water.hp$HP)) / 3

# Now that we have all the data we can analyze. The average attack power for
# all Electric types is 79.41. 
avg.atk.elec
# The average attack power for all Grass types is 81.82. 
avg.atk.grass
# Based off of this one analysis, one could choose Grass as on average it has a 
# higher attack power stat. But let's look further by comparing them each to 
# the types they are most effective against. 

# Next we look into the specific comparison of types for Electric type 
# Pokemon.
elec.vs.water <- (avg.atk.elec - mean(avg.water.hp$HP))
elec.vs.flying <- (avg.atk.elec - mean(avg.fly.hp$HP))

# Now the same step for Grass type Pokemon
grass.vs.water <- (avg.atk.grass - mean(avg.water.hp$HP))
grass.vs.rock <- (avg.atk.grass - mean(avg.rock.hp$HP))
grass.vs.ground <- (avg.atk.grass - mean(avg.ground.hp$HP))

avg.water.fly.hp - avg.atk.elec
# The average health points of all Flying and Water types against the average
# Electric attack leaves the average Flying and Water type with -3.89 HP.

# Now the same thing but with Grass against Ground, Rock, and Water.
avg.g.r.w.hp - avg.atk.grass
# The result would leave the average Ground, Rock, and Water type Pokemon with
# -8.83 HP. 

# Diving further into our analysis confirmed our initial belief:
# Choose a Grass type over an Electric type.
