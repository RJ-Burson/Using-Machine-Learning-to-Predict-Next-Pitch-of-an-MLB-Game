
library(xgboost)
library(cvms)
library(Ecdat)
library(boot)
library(dplyr)
library(ggplot2)
library(caret)
library(UBL)
library(ROSE)
library(scutr)
library(mclust)
library(parallel)
library(SMOTEWB)
library(cli)
library(digest)
library(fastmap)
library(rlang)
library(htmltools)
library(RMySQL)
library(readxl)
library(ggplot2)                     
library(GGally)
library(caTools)
library(skimr)
library(MASS)
library(caret)
library(class)
library(ROCR)
library(ISLR)
library(boot)
library(tree)
library(randomForest)
library(gbm)
library(tidyverse)
library(cutpointr)
library(car)
library(glmnet)
library(e1071)
library(dplyr)
library(summarytools)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(dplyr)
library(GLMcat)
library(data.table)
library(neuralnet)
library(stringi)
library(mltools)
library(data.table)
library(kknn)



###### Creating all IDs 


# Importing all the MLB ids for all of the players

id = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-0.csv")
id1 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-1.csv")
id2 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-2.csv")
id3 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-3.csv")
id4 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-4.csv")
id5 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-5.csv")
id6 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-6.csv")
id7 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-7.csv")
id8 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-8.csv")
id9 = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-9.csv")
ida = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-a.csv")
idb = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-b.csv")
idc = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-c.csv")
idd = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-d.csv")
ide = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-e.csv")
idf = read.csv("C:/Users/014497819/Downloads/register-master/register-master/Player IDs/people-f.csv")


# Combining all of the id datasets
ids = do.call("rbind", list(id, id1, id2, id3, id4, id5, id6, id7, 
                            id8, id9, ida, idb, idc, idd, ide, idf))

# Removing all the rows that dont have an mlbid
ids = subset(ids, ids$key_mlbam != "NA")

# Cleaning up the first and last name variables so there isn't any problem matching names later on in the code
ids$name_first = trimws(ids$name_first)
ids$name_first = sub("(\\w+),\\s(\\w+)","\\2 \\1", ids$name_first)
ids$name_first = gsub(" ","", ids$name_first)
ids$name_first = iconv(ids$name_first, to="ASCII//TRANSLIT")
ids$name_last = iconv(ids$name_last, to="ASCII//TRANSLIT")


#Creating the full name variable by combining the first and last name variables
ids = ids %>% mutate(Full_Name = paste(name_first, name_last, sep = " "))
ids$Full_Name = sub("(\\w+),\\s(\\w+)","\\2 \\1", ids$Full_Name)
ids$Full_Name = trimws(ids$Full_Name)




###### Stolen base threat variable 2021


# Importing stolen base data from the 2018-2020 season
sb = read.csv("C:/Users/014497819/Desktop/Thesis/Stolen_bases for 2021.csv")


#Trimming white space and converting first and last name to ascii to match player names later on
sb$first_name = iconv(sb$first_name, to="ASCII//TRANSLIT")
sb$last_name = iconv(sb$last_name, to="ASCII//TRANSLIT")
sb$last_name = trimws(sb$last_name)

# Create a full name variable by combining first and last name variables
sb <- sb %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))


# Group data by player
stolen_bases_by_player_2021 = sb %>% group_by(Full_Name) %>% summarise(total_stolen_bases = sum(r_total_stolen_base))

#Scaling the stolen bases variable
stolen_bases_by_player_2021$stand_sb = scale(stolen_bases_by_player_2021$total_stolen_bases)

#Fixing the full name variable to match player names later on
stolen_bases_by_player_2021$Full_Name = sub("(\\w+),\\s(\\w+)","\\2 \\1", stolen_bases_by_player_2021$Full_Name)
stolen_bases_by_player_2021$Full_Name = trimws(stolen_bases_by_player_2021$Full_Name)





###### Creating catcher defensive variables 2021

# Creating blocking and caught stealing dataset for catchers

#Importing catcher Blocking data from the years 2018-2020
df_blocking = read.csv("C:/Users/014497819/Desktop/Thesis/catcher_blocking for 2021.csv")

#Converting to ASCII to match players later on
df_blocking$first_name = iconv(df_blocking$first_name, to="ASCII//TRANSLIT")
df_blocking$last_name = iconv(df_blocking$last_name, to="ASCII//TRANSLIT")

#Creating the full name variable
df_blocking <- df_blocking %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))



#Importing catcher throwing data from the years 2018-2020
df_throwing = as_tibble(read.csv("C:/Users/014497819/Desktop/Thesis/catcher_throwing for 2021.csv"))

#Converting to ASCII to match players later on
df_throwing$first_name = iconv(df_throwing$first_name, to="ASCII//TRANSLIT")
df_throwing$last_name = iconv(df_throwing$last_name, to="ASCII//TRANSLIT")

#Creating the full name variable
df_throwing <- df_throwing %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))




# Merge the two catcher data frames together by the cathers Full Name
total <- merge(df_throwing, df_blocking, by="Full_Name")

# Only selecting columns needed from the catching data
catcher_data_2021 = total[, c(1,10, 34)]


# Scaling the throwing and blocking variables
catcher_data_2021 <- 
  catcher_data_2021 %>% 
  mutate(stand_blocking = scale(catcher_data_2021$blocks_above_average), stand_throwing = scale(catcher_data_2021$caught_stealing_above_average))

#Fixing the full name variable to match player names later on
catcher_data_2021$Full_Name = trimws(catcher_data_2021$Full_Name)
catcher_data_2021$Full_Name = sub("(\\w+),\\s(\\w+)","\\2 \\1", catcher_data_2021$Full_Name)
catcher_data_2021$Full_Name = iconv(catcher_data_2021$Full_Name, to="ASCII//TRANSLIT")





###### OPS variable 2021


#Importing OPS dataset for 2021 from the 2018-2020 seasons
ops_2021 = read.csv("C:/Users/014497819/Desktop/Thesis/OPS for 2021.csv")

#Only selecting the variables needed
ops_2021 = ops_2021[, c(1,2,5)]

#Trimming whitespace and converting to ASCII to match players later on
ops_2021$first_name = iconv(ops_2021$first_name, to="ASCII//TRANSLIT")
ops_2021$last_name = iconv(ops_2021$last_name, to="ASCII//TRANSLIT")
ops_2021$last_name = trimws(ops_2021$last_name)

#Creating the full name variable
ops_2021 <- ops_2021 %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))


#Trimming whitespace and converting to ASCII to match players later on
ops_2021$Full_Name = sub("(\\w+),\\s(\\w+)","\\2 \\1", ops_2021$Full_Name)
ops_2021$Full_Name = trimws(ops_2021$Full_Name)
ops_2021$Full_Name = iconv(ops_2021$Full_Name, to="ASCII//TRANSLIT")

#Only selecting the variables needed
ops_2021 = ops_2021[, c(3,4)]


#Averaging players OPS over the previous three years
ops_2021 = ops_2021 %>% group_by(Full_Name) %>% summarise(mean_on_base_plus_slg = mean(on_base_plus_slg))





###### Creating stolen base threat variable 2022


# Importing stolen base data from the 2019-2021 season to add runner stolen base threats
sb = read.csv("C:/Users/014497819/Desktop/Thesis/Stolen_bases for 2022.csv")


# Converting to ascii and trimming white space to match player names later on 
sb$first_name = iconv(sb$first_name, to="ASCII//TRANSLIT")
sb$last_name = iconv(sb$last_name, to="ASCII//TRANSLIT")
sb$last_name = trimws(sb$last_name)

# Create a full name variable by combining first and last name variables 
sb <- sb %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))

# Group data by player
stolen_bases_by_player_2022 = sb %>% group_by(Full_Name) %>% summarise(total_stolen_bases = sum(r_total_stolen_base))

#Scaling the stolen bases variable
stolen_bases_by_player_2022$stand_sb = scale(stolen_bases_by_player_2022$total_stolen_bases)


#Fixing the full name variable to match player names later on
stolen_bases_by_player_2022$Full_Name = sub("(\\w+),\\s(\\w+)","\\2 \\1", stolen_bases_by_player_2022$Full_Name)
stolen_bases_by_player_2022$Full_Name = trimws(stolen_bases_by_player_2022$Full_Name)




###### Creating catcher defensive variables 2022


#BLOCKING DATA

# Importing catcher Blocking data from the 2019-2021 seasons
df_blocking = read.csv("C:/Users/014497819/Desktop/Thesis/catcher_blocking for 2022.csv")

#Converting to ASCII to match players later on
df_blocking$first_name = iconv(df_blocking$first_name, to="ASCII//TRANSLIT")
df_blocking$last_name = iconv(df_blocking$last_name, to="ASCII//TRANSLIT")

#Creating the full name variable in the catcher blocking data
df_blocking <- df_blocking %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))



#THROWING DATA

#Importing catcher throwing data from the years 2018-2020
df_throwing = as_tibble(read.csv("C:/Users/014497819/Desktop/Thesis/catcher_throwing for 2022.csv"))

#Converting to ASCII to match players later on
df_throwing$first_name = iconv(df_throwing$first_name, to="ASCII//TRANSLIT")
df_throwing$last_name = iconv(df_throwing$last_name, to="ASCII//TRANSLIT")

#Creating the full name variable in the catcher blocking data
df_throwing <- df_throwing %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))


#Merging the two datasets by the full name of the catchers
total <- merge(df_throwing, df_blocking, by="Full_Name")

#Only selecting the variables needed
catcher_data_2022 = total[, c(1,10, 34)]


#Scaling the catcher blocking and throwing stats
catcher_data_2022 <- 
  catcher_data_2022 %>% 
  mutate(stand_blocking = scale(catcher_data_2022$blocks_above_average), stand_throwing = scale(catcher_data_2022$caught_stealing_above_average))


#Fixing the full name variable to match player names later on
catcher_data_2022$Full_Name = trimws(catcher_data_2022$Full_Name)
catcher_data_2022$Full_Name = sub("(\\w+),\\s(\\w+)","\\2 \\1", catcher_data_2022$Full_Name)
catcher_data_2022$Full_Name = iconv(catcher_data_2022$Full_Name, to="ASCII//TRANSLIT")



###### OPS variable 2022


# Importing the OPS dataset for 2022 from the 2019-2021 seasons
ops_2022 = read.csv("C:/Users/014497819/Desktop/Thesis/OPS for 2022.csv")

#Only selecting the variables needed
ops_2022 = ops_2022[, c(1,2,5)]

#Trimming whitespace and converting to ASCII to match players later on
ops_2022$first_name = iconv(ops_2022$first_name, to="ASCII//TRANSLIT")
ops_2022$last_name = iconv(ops_2022$last_name, to="ASCII//TRANSLIT")
ops_2022$last_name = trimws(ops_2022$last_name)

#Creating the full name variable
ops_2022 <- ops_2022 %>% mutate(Full_Name = paste(first_name, last_name, sep = " "))


#Trimming whitespace and converting to ASCII to match players later on
ops_2022$Full_Name = sub("(\\w+),\\s(\\w+)","\\2 \\1", ops_2022$Full_Name)
ops_2022$Full_Name = trimws(ops_2022$Full_Name)
ops_2022$Full_Name = iconv(ops_2022$Full_Name, to="ASCII//TRANSLIT")

#Only selecting the variables needed
ops_2022 = ops_2022[, c(3,4)]


#Averaging players OPS over the previous three years
ops_2022 = ops_2022 %>% group_by(Full_Name) %>% summarise(mean_on_base_plus_slg = mean(on_base_plus_slg))



###### Building the new train set


# Importing all the pitches from the 2021 season
df = read.csv("C:/Users/014497819/Desktop/Thesis/2021 Pitches.csv")

#Replacing all variables that have players id #s into the players names by matching players full names in the 2021 pitches dataset and the mlbids dataset
df[["batter"]] = ids[ match(df[["batter"]], ids[["key_mlbam"]]), 'Full_Name']
df[["pitcher"]] = ids[ match(df[["pitcher"]], ids[["key_mlbam"]]), 'Full_Name']
df[["fielder_2"]] = ids[ match(df[["fielder_2"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_3b"]] = ids[ match(df[["on_3b"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_2b"]] = ids[ match(df[["on_2b"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_1b"]] = ids[ match(df[["on_1b"]], ids[["key_mlbam"]]), 'Full_Name']


#Creating variables for the catchers and baserunners and populating them with NAs
dfv = cbind(df, catcher_blocking=NA, catcher_throwing = NA, sb_threat1= NA, sb_threat2= NA, sb_threat3= NA, OPS = NA) 


# Replacing the NAs with actual players scaled stats. Do so by matching players names from the ids dataset and the catcher blocking/throwing, stolen bases, and OPS datasets
dfv$catcher_throwing =  catcher_data_2021[ match(dfv$fielder_2, catcher_data_2021$Full_Name), 'stand_throwing']
dfv$catcher_blocking =  catcher_data_2021[ match(dfv$fielder_2, catcher_data_2021$Full_Name), 'stand_blocking']
dfv$sb_threat1 =  stolen_bases_by_player_2021[ match(dfv$on_1b, stolen_bases_by_player_2021$Full_Name), 'stand_sb']
dfv$sb_threat2 =  stolen_bases_by_player_2021[ match(dfv$on_2b, stolen_bases_by_player_2021$Full_Name), 'stand_sb']
dfv$sb_threat3 =  stolen_bases_by_player_2021[ match(dfv$on_3b, stolen_bases_by_player_2021$Full_Name), 'stand_sb']
dfv$OPS =  ops_2021[match(dfv$batter, ops_2021$Full_Name), 'mean_on_base_plus_slg']


# pick only the columns of data that are needed
dfv = dfv[,c(8,9,11,16,17,19,20,26,27,33:38,43,78:84,94:99)] 


### Replace all the on base variables that are populated with 'LAST PLAYER' and replace with 0 
dfv$sb_threat1[is.na(dfv$sb_threat1)] = 0  
dfv$sb_threat2[is.na(dfv$sb_threat2)] = 0
dfv$sb_threat3[is.na(dfv$sb_threat3)] = 0

#unlisting the stolen base threat variables and OPS variable
dfv$sb_threat1 = unlist(dfv$sb_threat1)
dfv$sb_threat2 = unlist(dfv$sb_threat2)
dfv$sb_threat3 = unlist(dfv$sb_threat3)
dfv$OPS = unlist(dfv$OPS)


# Replacing remaining NA values in the OPS and catcher variables with the mean values of each respective variable. The existing NAs still remain because some catchers didnt play enough to qualify for some the the statistics. So they arent in the OPS or catching datasets and no way to match their names.  
m =  mean(dfv$OPS, na.rm = TRUE)
dfv$OPS = ifelse(is.na(dfv$OPS), m, dfv$OPS)

l =  mean(dfv$catcher_throwing , na.rm = TRUE)
dfv$catcher_throwing = ifelse(is.na(dfv$catcher_throwing), l, dfv$catcher_throwing) 

i=  mean(dfv$catcher_blocking, na.rm = TRUE)
dfv$catcher_blocking = ifelse(is.na(dfv$catcher_blocking), i, dfv$catcher_blocking)



# Making batter stance, pitcher handedness, and what half of the inning it is into factors
dfv$stand = as.factor(ifelse(dfv$stand=="R",1,0))## Determining batter stance
dfv$p_throws = as.factor(ifelse(dfv$p_throws=="R",1,0))
dfv$inning_topbot = as.factor(ifelse(dfv$inning_topbot=="Top",1,0))



# Getting rid of the variable pitch name
new_train = dfv[,-c(1,5,16)]

# Filtering the data to have only pitches who threw at least 500 pitches in 2021
new_train = new_train %>% group_by(pitcher) %>% filter(n()>= 500) %>% ungroup()

#Getting rid of pitches that dont have a pitch name for whatever reason
new_train = new_train[-(which(new_train$pitch_name %in% "")),]

#Changing one of the pitch names to "Splitter" 
new_train$pitch_name = ifelse(new_train$pitch_name== "Split-Finger", "Splitter", new_train$pitch_name)

#Combining similar pitches (Slurve, Slow Curve, Knuckle Curve, and Curveball)
new_train$pitch_name = ifelse(new_train$pitch_name=="Slurve", "Curveball", new_train$pitch_name)
new_train$pitch_name = ifelse(new_train$pitch_name=="Slow Curve", "Curveball", new_train$pitch_name)
new_train$pitch_name = ifelse(new_train$pitch_name=="Knuckle Curve", "Curveball", new_train$pitch_name)





###### Building the new validation set


# Importing all the pitches from the 2022 season Pre All-Star break
df = read.csv("C:/Users/014497819/Desktop/Thesis/2022 Pitches Pre All-Star.csv")

#Replacing all variables that have players id #s into the players names by matching players full names in the 2021 pitches dataset and the mlbids dataset
df[["batter"]] = ids[ match(df[["batter"]], ids[["key_mlbam"]]), 'Full_Name']
df[["pitcher"]] = ids[ match(df[["pitcher"]], ids[["key_mlbam"]]), 'Full_Name']
df[["fielder_2"]] = ids[ match(df[["fielder_2"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_3b"]] = ids[ match(df[["on_3b"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_2b"]] = ids[ match(df[["on_2b"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_1b"]] = ids[ match(df[["on_1b"]], ids[["key_mlbam"]]), 'Full_Name']


#Creating variables for the catchers and baserunners and populating them with NAs
dfv = cbind(df, catcher_blocking=NA, catcher_throwing = NA, sb_threat1= NA, sb_threat2= NA, sb_threat3= NA, OPS = NA) 


# Replacing the NAs with actual players scaled stats. Do so by matching players names from the ids dataset and the catcher blocking/throwing, stolen bases, and OPS datasets
dfv$catcher_throwing =  catcher_data_2022[ match(dfv$fielder_2, catcher_data_2022$Full_Name), 'stand_throwing']
dfv$catcher_blocking =  catcher_data_2022[ match(dfv$fielder_2, catcher_data_2022$Full_Name), 'stand_blocking']
dfv$sb_threat1 =  stolen_bases_by_player_2022[ match(dfv$on_1b, stolen_bases_by_player_2022$Full_Name), 'stand_sb']
dfv$sb_threat2 =  stolen_bases_by_player_2022[ match(dfv$on_2b, stolen_bases_by_player_2022$Full_Name), 'stand_sb']
dfv$sb_threat3 =  stolen_bases_by_player_2022[ match(dfv$on_3b, stolen_bases_by_player_2022$Full_Name), 'stand_sb']
dfv$OPS =  ops_2022[ match(dfv$batter, ops_2022$Full_Name), 'mean_on_base_plus_slg']


# pick only the columns of data that are needed
dfv = dfv[,c(8,9,11,16,17,19,20,26,27,33:38,43,78:84,94:99)] 


### Replace all the on base variables that are populated with 'LAST PLAYER' and replace with 0
dfv$sb_threat1[is.na(dfv$sb_threat1)] = 0
dfv$sb_threat2[is.na(dfv$sb_threat2)] = 0
dfv$sb_threat3[is.na(dfv$sb_threat3)] = 0

#unlisting the stolen base threat variables and OPS variable
dfv$sb_threat1 = unlist(dfv$sb_threat1)
dfv$sb_threat2 = unlist(dfv$sb_threat2)
dfv$sb_threat3 = unlist(dfv$sb_threat3)
dfv$OPS = unlist(dfv$OPS)


# Replacing remaining NA values in the OPS and catcher variables with the mean values of each respective variable. The existing NAs still remain because some catchers didnt play enough to qualify for some the the statistics. So they arent in the OPS or catching datasets and no way to match their names. 
m = mean(dfv$OPS, na.rm = TRUE)
dfv$OPS = ifelse(is.na(dfv$OPS), m, dfv$OPS)

l =  mean(dfv$catcher_throwing , na.rm = TRUE)
dfv$catcher_throwing = ifelse(is.na(dfv$catcher_throwing), l, dfv$catcher_throwing) 

i=  mean(dfv$catcher_blocking, na.rm = TRUE)
dfv$catcher_blocking = ifelse(is.na(dfv$catcher_blocking), i, dfv$catcher_blocking)


# Making batter stance, pitcher handedness, and what half of the inning it is into factors
dfv$stand = as.factor(ifelse(dfv$stand=="R",1,0))## Determining batter stance
dfv$p_throws = as.factor(ifelse(dfv$p_throws=="R",1,0))
dfv$inning_topbot = as.factor(ifelse(dfv$inning_topbot=="Top",1,0))



# Getting rid of the variable pitch name
vald_set = dfv[,-c(1,5,16)]


#Getting rid of pitches that dont have a pitch name for whatever reason
vald_set = vald_set[-(which(vald_set$pitch_name %in% "")),]

#Changing one of the pitch names to "Splitter" 
vald_set$pitch_name = ifelse(vald_set$pitch_name== "Split-Finger", "Splitter", vald_set$pitch_name)

#Combine Some like pitches (Slurve, Slow Curve, Knuckle Curve, and Curveball)
vald_set$pitch_name = ifelse(vald_set$pitch_name=="Slurve", "Curveball", vald_set$pitch_name)
vald_set$pitch_name = ifelse(vald_set$pitch_name=="Slow Curve", "Curveball", vald_set$pitch_name)
vald_set$pitch_name = ifelse(vald_set$pitch_name=="Knuckle Curve", "Curveball", vald_set$pitch_name)




###### Building the new test set


# Importing all the pitches from the 2022 season Post All-Star break
df = read.csv("C:/Users/014497819/Desktop/Thesis/2022 Pitches Post All-Star.csv")

#Replacing all variables that have players id #s into the players names by matching players full names in the 2021 pitches dataset and the mlbids dataset
df[["batter"]] = ids[ match(df[["batter"]], ids[["key_mlbam"]]), 'Full_Name']
df[["pitcher"]] = ids[ match(df[["pitcher"]], ids[["key_mlbam"]]), 'Full_Name']
df[["fielder_2"]] = ids[ match(df[["fielder_2"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_3b"]] = ids[ match(df[["on_3b"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_2b"]] = ids[ match(df[["on_2b"]], ids[["key_mlbam"]]), 'Full_Name']
df[["on_1b"]] = ids[ match(df[["on_1b"]], ids[["key_mlbam"]]), 'Full_Name']


#Creating variables for the catchers and baserunners and populating them with NAs
dfv = cbind(df, catcher_blocking=NA, catcher_throwing = NA, sb_threat1= NA, sb_threat2= NA, sb_threat3= NA, OPS = NA) 


# Replacing the NAs with actual players scaled stats. Do so by matching players names from the ids dataset and the catcher blocking/throwing, stolen bases, and OPS datasets
dfv$catcher_throwing =  catcher_data_2022[ match(dfv$fielder_2, catcher_data_2022$Full_Name), 'stand_throwing']
dfv$catcher_blocking =  catcher_data_2022[ match(dfv$fielder_2, catcher_data_2022$Full_Name), 'stand_blocking']
dfv$sb_threat1 =  stolen_bases_by_player_2022[ match(dfv$on_1b, stolen_bases_by_player_2022$Full_Name), 'stand_sb']
dfv$sb_threat2 =  stolen_bases_by_player_2022[ match(dfv$on_2b, stolen_bases_by_player_2022$Full_Name), 'stand_sb']
dfv$sb_threat3 =  stolen_bases_by_player_2022[ match(dfv$on_3b, stolen_bases_by_player_2022$Full_Name), 'stand_sb']
dfv$OPS =  ops_2022[ match(dfv$batter, ops_2022$Full_Name), 'mean_on_base_plus_slg']


# pick only the columns of data that are needed
dfv = dfv[,c(8,9,11,16,17,19,20,26,27,33:38,43,78:84,94:99)] 


### Replace all the on base variables that are populated with 'LAST PLAYER' and replace with 0
dfv$sb_threat1[is.na(dfv$sb_threat1)] = 0
dfv$sb_threat2[is.na(dfv$sb_threat2)] = 0
dfv$sb_threat3[is.na(dfv$sb_threat3)] = 0

#unlisting the stolen base threat variables and OPS variable
dfv$sb_threat1 = unlist(dfv$sb_threat1)
dfv$sb_threat2 = unlist(dfv$sb_threat2)
dfv$sb_threat3 = unlist(dfv$sb_threat3)
dfv$OPS = unlist(dfv$OPS)


# Replacing remaining NA values in the OPS and catcher variables with the mean values of each respective variable. The existing NAs still remain because some catchers didnt play enough to qualify for some the the statistics. So they arent in the OPS or catching datasets and no way to match their names. 
m = mean(dfv$OPS, na.rm = TRUE)
dfv$OPS = ifelse(is.na(dfv$OPS), m, dfv$OPS)

l =  mean(dfv$catcher_throwing , na.rm = TRUE)
dfv$catcher_throwing = ifelse(is.na(dfv$catcher_throwing), l, dfv$catcher_throwing) 

i=  mean(dfv$catcher_blocking, na.rm = TRUE)
dfv$catcher_blocking = ifelse(is.na(dfv$catcher_blocking), i, dfv$catcher_blocking)



# Making batter stance, pitcher handedness, and what half of the inning it is into factors
dfv$stand = as.factor(ifelse(dfv$stand=="R",1,0))## Determining batter stance
dfv$p_throws = as.factor(ifelse(dfv$p_throws=="R",1,0))
dfv$inning_topbot = as.factor(ifelse(dfv$inning_topbot=="Top",1,0))



# Getting rid of the variable pitch name
test_set = dfv[,-c(1,5,16)]


#Getting rid of pitches that dont have a pitch name for whatever reason
test_set = test_set[-(which(test_set$pitch_name %in% "")),]

#Changing one of the pitch names to "Splitter" 
test_set$pitch_name = ifelse(test_set$pitch_name== "Split-Finger", "Splitter", test_set$pitch_name)

#Combine Some like pitches (Slurve, Slow Curve, Knuckle Curve, and Curveball)
test_set$pitch_name = ifelse(test_set$pitch_name=="Slurve", "Curveball", test_set$pitch_name)
test_set$pitch_name = ifelse(test_set$pitch_name=="Slow Curve", "Curveball", test_set$pitch_name)
test_set$pitch_name = ifelse(test_set$pitch_name=="Knuckle Curve", "Curveball", test_set$pitch_name)





###### Find which pitchers threw 500 pitches in both 2021 and 2022


#Combinging the Pre all-star break and post all-star break dataset to create a dataset containing all the pitches from the 2022 season
new_2022 = rbind(vald_set, test_set)

# Filtering the 2022 season dataset to have only pitches who threw at least 500 pitches in 2021 
new_2022 = new_2022 %>% group_by(pitcher) %>% filter(n()>= 500) %>% ungroup()


#Finding the pitchers who were in each of the 3 datasets. The 2021 season, 2022 pre all star break, and 2022 post all star break
h= unique(new_2022$pitcher)
j=unique(vald_set$pitcher)
n=unique(test_set$pitcher)

# Finding the pitchers who were in all three of the datasets
o = subset(new_train, new_train$pitcher %in% h)
d= subset(o, o$pitcher %in% j)
e= subset(d, d$pitcher %in% n)


i = unique(e$pitcher)

# Did it this complicated way because there are pitchers who threw 500 pitches in both seasons but are not in the validation or test_set because they got injured or came back from an injury halfway through the season. Walker Buehler is one example who got injured halfway through the 2022 season.





###### new validation/test 


#Creating the new training, validation, and test sets with only the pitchers that threw 500 pitches in both the 2021 and 2022 seasons and were in all3 datasets (2021 season, pre all-star break 2022, and post all-star break 2022)
new_train = new_train[new_train$pitcher %in% i,]
vald_set = vald_set[vald_set$pitcher %in% i,]
test_set = test_set[test_set$pitcher %in% i,]





###### Previous pitch and previous pitch result (TRAIN)


#Creating the three variables related to the previous pitch of an at bat for the training data
new_col_name = "prev_pitch"
new_col_value = "pitch_name"

new_col_name1 = "prev_pitch_result"
new_col_value1 = "description"

new_col_name2 = "prev_pitch_location"
new_col_value2 = "zone"

dfvnew_train <- new_train %>% group_by(new_train$pitcher) %>%
  mutate(!!new_col_name := lead(!!as.symbol(new_col_value))) %>% 
  mutate(!!new_col_name1 := lead(!!as.symbol(new_col_value1))) %>% 
  mutate(!!new_col_name2 := lead(!!as.symbol(new_col_value2)))


# Making the 3 previous pitch variable values = 0 when it is the first pitch of an at bat 
dfvnew_train$prev_pitch = ifelse(dfvnew_train$pitch_number == 1, 0, dfvnew_train$prev_pitch)
dfvnew_train$prev_pitch_result = ifelse(dfvnew_train$pitch_number == 1, 0, dfvnew_train$prev_pitch_result)
dfvnew_train$prev_pitch_location = ifelse(dfvnew_train$pitch_number == 1, 0, dfvnew_train$prev_pitch_location)

#Removing the columns not needed
dfvnew_train = dfvnew_train[, -c(2,3,8,9,10,27)]

#Making the previous pitch variables factors
dfvnew_train$prev_pitch =  as.factor(dfvnew_train$prev_pitch)
dfvnew_train$prev_pitch_result = as.factor(dfvnew_train$prev_pitch_result)
dfvnew_train$prev_pitch_location = as.factor(dfvnew_train$prev_pitch_location)


#Code to change factor variable levels from strings into integers for the variables pitch name, previous pitch, and the previous pitch result

#Pitch Name
dfvnew_train <- dfvnew_train %>% 
  mutate(pitch_name = as.factor(case_when((pitch_name == "4-Seam Fastball") ~ 1,
                                          (pitch_name == "Changeup") ~ 2,
                                          (pitch_name == "Curveball") ~ 3,
                                          (pitch_name == "Cutter") ~ 4,
                                          (pitch_name == "Sinker") ~ 5,
                                          (pitch_name == "Slider") ~ 6,
                                          (pitch_name == "Splitter") ~ 7,
                                          (pitch_name == "Sweeper") ~ 8)))


#Previous Pitch
dfvnew_train <- dfvnew_train %>% 
  mutate(prev_pitch = as.factor(case_when((prev_pitch == "0") ~ 0,
                                          (prev_pitch == "4-Seam Fastball") ~ 1,
                                          (prev_pitch == "Changeup") ~ 2,
                                          (prev_pitch == "Curveball") ~ 3,
                                          (prev_pitch == "Cutter") ~ 4,
                                          (prev_pitch == "Sinker") ~ 5,
                                          (prev_pitch == "Slider") ~ 6,
                                          (prev_pitch == "Splitter") ~ 7,
                                          (prev_pitch == "Sweeper") ~ 8)))



#Previous pitch result
dfvnew_train <- dfvnew_train %>% 
  mutate(prev_pitch_result = as.factor(case_when((prev_pitch_result == "0") ~ 0,
                                                 (prev_pitch_result == "ball") ~ 1,
                                                 (prev_pitch_result == "foul") ~ 2,
                                                 (prev_pitch_result == "called_strike") ~ 3,
                                                 (prev_pitch_result == "swinging_strike") ~ 4,
                                                 (prev_pitch_result == "blocked_ball") ~ 1,
                                                 (prev_pitch_result == "foul_tip") ~ 4,
                                                 (prev_pitch_result == "swinging_strike_blocked") ~ 4,
                                                 (prev_pitch_result == "foul_bunt") ~ 2,
                                                 (prev_pitch_result == "missed_bunt") ~ 4,
                                                 (prev_pitch_result == "bunt_foul_tip") ~ 4)))


# Omitting the final observations that still may have NAs in them
dfvnew_train= na.omit(dfvnew_train)




###### Previous pitch and previous pitch result (VALIDATION)



#Creating the three variables related to the previous pitch of an at bat for the validation data
new_col_name = "prev_pitch"
new_col_value = "pitch_name"

new_col_name1 = "prev_pitch_result"
new_col_value1 = "description"

new_col_name2 = "prev_pitch_location"
new_col_value2 = "zone"

dfvnew_valid <- vald_set %>% group_by(vald_set$pitcher) %>%
  mutate(!!new_col_name := lead(!!as.symbol(new_col_value))) %>% 
  mutate(!!new_col_name1 := lead(!!as.symbol(new_col_value1))) %>% 
  mutate(!!new_col_name2 := lead(!!as.symbol(new_col_value2)))


# Making the 3 previous pitch variable values = 0 when it is the first pitch of an at bat
dfvnew_valid$prev_pitch = ifelse(dfvnew_valid$pitch_number == 1, 0, dfvnew_valid$prev_pitch)
dfvnew_valid$prev_pitch_result = ifelse(dfvnew_valid$pitch_number == 1, 0, dfvnew_valid$prev_pitch_result)
dfvnew_valid$prev_pitch_location = ifelse(dfvnew_valid$pitch_number == 1, 0, dfvnew_valid$prev_pitch_location)


#Removing the columns not needed
dfvnew_valid = dfvnew_valid[, -c(2,3,8,9,10,27)]


#Making the previous pitch variables factors
dfvnew_valid$prev_pitch =  as.factor(dfvnew_valid$prev_pitch)
dfvnew_valid$prev_pitch_result = as.factor(dfvnew_valid$prev_pitch_result)
dfvnew_valid$prev_pitch_location = as.factor(dfvnew_valid$prev_pitch_location)



#Code to change factor variable levels from strings into integers for the variables pitch name, previous pitch, and the previous pitch result

#Pitch Name
dfvnew_valid <- dfvnew_valid %>% 
  mutate(pitch_name = as.factor(case_when((pitch_name == "4-Seam Fastball") ~ 1,
                                          (pitch_name == "Changeup") ~ 2,
                                          (pitch_name == "Curveball") ~ 3,
                                          (pitch_name == "Cutter") ~ 4,
                                          (pitch_name == "Sinker") ~ 5,
                                          (pitch_name == "Slider") ~ 6,
                                          (pitch_name == "Splitter") ~ 7,
                                          (pitch_name == "Sweeper") ~ 8)))


#Previous Pitch

dfvnew_valid <- dfvnew_valid %>% 
  mutate(prev_pitch = as.factor(case_when((prev_pitch == "0") ~ 0,
                                          (prev_pitch == "4-Seam Fastball") ~ 1,
                                          (prev_pitch == "Changeup") ~ 2,
                                          (prev_pitch == "Curveball") ~ 3,
                                          (prev_pitch == "Cutter") ~ 4,
                                          (prev_pitch == "Sinker") ~ 5,
                                          (prev_pitch == "Slider") ~ 6,
                                          (prev_pitch == "Splitter") ~ 7,
                                          (prev_pitch == "Sweeper") ~ 8)))



#Previous pitch result

dfvnew_valid <- dfvnew_valid %>% 
  mutate(prev_pitch_result = as.factor(case_when((prev_pitch_result == "0") ~ 0,
                                                 (prev_pitch_result == "ball") ~ 1,
                                                 (prev_pitch_result == "foul") ~ 2,
                                                 (prev_pitch_result == "called_strike") ~ 3,
                                                 (prev_pitch_result == "swinging_strike") ~ 4,
                                                 (prev_pitch_result == "blocked_ball") ~ 1,
                                                 (prev_pitch_result == "foul_tip") ~ 4,
                                                 (prev_pitch_result == "swinging_strike_blocked") ~ 4,
                                                 (prev_pitch_result == "foul_bunt") ~ 2,
                                                 (prev_pitch_result == "missed_bunt") ~ 4,
                                                 (prev_pitch_result == "bunt_foul_tip") ~ 4)))


# Omitting the final observations that still may have NAs in them
dfvnew_valid= na.omit(dfvnew_valid)




###### Previous pitch and previous pitch result (TEST)



#Creating the three variables related to the previous pitch of an at bat for the test data
new_col_name = "prev_pitch"
new_col_value = "pitch_name"

new_col_name1 = "prev_pitch_result"
new_col_value1 = "description"

new_col_name2 = "prev_pitch_location"
new_col_value2 = "zone"

dfvnew_test <- test_set %>% group_by(test_set$pitcher) %>%
  mutate(!!new_col_name := lead(!!as.symbol(new_col_value))) %>% 
  mutate(!!new_col_name1 := lead(!!as.symbol(new_col_value1))) %>% 
  mutate(!!new_col_name2 := lead(!!as.symbol(new_col_value2)))


# Making the 3 previous pitch variable values = 0 when it is the first pitch of an at bat
dfvnew_test$prev_pitch = ifelse(dfvnew_test$pitch_number == 1, 0, dfvnew_test$prev_pitch)
dfvnew_test$prev_pitch_result = ifelse(dfvnew_test$pitch_number == 1, 0, dfvnew_test$prev_pitch_result)
dfvnew_test$prev_pitch_location = ifelse(dfvnew_test$pitch_number == 1, 0, dfvnew_test$prev_pitch_location)


#Removing the columns not needed
dfvnew_test = dfvnew_test[, -c(2,3,8,9,10,27)]


#Making the previous pitch variables factors
dfvnew_test$prev_pitch =  as.factor(dfvnew_test$prev_pitch)
dfvnew_test$prev_pitch_result = as.factor(dfvnew_test$prev_pitch_result)
dfvnew_test$prev_pitch_location = as.factor(dfvnew_test$prev_pitch_location)


#Code to change factor variable levels from strings into integers for the variables pitch name, previous pitch, and the previous pitch result
#Pitch Name
dfvnew_test <- dfvnew_test %>% 
  mutate(pitch_name = as.factor(case_when((pitch_name == "4-Seam Fastball") ~ 1,
                                          (pitch_name == "Changeup") ~ 2,
                                          (pitch_name == "Curveball") ~ 3,
                                          (pitch_name == "Cutter") ~ 4,
                                          (pitch_name == "Sinker") ~ 5,
                                          (pitch_name == "Slider") ~ 6,
                                          (pitch_name == "Splitter") ~ 7,
                                          (pitch_name == "Sweeper") ~ 8)))


#Previous Pitch

dfvnew_test <- dfvnew_test %>% 
  mutate(prev_pitch = as.factor(case_when((prev_pitch == "0") ~ 0,
                                          (prev_pitch == "4-Seam Fastball") ~ 1,
                                          (prev_pitch == "Changeup") ~ 2,
                                          (prev_pitch == "Curveball") ~ 3,
                                          (prev_pitch == "Cutter") ~ 4,
                                          (prev_pitch == "Sinker") ~ 5,
                                          (prev_pitch == "Slider") ~ 6,
                                          (prev_pitch == "Splitter") ~ 7,
                                          (prev_pitch == "Sweeper") ~ 8)))



#Previous pitch result

dfvnew_test <- dfvnew_test %>% 
  mutate(prev_pitch_result = as.factor(case_when((prev_pitch_result == "0") ~ 0,
                                                 (prev_pitch_result == "ball") ~ 1,
                                                 (prev_pitch_result == "foul") ~ 2,
                                                 (prev_pitch_result == "called_strike") ~ 3,
                                                 (prev_pitch_result == "swinging_strike") ~ 4,
                                                 (prev_pitch_result == "blocked_ball") ~ 1,
                                                 (prev_pitch_result == "foul_tip") ~ 4,
                                                 (prev_pitch_result == "swinging_strike_blocked") ~ 4,
                                                 (prev_pitch_result == "foul_bunt") ~ 2,
                                                 (prev_pitch_result == "missed_bunt") ~ 4,
                                                 (prev_pitch_result == "bunt_foul_tip") ~ 4)))


# Omitting the final observations that still may have NAs in them
dfvnew_test= na.omit(dfvnew_test)





#################################################################################################


########## Here is where you balance the data for the pitcher trying to be predicted ############



#################################################################################################



###### KNN on final test set


start.time <- Sys.time()

classifier_knn <- knn(train = train_set, 
                      test = final_test, 
                      cl = train_set$pitch_name, 
                      k=1)


cm <- table(classifier_knn, final_test$pitch_name)
confusionMatrix(cm)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


