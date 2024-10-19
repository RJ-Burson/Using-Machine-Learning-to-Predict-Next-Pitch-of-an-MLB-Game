R program that utilizes machine learning to predict the next pitch that an MLB pitcher will throw in a game given the situation. 

Goal

The goal of this project was to build off and try to improve on previous pitch prediction research. In this project there are individual models built for each pitcher to predict the next pitch. This approach produced better results than trying to create models with clusters of similar pitchers and then sub setting to pitchers with the same pitch arsenals. 

The Data

We used pitches from the 2021 and 2022 MLB seasons. Scraped the pitch-by-pitch data from Baseball Savant, the code to scrape this data is also a part of this repository. Only included pitchers who threw more than 500 pitches in both seasons. 

Feature Selection and Engineering

Features Selected:

•	Batter: The player at bat, used to get a player’s OPS variable later

•	Pitcher: The player who is pitching, this is how we subset the data for pitcher specific models. Used to group pitchers when finding the previous pitch variables as well.

•	Description: The description of what happened on the pitch, which was used to help determine result of previous pitch

•	Zone: Where the pitch is located, used to help determine previous pitch location

•	Handedness: Pitcher and Batter handedness

•	Count: Balls and Strikes in the count

•	Runners on Base: Gives the identity of player at a base if the base is occupied. Helps determine which bases to put engineered variable stolen base threat at. 

•	Outs: How many outs in the inning

•	Inning: What inning the pitch occurred in, as well as if it occurred in the top or bottom of the inning

•	Catcher: Who is the player catching, used to match and get engineered catcher throwing and blocking variables later

•	At bat number: Number of at bat in the game

•	Pitch Number: What pitch of the at bat, Use this to help find the variable previous pitch and to determine when a new at bat starts

•	Pitch type: This is the dependent variable we are trying to predict (Fastball, Changeup, Curveball, etc.). Will also use this as the way to find the variable previous pitch.

•	Score: Home and Away score. As well as the score of the team at bat and the score of the team in the field 

Feature Cleaning & Engineering:

•	Converted 4 variables: pitch type, stand, pitcher handedness, and top/bottom of an inning from character to numerical factors

•	Pitch type cleanup: Combined three different curveball variants Slow Curveball, Knuckle Curve, and Slurve into the standard Curveball pitch type. In total there are 8 different pitch types trying to be predicted. (Fastball, Changeup, Curveball, Cutter, Sinker, Slider, Splitter, Sweeper) 

•	Stolen Base threat variable: Took the previous 3 years of stolen base data prior to the season the pitch is in. Found the total number of stolen bases players had over those 3 years and then scaled the column to create the stolen base threat value

•	Catcher blocking and throwing stats: Like the stolen base threat variables the catcher stats were also created from the previous 3 years of data. Took the catcher’s total blocks above average and caught stealing above average and then scaled the 2 variables

•	Hitter OPS: Took the previous 3 years of data and found the players mean OPS they had over those 3 years 

•	 Since Baseball Savant data does not have player names, the ids of and names of players were matched for the engineered stolen base threats, catching stats, and OPS using the Chadwick Baseball Bureau Register 

•	For the engineered catching variables and OPS, if a player didn’t meet qualifications for a leaderboard and didn’t have a value of his own the missing value was populated with the leaguewide average. 

•	Engineered three new variables related to results of the previous pitch. Grouped by pitcher and used the lead function in R to find the information of the previous pitch. 

-	Previous pitch name: The name of the previous pitch
-	Previous pitch location: The location of the previous pitch
-	Previous pitch result: categorized all results into 1 of 5 categories: 
 -0=first pitch of an AB, 1=ball=, 2=foul, 3=called strike, 4 = swinging strike
 	
Pitcher-Specific Model

•	Subset the training, validation, and testing sets to only include pitches from one pitcher

•	Balance the training set using R function ovun.sample to oversample minority classes

•	Got rid of any observation with NA values

•	Ended up Using a KNN model. For my actual thesis I experimented with numerous other models but none produced better results or quicker computation times than the default KNN model. 
 
