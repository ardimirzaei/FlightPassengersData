library(randomNames)

set.seed(1999)

Number_of_Passengers<-100
df<-randomNames(Number_of_Passengers, return.complete.data=TRUE)
df<-df[,c(3,4,1,2)]

# Seat Assignment
SeatNumbers<-matrix(character(0))
PlaneRows<-ceiling(Number_of_Passengers/6)
for(y in 1:PlaneRows){
	SeatNumbers<-c(SeatNumbers,paste0(rep(y,6),letters[1:6]))
} 
df$SeatNumbers<-SeatNumbers[1:Number_of_Passengers]

# Passenger Age
Age<- as.integer(runif(100,2,75)) # Or antoher function of      abs(as.integer(rnorm(Number_of_Passengers,35,12)))
df$Age<-Age

# Blood Types
BloodTypes<-c(rep("O-positive", ceiling(Number_of_Passengers*(38/100))) , rep("O-negative", ceiling(Number_of_Passengers*(7/100))) , rep("A-positive", ceiling(Number_of_Passengers*(34/100))) , rep("A-negative", ceiling(Number_of_Passengers*(6/100))) , rep("B-positive", ceiling(Number_of_Passengers*(9/100))) , rep("B-negative", ceiling(Number_of_Passengers*(2/100))) , rep("AB-positive", ceiling(Number_of_Passengers*(3/100))) , rep("AB-negative", ceiling(Number_of_Passengers*(1/100))) )
df$BloodTypes<-sample(BloodTypes,Number_of_Passengers) # The blood type is spread based on rareness of the blood type

# Height and Weight
# https://www.disabled-world.com/calculators-charts/height-weight-teens.php
# Sourced from there

Male_Ht_Wt<-read.csv("male_htwt.csv") # Children Average Weights and Heights (in KG and cm) for males
Female_Ht_Wt<-read.csv("female_htwt.csv") # Ditto for females

df_M<-merge(df[df$gender==0,], Male_Ht_Wt, by='Age', all.x=TRUE)
df_F<-merge(df[df$gender==1,], Female_Ht_Wt, by='Age', all.x=TRUE)
df<-rbind(df_M,df_F)

AdultWeights<-round(runif(sum(is.na(df$Weight)), 45, 95),1) # This is between 45 and 95 kg
df$Weight[is.na(df$Weight)]<-AdultWeights

AdultHeights<-round(runif(sum(is.na(df$Height)), 150, 200),1) # This is between 150 to 200 cm
df$Height[is.na(df$Height)]<-AdultHeights

# Beverages 

Non_Alcoholic_Beverages<-c("Coca-Cola", "Coca-Cola Zero Sugar", "Diet Coke", "Sprite", "DASANI SPARKLING Lime", "DASANI Bottled Water", "Minute Maid Apple Juice", "Cranberry Apple Juice Cocktail", "Orange Juice", "Mott Tomato Juice", "Mr Mrs T Bloody Mary Mix", "Seagram Ginger Ale", "Seltzer Water", "Tonic Water", "Illy Dark Roast coffee regular ", "Illy Dark Roast coffee decaffeinated", "Hot tea")
Alcoholic_Beverages<-c("Miller Lite", "Stella Artois", "Goose Island 312 Urban Wheat Ale", "Breckenridge Brewery Hop Peak IPA", "Redwood Vineyards Cabernet Sauvignon 2015", "Laboure Roi Chardonnay 2017 Vin de France", "Wheatley Craft Distilled Vodka", "Bacardi Superior Rum", "Dewar White Label Blended Scotch Whisky", "Jack Daniel Tennessee", "Baileys Irish Cream", "Bombay Sapphire Dry Gin", "Buffalo Trace Kentucky Straight Bourbon Whiskey", "Glenfarclas Single Malt Scotch Whisky")

Beverages<-c(rep(Non_Alcoholic_Beverages,ceiling(Number_of_Passengers*0.66)), rep(Alcoholic_Beverages,ceiling(Number_of_Passengers*0.34)))