FlightPassengersDataFrame<-function(Passengers=100,RandomSeed=NA, plot_Results=TRUE){
	require(randomNames)

	if (!is.na(RandomSeed)){ # If you want to control the same dataset each time, set randomseed to a number
	set.seed(RandomSeed)
	}
	
	Number_of_Passengers<-Passengers
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
	Age<- as.integer(runif(Number_of_Passengers,2,75)) # Or antoher function of      abs(as.integer(rnorm(Number_of_Passengers,35,12)))
	df$Age<-Age

	# Blood Types
	# Spread across rareness but i forgot the webstie, it was teh first on google
	BloodTypes<-c(rep("O-positive", ceiling(Number_of_Passengers*(0.38))) , rep("O-negative", ceiling(Number_of_Passengers*(0.07))) , rep("A-positive", ceiling(Number_of_Passengers*(0.34))) , rep("A-negative", ceiling(Number_of_Passengers*(0.06))) , rep("B-positive", ceiling(Number_of_Passengers*(0.09))) , rep("B-negative", ceiling(Number_of_Passengers*(0.02))) , rep("AB-positive", ceiling(Number_of_Passengers*(0.03))) , rep("AB-negative", ceiling(Number_of_Passengers*(0.01))) )
	df$BloodTypes<-sample(BloodTypes,Number_of_Passengers) # The blood type is spread based on rareness of the blood type

	# Height and Weight
	# https://www.disabled-world.com/calculators-charts/height-weight-teens.php
	# Sourced from there

	# Uncomment if you have local files
	# Male_Ht_Wt<-read.csv("male_htwt.csv") # Children Average Weights and Heights (in KG and cm) for males
	# Female_Ht_Wt<-read.csv("female_htwt.csv") # Ditto for females

	Male_Ht_Wt<-read.csv("https://raw.githubusercontent.com/ardimirzaei/FlightPassengersData/master/male_htwt.csv") # Children Average Weights and Heights (in KG and cm) for males
	Female_Ht_Wt<-read.csv("https://raw.githubusercontent.com/ardimirzaei/FlightPassengersData/master/female_htwt.csv") # Ditto for females

	df_M<-merge(df[df$gender==0,], Male_Ht_Wt, by='Age', all.x=TRUE)
	df_F<-merge(df[df$gender==1,], Female_Ht_Wt, by='Age', all.x=TRUE)
	df<-rbind(df_M,df_F)

	AdultWeights<-round(runif(sum(is.na(df$Weight)), 45, 95),1) # This is between 45 and 95 kg
	df$Weight[is.na(df$Weight)]<-AdultWeights

	AdultHeights<-round(runif(sum(is.na(df$Height)), 150, 200),1) # This is between 150 to 200 cm
	df$Height[is.na(df$Height)]<-AdultHeights

	# Beverages 
	# Made using the united airlines website https://www.united.com/web/en-US/content/travel/inflight/dining/beverages/default.aspx 
	Non_Alcoholic_Beverages<-c("Coca-Cola", "Coca-Cola Zero Sugar", "Diet Coke", "Sprite", "DASANI SPARKLING Lime", "DASANI Bottled Water", "Minute Maid Apple Juice", "Cranberry Apple Juice Cocktail", "Orange Juice", "Mott Tomato Juice", "Mr Mrs T Bloody Mary Mix", "Seagram Ginger Ale", "Seltzer Water", "Tonic Water", "Illy Dark Roast coffee regular ", "Illy Dark Roast coffee decaffeinated", "Hot tea")
	Alcoholic_Beverages<-c("Miller Lite", "Stella Artois", "Goose Island 312 Urban Wheat Ale", "Breckenridge Brewery Hop Peak IPA", "Redwood Vineyards Cabernet Sauvignon 2015", "Laboure Roi Chardonnay 2017 Vin de France", "Wheatley Craft Distilled Vodka", "Bacardi Superior Rum", "Dewar White Label Blended Scotch Whisky", "Jack Daniel Tennessee", "Baileys Irish Cream", "Bombay Sapphire Dry Gin", "Buffalo Trace Kentucky Straight Bourbon Whiskey", "Glenfarclas Single Malt Scotch Whisky")

	Beverages<-c(rep(Non_Alcoholic_Beverages,ceiling(Number_of_Passengers*0.66)), rep(Alcoholic_Beverages,ceiling(Number_of_Passengers*0.34)))
	df$Beverages<-sample(Beverages, Number_of_Passengers)
	df$Beverages[df$Age<18]<-sample(Non_Alcoholic_Beverages, sum(df$Age<18), replace=TRUE) # Makse sure the children are not hardcore alcholics

	df$BeveragesCount<-round(runif(Number_of_Passengers,1,5))
	 
	AlcoBeverageIndex<-!is.na(match(df$Beverages, Alcoholic_Beverages))
	df$BeveragesAlcoholic<-AlcoBeverageIndex
	df$BeveragesAlcoholic<-ifelse(df$BeveragesAlcoholic, "Yes", "No")

	# Food

	# FoodOptions<-c("Chicken","Beef","Vegetarin","Fish","Special")
	# Spread is based on my guess. 
	Food<-c(rep("Chicken",ceiling(Number_of_Passengers*0.35)), rep("Beef",ceiling(Number_of_Passengers*0.35)), rep("Vegetarin",ceiling(Number_of_Passengers*0.15)),rep("Fish",ceiling(Number_of_Passengers*0.10)),rep("Special",ceiling(Number_of_Passengers*0.05)))
	df$Food<-sample(Food, Number_of_Passengers)


	# Factor Encoding
	Exportable_Dataframe<-data.frame(df)

	Exportable_Dataframe$gender<-factor(Exportable_Dataframe$gender)
	levels(Exportable_Dataframe$gender)<-c('Male','Female')

	Exportable_Dataframe$ethnicity<-factor(Exportable_Dataframe$ethnicity)
	levels(Exportable_Dataframe$ethnicity)<-c("American Indian or Native Alaskan", "Asian or Pacific Islander", "Black (not Hispanic)", "Hispanic", "White (not Hispanic)", "Middle-Eastern, Arabic")


	Exportable_Dataframe$BloodTypes<-factor(Exportable_Dataframe$BloodTypes)
	levels(Exportable_Dataframe$BloodTypes)<-c("O-positive" , "O-negative" , "A-positive" , "A-negative" , "B-positive" , "B-negative" , "AB-positive" , "AB-negative" )


	Exportable_Dataframe$Food<-factor(Exportable_Dataframe$Food)
	levels(Exportable_Dataframe$Food)<-c("Chicken","Beef","Vegetarin","Fish","Special")


	Exportable_Dataframe$BeveragesAlcoholic<-factor(Exportable_Dataframe$BeveragesAlcoholic)
	levels(Exportable_Dataframe$BeveragesAlcoholic)<-c("No","Yes")

	Exportable_Dataframe<- Exportable_Dataframe[order(Exportable_Dataframe$first_name),]

	if (plot_Results){ # Will auto plot, chagne to FALSE if you don't want to plot. 
		PlotsDataFrame(Exportable_Dataframe)
	}
	


	return(Exportable_Dataframe)
}

PlotsDataFrame<-function(df){
	# Plot for quick view
	# Plot: Age, gender, ethnicity, blood types, weights, heights, beverages count, beverages alcoholic, food
	par(mfrow=c(3,3)) # Change the layout

	plot(df$Age, xlab="Age", ylab='Count')
	plot(df$gender, xlab="gender", ylab='Count')
	plot(df$ethnicity, xlab="ethnicity", ylab='Count')
	plot(df$BloodTypes, xlab="BloodTypes", ylab='Count')
	plot(df$Weight, xlab="Weight", ylab='Count')
	plot(df$Height, xlab="Height", ylab='Count')
	plot(df$BeveragesCount, xlab="BeveragesCount", ylab='Count')
	plot(df$BeveragesAlcoholic, xlab="BeveragesAlcoholic", ylab='Count')
	plot(df$Food, xlab="Food", ylab='Count')


	par(mfrow=c(1,1))
}