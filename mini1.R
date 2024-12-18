house= read.csv("house1.csv")
str(house)
summary(house)
house<- na.omit(house)
areas=unique(house$area_type)
print(areas)
locations=unique(house$location)
print(locations)

df=house[-1:-2]
print(df)

#Analysing the data using histogram
hist(df$bath,col = "blue",xlab = "No of bathrooms")
hist(df$balcony,col = "magenta",xlab = "No of balconies")
hist(df$total_sqft,col = "yellow",xlab = "Total  square feet",xlim = c(0,10000))
hist(df$bedrm,col = "green",xlab="no of bedrooms",xlim = c(0,20))
hist(df$price,col="black",xlab = "Price",xlim = c(0,1000))

#relationship of price with total square feet,balcony,bedrooms
plot(x=df$price,y=df$bedrm,col="red",ylim = c(0,10),xlim = c(0,500),xlab = "Price",ylab = "No of bedrooms")
plot(x=df$price,y=df$bath,col="blue",xlim = c(0,500),xlab = "Price",ylab = "No of bathrooms")
plot(x=df$price,y=df$balcony,col="black",xlim = c(0,200),xlab = "Price",ylab = "No of balcony")
plot(x=df$price,y=df$total_sqft,col="red",xlim = c(0,200),ylim=c(0,500),xlab = "Price",ylab = "Total Square feet")

#correlation 
cor(df)
cor(df$price,df$bedrm)
cor(df$price,df$total_sqft)
cor(df$price,df$bath)
cor(df$price,df$balcony)
pairs(~price+bath+balcony+total_sqft+bedrm,data = df)


#regression model
model=lm(price~bedrm+total_sqft+bath+balcony,data=df)
summary(model)
print(model)


  

area_type_input <- readline("Enter the area type: ")
location_input <- readline("Enter the location: ")
total_sqft_input <- as.numeric(readline("Enter the total square feet: "))
bath_input <- as.numeric(readline("Enter the number of bathrooms: "))
balcony_input <- as.numeric(readline("Enter the number of balconies: "))
num_rooms_input <- as.numeric(readline("Enter the number of bedrooms: "))


# Create a new data frame with user inputs
new_data <- data.frame(
  area_type = area_type_input,
  location = location_input,
  total_sqft = total_sqft_input,
  bath = bath_input,
  balcony = balcony_input,
  bedrm = num_rooms_input
)

# Predict the price for the new data
new_price <- predict(model, new_data)
print(paste("Predicted Price for user input is:", new_price))

#top 10 priced houses
top10priced <- head(house[order(-house$price), ], 10)
print(top10priced)


# Create a pie chart
pie(top10priced$price, labels = top10priced$price,col=colors(length(top10priced)))

# Position the legend in the top-left corner
legend("topleft", legend = top10priced$location, cex = 0.8, bty = "n",fill=colors(length(top10priced)))

