# Alicia Kaggle project of predicting Airbnb price using machine learning algorithms

# Libraries ---------------------------------------------------------------
library(caret)
library(dplyr)
library(xgboost)
library(ggplot2)
library(tidyr)

# Set working directory ----------------------------------------------------
setwd('/Users/apple/Desktop/kaggle')
getwd()

# Read in analysisData and scoringData -------------------------------------
data <- read.csv('analysisData.csv')
scoringData <- read.csv('scoringData.csv')

#Delete useless dirty variables in analysisData and scoringData -------------
data <- select(data, -listing_url,-scrape_id,-last_scraped,-name,-summary,-space,
               -description,-experiences_offered,-neighborhood_overview,-notes,
               -transit,-access,-interaction,-house_rules,-thumbnail_url,-medium_url,
               -xl_picture_url,-host_url,-host_name,-host_about,
               -jurisdiction_names,-picture_url,
               -host_acceptance_rate,-host_thumbnail_url,
               -amenities,-first_review,-last_review,-requires_license,-license,
               -country,-country_code,-has_availability,-calendar_last_scraped)

scoringData <- select(scoringData, -listing_url,-scrape_id,-last_scraped,-name,-summary,-space,
                      -description,-experiences_offered,-neighborhood_overview,-notes,
                      -transit,-access,-interaction,-house_rules,-thumbnail_url,-medium_url,
                      -xl_picture_url,-host_url,-host_name,-host_about,
                      -jurisdiction_names,-picture_url,
                      -host_acceptance_rate,-host_thumbnail_url,
                      -amenities,-first_review,-last_review,-requires_license,-license,
                      -country,-country_code,-has_availability,-calendar_last_scraped)

# Further delete useless variables after careful observation ------------------
as.data.frame(table(data$host_location))
as.data.frame(table(data$state))
as.data.frame(table(data$market))
as.data.frame(table(data$host_neighbourhood))
as.data.frame(table(data$street))
as.data.frame(table(data$smart_location))
summary(data$weekly_price)
summary(data$monthly_price)
summary(data$square_feet)

data <- select(data,-host_location,-state,-market,-host_neighbourhood,-street,-smart_location)
data <- select(data,-weekly_price,-monthly_price,-square_feet)

scoringData <- select(scoringData,-host_location,-state,-market,-host_neighbourhood,-street,-smart_location)
scoringData <- select(scoringData,-weekly_price,-monthly_price,-square_feet)

# Data visualization (numeric)---------------------------------------------------
dataClean <- predict(preProcess(data, method = 'medianImpute'),
                             newdata = data)
data_numeric = dataClean[sapply(dataClean, class) == 'numeric' |  
                              sapply(dataClean, class) == 'integer' | 
                              sapply(dataClean, class) == 'logic']
corMatrix = as.data.frame(cor(data_numeric))
corMatrix$var1 = rownames(corMatrix)
corMatrix %>%
  gather(key=var2,value=r,1:30)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'red',high='green',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))

# Data visualization (non-numeric)------------------------------------------------
subset_1  <- dataClean[,c('price','host_is_superhost','host_has_profile_pic','host_identity_verified')]
pairs(subset_1,pch = 21,cex = .7,col="black",lwd= 0.5)

subset_2  <- dataClean[,c('price','room_type','property_type','require_guest_profile_picture','instant_bookable','is_location_exact')]
pairs(subset_2,pch = 21,cex = .7,col="red",lwd= 0.5)

subset_3  <- dataClean[,c('price','is_business_travel_ready','cancellation_policy','require_guest_phone_verification','instant_bookable')]
pairs(subset_3,pch = 21,cex = .7,col="blue",lwd= 0.5)

# Make columns of analysisData and scoringData be the same --------------------
scoringDataWithPrice <- scoringData
scoringDataWithPrice$price <- NA

# Combine all data together ----------------------------------------------------
combinedData <- rbind(data, scoringDataWithPrice)

# Select useful variables based on data visualization ---------------------------
combinedDataSelected <- select(combinedData,price,host_is_superhost,host_listings_count,
                    latitude,longitude,host_identity_verified,id,host_id,
                     room_type,accommodates,bathrooms,bedrooms,beds,
                    cleaning_fee,security_deposit,guests_included,is_location_exact,
                    extra_people,minimum_nights,maximum_nights,host_has_profile_pic,
                     availability_30,availability_90,availability_365,
                    require_guest_profile_picture,
                     is_business_travel_ready,review_scores_value,instant_bookable,
                    review_scores_location,review_scores_rating,host_identity_verified,
                    review_scores_cleanliness,review_scores_communication,
                    number_of_reviews,reviews_per_month,calculated_host_listings_count,
                    require_guest_phone_verification)

# Fill in missing values (NAs) -------------------------------------------------
combinedDataClean <- predict(preProcess(combinedDataSelected, method = 'medianImpute'),
                             newdata = combinedDataSelected)

# Convert categorical to numeric variables -------------------------------------
levels(combinedDataClean$host_is_superhost) <- c(0,1)
levels(combinedDataClean$host_identity_verified) <- c(0,1)
levels(combinedDataClean$host_has_profile_pic) <- c(0,1)
levels(combinedDataClean$host_identity_verified) <- c(0,1)
levels(combinedDataClean$is_location_exact) <- c(0,1)
levels(combinedDataClean$instant_bookable) <- c(0,1)
levels(combinedDataClean$is_business_travel_ready) <- c(0,1)
levels(combinedDataClean$room_type) <- c(0,1,2)
levels(combinedDataClean$require_guest_profile_picture) <- c(0,1)
levels(combinedDataClean$require_guest_phone_verification) <- c(0,1)

#split into train and test data -----------------------------------------------
analysis <- combinedDataClean[1:29142,]
scoring <- combinedDataClean[29143:36428,]

# logarithmic transformation of price -----------------------------------------
analysis <- analysis[analysis$price>0,]
scoring <- scoring[scoring$price>0,]

analysis$LogPrice <- log(analysis$price)
scoring$LogPrice <- log(scoring$price)
analysisPrice <- analysis$price
analysisLogPrice <- analysis$LogPrice
analysis <- select(analysis,-price,-LogPrice)
scoring <- select(scoring,-price,-LogPrice)

# put our test & train data into two seperates Dmatrixs objects ---------------
dtrain <- xgb.DMatrix(data = as.matrix(sapply(analysis, as.numeric)),label=analysisLogPrice)
dtest <- xgb.DMatrix(data = as.matrix(sapply(scoring, as.numeric)))

# set parameters --------------------------------------------------------------
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.01, #default = 0.3
  gamma=0,
  max_depth=9, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)

# Do cross validation to determine the best number of rounds --------------------
xgbcv <- xgb.cv(params = default_param, data = dtrain, nrounds = 3000, nfold = 5,
               showsd = T, stratified = T, print_every_n = 50, 
                early_stopping_rounds = 10, maximize = F)

#train the model using the best iteration found by cross validation -------------
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 1109)

# Calculate RMSE for the train data ---------------------------------------------
XGBpred <- predict(xgb_mod,newdata=dtrain)
rmse <- sqrt(mean((exp(XGBpred)-analysisPrice)^2))
rmse

# Apply the model to test data and submit the result -----------------------------
pred <- predict(xgb_mod,newdata=dtest)
predictions_XGB <- exp(pred)
submissionFile = data.frame(id = scoringData$id, price = predictions_XGB)
write.csv(submissionFile, 'xgboost_submission2.csv',row.names = F)
