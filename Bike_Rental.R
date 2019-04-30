rm(list = ls())
setwd("C:/Users/user/Documents/Github/Bike-Rental")

libraries = c("data.table", "plyr","dplyr", "ggplot2","gridExtra","rpart","dplyr","gbm","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(libraries, require, character.only = TRUE)

daily_data = read.csv('day.csv', header = T, as.is = T)
head(daily_data)
str(daily_data)
names(daily_data)
summary(daily_data)

sapply(daily_data, function(x) {sum(is.na(x))})

daily_data = subset(daily_data,select = -c(instant,dteday,casual,registered))
setnames(daily_data, old = c("yr", "mnth", "weathersit", "cnt", 'hum'), new = c('year', 'month', 'weather_type', 'total_count', "humidity"))

categorical_features = c("season","year","month","holiday","weekday","workingday","weather_type")
numerical_features = c("temp","atemp","humidity","windspeed")

daily_data[categorical_features] = lapply(daily_data[categorical_features], as.factor)

numeric_index = sapply(daily_data,is.numeric) #selecting only numeric
numeric_data = daily_data[,numeric_index]
cnames = colnames(numeric_data)
for (i in 1:length(numerical_features))
{
  assign(paste0("density", i), ggplot(aes_string(x = cnames[i]), data = numeric_data) +
           geom_density(color="green", fill="#CCFFFF") +
           ggtitle(paste("Density plot for", cnames[i])) +
           theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5)) + geom_vline(aes_string(xintercept = mean(numeric_data[,i])), color="blue", linetype="dashed", size=1))
}
grid.arrange(density1, density2, density3, density4,ncol=2)


factor_data = daily_data[,categorical_features]
fcnames = colnames(factor_data)

for( i in 1:length(fcnames))
{
  assign(paste0("bar_univarite_", i), ggplot(aes_string(x = fcnames[i]), data = factor_data) +
           geom_bar(stat = 'count', position = 'dodge', fill = "#CCFFFF", col = 'black') + 
           geom_label(stat = 'count', aes(label = ..count..),  col = 'black') +
           ggtitle(paste("Univarite bar plot for", fcnames[i])) + theme(text=element_text(size=10,  family="serif"), plot.title = element_text(hjust = 0.5) ) )
}
grid.arrange(bar_univarite_1, bar_univarite_2, bar_univarite_3, bar_univarite_4, bar_univarite_5, bar_univarite_6, bar_univarite_7,ncol=2)

for(i in 1:ncol(numeric_data)) {
  assign(paste0("box",i), ggplot(data = numeric_data, aes_string(y = numeric_data[,i])) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.size = 1) +
           labs(y = colnames(numeric_data[i])) +
           ggtitle(paste("Boxplot: ",colnames(numeric_data[i]))))
}
gridExtra::grid.arrange(box1,box2,box3,box4,box5,ncol=2)
gridExtra::grid.arrange(box5)

for(i in cnames){
  val = daily_data[,i][daily_data[,i] %in% boxplot.stats(daily_data[,i])$out]
  print(paste(i,length(val)))
  daily_data[,i][daily_data[,i] %in% val] = NA
}
daily_data = knnImputation(daily_data, k = 5)
sum(is.na(daily_data))

vif(numeric_data)
corrgram(numeric_data, order = F, upper.panel=panel.pie, 
         text.panel=panel.txt, main = "Correlation Plot")
for(i in categorical_features){
  print(i)
  aov_summary = summary(aov(daily_data$total_count~daily_data[,i],data = daily_data))
  print(aov_summary)
  
}
daily_data  = subset(daily_data,select = -c(temp,weekday))
daily_data$total_count = (daily_data$total_count-min(daily_data$total_count))/(max(daily_data$total_count)-min(daily_data$total_count))


set.seed(123)
train_index = sample(1:nrow(daily_data), 0.8 * nrow(daily_data))
train = daily_data[train_index,]
test = daily_data[-train_index,]

#rpart for regression
dt_model = rpart(total_count ~ ., data = train, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[,-10])

#Create dataframe for actual and predicted values
df = data.frame("actual"=test[,10], "pred"=dt_predictions)
head(df)

#calculate MAPE
regr.eval(trues = test[,10], preds = dt_predictions, stats = c("mae","mse","rmse","mape"))

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], dt_predictions)

####
rf_model = randomForest(total_count~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,rf_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,10], preds = rf_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], rf_predictions)
#Calculate R Squared
1 - (sum((test[,10]-rf_predictions)^2)/sum((test[,10]-mean(test[,10]))^2))

gbm_model = gbm(
  formula = train$total_count ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 100,
  interaction.depth = 5,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = NULL, # will use all cores by default
  verbose = TRUE
) 
gbm_pred <- predict(gbm_model, newdata = test[,-10], type = "link")

regr.eval(trues = test[,10], preds = gbm_pred, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], gbm_pred)
#Calculate R Squared
1 - (sum((test[,10]-gbm_pred)^2)/sum((test[,10]-mean(test[,10]))^2))
