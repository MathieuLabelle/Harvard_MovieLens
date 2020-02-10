## ----Load useful libraries,echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")


## ----Load Dataset-----------------------------------------------------------------------------------------------
#Please save the data on your working directory, or create them with file attached to submission.
load("edx.Rda")
load("validation.Rda")


## ----Show summary of the whole dataset--------------------------------------------------------------------------
kable(summary(rbind(edx,validation)),caption="Summary of the Datasets")


## ----Show Header for edx----------------------------------------------------------------------------------------
kable(head(edx), caption = "Head of Edx")


## ----Rating given-----------------------------------------------------------------------------------------------
kable(edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(10) %>%
	arrange(desc(count)), caption= "Given Ratings")


## ----Create EDX_Tidy and Validation Tidy------------------------------------------------------------------------
#Modify edx
edx_Tidy_temp <- edx %>% 
  mutate(time_stamp=date(as_datetime(timestamp)),
         released=(str_remove_all(str_extract(title, "\\(\\d{4}\\)"),"\\(|\\)"))) %>% mutate(RY_TS=round_date(time_stamp,unit = "year")) %>% select(userId, movieId, rating, time_stamp, RY_TS, released, genres)

#Modify Validation
validation_Tidy_temp <- validation %>% 
  mutate(time_stamp=date(as_datetime(timestamp)),
         released=(str_remove_all(str_extract(title, "\\(\\d{4}\\)"),"\\(|\\)"))) %>% mutate(RY_TS=round_date(time_stamp,unit = "year")) %>% select(userId, movieId, rating, time_stamp, RY_TS, released, genres)

# Make sure userId, movieId, RY_TS, released and genres in validation set are also in edx set
validation_Tidy <- validation_Tidy_temp %>% 
  semi_join(edx_Tidy_temp, by = "movieId") %>%
  semi_join(edx_Tidy_temp, by = "userId")%>%
  semi_join(edx_Tidy_temp, by = "released")%>%
  semi_join(edx_Tidy_temp, by = "RY_TS") %>%
  semi_join(edx_Tidy_temp, by = "genres")

# Add rows removed from validation set back into edx set
removed <- anti_join(validation_Tidy_temp, validation_Tidy)
edx_Tidy <- rbind(edx_Tidy_temp, removed)

#In fact if we look at the DIM of removed it contains no entries, so tidy and tidy_temp were the same

#Get rid of useless files
rm(removed,validation_Tidy_temp, edx_Tidy_temp)


## ----Show New Headers-------------------------------------------------------------------------------------------
#Show new Header
kable(head(edx_Tidy), caption = "Head of Edx_Tidy")


## ----Compute summary--------------------------------------------------------------------------------------------
rating <- summary(edx_Tidy$rating)

kable(rbind(rating), caption = "Statistics")

#Computing average rating of edx_Tidy
Std<-sd(edx_Tidy$rating)
print(paste0("Standard Deviation for the rating is: ",Std))

#Computing average rating of edx_Tidy
mu<-mean(edx_Tidy$rating)
print(paste0("Mean of ratings is: ",mu))


## ----average rating distributions,fig.height=2.8,fig.width=3.5--------------------------------------------------
#Movies distribution by Average Rating
edx_Tidy%>% group_by(movieId)%>% summarise(Average_Rating=mean(rating))%>%ggplot(aes(Average_Rating))+geom_histogram(bins=30,fill="gold3")+ggtitle("Movies by average rating")+theme_bw()+geom_vline(xintercept = mu,color="black")  + 
scale_x_continuous(breaks=c(1,2,3,mu,4,5), labels=c("1","2","3","mean","4","5")) 

#Users distribution by Average Rating
edx_Tidy%>% group_by(userId)%>% summarise(Average_Rating=mean(rating))%>%ggplot(aes(Average_Rating))+geom_histogram(bins=30,fill="gold3")+ggtitle("Users by average rating")+theme_bw()+geom_vline(xintercept = mu,color="black") + scale_x_continuous(breaks=c(1,2,3,mu,4,5), labels=c("1","2","3","mean","4","5")) 


## ----Histograms number of ratings, fig.height=2.8,fig.width=3.5-------------------------------------------------
edx_Tidy%>% group_by(movieId) %>% summarise(Number_of_ratings=n())%>%ggplot(aes(Number_of_ratings))+geom_histogram(bins=30,fill="darkgoldenrod3")+ggtitle("Movies by number of ratings")+scale_x_continuous(trans = 'log10') +theme_bw()

edx_Tidy%>% group_by(userId) %>% summarise(Number_of_ratings=n())%>%ggplot(aes(Number_of_ratings))+geom_histogram(bins=30,fill="darkgoldenrod3")+ggtitle("Users by number of ratings")+scale_x_continuous(trans = 'log10') +theme_bw()


## ----Histogram rating by release,fig.height=2.8,fig.width=7-----------------------------------------------------
#Histogram of realease_date by average rating
edx_Tidy%>% group_by(released)%>%ggplot(aes(released))+geom_bar(fill="gold3")+ggtitle("Ratings by release year")+theme_bw()+scale_y_continuous(labels = comma,trans = 'log10')+ theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 5))


## ----Histogram average rating by release,fig.height=2.8,fig.width=7---------------------------------------------
#Histogram of realease_date by average rating
edx_Tidy%>% group_by(released)%>%summarise(Average_of_ratings=mean(rating))%>%ggplot(aes(released,Average_of_ratings))+geom_point(color="darkgoldenrod3")+ggtitle("Average rating by release year") + theme_bw() + scale_y_continuous(trans = 'log10',breaks=c(3.3,3.5,3.7,mu,3.9), labels=c("3.3","3.5","3.7","mean","3.9")) + theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 5)) + geom_hline(yintercept = mu,color="black")


## ----Looking at seasonality in time_stamp, fig.height=2.8,fig.width=3.3-----------------------------------------
#Average rating by day
edx_Tidy%>% mutate(Weekday=weekdays(time_stamp))%>%group_by(Weekday)%>%summarise(Average_of_ratings=mean(rating))%>%ggplot(aes(Weekday,Average_of_ratings))+geom_point(color="darkgoldenrod3")+ggtitle("Average rating by day")+theme_bw()+geom_hline(yintercept = mu,color="black") + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12))

#Average rating by month
Month<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
edx_Tidy%>% mutate(Month=month(time_stamp))%>%group_by(Month)%>%summarise(Average_of_ratings=mean(rating))%>%ggplot(aes(Month,Average_of_ratings))+geom_point(color="darkgoldenrod3")+ggtitle("Average rating by month")+theme_bw()+geom_hline(yintercept = mu,color="black") + scale_x_continuous(breaks=seq(1,12,1), labels=Month)


## ----Looking at time_stamp, fig.height=2.8,fig.width=3.3--------------------------------------------------------
#Average rating by day
edx_Tidy%>% mutate(R_months=round_date(time_stamp,"month"))%>%group_by(R_months)%>%summarise(Average_of_ratings=mean(rating))%>%ggplot(aes(R_months,Average_of_ratings))+geom_point(color="darkgoldenrod3")+ggtitle("Average rating by day")+theme_bw()+geom_hline(yintercept = mu,color="black") + theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 12))

#Average rating by month
edx_Tidy%>% mutate(R_years=round_date(time_stamp,"year"))%>%group_by(R_years)%>%summarise(Average_of_ratings=mean(rating))%>%ggplot(aes(R_years,Average_of_ratings))+geom_point(color="darkgoldenrod3")+ggtitle("Average rating by month")+theme_bw()+geom_hline(yintercept = mu,color="black")


## ----Table for genres bias--------------------------------------------------------------------------------------
#Top ten table for genre bias
Top_Bias_genre<-edx%>%group_by(genres) %>% filter(n()>10000)%>% summarise(Average_Rating=round(mean(rating)-mu,digits=2)) %>%top_n(10,Average_Rating)%>%arrange(desc(Average_Rating))

#Bottom ten table for genre bias
Bottom_Bias_genre<-edx%>%group_by(genres) %>% filter(n()>10000)%>% summarise(Average_Rating=round(mean(rating)-mu,digits=2)) %>%top_n(10,-Average_Rating)%>%arrange(Average_Rating)

#show table
kable(cbind(Top_Bias_genre, Bottom_Bias_genre))


## ----coding RMSE as a function----------------------------------------------------------------------------------
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


## ----Create our train and test sets from edx_Tidy---------------------------------------------------------------
#Using function that split edx_Tidy, here in 80% for Train set  and 20% for Test set, we use set.seed in order to get always the same partition.
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx_Tidy$rating, times = 1,p = 0.2, list = FALSE)
edx_Tidy_train_temp<- edx_Tidy[-test_index,]
edx_Tidy_test_temp <- edx_Tidy[test_index,]
  
#Remove in edx_Tidy_test set the ratings where users or movies were not present in edx_Tidy_train.
test_and_addTrain<-edx_Tidy_test_temp 
edx_Tidy_test  <- edx_Tidy_test_temp  %>% semi_join(edx_Tidy_train_temp, by = "movieId") %>% semi_join(edx_Tidy_train_temp, by = "userId")%>% semi_join(edx_Tidy_train_temp, by = "released")%>% semi_join(edx_Tidy_train_temp, by = "RY_TS")%>% semi_join(edx_Tidy_train_temp, by = "genres")

#To keep Edx_Tidy set globally unchanged, we need to add the rating we just removed from the Test set.
addTrain<-anti_join(test_and_addTrain,edx_Tidy_test)
edx_Tidy_train<-rbind(edx_Tidy_train_temp ,addTrain)

#Delete useless Dataset
rm(test_index, edx_Tidy_train_temp, edx_Tidy_test_temp, test_and_addTrain, addTrain)


## ----Simple model-----------------------------------------------------------------------------------------------
#Compute teh rating average of our train set
mu<-mean(edx_Tidy_train$rating)

#Predict ratings on our test set
predicted_ratings <- 
          edx_Tidy_test %>% 
          mutate(pred = mu) %>% 
          .$pred
#Calculate RMSE for the given lambda
r<-RMSE(predicted_ratings, edx_Tidy_test$rating)
r<-round(r,5)
model_rmse<-tibble(Model = "Simple model using average", RMSE = r)
kable(model_rmse)


## ----Model with 1 bias------------------------------------------------------------------------------------------
#Compute movie bias.
b_i <- edx_Tidy_train %>% 
       group_by(movieId) %>%
       summarize(b_i = sum(rating - mu)/n())
#Predict ratings on our test set
  predicted_ratings <- 
          edx_Tidy_test %>% 
          left_join(b_i, by = "movieId") %>%
          mutate(pred = mu + b_i) %>% 
          mutate(pred_cap_floor = ifelse(pred < 0.5, 0.5, ifelse(pred > 5, 5, pred))) %>%
          .$pred_cap_floor
r<-RMSE(predicted_ratings, edx_Tidy_test$rating)
r<-round(r,5)
model_rmse<-bind_rows(model_rmse,tibble(Model = "Model with movie bias", RMSE = r))
kable(model_rmse)


## ----Model with 2 bias------------------------------------------------------------------------------------------
#Compute movie bias
  b_i <- edx_Tidy_train %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/n())
#Compute user bias
b_u <- edx_Tidy_train %>% 
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - b_i - mu)/n())
#Predict ratings on our test set
predicted_ratings <- edx_Tidy_test %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          mutate(pred = mu + b_i + b_u) %>% 
          mutate(pred_cap_floor = ifelse(pred < 0.5, 0.5, ifelse(pred > 5, 5, pred))) %>%
          .$pred_cap_floor
#Calculate RMSE for the given lambda
r<-RMSE(predicted_ratings, edx_Tidy_test$rating)
r<-round(r,5)
model_rmse<-bind_rows(model_rmse,tibble(Model = "Model with Movie & user bias", RMSE = r))
kable(model_rmse)


## ----Model with 2 bias regularized------------------------------------------------------------------------------
# Create a sequence of lambda
lambdas<-seq(3,6,0.25)

#For each lambda of lambdas, compute bias and RMSE in the Train set.
rmses <- sapply(lambdas, function(l){
  #Compute movie bias
  b_i <- edx_Tidy_train %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+l))
  #Compute user bias
  b_u <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  #Predict ratings on our test set
  predicted_ratings <- 
          edx_Tidy_test %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          mutate(pred = mu + b_i + b_u) %>% 
          mutate(pred_cap_floor = ifelse(pred < 0.5, 0.5, ifelse(pred > 5, 5, pred))) %>%
          .$pred_cap_floor
  #Calculate RMSE for the given lambda
  return(RMSE(predicted_ratings, edx_Tidy_test$rating))
})


## ----Plot RMSE by Lambda 2 bias model,fig.height=2.8,fig.width=7------------------------------------------------
#Plot RMSE as a function of lambda
qplot(lambdas, rmses) +theme_bw()+ggtitle("RMSE by lambda")+xlab("Lambda")+ylab("RMSE")


## ----results with 2 bias model regularized----------------------------------------------------------------------
r<-rmses[which.min(rmses)]
r<-round(r,5)
model_rmse<-bind_rows(model_rmse,tibble(Model = "Model with 2 bias regularized", RMSE = r))
kable(model_rmse)


## ----Model with 3 bias regularized------------------------------------------------------------------------------
# Create a sequence of lambda
lambdas<-seq(4,5,0.05)

#For each lambda of lambdas, compute bias and RMSE in the Train set
rmses <- sapply(lambdas, function(l){
  #Compute movie bias
  b_i <- edx_Tidy_train %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+l))
  #Compute user bias 
  b_u <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  #Compute release date bias
  b_y <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          group_by(released) %>%
          summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  #Predict ratings on our test set
  predicted_ratings <- 
          edx_Tidy_test %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(b_y, by = "released") %>%
          mutate(pred = mu + b_i + b_u + b_y) %>% 
          mutate(pred_cap_floor = ifelse(pred < 0.5, 0.5, ifelse(pred > 5, 5, pred))) %>%
          .$pred_cap_floor
  #Calculate RMSE for the given lambda
  return(RMSE(predicted_ratings, edx_Tidy_test$rating))
})


## ----Plot RMSE by Lambda 3 bias model,fig.height=2.8,fig.width=7------------------------------------------------
#Plot RMSE as a function of lambda
qplot(lambdas, rmses) +theme_bw()+ggtitle("RMSE by lambda")+xlab("Lambda")+ylab("RMSE")


## ----results with 3 bias model regularized----------------------------------------------------------------------
r<-rmses[which.min(rmses)]
r<-round(r,5)
model_rmse<-bind_rows(model_rmse,tibble(Model = "Model with 3 bias regularized", RMSE = r))
kable(model_rmse)


## ----Model 4 bias regularized-----------------------------------------------------------------------------------
# Create a sequence of lambda
lambdas<-seq(2,6,0.25)

#For each lambda of lambdas, compute bias and RMSE in the Train set.
rmses <- sapply(lambdas, function(l){
  #Compute movie bias
  b_i <- edx_Tidy_train %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+l))
  #Compute user bias
  b_u <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  #Compute release date bias
  b_y <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          group_by(released) %>%
          summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  #Compute genre bias
  b_g <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          left_join(b_y, by="released") %>%
          group_by(genres) %>%
          summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  #Predict ratings on our test set
  predicted_ratings <- 
          edx_Tidy_test %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(b_y, by = "released") %>%
          left_join(b_g, by="genres") %>%
          mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
          mutate(pred_cap_floor = ifelse(pred < 0.5, 0.5, ifelse(pred > 5, 5, pred))) %>%
          .$pred_cap_floor
  #Calculate RMSE for the given lambda
  return(RMSE(predicted_ratings, edx_Tidy_test$rating))
})


## ----Plot RMSE by Lambda 4 bias model,fig.height=2.8,fig.width=7------------------------------------------------
#Plot RMSE as a function of lambda
qplot(lambdas, rmses) +theme_bw()+ggtitle("RMSE by lambda")+xlab("Lambda")+ylab("RMSE")


## ----results with 4 bias model regularized----------------------------------------------------------------------
r<-rmses[which.min(rmses)]
r<-round(r,5)
model_rmse<-bind_rows(model_rmse,tibble(Model = "Model with 4 bias regularized", RMSE = r))
kable(model_rmse)


## ----Model 5 bias regularized-----------------------------------------------------------------------------------
# Create a sequence of lambda
lambdas<-seq(4.5,5,0.05)

#For each lambda of lambdas, compute bias and RMSE in the Train set.
rmses <- sapply(lambdas, function(l){
  #Compute movie bias
  b_i <- edx_Tidy_train %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+l))
  #Compute user bias
  b_u <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  #Compute release date bias
  b_y <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          group_by(released) %>%
          summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  #Compute genre bias
  b_g <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          left_join(b_y, by="released") %>%
          group_by(genres) %>%
          summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  #Compute rating year bias
  b_ry <- edx_Tidy_train %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          left_join(b_y, by="released") %>%
          left_join(b_g, by="genres") %>%
          group_by(RY_TS) %>%
          summarize(b_ry = sum(rating - b_i - b_u - -b_y - b_g - mu)/(n()+l))
  #Predict ratings on our test set
  predicted_ratings <- 
          edx_Tidy_test %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(b_y, by = "released") %>%
          left_join(b_g, by="genres") %>%
          left_join(b_ry, by="RY_TS") %>%
          mutate(pred = mu + b_i + b_u + b_y + b_g + b_ry) %>% 
          mutate(pred_cap_floor = ifelse(pred < 0.5, 0.5, ifelse(pred > 5, 5, pred))) %>%
          .$pred_cap_floor
  #Calculate RMSE for the given lambda
  return(RMSE(predicted_ratings, edx_Tidy_test$rating))
})


## ----model Plot RMSE by Lambda 5,fig.height=2.8,fig.width=7-----------------------------------------------------
#Plot RMSE as a function of lambda
qplot(lambdas, rmses) +theme_bw()+ggtitle("RMSE by lambda")+xlab("Lambda")+ylab("RMSE")


## ----results with 5 bias model regularized----------------------------------------------------------------------
l <- lambdas[which.min(rmses)]
r<-rmses[which.min(rmses)]
r<-round(r,5)
model_rmse<-bind_rows(model_rmse,tibble(Model = "Model with 5 bias regularized", RMSE = r))
kable(model_rmse)


## ----Apply our System to edx_Tidy and evaluate it with valuation_Tidy set---------------------------------------
#compute mean
mu<-mean(edx_Tidy$rating)
print(paste0("Using the mean of Edx_Tidy, equals to: ",mu))
#Coremodel 4 bias
#Compute movie bias
b_i <- edx_Tidy %>%
          group_by(movieId) %>%
          summarize(b_i = sum(rating - mu)/(n()+l))
#Compute user bias
b_u <- edx_Tidy %>% 
          left_join(b_i, by="movieId") %>%
          group_by(userId) %>%
          summarize(b_u = sum(rating - b_i - mu)/(n()+l))
#Compute realease date bias
b_y <- edx_Tidy %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          group_by(released) %>%
          summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
#Compute genre bias
b_g <- edx_Tidy %>% 
          left_join(b_i, by="movieId") %>%
          left_join(b_u, by="userId") %>%
          left_join(b_y, by="released") %>%
          group_by(genres) %>%
          summarize(b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
#Predict ratings on our validation set
predicted_ratings <- 
          validation_Tidy %>% 
          left_join(b_i, by = "movieId") %>%
          left_join(b_u, by = "userId") %>%
          left_join(b_y, by = "released") %>%
          left_join(b_g, by="genres") %>%
          mutate(pred = mu + b_i + b_u + b_y + b_g) %>% 
          mutate(pred_cap_floor = ifelse(pred < 0.5, 0.5, ifelse(pred > 5, 5, pred))) %>%
          .$pred_cap_floor

#Calculate RMSE for the given lambda
r<-RMSE(predicted_ratings, validation_Tidy$rating)
r<-round(r,5)
model_rmse<-bind_rows(model_rmse,tibble(Model = "Final assessment with Validation set", RMSE = r))
kable(model_rmse)

