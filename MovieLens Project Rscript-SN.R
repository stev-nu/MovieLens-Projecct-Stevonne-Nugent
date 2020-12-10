##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(gt)
library(scales)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#title = as.character(title),
#genres = as.character(genres))
#if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#saving the edx and validation datasets
saveRDS(edx, file = "edx.Rds")
saveRDS(validation, file = "validation.Rds")

#Exploring the edx dataset 
dim(edx)

#Table 1: First 6 Rows of the MovieLens 10M Dataset
movielens <- readRDS("movielens.Rds")

#create head of movielens data
movielens_head <- movielens %>% head()
saveRDS(movielens_head, file = "movielens_head.Rds")
movielens_head %>% gt()

#check for any missing values
any(is.na(edx)) #returns FALSE, ie there are no missing values

#examine data points, number of data points in the movilens and edx datasets
movielens %>% summarize(number_of_users = n_distinct(userId), #number of distinct users
                        number_of_movies = n_distinct(movieId), #number of distinct movie IDs
                        number_of_titles = n_distinct(title), #number of distinct titles
                        number_of_genres = n_distinct(genres), #number of distinct genre groups
                        number_of_timestamp = n_distinct(timestamp)) #number of distinct timestamps

edx %>% summarize(number_of_users = n_distinct(userId), #number of distinct users
                  number_of_movies = n_distinct(movieId), #number of distinct movie IDs
                  number_of_titles = n_distinct(title), #number of distinct titles
                  number_of_genres = n_distinct(genres)) #number of distinct genre groups

#Data wrangling to edx to create new columns for data that may be needed in the recommendation stage of the project. 
#Note original name has been maintained
#change timestamp to an actual date in yyyy-mm-dd format then split into year and month
edx$reviewdatetime <- as_datetime(edx$timestamp) #changing timestamp to date and time
edx$reviewdate <- as_date(edx$reviewdatetime) #abstracting date from date and time column
edx$reviewyear <- as.numeric(format(edx$reviewdate,'%Y')) #abstracting year from date column
edx$reviewmonth <- as.numeric(format(edx$reviewdate,'%m')) #abstracting month from date column

#separate the movie year from the title of the movie and place in a separate column
edx$movieyear <- sub("\\).*", "", sub(".*\\(", "", edx$title))

#saving transformed edx set
saveRDS(edx, file = "edx.Rds")

#creating edx head dataset, which represents the first six rows of the edx dataset
edx_head <- edx %>% head()
saveRDS(edx_head, file = "edx_head.Rds")


#exploring the newly processed edx data to examine trends etc
#Table 2: First 6 Rows of Select Variables of the Processed edx Dataset
edx_head %>% select(4,5,8,9,10,11) %>% knitr::kable()
edx_head %>% select(4,5,8,9,10,11) %>% gt()

#set rating and movieyear as.numeric as is needed to facilitate data exploration
edx$rating <- as.numeric(edx$rating)
edx$movieyear <- as.numeric(edx$movieyear)

#to test the models we separate the edx dataset into a test set and a train set, edxtest_a and edxtrain
# if using R 3.5 or earlier, use `set.seed(1)`
set.seed(1, sample.kind="Rounding")  # set seed of random numbers so results can be replicated.
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edxtrain <- edx[-test_index,]
edxtest_a <- edx[test_index,]

#ensuring userId and movieId in edxtest are also in edxtrain
edxtest <- edxtest_a %>% 
  semi_join(edxtrain, by = "movieId") %>%
  semi_join(edxtrain, by = "userId")

#add rows removed from edxtest_a and place them in edxtrain
removed_edxtest_a <- anti_join(edxtest_a, edxtest)
edxtrain <- rbind(edxtrain, removed_edxtest_a)

#saving the edxtest and edxtrain datasets
saveRDS(edxtrain, file = "edxtrain.Rds")
saveRDS(edxtest, file = "edxtest.Rds")

#Explore the ratings data in the edxtrain dataset by movie, genre, user etc 
edxtrain %>% summarize(number_of_users = n_distinct(userId), #number of distinct users
                  number_of_movies = n_distinct(movieId), #number of distinct movie IDs
                  number_of_titles = n_distinct(title), #number of distinct titles
                  number_of_genres = n_distinct(genres)) #number of distinct genre groups

#rating variable
#descriptive statistics of the rating variable in edxtrain
descriptive_stats <- edxtrain %>% summarize(
  minimum = min(rating),  #calculate minimum rating
  maximum = max(rating), #calculate maximum rating
  first_quartile = quantile(rating,0.25), #calculate first quartile rating
  median_rating = quantile(rating,0.5), #calculate median rating
  third_quartile = quantile(rating,0.75), #calculate third quartile rating
  average = mean(rating), #calculate average rating
  std_dev = sd(rating), #calculate standard deviation 
  range = maximum - minimum #calculate range
  ) %>% pivot_longer(1:8, names_to = "Descriptive Stats", values_to = "Values") 

#saving descriptive stats dataframe created above
saveRDS(descriptive_stats, file = "descriptive_stats.Rds")

#create table for descriptive statistics 
#Table 3: Descriptive Statistics of Ratings in the edxtrain Dataset
descriptive_stats %>% knitr::kable()
descriptive_stats %>% gt()

#Rating distribution and ratings by year - Figure 1
ggplot(edxtrain, aes(rating)) + #create graph
  geom_histogram(bins = 20, fill = "blue") + #adding layer histogram
  labs(title = "Figure 1: Rating Distribution", x = "Rating") + #labeling graph and axes
  theme(panel.grid.major = element_line(colour = "azure3"), #adding colors and sizing to graph
    panel.grid.minor = element_line(colour = NA), 
    axis.text = element_text(size = 10, colour = "black"), 
    axis.text.x = element_text(size = 10, 
        colour = "black"), axis.text.y = element_text(size = 10, 
        colour = "black"))

#movieId variable unique count
edxtrain %>% 
  summarize(number_of_movies = n_distinct(movieId))

#Rating distribution by movie - Figure 2
ggplot(edxtrain, aes(movieId)) + #create plot
  geom_histogram(bins = 100, fill = "blue") + #adding histogram layer
  labs(title = "Figure 2: Rating Distribution by MovieId", x = "MovieId") + #adding labels and title
  theme(panel.grid.major = element_line(colour = "azure3"), #adding themes, colours etc
  panel.grid.minor = element_line(colour = NA), 
  axis.text = element_text(size = 10, colour = "black"), 
  axis.text.x = element_text(size = 10, 
  colour = "black"), axis.text.y = element_text(size = 10, colour = "black"))

#shows the top 10 movies with the highest number of ratings and their average rating
top10_rated_movies <- edxtrain %>% 
  group_by(movieId, title) %>% #group by/organize by movieId and title
  summarize(count = n(), avg_rating = mean(rating)) %>% #calculate number of times movieId and title are present and calculate average rating
  ungroup() %>% #ungroup data to facilitate creating tables
  arrange(desc(count)) %>% #arranging by count in descending order
  head(n=10) #returns top 10

#saving top 10 rated movies dataframe created above
saveRDS(top10_rated_movies, file = "top10_rated_movies.Rds")

#create table for top 10 rated movies of edxtrain dataset
#Table 4: Top 10 Rated Movies
knitr::kable(top10_rated_movies)
top10_rated_movies %>% gt()

#shows 10 of the 159 movies that were rated once and their rating (arranged by highest rating)
once_rated_movies <- edxtrain %>% 
  group_by(movieId) %>% #grouping by movieId variable
  mutate(count_movieId = n()) %>% #calculating the number of times the movieId was rated
  ungroup() %>% #ungrouping to create tables
  filter(count_movieId==1) %>% #getting movies that were rated once
  select(movieId,title,rating) %>% #choosing the columns that are needed
  arrange(desc(rating)) %>% #arranging by rating in descending order
  head(n=10) #returns the top 10

#saving once rated movies dataframe created above
saveRDS(once_rated_movies, file = "once_rated_movies.Rds")

#create table top 10 once rated movies
#Table 5: Movies Rated Only Once with High Movie Ratings
knitr::kable(once_rated_movies)
once_rated_movies %>% gt()

#exploring userId variable
#shows how active users are in the number of ratings they give
edxtrain_users <- edxtrain %>% 
  group_by(userId) %>% #grouping by userId
  summarize(number_of_reviews=n(), avg_rating = mean(rating)) %>% 
  #calculating the number of times a user gave a rating and their average rating
  arrange(desc(number_of_reviews)) #arranging in descending order by number of times user rated

#descriptive stats on users
descriptive_stats_users <- edxtrain_users %>% summarize(
  minimum = min(number_of_reviews), #calculating minimum number of times user gave ratings
  maximum = max(number_of_reviews),  #calculating maximum number of times user gave ratings
  first_quartile = quantile(number_of_reviews,0.25),  #calculating 1st quartile number of times user gave ratings
  median_rating = quantile(number_of_reviews,0.5),  #calculating median number of times user gave ratings
  third_quartile = quantile(number_of_reviews,0.75),  #calculating 3rd quartile number of times user gave ratings
  average = mean(number_of_reviews),  #calculating average number of times user gave ratings
  std_dev = sd(number_of_reviews), #calculating the standard deviation of the number of times user gave ratings
  range = maximum - minimum
) %>% pivot_longer(1:8, names_to = "Descriptive Statistics", values_to = "Number of Reviews") 

#descriptive stats on user rating
descriptive_stats_userrating <- edxtrain_users %>% summarize(
  minimum = min(avg_rating), #calculate minimum average rating of users
  maximum = max(avg_rating), #calculate maximum average rating of users
  first_quartile = quantile(avg_rating,0.25), #calculate 1st quartile average rating of users
  median_rating = quantile(avg_rating,0.5), #calculate median average rating of users
  third_quartile = quantile(avg_rating,0.75), #calculate 3rd quartile average rating of users
  average = mean(avg_rating), #calculate average of the average rating of users
  std_dev = sd(avg_rating), #calculate standard deviation of average rating of users
  range = maximum - minimum
) %>% pivot_longer(1:8, names_to = "Descriptive Statistics", values_to = "User Average Rating")

#saving dataframes created above for descriptive stats of users and user rating
saveRDS(descriptive_stats_users, file = "descriptive_stats_users.Rds")
saveRDS(descriptive_stats_userrating, file = "descriptive_stats_userrating.Rds")

#Table 6a: Descriptive Statistics of Rating Frequency by User
knitr::kable(descriptive_stats_users)
descriptive_stats_users %>% gt()

#Table 6b: Descriptive Statistics of Average Rating by User
knitr::kable(descriptive_stats_userrating)
descriptive_stats_userrating %>% gt()

#exploring the genres variable
#creating a table to identify distinct genres in each genre group, that is, remove pipe-separation
genre_dist <- edxtrain %>% 
  separate_rows(genres, sep = "\\|") %>% #removing pipe and creating a new row for each distinct genre
  group_by(genres) %>% #grouping by each distinct genre
  summarize(count = n()) %>% #calculate total number of times each distinct genre was rated
  arrange(desc(count)) #arranging in descending order

#Table: Genre Distribution
knitr::kable(genre_dist)

#using generated genre distribution data frame to plot a genre distribution graph - Figure 3
genre_dist %>% ggplot(aes(x = reorder(genres,count), y=count, label=count)) + #creating plot, ordering data
  geom_segment(aes(xend=genres,yend=0)) + #adding lollipop layer
  geom_point(size=5, colour="blue") + coord_flip() +
  geom_text(aes(label = scales::comma(round(count))), size = 3.5, vjust = 1.65, hjust = 0.5) + #labeling data points on graph
  labs(title = "Figure 3: Genre Distribution", x = "Genre", y = "Count") #label graph and axes

#create table to show unique genre groups and separate by the top 10 and bottom 10
genre_top10 <- edxtrain %>% 
  group_by(genres) %>% #grouping by unique genre groups
  summarize(count=n(), average = mean(rating)) %>% #calculating the average
  arrange(desc(average)) %>% #arranging in descending order
  head(10) #pulling the top 10 based on average rating

genre_bottom10 <- edxtrain %>% 
  group_by(genres) %>% #grouping by unique genre groups
  summarize(count=n(), average = mean(rating)) %>% #calculating the average
  arrange(average) %>% #arranging in ascending order
  head(10) #pulling the bottom 10 based on average rating

#saving data frames created for genres top 10 and bottom 10
saveRDS(genre_top10, file = "genre_top10.Rds")
saveRDS(genre_bottom10, file = "genre_bottom10.Rds")

#Table 7: Top 10 Genre Groups by Average Rating
knitr::kable(genre_top10) #creating table
genre_top10 %>% gt()

#Table 8: Top 10 Genre Groups by Average Rating
knitr::kable(genre_bottom10) #creating table
genre_bottom10 %>% gt()

#####exploring the movieyear variable
#table showing number of reviews by year
movie_by_year <- edxtrain %>% 
  group_by(movieyear) %>% #grouping by movieyear
  summarize(count = n()) 

#plot the number of ratings by movie year, highlighting the movie years that received more than 50,000 ratings - Figure 4
movie_by_year %>% ggplot(aes(movieyear, count)) + #creating plot
  coord_flip() + #sflipping the axes
  geom_segment(aes(xend=movieyear,yend=0)) + #adding lollipop layer to graph
  geom_point(size=3, colour="blue") + 
  labs(y = "Number of Ratings", x = "Movie Year", title = "Figure 4: Number of Movie Ratings by Movie Year") +
  geom_text(data=subset(movie_by_year, count>50000), aes(label = scales::comma(round(count))),
            size = 3, vjust = 0.5, hjust = -0.2) #labeling and limiting labeling to movie years that received more than 50,000 ratings

#creating tables to show the top 10 and bottom 10 movie years by average rating
movieyear_top10 <- edxtrain %>% 
  group_by(movieyear) %>% #grouping by movie year
  summarize(average = mean(rating)) %>% #calculating average rating by movie year
  arrange(desc(average)) %>% #arranging in descending order
  head(10) #pull the top 10 movie years by average rating

movieyear_bottom10 <- edxtrain %>% 
  group_by(movieyear) %>% #grouping by movie year
  summarize(average = mean(rating)) %>% #calculating average rating by movie year
  arrange(average) %>% #arranging in order
  head(10) #pull the bottom 10 movie years by average rating

#saving movie year dataframes created above
saveRDS(movieyear_top10, file = "movieyear_top10.Rds")
saveRDS(movieyear_bottom10, file = "movieyear_bottom10.Rds")

#Table 9: Top 10 Movie Years by Average Rating
knitr::kable(movieyear_top10)
movieyear_top10 %>%  gt() #create table

#Table 10: Bottom 10 Movie Years by Average Rating
knitr::kable(movieyear_bottom10)
movieyear_bottom10 %>% gt() #create table


#in a recommendation system the simplest model one could use is to always predict the mean
#predict mean will be the baseline model - The Mean Model
avg <- mean(edxtrain$rating) #calculate average rating
avg

#we then predict the RMSE on the edxtest dataset
predictions_avg <- rep(avg, nrow(edxtest)) #using the edxtest dataset to check the model
rmse_mean_model <- RMSE(edxtest$rating, predictions_avg) #calculating the RMSE of the mean model using edxtest dataset
rmse_mean_model

#create a results data frame that we will continue adding to as we progress along
rmse_results <- data.frame(model ="The Mean Model", RMSE = rmse_mean_model)
rmse_results

#Regularization 1 
#modelling the movie effect with regularization - as low frequency rated movies have high ratings
#regularization will penalize large estimates that are from small samples

#compute regularized estimates of b_i using lambda (penalty term)
lambdas_reg1 <- seq(1,2,0.05) #setting range for lambdas

#model tuning
rmses_reg1 <- sapply(lambdas_reg1, function(reg1){ #optimizing rmse with different values of lambda
  
  temp <- edxtrain %>%  group_by(movieId) %>%  #grouping by movieId
  summarize(b_i = sum((rating - avg))/(n()+reg1)) #calculating movie effect while controlling for overall average rating
  
  predicted_ratings <- edxtest %>% 
    left_join(temp, by = 'movieId') %>% #joining by movieId
    mutate(pred = avg + b_i) %>% #predicting movie ratings using movie effect with varying lambdas
    mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
    mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
    pull(pred)
  return(RMSE(predicted_ratings, edxtest$rating))
})

#plotting lambdas for first regularization model - Figure 5
qplot(lambdas_reg1, rmses_reg1) +  #creating plot of lambdas 
  labs(title = expression(paste("Figure 5: Optimal Value of  ", "", hat(lambda)[i],
                                " that minimizes the RMSE for ",  "\n",
                                hat(e)[i][j], " = ",  r[i][j]-bar(b)-hat(b)[i])),
       x = expression(hat(lambda)[i]),
       y = "RMSE"
       
  )+
  annotate("text", x = lambdas_reg1[which.min(rmses_reg1)]+0.05, y = 0.9429385, colour="red", #labeling the graph to show optimal lambda
           label = paste(" =", lambdas_reg1[which.min(rmses_reg1)]), size=5)+
  annotate("text", x = lambdas_reg1[which.min(rmses_reg1)], y = 0.9429385, colour="red",
           label = expression(hat(lambda)[i]), size=5) #best lambda is 1.6

#using best lambda
lambda_reg1 <- lambdas_reg1[which.min(rmses_reg1)] #setting lambda to ptimal lambda

movie_effect <- edxtrain %>% group_by(movieId) %>% #grouping by movieId
  summarize(b_i = sum((rating - avg))/(n()+lambda_reg1)) #calculating movie effect while controlling for overall average

bi <- edxtest %>% 
  left_join(movie_effect, by="movieId") %>% #joining movie effect by movieId
  mutate(pred = avg + b_i) %>% #predicting movie ratings using movie effect using optimal lambda
  mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
  mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
  pull(pred)

rmse_movie_effect <- RMSE(bi, edxtest$rating) #calculating the RMSE of movie effect using edxtest dataset
rmse_movie_effect

#adding RMSE movie effect to results data frame
rmse_results <- bind_rows(data.frame(model ="The Mean Model", RMSE = rmse_mean_model),
                          data.frame(model ="Regularized Model: Movie Effect", RMSE = rmse_movie_effect))

###########################regularization 2 movie and user effect
lambdas_reg2 <- seq(4,6,0.1) #setting range for lambdas

rmses_reg2 <- sapply(lambdas_reg2, function(reg2){ #optimizing rmse with different values of lambda
  
  temp <- edxtrain %>%  
    left_join(movie_effect, by = 'movieId') %>% #joining by movie effect
      group_by(userId) %>% #grouping by userId
      summarize(b_j = sum((rating - avg - b_i))/(n()+reg2)) #calculating user effect while controlling for movie effect
  
  predicted_ratings <- edxtest %>% 
    left_join(movie_effect, by = 'movieId') %>% #joining by movieId
    left_join(temp, by = 'userId') %>% #joining by userId
    mutate(pred = avg + b_i + b_j) %>% #predicting movie ratings using movie effect using varying lambdas
    mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
    mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
    pull(pred)
  return(RMSE(predicted_ratings, edxtest$rating))
})

#plotting lambdas for second regularization model - Figure 6
qplot(lambdas_reg2, rmses_reg2) + #creating plot of lambdas 
  labs(title = expression(paste("Figure 6: Optimal Value of  ", "", hat(lambda)[j],
                                " that minimizes the RMSE for ",  "\n",
                                hat(e)[i][j], " = ",  r[i][j]-bar(b)-hat(b)[i]-hat(b)[j])),
       x = expression(hat(lambda)[j]),
       y = "RMSE") +
  annotate("text", x = lambdas_reg2[which.min(rmses_reg2)]+0.07, y = 0.8640645, colour="red", #labeling to show optimal lambda
           label = paste(" =", lambdas_reg2[which.min(rmses_reg2)]), size=5) +
  annotate("text", x = lambdas_reg2[which.min(rmses_reg2)], y = 0.8640645, colour="red",
           label = expression(hat(lambda)[j]), size=5) #best lambda is 4.6

#using best lambda for movie and user effect
lambda_reg2 <- lambdas_reg2[which.min(rmses_reg2)] #set lambda to optimal lambda

user_effect <- edxtrain %>%  
  left_join(movie_effect, by = 'movieId') %>% #joing by movieId
  group_by(userId) %>% #grouping by userId
  summarize(b_j = sum((rating - avg - b_i))/(n()+lambda_reg2)) #calculating user effect while controlling for movie effect


bj <- edxtest %>% 
  left_join(movie_effect, by = 'movieId') %>% #joining by movieId
  left_join(user_effect, by = 'userId') %>% #joining by userId
  mutate(pred = avg + b_i + b_j) %>% #predicting movie ratings using user & movie effect with optimal lambda
  mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
  mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
  pull(pred)

rmse_user_effect <- RMSE(bj, edxtest$rating) #calculate RMSE for user effect using edxtest dataset
rmse_user_effect

#adding user effect RMSE to RMSE results data frame
rmse_results <- bind_rows(data.frame(model ="The Mean Model", RMSE = rmse_mean_model),
                          data.frame(model ="Regularized Model: Movie Effect", RMSE = rmse_movie_effect),
                          data.frame(model ="Regularized Model: Movie + User Effect", RMSE = rmse_user_effect))

###########################regularization 3 movie, user and genre effect
lambdas_reg3 <- seq(0,10,1)  #setting range for lambdas

rmses_reg3 <- sapply(lambdas_reg3, function(reg3){ #optimizing rmse with different values of lambda
  
  temp <- edxtrain %>%  
    left_join(movie_effect, by = 'movieId') %>% #joining by movieId
    left_join(user_effect, by = 'userId') %>% #joining by userId
    group_by(genres) %>% #grouping by genres
    summarize(b_g = sum((rating - avg - b_i - b_j))/(n()+reg3)) #calculating genre effect while controlling for movie & user effect
  
  
  predicted_ratings <- edxtest %>% 
    left_join(movie_effect, by = 'movieId') %>% #joining by movieId
    left_join(user_effect, by = 'userId') %>% #joining by userId
    left_join(temp, by = 'genres') %>% #joining by genres
    mutate(pred = avg + b_i + b_j + b_g) %>% #predicting movie ratings using user, movie & genre effect with varying lambda
    mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
    mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
    pull(pred)
  return(RMSE(predicted_ratings, edxtest$rating))
})

#plotting lambdas for third regularization model - Figure 
qplot(lambdas_reg3, rmses_reg3) +  #creating plot of lambdas 
  labs(title = expression(paste("Figure 7: Optimal Value of  ", "", hat(lambda)[g],
                                " that minimizes the RMSE for ",  "\n",
                                hat(e)[i][j], " = ",  r[i][j]-bar(b)-hat(b)[i]-hat(b)[j]-hat(b)[g])),
       x = expression(hat(lambda)[g]),
       y = "RMSE") +
  annotate("text", x = lambdas_reg3[which.min(rmses_reg3)]+5, y = 0.86372475, colour="red",
           label = paste(" =", lambdas_reg3[which.min(rmses_reg3)]), size=5) +
  annotate("text", x = lambdas_reg3[which.min(rmses_reg3)]+4.7, y = 0.86372475, colour="red", #labeling to show optimal lambda
           label = expression(hat(lambda)[g]), size=5) #best lambda is 0

#using best lambda for movie, user and genre effect
lambda_reg3 <- lambdas_reg3[which.min(rmses_reg3)]

genre_effect <- edxtrain %>%  
  left_join(movie_effect, by = 'movieId') %>% #joining by movieId
  left_join(user_effect, by = 'userId') %>% #joining by userId
  group_by(genres) %>% #grouping by genres
  summarize(b_g = sum((rating - avg - b_i - b_j))/(n()+lambda_reg3)) #calculating genre effect while controlling for movie & user effect


bg <- edxtest %>% 
  left_join(movie_effect, by = 'movieId') %>% #joining by movieId
  left_join(user_effect, by = 'userId') %>% #joining by userId
  left_join(genre_effect, by = 'genres') %>% #joining by genres
  mutate(pred = avg + b_i + b_j + b_g) %>% #predicting movie ratings using user, movie & genre effect with optimal lambda
  mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
  mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
  pull(pred)

rmse_genre_effect <- RMSE(bg, edxtest$rating) #calculate RMSE of genre effect using edxtest dataset
rmse_genre_effect

#adding RMSE of genre effect to RMSE results data frame
rmse_results <- bind_rows(data.frame(model ="The Mean Model", RMSE = rmse_mean_model),
                          data.frame(model ="Regularized Model: Movie Effect", RMSE = rmse_movie_effect),
                          data.frame(model ="Regularized Model: Movie + User Effect", RMSE = rmse_user_effect),
                          data.frame(model ="Regularized Model: Movie + User + Genre Effect", RMSE = rmse_genre_effect))

###########################Regularization 4 movie, user, genre and movieyear effect

#plot average movie year effect
lambdas_reg4 <- seq(4.5,6.5,0.1)  #setting range for lambdas

rmses_reg4 <- sapply(lambdas_reg4, function(reg4){ #optimizing rmse with different values of lambda
  
  temp <- edxtrain %>%  
    left_join(movie_effect, by = 'movieId') %>% #joining by movieId
    left_join(user_effect, by = 'userId') %>% #joining by userId
    left_join(genre_effect, by = 'genres') %>% #joining by genres
    group_by(movieyear) %>% #grouping by movieyear
    summarize(b_y = sum((rating - avg - b_i - b_j - b_g))/(n()+reg4)) #calculating movieyear effect while controlling for movie, user & genre effect
  
  predicted_ratings <- edxtest %>% 
    left_join(movie_effect, by = 'movieId') %>% #joining by movieId
    left_join(user_effect, by = 'userId') %>% #joining by userId
    left_join(genre_effect, by = 'genres') %>%  #joining by genres
    left_join(temp, by = 'movieyear') %>% #joining by movieyear
    mutate(pred = avg + b_i + b_j + b_g + b_y) %>% #predicting movie ratings using user, movie, genre & movieyear effect with varying lambda
    mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
    mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
    pull(pred)
  return(RMSE(predicted_ratings, edxtest$rating))
})

#plotting lambdas for fourth regularization model - Figure 8
qplot(lambdas_reg4, rmses_reg4) +  #creating plot of lambdas 
  labs(title = expression(paste("Figure 8: Optimal Value of  ", "", hat(lambda)[y], #labeling
                                " that minimizes the RMSE for ",  "\n",
                                hat(e)[i][j], " = ",  r[i][j]-bar(b)-hat(b)[i]-hat(b)[j]-hat(b)[g]-hat(b)[y])),
       x = expression(hat(lambda)[y]),
       y = "RMSE") +
  annotate("text", x = lambdas_reg4[which.min(rmses_reg4)]+0.15, y = 0.86355684875, colour="red",
           label = paste(" = ", lambdas_reg4[which.min(rmses_reg4)]), size=5) +
  annotate("text", x = lambdas_reg4[which.min(rmses_reg4)]+0.05, y = 0.86355684875, colour="red", #labeling to show optimal lambda
           label = expression(hat(lambda)[y]), size=5) #best lambda is 5.4

#using best lambda for movie, user, genre and movieyear effect
lambda_reg4 <- lambdas_reg4[which.min(rmses_reg4)] #setting lambda to optimal lambda

movieyear_effect <- edxtrain %>%  
  left_join(movie_effect, by = 'movieId') %>% #joining by movieId
  left_join(user_effect, by = 'userId') %>% #joining by userId
  left_join(genre_effect, by = 'genres') %>% #joining by genres
  group_by(movieyear) %>% #grouping by movieyear
  summarize(b_y = sum((rating - avg - b_i - b_j - b_g))/(n()+lambda_reg4)) #calculating movieyear effect while controlling for movie, user & genre effect

by <- edxtest %>% 
  left_join(movie_effect, by = 'movieId') %>% #joining by movieId
  left_join(user_effect, by = 'userId') %>% #joining by userId
  left_join(genre_effect, by = 'genres') %>% #joining by genres
  left_join(movieyear_effect, by = 'movieyear') %>% #joining by movieyear
  mutate(pred = avg + b_i + b_j + b_g + b_y) %>% #predicting movie ratings using user, movie, genre & movieyear effect with optimal lambda
  mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
  mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
  pull(pred)

rmse_movieyear_effect <- RMSE(by, edxtest$rating) #calculate RMSE for movieyear effect using edxtest dataset
rmse_movieyear_effect

#adding RMSE of movieyear effect to RMSE results data frame
rmse_results <- bind_rows(data.frame(model ="The Mean Model", RMSE = rmse_mean_model),
                          data.frame(model ="Regularized Model: Movie Effect", RMSE = rmse_movie_effect),
                          data.frame(model ="Regularized Model: Movie + User Effect", RMSE = rmse_user_effect),
                          data.frame(model ="Regularized Model: Movie + User + Genre Effect", RMSE = rmse_genre_effect),
                          data.frame(model ="Regularized Model: Movie + User + Genre + MovieYear Effect", RMSE = rmse_movieyear_effect))

#saving final RMSE results table
saveRDS(rmse_results, file = "rmse_results.Rds")

#creating a table with all the RMSE results from all the models used
knitr::kable(rmse_results)
rmse_results %>% gt()

#transforming validation dataset to ensure all variables used and format of variables to build model from edx dataset
#is in the validation dataset 
validation$reviewdatetime <- as_datetime(validation$timestamp) #changing timestamp to date and time
validation$reviewdate <- as_date(validation$reviewdatetime) #abstracting date from date and time column
validation$reviewyear <- as.numeric(format(validation$reviewdate,'%Y')) #abstracting year from date column
validation$reviewmonth <- as.numeric(format(validation$reviewdate,'%m')) #abstracting month from date column

#separate the movie year from the title of the movie and place in a separate column
validation$movieyear <- sub("\\).*", "", sub(".*\\(", "", validation$title))

#setting rating and movieyear as numeric similar to the edx dataset 
validation$rating <- as.numeric(validation$rating)
validation$movieyear <- as.numeric(validation$movieyear)

#testing the final model with movie, user, genre and movieyear effect against the validation set
validation_test <- validation %>% 
  left_join(movie_effect, by = 'movieId') %>% #joining by movieId
  left_join(user_effect, by = 'userId') %>% #joining by userId
  left_join(genre_effect, by = 'genres') %>% #joining by genres
  left_join(movieyear_effect, by = 'movieyear') %>%  #joining by movieyear
  mutate(pred = avg + b_i + b_j + b_g + b_y) %>% #predicting movie ratings using user, movie, genre & movieyear effect
  mutate(pred=ifelse(pred<0.5,0.5, pred)) %>%  #limiting rating to lowest value of 0.5 
  mutate(pred=ifelse(pred>5.0,5.0,pred)) %>%  #limiting rating to highest value of 5.0
  pull(pred)

rmse_validation <- RMSE(validation_test, validation$rating) #calculating RMSE of the final model using the hold-out validation dataset
rmse_validation #RMSE on validation set is 0.8646388 'hit the target'

