#Introduction

#package installs
#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

#libraries
#library(tidyverse)
#library(caret)

# MovieLens 10M dataset:
# edx: https://drive.google.com/open?id=1e2hKKMvgcyM4pHQYRUC_4l55Qudep46z
# validation: https://drive.google.com/open?id=19Nh8bw3MKoxF7fN1Maa3cYIvVKTsG82i
#edx <- readRDS("C:/Users/giand/Desktop/edx.rds")
#validation <- readRDS("C:/Users/giand/Desktop/validation.rds")


################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

#Summarise data
head(edx)
summary(edx)

# Movies, Users and Genres in Database
edx %>% summarise(
  unique_movies = n_distinct(movieId),
  unique_users = n_distinct(userId),
  unique_genres = n_distinct(genres))
# Ratings Mean
rating_mean <- mean(edx$rating)
rating_mean

# Ratings Histogram
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "blue") +
  xlab("Rating") +
  ylab("Count") +
  ggtitle("Ratings Histogram") +
  theme(plot.title = element_text(hjust = 0.5))

# Users Number of Ratings
edx %>% 
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "blue", bins=20) +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Users") +
  ggtitle("Users Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

# Movies Rated- Users average rating
edx %>%
  group_by(userId) %>%
  summarise(mu_user = mean(rating), number = n()) %>%
  ggplot(aes(x = mu_user, y = number)) +
  geom_bin2d( ) +
  scale_fill_gradientn(colors = grey.colors(10)) +
  labs(fill="User Count") +
  scale_y_log10() +
  ggtitle("Users Average Ratings per Number of Rated Movies") +
  xlab("Average Rating") +
  ylab("Number of Movies Rated") +
  theme(plot.title = element_text(hjust = 0.5))

# Ratings Movies - Number of Ratings
edx %>% 
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "black", bins=30) +
  scale_x_log10() +
  xlab("Number of Ratings") +
  ylab("Number of Movies") +
  ggtitle("Movie Number of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

##Analysis and Results

#1. Prediction based on Mean Rating
mu<- mean(edx$rating)
mu
mean_rmse<- RMSE(validation$rating, mu)
mean_rmse

#Save the results in data frame
rmse_results<- data.frame(Method= "Mean Rating", RMSE=mean_rmse)
rmse_results%>% knitr::kable()


#2. Movie effect Model, importing movie's mean rating bias (b_i)
mu<- mean(edx$rating)
movie_avg<- edx %>% 
  group_by(movieId) %>%
  summarise(b_i=mean(rating-mu))

predicted_ratings<- mu + validation %>% 
  left_join(movie_avg, by= "movieId") %>%
  .$b_i

movie_RMSE<-RMSE(predicted_ratings, validation$rating)
rmse_results<-bind_rows(rmse_results, data.frame(Method= " Movie Effect Model", RMSE= movie_RMSE))
rmse_results %>% knitr::kable()

#3. Movie and User Effect Model
user_avgs<- edx %>% left_join(movie_avg, by="movieId") %>%
  group_by(userId)%>%
  summarise(b_u = mean(rating-mu-b_i))

predicted_ratings<- validation %>% 
  left_join(movie_avg, by="movieId") %>%
  left_join(user_avgs, by= "userId") %>%
  mutate(pred=mu + b_i + b_u) %>% .$ pred

user_movie_model_RMSE<- RMSE(predicted_ratings, validation$rating)
rmse_results<- bind_rows(rmse_results, data.frame(Method= "Movie and User Effct Model", RMSE= user_movie_model_RMSE))
rmse_results %>% knitr::kable()


##Regularization

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, validation$rating))
  
})

rmse_regularisation <- min(rmses)
rmse_regularisation


# Plot RMSE against Lambdas to find optimal lambda
qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,data_frame(method="Regularised Movie and User Effects Model", RMSE = rmse_regularisation))
rmse_results %>% knitr::kable()

