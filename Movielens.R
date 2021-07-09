
# Turning of scientific notation for numbers:
options(scipen = 999)
# installing the necessary libraries:
if(!require(Metrics)) install.packages("Metrics", 
                                       repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", 
                                        epos = "http://cran.us.r-project.org")
## 1. INTRODUCTION

# installing the required packages (tidyverse, caret, data.table):
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table",
                                          repos = "http://cran.us.r-project.org")
# loading the required packages (tidyverse, caret, data.table):
library(tidyverse)
library(caret)
library(data.table)

# Downloading and arranging "MovieLens 10M dataset" from 
# "http://files.grouplens.org/datasets/movielens/ml-10m.zip"):
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Have a glimpse at the dataset
movielens %>% glimpse()

# The number of distinct users and movies:
paste("There are ", n_distinct(movielens$userId), 
      " distinct users in the `movielens` dataset.")
paste("There are ", n_distinct(movielens$movieId), 
      " distinct movies in the `movielens` dataset")

# Displaying distribution of the number of movies watched by users:
movielens_group_user <- movielens %>%
  group_by(userId) %>% 
  summarize(number_of_movies = n_distinct(movieId))
movielens_group_user %>%
  ggplot(aes(x = number_of_movies)) +
  geom_bar() + 
  ggtitle("Distribution of number of movies watched by each user")


# Calculating the descriptive statistics of the `number_of_movies` 
# watched by each user:
summary(movielens_group_user$number_of_movies)



# Displaying distribution of the number of users by each movie:
movielens_group_movie <- movielens %>%
  group_by(title) %>% 
  summarize(number_of_users = n_distinct(userId))

# Printing the titles of the top five most frequently seen movies:
head(movielens_group_movie[order(movielens_group_movie$number_of_users,
                                 decreasing = TRUE),])



# Displaying a histogram of the values in the `rating` column 
# of the `movielens` dataset:
movielens %>%
  ggplot(aes(x = rating)) +
  geom_bar() + 
  ggtitle("Distribution of Ratings")

# Calculating the descriptive statistics of the `ratings` column in 
# `movielens` dataset:
summary(movielens$rating)

# Printing the titles of the top five highest rated movies:
movielens %>%
  group_by(title) %>% 
  summarize(average_rating = mean(rating)) %>%
  arrange(desc(average_rating)) %>%
  head()

# Printing the first top five highest rated movies watched 
# by at least 100 users:
movielens %>%
  group_by(title) %>% 
  summarize(average_rating = mean(rating), 
            number_of_users = n_distinct(userId)) %>%
  filter(number_of_users>100) %>%
  arrange(desc(average_rating)) %>%
  head()

# Finding the median number of movies watched by each user:
median_movies_user <- median(movielens_group_user$number_of_movies)

# Creating a mask for the five times of the median number of 
# movies per user:
mask_user <- movielens_group_user[movielens_group_user$number_of_movies>5*median_movies_user,]

# Applying the `mask_user`to the actual `movielens` dataset:
movielens_ <- movielens[movielens$userId %in% mask_user$userId,]
# Printing the number of users in the final dataset:
nrow(movielens_)

## 3. METHODS AND ANALYSIS

# Validation set will be 10% of MovieLens data:
set.seed(1) 
test_index <- createDataPartition(y = movielens_$rating, times = 1, 
                                  p = 0.1, list = FALSE)
edx <- movielens_[-test_index,]
temp <- movielens_[test_index,]

# Making sure that userId and movieId in validation set are also in edx set:
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Converting `edx` dataset to a `dataframe`:
edx <- as.data.frame(edx)

# Selecting `userId`, `title` and `rating`:
edx_ <- edx[c("userId","title","rating")]

# Loading `reshape2` library for `dcast()` function:
library(reshape2)
# Creating user-rating matrix:
user_rating_matrix <- reshape2::dcast(data = edx_, 
                                      formula = userId ~ title,
                                      fun.aggregate=mean, value.var = "rating")

# Checking the class of `user_rating_matrix`:
class(user_rating_matrix)

# Checking the dimentions of `user_rating_matrix`:
dim(user_rating_matrix)

# Printing the first 5 rows and first 4 columns of `user_rating_matrix`:
user_rating_matrix[1:5,1:4]

# Setting the userId as index (row names) of the user_rating_matrix dataframe:
rownames(user_rating_matrix) <- user_rating_matrix$userId

# Eliminating the `userId` column of the `user_rating_matrix`:
user_rating_matrix <- user_rating_matrix[,-1]

# Printing the first 5 rows and first 4 columns of `user_rating_matrix`:
user_rating_matrix[1:5,1:4]

# Finding the average of the ratings given by each user in 
# user_rating_matrix:
average_ratings <- rowMeans(user_rating_matrix, na.rm=TRUE)

# Creating a function for subtracting `average_ratings` 
# vector from another vector:
subtracting <- function(x) {
    dif <- x - average_ratings
    return(dif)
}

# Subtracting the row averages from each column in user_rating_matrix, 
# to center each users ratings around 0:
user_rating_matrix_centered <- apply(user_rating_matrix, 2, subtracting )

# Printing the first 5 rows and first 4 columns of `user_rating_matrix_centered`:
user_rating_matrix_centered[1:5,1:4]

#Filling the empty values in the newly created `user_rating_matrix_centered` with zeros:
user_rating_matrix_normed <- replace_na(user_rating_matrix_centered,0)

# Printing the first 5 rows and first 4 columns of `user_rating_matrix_normed`:
user_rating_matrix_normed[1:5,1:4]

#### Spartsity

# Counting the number of empty cells in `user_rating_matrix`:
nas <- sum(is.na(user_rating_matrix))

# Counting the total number of cells in `user_rating_matrix`: 
size <- dim(user_rating_matrix)[1] * dim(user_rating_matrix)[2]

# Calculating the sparsity of the `user_rating_matrix` by 
# dividing `nas` by `size` and printing the result.
sparsity <- nas/size
print(sparsity)

As can be seen, the `user_rating_matrix`dataframe which we use in our analysis is over 98.5% empty. This means that less than 1.5% of the dataframe includes any data. This suggests that it would be limited in its value for making predictions using KNN.

We can also count how often each movie in the `user_rating_matrix` dataframe has been given a rating, and then see the distribution of the number of times each movie has been rated.

# Creating a counter function which counts non-empty cells in its argument:
count_nonna <- function(x) {
  length(x) - sum(is.na(x))
}

# Counting the number of non-empty cells in each column of `user_rating_matrix`: 
nas_col <- apply(user_rating_matrix, 2, count_nonna)

# Converting the `nas_col` vector into a dataframe and printing the first 6 rows:
nas_col_df <- as.data.frame(nas_col)
colnames(nas_col_df) <- "times_rated"
head(nas_col_df)

# Creating a distribution graph of the number of times each movie has rated:
nas_col_df %>%
  ggplot(aes(x=times_rated)) +
  geom_bar() + 
  ggtitle("Distribution of number of times each movie has been rated")

# Calculating the statistics of the `times_rated` column:
summary(nas_col_df$times_rated)

# Calculating proportional (to the number of users) statistics of 
# the `times_rated` column:
summary(nas_col_df$times_rated)/nrow(user_rating_matrix)

### MATRIX FACTORIZATION 

#### Singular Value Decomposition

# Computing the Singular Value Decomposition (SVD) matrix of the 
# `user_ratings_normed` dataset:
SVD_ <- svd(user_rating_matrix_normed)

# Checking the elements of SVD_:
names(SVD_)

# Diagonalizing the vector `d` in SVD_ elements, and finding 
# sigma component of matrix factorization:
sigma <- diag(SVD_$d) 

# Extracting `u` element of `SVD_` list as U:
U <- SVD_$u

# Extracting `v` element of `SVD_` list as Vt:
Vt <- t(SVD_$v)

# Multiplication of U, sigma and Vt (prediction of 
# `user_ratings_normed` dataset):
U_sigma_Vt <- U %*% sigma %*% Vt


# Creating a function for adding `average_ratings` 
# (original dataframe's row averages) vector to another vector:
  adding <- function(x) {
    add <- x + average_ratings
    return(add)
  }
  
  # Adding the row averages to each column in U_sigma_Vt, 
  # to uncenter each users ratings (prediction results):
  user_rating_matrix_uncentered <- apply(U_sigma_Vt, 2, adding)
  
  # Printing the first 20 rows and first 20 columns of `user_rating_matrix_uncentered`:
  user_rating_matrix_uncentered[1:20,1:20]
  
  dim(sigma)
  dim(U)
  dim(Vt)
  
  # Creating a dataframe of the prediction results:
  user_rating_matrix_pred <- as.data.frame(user_rating_matrix_uncentered)
  
  # Adding row and column names to `user_rating_matrix_pred` dataset:
  rownames(user_rating_matrix_pred) <- rownames(user_rating_matrix)
  colnames(user_rating_matrix_pred) <- colnames(user_rating_matrix)
    
  # Printing the first 20 rows and first 5 columns of `user_rating_matrix_pred` dataset:
  user_rating_matrix_pred[1:20,1:5]
  
  # Printing the first 20 rows and first 5 columns of `user_rating_matrix`:
  user_rating_matrix[1:20,1:5]
  
  # Creating the mask to find filter the actually rated items by users:
  mask<- !is.na(user_rating_matrix)
  
  # loading the `Metrics` library for `rmse()` function:
  library(Metrics)
  
  # Finding the RMSE by comparing the `user_rating_matrix_pred` 
  # (giving the rating predictions) and
  # `user_rating_matrix` (actual ratings) datasets:
  RMSE_model <- rmse(user_rating_matrix[mask],user_rating_matrix_pred[mask])
  paste("RMSE of the model is:",RMSE_model)
  
  ### MODEL VALIDATION AND RESULTS
  
  In order to validate the RMSE result we found above, we conduct SVD algorithm also with the `validation` dataset and with the same process and found the final RMSE.
  
  # Converting `validation` dataset to a `dataframe`:
  validation <- as.data.frame(validation)
  
  # Selecting `userId`, `title` and `rating`:
  validation_ <- validation[c("userId","title","rating")]
  
  # Calculating the dimensions of the `validation_` dataset:
  dim(validation_)
  
  
  # Creating user-rating matrix_val:
  user_rating_matrix_val <- reshape2::dcast(data = validation, 
                                            formula = userId ~ title, 
                                            fun.aggregate=mean, value.var = "rating")
  
  # Checking the dimentions of `user_rating_matrix_val`:
  dim(user_rating_matrix_val)
  
  # Setting the userId as index (row names) of the user_rating_matrix_val dataframe:
  rownames(user_rating_matrix_val) <- user_rating_matrix_val$userId
  
  # Eliminating the `userId` column of the `user_rating_matrix_val`:
  user_rating_matrix_val <- user_rating_matrix_val[,-1]
  
  # Finding the average of the ratings given by each user in user_rating_matrix_val:
  average_ratings_val <- rowMeans(user_rating_matrix_val, na.rm=TRUE)
  
  # Creating a function for subtracting `average_ratings_val` vector from another vector:
  subtracting_val <- function(x) {
    dif <- x - average_ratings_val
    return(dif)
  }
  
  # Subtracting the row averages from each column in user_rating_matrix_val, 
  # to center each users ratings around 0:
  user_rating_matrix_centered_val <- apply(user_rating_matrix_val, 2, 
                                           subtracting_val )
  
  library(tidyr)
  # Filling the empty values in the newly created `user_rating_matrix_centered_val` with zeros:
  user_rating_matrix_normed_val <- replace_na(user_rating_matrix_centered_val,0)
  
  # Computing the Singular Value Decomposition (SVD) matrix of the 
  # `user_ratings_normed_val` dataset:
  SVD_val <- svd(user_rating_matrix_normed_val)
  
  # Diagonalizing the vector `d` in SVD_val elements, and finding sigma component 
  # of matrix factorization:
  sigma_val <- diag(SVD_val$d) 
  
  # Extracting `u` element of `SVD_val` list as U_val:
  U_val <- SVD_val$u
  
  # Extracting `v` element of `SVD_` list as Vt_val:
  Vt_val <- SVD_val$v
  
  # Multiplication of U_val, sigma_val and Vt_val (prediction of `user_ratings_normed` dataset):
  U_sigma_Vt_val <- U_val %*% sigma_val %*% t(Vt_val)
  
  # Creating a function for adding `average_ratings_val` (original dataframe's 
  # row averages) vector to another vector:
  adding_val <- function(x) {
    add <- x + average_ratings_val
    return(add)
  }
  
  # Adding the row averages to each column in U_sigma_Vt_val, to uncenter each 
  # users ratings (prediction results):
  user_rating_matrix_uncentered_val <- apply(U_sigma_Vt_val, 2, adding_val)
  
  # Creating a dataframe of the prediction results:
  user_rating_matrix_pred_val <- as.data.frame(user_rating_matrix_uncentered_val)
  
  # Adding row and column names to `user_rating_matrix_pred_val` dataset:
  rownames(user_rating_matrix_pred_val) <- rownames(user_rating_matrix_val)
  colnames(user_rating_matrix_pred_val) <- colnames(user_rating_matrix_val)
  
  # Creating the mask to find filter the actually rated items by users:
  mask_val<- !is.na(user_rating_matrix_val)
  
  # Calculating the RMSE of the validation model:
  RMSE_validation <- rmse(user_rating_matrix_val[mask_val],user_rating_matrix_pred_val[mask_val])
  paste("RMSE of the validation model is:",RMSE_validation)

# Selecting User 96 from `user_rating_matrix_pred-val` dataframe:
user_96_ratings <- user_rating_matrix_pred_val["96",]
# Sort the ratings of User 96 from high to low and selecting the first 
# 5 movies, and creating a mask:
mask_96 <- order(user_96_ratings,decreasing = T)[1:5]
# Printing the names of these 5 movies:
names(user_rating_matrix_pred_val)[mask_96]