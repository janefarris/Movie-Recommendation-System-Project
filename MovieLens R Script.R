#####This file contains the R script for the Haravrd Edx Capstone Course MovieLens Projct

# Create edx set & validation set (final hold-out test set)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
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

#remove unnecessary files from the working directory
rm(dl, ratings, movies, test_index, temp, movielens, removed)

##Data Structure
#Structure and size of the edx dataset used to train ML algorithms
str(edx)
dim(edx)
#Structure and size of the validation dataset used as a final hold-out test set
str(validation)
dim(validation)

##Data Cleaning
sum(is.na(edx))
summary(edx) #alternate way to see there are no missing values in the edx dataset

sum(is.na(validation))
summary(validation) #alternate way to see there are no missing values in the validation dataset

library(lubridate)
edx <- mutate(edx, date = date(as_datetime(timestamp, origin="1970-01-01")))
#The date of rating is now in the correct form YYYY/MM/DD
head(edx$date)
#Drop the timestamp column
edx <- edx %>% select(-timestamp)

validation <- validation %>% mutate(validation,date = date(as_datetime(timestamp, origin="1970-01-01")))
#The date of rating is now in the correct form YYYY/MM/DD
head(validation$date)
#Drop the timestamp column
validation <- validation %>% select(-timestamp)

#Separate the movie release year and the movie title from the title column
#The last 6 characters of the title string represent the year in brackets
#Thus the indices -5 to -2 represent the release year without brackets
edx <- mutate(edx,year_released = as.numeric(str_sub(title,-5,-2)))
validation <- mutate(validation,year_released = as.numeric(str_sub(title,-5,-2)))

#Remove the year from the title column 
library(stringr)
edx <- mutate(edx,title = str_sub(title,1,-7))
validation <-mutate(validation,title = str_sub(title,1,-7))

#We can see that there are no ratings that occur before the movie premier date
edx %>% filter(date<year_released)
validation %>% filter(date<year_released)

#All distinct combinations of genre types in the dataset
all_genre_combinations <- unique(edx$genres)

#Detect genre types based on the separater "|"
genre_types <- unlist(strsplit(edx$genres, split = "\\|"))
#Keep only one of each genre
genre_list <- unique(genre_types)
#We can see there are 20 different genre types
genre_list

#Create 20 binary columns corresponding to each genre type in the genre_list object
edx <- edx %>% mutate(comedy=ifelse(str_detect(genres, "Comedy"),1,0),
romance=ifelse(str_detect(genres, "Romance"),1,0),
action=ifelse(str_detect(genres, "Action"),1,0),
crime=ifelse(str_detect(genres, "Crime"),1,0),
thriller=ifelse(str_detect(genres, "Thriller"),1,0),
drama=ifelse(str_detect(genres, "Drama"),1,0),
scifi=ifelse(str_detect(genres, "Sci-Fi"),1,0),
adventure=ifelse(str_detect(genres, "Adventure"),1,0),
children=ifelse(str_detect(genres, "Children"),1,0),
fantasy=ifelse(str_detect(genres, "Fantasy"),1,0),
war=ifelse(str_detect(genres, "War"),1,0),
animation=ifelse(str_detect(genres, "Animation"),1,0),
musical=ifelse(str_detect(genres, "Musical"),1,0),
western=ifelse(str_detect(genres, "Western"),1,0),
mystery=ifelse(str_detect(genres, "Mystery"),1,0),
filmnoir=ifelse(str_detect(genres, "Film-Noir"),1,0),
horror=ifelse(str_detect(genres, "Horror"),1,0),
documentary=ifelse(str_detect(genres, "Documentary"),1,0),
imax=ifelse(str_detect(genres, "IMAX"),1,0),
none=ifelse(str_detect(genres, "(no genres listed)"),1,0))
head(edx)
validation <- validation %>% mutate(comedy=ifelse(str_detect(genres, "Comedy"),1,0),
romance=ifelse(str_detect(genres, "Romance"),1,0),
action=ifelse(str_detect(genres, "Action"),1,0),
crime=ifelse(str_detect(genres, "Crime"),1,0),
thriller=ifelse(str_detect(genres, "Thriller"),1,0),
drama=ifelse(str_detect(genres, "Drama"),1,0),
scifi=ifelse(str_detect(genres, "Sci-Fi"),1,0),
adventure=ifelse(str_detect(genres, "Adventure"),1,0),
children=ifelse(str_detect(genres, "Children"),1,0),
fantasy=ifelse(str_detect(genres, "Fantasy"),1,0),
war=ifelse(str_detect(genres, "War"),1,0),
animation=ifelse(str_detect(genres, "Animation"),1,0),
musical=ifelse(str_detect(genres, "Musical"),1,0),
western=ifelse(str_detect(genres, "Western"),1,0),
mystery=ifelse(str_detect(genres, "Mystery"),1,0),
filmnoir=ifelse(str_detect(genres, "Film-Noir"),1,0),
horror=ifelse(str_detect(genres, "Horror"),1,0),
documentary=ifelse(str_detect(genres, "Documentary"),1,0),
imax=ifelse(str_detect(genres, "IMAX"),1,0),
none=ifelse(str_detect(genres, "(no genres listed)"),1,0))
head(validation)

#Remove the extra space at the end of all movie titles
edx <- mutate(edx, title=str_trim(title, side = c("right")))
validation <- mutate(validation, title=str_trim(title,side=c("right")))
#Relocate "The" if it's in the movie title
edx <- mutate(edx, title = sub('^(.*), The', 'The \\1', title))
validation <- mutate(validation, title = sub('^(.*), The', 'The \\1', title))  

##Data Exploration & Visualization
#Number of distinct movies in the dataset
n_distinct(edx$movieId) 
#Number of distinct users in the dataset
n_distinct(edx$userId)

#first rating date
max(edx$date)
#last rating date
min(edx$date)


#Data Exploration: Year of Rating
#Ratings vs. Year
library(ggthemes)
edx %>% mutate(year = year(date)) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "grey", fill="sky blue") + 
  ggtitle("Rating Distribution By Year") +
  xlab("Year of Rating") +
  ylab("Number of Ratings") +
  theme_minimal()

#Data Exploration: Movie Release Date (Movie Age)
#Age of movie distribution
edx %>%
  group_by(year_released) %>%
  summarise(count=n()) %>%
  ggplot(aes(year_released,count)) +
  geom_bar(stat="identity",color="dark grey",fill="sky blue") +
  labs(x="Movie Release Year",y="Count",title="Distribution of Movie Age") +
  theme_minimal()

#Calculate the average rating for each movie age
avg_rating <- edx %>% select(year_released, rating) %>% group_by(year_released) %>% summarise(rating = mean(rating))
head(avg_rating)
#Movie age with the highest average rating
avg_rating[which.max(avg_rating$rating),]
#Movie age with the lowest average rating
avg_rating[which.min(avg_rating$rating),]

#Age of movie vs average movie rating
edx %>% group_by(year_released) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year_released, rating)) +
  geom_point(color = "sky blue") +
  geom_line(color = "sky blue") +
  labs(x="Movie Release Year",y="Average Rating",title="Average Movie Rating by Movie Age") +
  theme_minimal()

#Data Exploration: Movie Titles
#Most rated movies
most_rated <- edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
head(most_rated)

#Least rated movies
least_rated <- edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(count)
head(least_rated)

#Find and plot the top 25 most rated movies
most_rated[1:25,] %>% 
  ggplot(aes(reorder(title, count),count)) +
  geom_bar(stat='identity',fill="sky blue") + 
  coord_flip(y=c(0, 35000)) +
  geom_text(aes(label= count),hjust=1.1, size=2) +
  labs(x="Movie Title", y="Number of ratings",title="Top 25 Most Rated Movies") +
  theme_minimal()

#Plot the distribution of movie titles
edx %>% 
  group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color="dark grey",fill = "sky blue") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_minimal()

#Data Exploration: Users
#Find how many movies each user has rated
ratings_by_user <- edx %>% count(userId)
#Most movies a user has rated
max(ratings_by_user)
#Least amount of movies a user has rated
min(ratings_by_user)

edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "dark grey",fill="sky blue") + 
  scale_x_log10() + 
  labs(x="Number of Movies Rated",y="Users",title="Distribution of User Ratings") +
  theme_minimal()

#Data Exploration: Genres
#How many movie ratings are in each genre
genre_count <- edx %>% select(-userId,-movieId,-rating,-title,-genres,-date,-year_released) %>% summarise_all(sum)
genre_count
#Genre with the most amount of ratings
which.max(genre_count)
#Genre with the least amount of ratings
which.min(genre_count)

genre_count <- transpose(genre_count)
genre_count <- genre_count %>% mutate(genres=genre_list)

#Bar Plot of the distribution of Ratings per Genre
options(scipen=10000) #Don't use scientific notation
genre_count %>% 
  ggplot(aes(reorder(genres,V1),V1)) +
  geom_bar(stat = "identity",fill="sky blue") +
  coord_flip() +
  geom_text(aes(label= V1),hjust=-0.1, size=2) +
  labs(x="Genre",y="Rating Count",title="Distribution of Ratings By Genre") +
  theme_minimal()

#Find the average rating of a movie in the comedy genre
edx %>% 
  filter(comedy==1) %>%
  summarise(mean=mean(rating)) %>%
  pull(mean)

#Set the number of significant figures to use
options(pillar.sigfig = 7)
#Create a result table
genre_averages <- tibble(Genre = "Comedy", Average_Rating = 3.436908)
genre_averages <- genre_averages %>% 
  add_row(Genre="Romance",Average_Rating=(edx%>%filter(romance==1)%>%
                                            summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Action",Average_Rating=(edx%>%filter(action==1)%>%
                                           summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Crime",Average_Rating=(edx%>%filter(crime==1)%>%
                                          summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Thriller",Average_Rating=(edx%>%filter(thriller==1)%>%
                                             summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Drama",Average_Rating=(edx%>%filter(drama==1)%>%
                                          summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="SciFi",Average_Rating=(edx%>%filter(scifi==1)%>%
                                          summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Adventure",Average_Rating=(edx%>%filter(adventure==1)%>%
                                              summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Children",Average_Rating=(edx%>%filter(children==1)%>%
                                             summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Fantasy",Average_Rating=(edx%>%filter(fantasy==1)%>%
                                            summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="War",Average_Rating=(edx%>%filter(war==1)%>%
                                        summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Animation",Average_Rating=(edx%>%filter(animation==1)%>%
                                              summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Musical",Average_Rating=(edx%>%filter(musical==1)%>%
                                            summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Western",Average_Rating=(edx%>%filter(western==1)%>%
                                            summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Mystery",Average_Rating=(edx%>%filter(mystery==1)%>%
                                            summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Film Noir",Average_Rating=(edx%>%filter(filmnoir==1)%>%
                                              summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Horror",Average_Rating=(edx%>%filter(horror==1)%>%
                                           summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="Documentary",Average_Rating=(edx%>%filter(documentary==1)%>%
                                                summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="IMAX",Average_Rating=(edx%>%filter(imax==1)%>%
                                         summarise(mean=mean(rating))%>%pull(mean))) %>%
  add_row(Genre="None",Average_Rating=(edx%>%filter(none==1)%>%
                                         summarise(mean=mean(rating))%>%pull(mean)))
genre_averages %>%
  ggplot(aes(reorder(Genre,Average_Rating),Average_Rating)) +
  geom_bar(stat = "identity",fill="sky blue") +
  coord_flip() +
  geom_text(aes(label= round(Average_Rating,3)),hjust=1.1, size=2) +
  labs(x="Genre",y="Average Rating",title="Average Ratings By Genre") +
  theme_minimal()

#Plot the genre distribution by movie release year in terms of proportion
library(scales)
edx %>% 
  select(year_released,comedy,romance,action,crime,thriller,drama,
         scifi,adventure,children,fantasy,war,animation,musical,western,mystery,
         filmnoir,horror,documentary,imax,none) %>%
  group_by(year_released) %>%
  summarise_all(sum) %>%
  gather("Type","Value",-year_released) %>%
  ggplot(aes(x=year_released,y=Value,fill=Type)) +
  geom_bar(stat = "identity",position="fill") +
  scale_y_continuous(labels = percent) +
  labs(x="Year Released",y="Percentage",title="Genre Distribution by Movie Release Year") +
  theme_minimal()

#Plot the genre distribution by movie release year
options(scipen=10000) #Don't use scientific notation
edx %>% 
  select(year_released,comedy,romance,action,crime,thriller,drama,
         scifi,adventure,children,fantasy,war,animation,musical,western,mystery,
         filmnoir,horror,documentary,imax,none) %>%
  group_by(year_released) %>%
  summarise_all(sum) %>%
  gather("Type","Value",-year_released) %>%
  ggplot(aes(x=year_released,y=Value,col=Type)) +
  geom_line(stat = "identity") +
  labs(x="Year Released",y="Count",title="Genre Distribution by Movie Release Year") +
  theme(legend.title = element_text(color = "black", size = 8),
        legend.text = element_text(color = "black", size = 6)) +
  theme_minimal()

#Data Exploration: Rating Values
#Most given ratings
rating_count <- edx %>% group_by(rating) %>% summarise(count=n()) %>% arrange(desc(count))
rating_count

options(scipen=10000) #Don't use scientific notation
#Plot the distribution of ratings
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_bar(stat="identity",color="dark grey",fill="sky blue") +
  labs(x="Rating",y="Count",title="Distribution of Ratings") +
  theme_minimal()

#Modelling
#Create Root Mean Squared Error (RMSE) function
rmse <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}

##Baseline Model: Average Rating
set.seed(1)
#Calculate the average rating of all movies in the dataset
avg_rating <- mean(edx$rating)
#Calculate RMSE using the average rating as the predicted value for each movie title and the ratings from the final hold out validation set as the observed values
rmse(validation$rating,avg_rating)

#Model 1
#Find the movie ID estimates
movie_effects <- edx %>% 
group_by(movieId) %>% 
summarize(b_i = mean(rating - avg_rating))

#Add the b_i estimates to our baseline model
predicted_ratings <- avg_rating + validation %>% 
left_join(movie_effects, by='movieId') %>%
pull(b_i)

#Check how our RMSE improves from our baseline model with the movie effect term
rmse(predicted_ratings, validation$rating)

#Model 2
#Find the user ID estimates
user_effects <- edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - avg_rating))

#Add the b_u estimates to our baseline model
predicted_ratings <- avg_rating + validation %>% 
  left_join(user_effects, by='userId') %>%
  pull(b_u)

#Check how our RMSE improves from our baseline model with the user effect term
rmse(predicted_ratings, validation$rating)

#Model 3
#Find the user ID estimates given the movie ID effect
user_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - avg_rating - b_i))

#Add the b_i & b_u estimates to our baseline model
predicted_ratings <- validation %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(prediction = avg_rating + b_i + b_u) %>%
  pull(prediction)

#Check how our RMSE improves from our baseline model with the movie & user effect terms
rmse(predicted_ratings, validation$rating)

#Model 4
#Create a linear model with genre types as predictors
model4 <- lm(rating~comedy+romance+action+crime+thriller+drama+scifi+
               adventure+children+fantasy+war+animation+musical+western+mystery+
               filmnoir+horror+documentary+imax,data = edx)
summary(model4)
predicted_ratings <- predict(model4,validation)
rmse(predicted_ratings, validation$rating)

#Model 5
#Find the estimate of each of the 19 genre effects, each time expanding the model
comedy_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  group_by(comedy) %>% 
  summarize(b_c = mean(rating-avg_rating-b_i-b_u))

drama_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  group_by(drama) %>% 
  summarize(b_d = mean(rating-avg_rating-b_i-b_u-b_c))

action_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  group_by(action) %>% 
  summarize(b_a = mean(rating - avg_rating - b_i - b_u - b_c - b_d))

thriller_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  group_by(thriller) %>% 
  summarize(b_t = mean(rating - avg_rating - b_i - b_u - b_c - b_d - b_a))

musical_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  group_by(musical) %>% 
  summarize(b_m = mean(rating - avg_rating - b_i - b_u - b_c - b_d - b_a - b_t))

filmnoir_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  group_by(filmnoir) %>% 
  summarize(b_n = mean(rating - avg_rating - b_i - b_u - b_c - b_d - b_a - b_t - b_m))

romance_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  group_by(romance) %>% 
  summarize(b_r = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n))

crime_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  group_by(crime) %>% 
  summarize(b_cr = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r))

scifi_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  group_by(scifi) %>% 
  summarize(b_s = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr))

adventure_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  group_by(adventure) %>% 
  summarize(b_ad = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s))

children_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  group_by(children) %>% 
  summarize(b_ch = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s-b_ad))

fantasy_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  group_by(fantasy) %>% 
  summarize(b_f = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s-b_ad-b_ch))

war_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  group_by(war) %>% 
  summarize(b_w = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                       -b_ad-b_ch-b_f))

animation_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  group_by(animation) %>% 
  summarize(b_an = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                        -b_ad-b_ch-b_f-b_w))

western_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  group_by(western) %>% 
  summarize(b_we = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                        -b_ad-b_ch-b_f-b_w-b_an))

mystery_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  left_join(western_effects, by='western') %>%
  group_by(mystery) %>% 
  summarize(b_my = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                        -b_ad-b_ch-b_f-b_w-b_an-b_we))

horror_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  left_join(western_effects, by='western') %>%
  left_join(mystery_effects, by='mystery') %>%
  group_by(horror) %>% 
  summarize(b_h = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                       -b_ad-b_ch-b_f-b_w-b_an-b_we-b_my))

documentary_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  left_join(western_effects, by='western') %>%
  left_join(mystery_effects, by='mystery') %>%
  left_join(horror_effects, by='horror') %>%
  group_by(documentary) %>% 
  summarize(b_do = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                        -b_ad-b_ch-b_f-b_w-b_an-b_we-b_my-b_h))

imax_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  left_join(western_effects, by='western') %>%
  left_join(mystery_effects, by='mystery') %>%
  left_join(horror_effects, by='horror') %>%
  left_join(documentary_effects, by='documentary') %>%
  group_by(imax) %>% 
  summarize(b_im = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                        -b_ad-b_ch-b_f-b_w-b_an-b_we-b_my-b_h-b_do))

predicted_ratings <- validation %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  left_join(western_effects, by='western') %>%
  left_join(mystery_effects, by='mystery') %>%
  left_join(horror_effects, by='horror') %>%
  left_join(documentary_effects, by='documentary') %>%
  left_join(imax_effects, by='imax') %>%
  mutate(prediction = avg_rating+b_i+b_u+b_c+b_d+b_a+b_t+b_m+b_n+b_r+b_cr+b_s
         +b_ad+b_ch+b_f+b_w+b_an+b_we+b_my+b_h+b_do) %>%
  pull(prediction)

#Check how our RMSE improves from our baseline model with the movie,user & genre effect terms
rmse(predicted_ratings, validation$rating)

#Model 6
#Find the age of a movie effect (year the movie was released)
year_effects <- edx %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  left_join(western_effects, by='western') %>%
  left_join(mystery_effects, by='mystery') %>%
  left_join(horror_effects, by='horror') %>%
  left_join(documentary_effects, by='documentary') %>%
  left_join(imax_effects, by='imax') %>%
  group_by(year_released) %>% 
  summarize(b_y = mean(rating-avg_rating-b_i-b_u-b_c-b_d-b_a-b_t-b_m-b_n-b_r-b_cr-b_s
                       -b_ad-b_ch-b_f-b_w-b_an-b_we-b_my-b_h-b_do-b_im))

predicted_ratings <- validation %>% 
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  left_join(comedy_effects, by='comedy') %>%
  left_join(drama_effects, by='drama') %>%
  left_join(action_effects, by='action') %>%
  left_join(thriller_effects, by='thriller') %>%
  left_join(musical_effects, by='musical') %>%
  left_join(filmnoir_effects, by='filmnoir') %>%
  left_join(romance_effects, by='romance') %>%
  left_join(crime_effects, by='crime') %>%
  left_join(scifi_effects, by='scifi') %>%
  left_join(adventure_effects, by='adventure') %>%
  left_join(children_effects, by='children') %>%
  left_join(fantasy_effects, by='fantasy') %>%
  left_join(war_effects, by='war') %>%
  left_join(animation_effects, by='animation') %>%
  left_join(western_effects, by='western') %>%
  left_join(mystery_effects, by='mystery') %>%
  left_join(horror_effects, by='horror') %>%
  left_join(documentary_effects, by='documentary') %>%
  left_join(imax_effects, by='imax') %>%
  left_join(year_effects, by='year_released') %>%
  mutate(prediction = avg_rating+b_i+b_u+b_c+b_d+b_a+b_t+b_m+b_n+b_r+b_cr+b_s
         +b_ad+b_ch+b_f+b_w+b_an+b_we+b_my+b_h+b_do+b_y) %>%
  pull(prediction)

#Check how our RMSE improves from our baseline model with the movie,user,genre & year effect terms
rmse(predicted_ratings, validation$rating)

#Model 7
#Find the optimal lambda by first testing a sequence of potential lambdas
lambda <- seq(0, 10, 0.25)
#Calculate the rmses for each of the lambda values
rmses <- sapply(lambda, function(l){
b_i <- edx %>% 
group_by(movieId) %>%
summarize(b_i = sum(rating - avg_rating)/(n()+l))

b_u <- edx %>% 
left_join(b_i, by="movieId") %>%
group_by(userId) %>%
summarize(b_u = sum(rating - b_i - avg_rating)/(n()+l))

b_y <- edx %>% 
left_join(b_i, by='movieId') %>%
left_join(b_u, by='userId') %>%
group_by(year_released) %>% 
summarize(b_y = sum(rating-b_i-b_u-avg_rating)/(n()+l))

b_g <- edx %>% 
left_join(b_i, by='movieId') %>%
left_join(b_u, by='userId') %>%
left_join(b_y, by='year_released') %>%
group_by(genres) %>% 
summarize(b_g = sum(rating-b_i-b_u-b_y-avg_rating)/(n()+l))

predicted_ratings <- validation %>% 
left_join(b_i, by = "movieId") %>%
left_join(b_u, by = "userId") %>%
left_join(b_y, by = "year_released") %>%
left_join(b_g, by = "genres") %>%
mutate(pred = avg_rating + b_i + b_u + b_y + b_g) %>%
pull(pred)

return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambda, rmses)
#The optimal lambda is 4.75
min_lambda <- lambda[which.min(rmses)]
min(rmses)

#Results
#Set the number of significant figures for the RMSE
options(pillar.sigfig = 7)
#Create a result table
results <- tibble(Method = "Baseline Model", RMSE = 1.061202)
results <- results %>% 
add_row(Method="Linear Model - Movie Effect", RMSE = 0.9439087) %>%
add_row(Method="Linear Model - User Effect", RMSE = 0.978336) %>%
add_row(Method="Linear Model - Movie & User Effectt", RMSE = 0.8653488) %>%
add_row(Method="Linear Model - Genre Effect", RMSE = 1.042038) %>%
add_row(Method="Linear Model - Movie, User & Genre Effect", RMSE = 0.8652027) %>%
add_row(Method="Linear Model - Movie, User, Genre & Year Effect", RMSE = 0.8648744) %>%
add_row(Method="Regularized Model - Movie, User, Genre & Year Effect", RMSE = 0.8642527)
results