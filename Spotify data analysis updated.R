# This is a spotify dataset with values of the top songs from 2010 to 2019

 #Create an object called spotify
spotify <- read.csv("C:/Users/hp/Downloads/Spotify Data/top10s.csv")

#View the created object 
View(spotify)

#Check the summary statistics
summary(spotify)

#let's check for null values
 sum(is.na(spotify))
#The output is zero, so there are no null values in this dataset.
 
#From the summary statistics I can see that the max value for the pop column ( which is a measure of the popularity of the song) is 99 )
 install.packages("magrittr") 
 install.packages('plyr')
 install.packages("dplyr") 
 library(magrittr)
 library(dplyr) 
 spotify_grp <- spotify %>% group_by(year)
 
 # summarise on grouped data.
 spotify_top_pop <- spotify_grp %>% summarise(max_pop = max(pop), min_pop = min(pop))
 spotify_top_pop
 
 #So, I have successfully grouped the dataset according the the years, and displayed the maximum and minimum of the popularity values. From this I can some conclusions as to how the popularity level of songs are actually changing with respect to year.
 #For example the years with the highest popularity rates are 2019, 2013, 2017, and 2018. They also have the highest min_popularity values meaning that many songs released during these years are very popular compared to other years.
 
 # I can also explore the data to find out the most successful artists during this range of time, this is represented as the artists with the highest number of songs in this list.
 table(spotify$artist)
 
 result <- as.data.frame(table(spotify$artist))
 result <- result %>% arrange(desc(Freq))
 result
 #I will convert the resulting dataframe into a csv file and save it in my preferred location.
 write.csv(result, file= 'C:/Users/hp/Downloads/Spotify Data/top artists.csv')
 # Going through the resulting dataframe, I discovered that the most successful artists during this period of time are; Katy Perry, Justin Bieber, Rihanna, Maroon 5, Lady Gaga and Bruno Mars.
 
 # In the spirit of Exploratory Data Analysis, I can go ahead to say, I want to get details on the most popular genre of songs, ranking them by their cumulative popularity score.
spotify_genre <- spotify %>% group_by(top.genre)
spotify_genre_grp <- spotify_genre %>% summarise(total_pop_value= sum(pop))
spotify_genre_grp <- spotify_genre_grp %>% arrange(desc(total_pop_value))
spotify_genre_grp
#From the results I see that dance pop, pop and Canadian pop are the genres with the highest popularity values

#I will save the result to a csv file, so that I can view this later
write.csv(spotify_genre_grp,file='C:/Users/hp/Downloads/Spotify Data/spotify_genre_grp.csv')

  #scatter plot
 plot(spotify$bpm, spotify$pop, col='red', xlab='beats_per_min', ylab='popularity')
 plot(spotify$nrgy, spotify$pop, col='red', xlab='energy of the song', ylab='popularity')
 plot(spotify$dnce, spotify$pop, col='red', xlab='danceability of the song', ylab='popularity')
 plot(spotify$dB, spotify$pop, col='red', xlab='Decibel:loudness of the song', ylab='popularity')
 plot(spotify$live, spotify$pop, col='red', xlab='Liveness', ylab='popularity')
 plot(spotify$val, spotify$pop, col='red', xlab='Valence: level of positivity', ylab='popularity')
 plot(spotify$dur, spotify$pop, col='red', xlab='duration of the song', ylab='popularity')
 plot(spotify$acous, spotify$pop, col='red', xlab='acousticness of the song', ylab='popularity') 
 plot(spotify$spch, spotify$pop, col='red', xlab='speechiness of the song', ylab='popularity')
 
 #After examining all the plots, none of the plots really show any kind of linear relationship between the variables examined. It either the data points are huddled up together, or too widely dispersed.
 #This can be further explored by getting a correlation matrix. A correlation matrix only works with a numeric dataset, so we need to select the
 
 numer_spotify<- dplyr::select ( spotify,bpm,nrgy,dnce,dB,live,val,dur,acous,spch,pop)
 
 correlation_matrix <- cor(numer_spotify)
 correlation_matrix
 
 #Looking at the last column of the correlation matrix, the column contains the correlation of the other values against the pop column, and I can see that none of the values are large enough to indicate any linear relationship with the popularity value of the song. The largest in magnitude is '0.15', which is too small, and too close to zero to be counted as significant.
 
 #I have looked at most of the features in the dataset, viewing the 'pop' column as dependent on those other columns, this is an assumption, and it is done for the sake of Exploratory data analysis, trying to understand what's going on between different variables in a dataset.
 # Now I will look at the genre column, and try to see how this can be a predictor of whether a song makes it to the top songs for the year list.
 # First, I will need to get the number of times each genre featured on the original dataset.
 
 
spotify_genre_value <- as.data.frame(table(spotify$top.genre))
spotify_genre_count<- spotify_genre_value %>% arrange(desc(Freq))   # Once that is done, it can also be converted to csv file and saved to my computer
write.csv(spotify_genre_count, file= 'C:/Users/hp/Downloads/Spotify Data/spotify_genre_count.csv')
# So, by doing som elittle mathematics
no_of_songs<- sum(spotify_genre_count$Freq)
no_of_songs
# The total number of songs in this dataset is 603.
#So, if a newly released song is dance pop, it has (327/603) * 100= 54.23 percent chance of being on the Billboard's top songs list for the coming year. 
# if a newly released song is pop, it has (60/603) * 100= 9.95 percent chance of being on the Billboard's top songs list for the coming year. 
# As for Canadian pop, if a newly released song belongs to this genre, it has (34/603) * 100= 5.64 percent chance of being on the Billboard's top songs list for the coming year.

#These are predictors and something that an upcoming singer, or someone that just wants to start their music career might need to take into consideration. 