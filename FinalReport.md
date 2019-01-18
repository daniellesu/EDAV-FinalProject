---
title: "Loud and Proud: Exploring DSI's Taste in Music"
author: "Hammaad Adam, Lea Collin, Ling (Kelly) He, Danielle Su"
date: December 10, 2018
output:
  html_document:
    keep_md: true
---

Note: All of the code for this project can be found [here](https://github.com/LeaCollin0518/EDAV-FinalProject).









## I. Introduction

The focus of our final project is exploring the music tastes of our classmates here at Columbia's Data Science Institute (DSI). We specifically chose to use the "Your Top Songs 2017" playlists that Spotify creates because this playlist is created for almost all Spotify users and it is static, so we would not have to worry about differences in the time we are pulling the playlists. This is a playlist with each user's 100 most listened to songs in 2017. With this dataset, we examined the listening patterns of our classmates, the different metrics of each song, how these metrics are correlated, and trends both within our class and in comparison with US/global populations. 

Our team members are Hammaad Adam, Lea Collin, Ling (Kelly) He, and Danielle Su. Lea contributed to cleaning the survey data and exploring what features make songs and artists popular for this year's class. Kelly worked on exploring general audio features of all the songs and some of the survey input data. Danielle worked on creating the initial data gathering survey and exploring trends within the class in terms of how students differ. Hammaad worked on exploring the commonalities and key differences between different students' playlists, by songs, artists, and genre. Danielle and Kelly also created the interactive D3 component of the project. All group members contributed to writing this report. 

## II. Data Description

Spotify's "Your Top Songs 2017" playlist was only created for users that met certain criteria: user listened to at least 5 different artists, 30 different tracks, and 60 minutes of music from January 1st to November 1st, 2017. An interesting implication of the dates used for this playlist is that all of December, and therefore likely a lot of holiday music, is not included in the data.
  
We collected our data using Spotify's API and spotifyr, a R wrapper, that was developed to be used with it.(For more information on the wrapper see [here](https://www.rcharlie.com/spotifyr/). Using spotifyr, you can input a user ID and a playlist ID, and extract all the metadata Spotify has on each track of the playlist. All of the essential attributes of the track are included, such as track name and artist name. In addition to that, Spotify also has their own calculated audio features for each track including: genre, danceability, valence, energy, loudness, and track popularity. A more thorough description of these features can be found under **Audio Features**, later on in this report.

In order to actually get the Spotify user IDs and playlist IDs for our classmates, we sent out a survey giving detailed instructions on how to provide this information. We also asked classsmates to optionally add information about themselves, such as age, gender, and nationality, so that we could explore whether there were any trends within these smaller subsets of our class population.

Note: since we compiled our own dataset, it is not publicly available on the internet, and we cannot provide a link to the source.

## III. Analysis of Data Quality

The quality of the data Spotify provided was excellent in terms of the number of missing values, the only features that had missing values were columns such as "album image", which was not used for our analysis. The downside to using Spotify's API is the dependency on Spotify's algorithms for determining audio feature data. For example, we know that a track's popularity value (ranging from 0 to 100) is calculated based on the total number of streams and the recency of those streams. Since this was our primary method of knowing the popularity of the track, we relied on its accuracy. However, if given more time for this project, we would have liked to explore other popularity measures such as Billboard top charts, Twitter activity, Instagram mentions, and music award recipients.

One primary issue of our dataset, is the small number of respondents to the survey. While 2400 tracks is a substantial amount in terms of audio features, it is only 24 students, so when trying to subset that population in terms of gender or age, the analysis is not robust. Another thing that we did not realize would be such a big factor is the fact that Spotify is not available in many countries, such as China and India, where a lot of our classmates were living in 2017.

Below, we look at the different missing data patterns. 


```r
library(extracat)
survey_data <- read_csv("StudentResponses.csv", col_names = TRUE)
```

```
## Parsed with column specification:
## cols(
##   Timestamp = col_character(),
##   Username = col_character(),
##   `Columbia Uni ID:` = col_character(),
##   `Age:` = col_integer(),
##   `Gender:` = col_character(),
##   `Nationality:` = col_character(),
##   `Location in 2017:` = col_character(),
##   `Undergraduate University:` = col_character(),
##   `How often would you say you used Spotify in 2017?` = col_integer(),
##   `Spotify User Name:` = col_character(),
##   `"Your Top Songs 2017" Playlist URI:` = col_character(),
##   `If you don't use Spotify or if you also you use other platforms to listen to music, please list what you use most frequently:` = col_character(),
##   `Should you have unique listening patterns in some way, would you agree to be mentioned by name in our project presentation to the class?` = col_character()
## )
```

```r
names(survey_data) <- c("Timestamp", "email", "uni", "age", "gender", "nationality", "location2017", "undergrad", "useFrequency", "username", "playlistID", "otherPlatform", "consent")
visna(survey_data, sort = 'b')
```

![](FinalReport_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

As we can see from this plot, missing the "otherPlatform" variable (and no other variables) is the most common missing pattern in the survey data. The "otherPlatform" column was an optional survey question asking what people use for music-listening platforms apart from Spotify (such as Apple Music, Pandora, etc.). This information would be useful if we were to continue the project, we could explore other data from other music-listening platforms.

The second most common missing pattern is to not have any columns missing. Out of all the students that provided their Spotify information, one student mistakenly gave us a playlist other than the "Your Top Songs 2017", so we were unable to include him in our analysis. For the most part, this graph shows that we were able to capture the most pertinent information from our respondents. Though we were not able to collect as much data as we would have liked, the data we did collect was of fairly good quality in terms of the information we wanted to gather. 

We did quite a bit of analysis using artist's genre so we took a look at how many artists have this information available. As we see below, only 5.3%% of artists in the dataset have missing genre information, or 128 out of 2400 rows in the dataset.


```r
data_2 = data
data_2[which(data_2$artist_genres == "NULL"),] <- NA
print (sum(is.na(data_2$artist_genres))/nrow(data_2))
```

```
## [1] 0.05
```

```r
print (sum(is.na(data_2$artist_genres)))
```

```
## [1] 120
```

## IV. Main Analysis

### Data Cleaning

Before we get to the analysis, it is worth briefly dicussing our data cleaning process. The respondents to the survey were instructed on how to find, copy, and paste their user and playlist ID's from the Spotify application or Spotify webplayer. However, the actual ID's still needed to be extracted from the strings that were copied. In order to extract the ID's, we used R's **stringr** package and Regex. Once these ID's were extracted, it was straightforward to iterate through each ID, get the playlist features using the Spotify API, and progressively build up a dataframe with everyone's songs. We then pulled in additional information (e.g. album release date, genre) for each track, once again using the Spotify API. 

The code for these processes can be found on github here: 

[Data Cleaning](https://github.com/LeaCollin0518/EDAV-FinalProject/blob/master/lea-Work/DataCleaning.R)

[Getting Album Data](https://github.com/LeaCollin0518/EDAV-FinalProject/blob/master/CleaningAlbumData.Rmd)

[Getting Genre](https://github.com/LeaCollin0518/EDAV-FinalProject/blob/master/Hammaad-Work/DataImportGenre.R)

### Key Questions

Now that we've discussed our data collection and cleaning, let's dive into the analysis. Our data lends itself to a multitude of fascinating projects, and narrowing the scope of our focus was a real challenge. However, we ultimately decided to focus on the key questions that allow us to comprehensively characterize the music listening habits and tastes of our DSI class. To this end, our analysis was guided by four overarching questions: 

1. What are the distinctive features of a song that is popular within the DSI cohort?
2. How do DSI listening habits compare with US/global trends?
3. How similar are DSI students in their music preferences?
4. What are the key ways in which DSI students differ from one another?

### 1. What are the distinctive features of a song popular within the DSI cohort?

**Audio Features**

One of the main questions we wanted to address was "what makes a DSI song popular?" To begin answering this question, we examined the various audio features of the 2400 songs collected from our survey and how they correlate with each other and other survey input variables. In particular, we examined the following eight audio features: danceability, energy, loudness, valence, tempo, acousticness, speechiness, and instrumentalness. Most of these metrics have values between 0 and 1, representing the intensity of each variable (closer to 0 indicates low intensity and closer to 1 indicates high intensity). However, loudness and tempo needed to be normalized to values between 0 and 1. We excluded the liveness variable since it detects the presence of an audience in the recording rather than measuring any characteristic of the song, which we thought was not a particularly interesting feature. 

Descriptions of the chosen audio features:
1. **Danceability** describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity.
2. **Energy** represents a perceptual measure of intensity and activity.
3. **Loudness** is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db.
4. **Valence** describes the musical positiveness conveyed by a track.
5. **Tempo** represents the overall estimated tempo of a track in beats per minute (BPM).
6. **Acousticness** is a confidence measure of whether the track is acoustic.
7. **Speechiness** detects the presence of spoken words in a track.
8. **Instrumentalness** predicts whether a track contains no vocals.

First we took a holistic approach and created a parallel coordinates plot of all eight audio features of interest. We also thought it would be interesting to see if there are potential clusters or trends among these features conditioned on our survey input variables such as gender and location in 2017. 

Out of the 24 respondents, a third were female and the rest were male. In the parallel coordinate plot grouped by gender, we observe that each gender's audio feature behavior differs slightly. Specifically, female users tend to listen to music with low acousticness, speechiness, and instrumentalness, whereas the male behavior for those features is more varied. In general, all features are well spread out in the value range between 0 and 1, except for loudness, which skews high, and speechiness, which skews low.




```r
ggparcoord(df, columns = 1:8, alphaLines = .2, 
           scale = "uniminmax", groupColumn = "gender", splineFactor = 10) +
  ggtitle("Parallel Coordinates Plot by Gender") +
    theme_minimal(15) + theme(legend.position = "bottom") + 
      scale_color_manual(name="gender", labels = c("Female", "Male"), 
                          values = c("Female"="#663399", "Male"="#66FFFF")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](FinalReport_files/figure-html/parcoord-1.png)<!-- -->

We also created a parallel coordinate plot grouped by the location of the student in 2017 in order to determine whether the location of Spotify users affected their music tastes. Due to our limited data, we did not have enough geographic information for countries other the USA, so we decided to group all the other countries into one category called "OTHER". Half of the 24 users were in USA in 2017. The two country groups are more intertwined compared to the gender groups in the previous plot, indicating that country did not have as much impact on users' music taste as gender (at least as it pertains to our limited sample).




```r
ggparcoord(df, columns = 1:8, alphaLines = .2, 
           scale = "uniminmax", groupColumn = "clean_country", 
           splineFactor = 10) +
  ggtitle("Parallel Coordinates Plot by 2017 Location") +
    theme_minimal(15) + theme(legend.position = "bottom") +
      scale_color_manual(name="Country in 2017", labels = c("USA","OTHER"), 
                          values = c("USA"="#FF6666", "OTHER"="#3399FF")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](FinalReport_files/figure-html/parcoord2-1.png)<!-- -->

After this initial assessment, we wanted to analyze the quantitative relationships between the audio features. The graphs below show the correlation between each pair of features. It's not surprising that energy and loudness are the most positively correlated features, while energy and acousticness are the most negatively correlated features. As the name suggests, acousticness is a measure of how likely the track is to be acoustic, and generally acoustic songs are considered to be more mellow and less high-energy.


```r
ggcorr(df[1:8], palette = "RdBu", label = TRUE, label_size = 2.5,
size = 3, legend.position = "bottom", hjust = 1) +
  theme_minimal(15) +
    ggtitle("How are audio features correlated?") 
```

![](FinalReport_files/figure-html/corr-1.png)<!-- -->


```r
scatter_vars <- ggpairs(cont_var, title = "", axisLabels = "show", progress = FALSE)
scatter_vars
```

![](FinalReport_files/figure-html/scatter-1.png)<!-- -->

It is interesting to see that track popularity is not really correlated with any of the audio features. This again begs the question of what goes into calculating the track popularity variable, and perhaps whether it is a score relative to the genre the song belongs to. Spotify claims that it calculates danceability based on the tempo but not only is there almost no correlation between danceability and tempo, but they are even slightly negatively correlated! This might be interesting to look into further given more time.

**Popular Artists**

We now turn our attention to exploring the most popular artists of the dataset. We formulated two ways to measure popularity: how many songs each artist has in the combined playlist (i.e. all 2400 songs), and how many distinct playlists the artist appears in. The second is likely a better measure of overall popularity, as it is less sensitive to outliers - for example, if a student listens to many songs from that artist it might be a large percentage of the total combined playlist, but not necessarily and indication of popularity overall. The results of both these measures, however, are quite similar, as illustrated in the bar charts below. 




```r
ggplot(head(artist_df, 30) , aes(x=reorder(artist_name, -count), y= count)) +
geom_col(color="darkblue", fill="lightblue") +
  ggtitle("Which artist has the most songs in the data set?") +
    theme_minimal()+
      xlab("") + ylab("Number of songs") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](FinalReport_files/figure-html/unnamed-chunk-5-1.png)<!-- -->




```r
#bar graph
ggplot(head(artist_df, 30) , aes(x=reorder(artist_name, -appearance), y= appearance)) +
  geom_col(color="darkblue", fill="lightblue") +
    ggtitle("Which artist appears in the most distinct playlists?") +
      theme_minimal() + xlab("") + ylab("Number of users") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](FinalReport_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

When looking at what makes an artist popular for DSI, we also wanted to see if the popularity of the artist on Spotify matched the popularity of the artist in DSI. To look at this, we have a density curve of artist popularity (Spotify's metric) between popular and less popular artists in DSI.


```r
lea_all_data <- rbind(edav_top_10_artists, edav_not_top_10_artists)

diff_art_pop <- ggplot(lea_all_data, aes(x = normalized_artist_pop, fill = playlist_name)) + 
  geom_density(alpha = 0.4) + xlab("Artist Popularity") + ylab("Density") +
  guides(fill=guide_legend(title="Playlist Name"))

diff_art_pop
```

![](FinalReport_files/figure-html/art_pop-1.png)<!-- -->

From this graph, we see that the artists that are popular with DSI students are also considered to be popular by Spotify, so DSI's aritst tastes match closely with the rest of Spotify users. 


```r
diff_art_dance <- ggplot(lea_all_data, aes(x = normalized_tempo, fill = playlist_name)) + 
  geom_density(alpha = 0.4) + xlab("Tempo") + ylab("Density") +
  guides(fill=guide_legend(title="Playlist Name"))

diff_art_dance
```

![](FinalReport_files/figure-html/art_dance-1.png)<!-- -->

From the graph above, we see something that is slightly inconsistent with what we saw with the most popular DSI songs. We see here that the most popular DSI artists actually have songs that are generally slower paced than less popular songs in DSI, which is an interesting discrepancy.

**Popular Genres**

Another interesting question to explore is how popular various genres are within the cohort. We plotted the 10 most popular genres below, along with the proportion of all songs that fall into each. An important note is that Spotify's genre information is at an artist level, not a track level. A partial consequence of this fact is that one song can be tagged as multiple genres, as the same artist can dabble in multiple musical styles. However, this is not necessarily inaccurate, as the song-genre mapping is not strictly one-to-one. For example, "Bad and Boujee" by Migos is predominantly a trap song, but can also be said to be rap, hip hop, or pop rap. Further, songs by and large reflect the primary genre of their artist. As a result, while classifying genres at an artist level is not perfect, it is good enough to draw broad conclusions. 




```r
ggplot(dsi_top_genres[1:10,], aes(x = fct_reorder(Genre, percent), y = percent)) + 
  geom_bar(stat = "identity") + 
    ylab("Proportion of All Songs") + xlab("") + 
      coord_flip() + theme_minimal()
```

![](FinalReport_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

While pop is overwhelmingly and predictably the most popular genre, there is little difference between the 2nd and the 10th ranked genres, indicating the DSI cohort is quite varied in its music tastes.

### 2. How do DSI listening habits compare with global trends?

The parallel coordinate plots presented earlier raises the question: are there features specific to DSI songs that are more prominent than in other popular music? To begin answering this question, we shall compare the most popular songs within DSI to the songs that were most popular in the country and the world in 2017. In order to do this, we got the most common 50 songs from the dataset we created. We defined the most common songs simply as the songs that appeared the highest number of times in the dataset. 

Once we got the top DSI songs, we looked at different features and different plotting techniques to see if there was anything unique about these songs. We decided to compare the songs to three of Spotify's playlists. We compared these to: "Global Top 50", "United States Top 50" and "Top Tracks 2017", since these are playlists with some of the most popular music on Spotify. We chose to compare the continuous features that Spotify's API provides. Because these are continuous and we were comparing across different playlists, we created density curves and boxplots to easily compare between all of the different playlists. Note that although we looked at all of the features and made graphs for them, only the most revealing are included in this report below.

We find that there are three key points of differentiation: loudness, tempo and genre.

**Loudness**




```r
diff_loud_songs <- ggplot(lea_all_data, aes(x = normalized_loudness, fill = playlist_name)) +
  geom_density(alpha = 0.4) + xlab("Loudness") + ylab("Density") +
    guides(fill=guide_legend(title="Playlist Name"))

diff_loud_songs
```

![](FinalReport_files/figure-html/loud_curve-1.png)<!-- -->




```r
edav_loud_box <- ggplot(lea_all_data, aes(x=reorder(playlist_name, -1*normalized_loudness, FUN=median), y=normalized_loudness)) + 
  geom_boxplot(fill='#CCCCFF') + 
    theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle("EDAV, Keep It Down") +
        xlab("Playlist") + ylab("Loudness") + 
          theme_minimal(15) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))

edav_loud_box
```

![](FinalReport_files/figure-html/loud_box-1.png)<!-- -->

As we can see above, DSIers, for whatever reason, really enjoy loud music. From the loudness boxplot, we see that even when we include the 'less popular' EDAV songs, they are still generally much louder than the songs on the other playlists. What's even more interesting and is revealed from both graphs is that there is not even a wide range in loudness of music for DSIers compared to the other playlists. Almost all the songs are concentrated around fairly high loudness values. Given more time, it could be interesting to look at what songs specifically are causing this trend in the playlists.

**Tempo**

Though not as drastic a difference as loudness, we found that DSIers also enjoy slightly faster music than the rest of Spotify users. This finding was again revealed by a density curve and boxplot, included below. 




```r
diff_tempo_songs <- ggplot(lea_all_data, aes(x = normalized_tempo, fill = playlist_name)) +
  geom_density(alpha = 0.4) + xlab("Tempo") + ylab("Density") +
    guides(fill=guide_legend(title="Playlist Name"))

diff_tempo_songs
```

![](FinalReport_files/figure-html/tempo_curve-1.png)<!-- -->




```r
edav_tempo_box <- ggplot(lea_all_data, aes(x = reorder(playlist_name, -1*normalized_tempo, FUN = median), y = normalized_tempo)) +
  geom_boxplot(fill='#CCCCFF') + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal(15) +
  xlab("Playlist") + ylab("Tempo") + ggtitle("Why are you in such a rush?") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

edav_tempo_box
```

![](FinalReport_files/figure-html/tempo_box-1.png)<!-- -->
  
**Genre**

We can also compare genre popularity for the DSI cohort vs the global Spotify population. The following dot plot compares genres within the DSI playlist to Spotify's "Top Tracks of 2017" playlist. 




```r
genre_dotplot <- ggplot(top_genres, aes(x=percent, 
                         y = fct_rev(Genre), color=Name)) +
  geom_point() + ylab("") + theme_dotplot +
    theme(legend.position="top") + 
      theme(legend.title=element_blank()) +
          xlab("Percentage of Songs")

genre_dotplot
```

![](FinalReport_files/figure-html/genre_compare-1.png)<!-- -->

Overall, DSI tastes are quite different from the mainstream! Though pop is the most common genre, it is vastly under-represented in the aggregate DSI playlist when compared to 2017's top tracks. Additionally, DSI students seem to like rock genres more and rap/hip hop genres less than the global population. 

We now shift our attention to the latter two questions, which focus on the similarities and differences between the listening habits of specific individuals win the DSI.

### 3. How similar are DSI students in their music preferences?

First, we'd like to examine the overlap in musical tastes between DSI students. An obvious first question to ask is: how similar are any two individuals' playlists? One way to answer this question is by determining how many songs any two users have in common. We can summarize this in the following heatmap, where the colour of each cell represents the number of songs the two playlists have in common. For the sake of contrast, we omit the songs shared by a playlist and itself (the diagonal elements of the matrix).




```r
theme_heat <- theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 60, hjust=0))

song_heatmap <- ggplot(song_intersection, aes(x = Name, y = fct_rev(sharer))) + 
                  geom_tile(aes(fill = nshared/100), color = "white") + 
                    coord_fixed() + theme_heat + scale_x_discrete(position = "top") +
                      scale_fill_gradient2(low = "white", mid = "white", high = "darkblue", 
                        name = "Songs Shared \n (% of Total)", labels = scales::percent) + 
                          xlab("") + ylab("")

song_heatmap
```

![](FinalReport_files/figure-html/heatmap-1.png)<!-- -->

Each user is fairly unique: the largest degree of overlap between any two playlists is ~10%. Lea, Hammaad, and Yimin have the most songs in common, whereas Eric's taste is almost entirely distinct. 

### 4. What are the key ways in which DSI students differ from one another?

A key feature of every playlist is diversity: the range of music a student listens to. We can define diversity in a number of ways, but for the purposes of this project, we will view it in two ways: variety of artists and variety of genres. Let's first look at aritst diversity. The two charts below illustrate the range in artist diversity between different users. The graph on the left shows the total number of artists represented in a user's playlist; the graph on the right shows the proportion of a playlist accounted for by a user's 10 favorite artists.




```r
adiv1 <- ggplot(user_total_artists, aes(x=fct_reorder(Name, nartists), 
                                        y = nartists)) + 
          geom_bar(stat = "identity") + 
            ylab("Number of Distinct Artists") + xlab("") + 
              ggtitle('Number of Distinct \n Artists') +
              coord_flip() + theme_minimal()

adiv2 <- ggplot(user_top10_concentration, aes(x = fct_reorder(Name, Sum), 
                                              y = Sum)) + 
          geom_bar(stat = "identity") + 
          theme_minimal () + xlab("") + 
            ggtitle('Proportion of Songs from \n Top 10 Artists') + 
              coord_flip() 

grid.arrange(adiv1, adiv2, nrow =1 , ncol = 2)
```

![](FinalReport_files/figure-html/artistsdiv1-1.png)<!-- -->

There is clearly a massive range in artist diversity. Users like Zach and Shadi have a large number of artists in their playlists and a small percentage accounted for by their top artists, while Andrea and Mert have just the opposite. The strong inverse relationship between these two measures of artist diversity is reflected in the following scatterplot.




```r
ggplot(user_artist_concentration, aes(y=nartists, x=top10)) + 
  geom_point() + ylab("Number of Distinct Artists") + 
    xlab("Proportion of Songs from Top 10 Artists")
```

![](FinalReport_files/figure-html/artistsdiv2-1.png)<!-- -->

We now turn our attention to genres. Measuring a playlist's genre diversity is trickier than artist diversity, as each song can have multiple genre tags. In order to overcome this, we introduce the concept of spanning genres: the minimum number of distinct genres required to span a user's entire playlist. (Note: the code to compute genre diversity is fairly complex, and is not shown here for the sake of brevity. Please see the appendix for details). The chart on the left shows the genre spans for each user. We can also measure genre diversity by the percentage of a playlist's songs that fall into the user's favorite genres, as shown in the chart on the right.




```r
gdiv1 <- ggplot(user_n_genres, 
          aes(x = fct_reorder(factor(Name), ngenres), 
              y = ngenres)) + 
          geom_bar(stat = "identity", fill = "#CCCCFF") + 
          theme_minimal () + ylab("") + xlab("") +
            ggtitle('Number of Spanning \n Genres') + 
              coord_flip()

gdiv2 <- ggplot(user_n_genres, 
          aes(x = fct_reorder(factor(Name), top_genre_songs), 
              y = top_genre_songs/100)) + 
            geom_bar(stat = "identity", fill = "#CCCCFF") + 
              theme_minimal () + ylab("") + xlab("") +
                ggtitle('Proportion of Songs in \n Top Genre') + 
                  scale_y_continuous(labels = scales::percent) +
                    coord_flip()

grid.arrange(gdiv1, gdiv2, nrow =1 , ncol = 2)
```

![](FinalReport_files/figure-html/genrediv1-1.png)<!-- -->

Again, we observe that the DSI has a massive range in genre diversity. The most striking example is Tom: ~90% of his songs come from the same genre (rap)! On the other end of the spectrum, Nico and Zach have both a small percentage of songs in one genre, and a large number of spanning genres. 

It is also worth investigating the difference between genre and artist diversity. Are there users who listen to a large number of artists, but a small number of genres? Or is there a strong positive relation between the number of distinct artists and number of spanning genres? We investigate these relationships with the following scatterplots.


```r
artist_genre_concentration <- user_artist_concentration %>% inner_join(user_n_genres, by = "Name")

agdiv1 <- ggplot(artist_genre_concentration, aes(x= nartists, y = ngenres)) +
            geom_point() + theme_classic() + xlab("Number of Distinct Artists") + 
              ylab("Number of Spanning Genres")

agdiv2 <- ggplot(artist_genre_concentration, aes(y= top_genre_songs, x = top10)) + 
            geom_point() + theme_classic() + 
              ylab("% of Songs in Top Genre") + 
                xlab("% of Songs By Top 10 Artists")

grid.arrange(agdiv1, agdiv2, nrow =1 , ncol = 2)
```

![](FinalReport_files/figure-html/genreartistdiv-1.png)<!-- -->

There seems to be a positive association between number of spanning genres and number of distinct artists (left panel). This makes sense, but is not particularly surprising. The chart on the right, however, is more interesting. While there does seem to be a generally positive association, it is not very strong. For example, a large number of people have ~40% of songs in their top genre, but the proportion represented by their top 10 artist varies widely. Our hypothesis is that with more sample, we would see clearer clusters. These clusters would represent artist-genre joint diversity: whether a user listens to a lot of artists across a bunch of different genres (high diversity) or a large number of artists across a small number of genres (medium diversity), or a small number of artists across a small number of genres (low diversity). 





In examining the differences between students in our DSI cohort, we wanted to see who listens to the most obscure music, or the most "indie", and who listens to the most popular music, or the most "mainstream". The best variable to do this with is Spotify's track popularity variable, this is a number assigned from 0 to 100 that is calculated based on an algorithm that takes the total number of streams for that track and weighs more recent streams more heavily. The exact formula for this number is unknown, but it is our best measure in the dataset for how popular a song is. One caveat to this variable is that we pulled the track popularity when we built our dataset, and these playlists are from 2017, so the track_popularity is indicative of how popular that song is today rather than in 2017 when the student was listening to it. Therefore the results could also be interpreted as, who listens to the music that stays popular throughout a year's worth of time.

We first look at this variable in conjunction with artist popularity, another possible indicator someone's music tastes being "mainstream" or "indie". The Cleveland dot plot below averages the track and artist popularity for each DSIers playlists. The artist popularity metric is formed similarly to the track popularity metric by Spotify. The plot reveals that Lea and Hammaad both listen to the most popular music (which is not too surprising since they have the most songs in common) and Eric listens to the leas popular music (again not surprising since he had the least number of songs in common with anyone else). An interesting thing to note from the graph is that Tom has a lower track popularity but one of the highest artist popularities.




```r
pop_cd <- ggplot(tidy_avg, aes(x = Percentage, y = Name, color = Category)) +
          geom_point() + ylab("") + theme_minimal() +
          ggtitle("Hammaad and Lea Are Basic") +
        scale_color_manual(values=c("red", "blue")) +
    theme(legend.title=element_text(size=15), legend.text=element_text(size=13))

pop_cd
```

![](FinalReport_files/figure-html/pop_dot_plot-1.png)<!-- -->

Below we created 2 sets of boxplots - they are boxplots of the distribution of their playlist's popularity variables for each student in our dataset. You can see based on the first set which has track popularity that Lea listens to the most popular music in terms of having the highest median track popularity value and Eric has the lowest median track popularity. Additionally, from the boxplots you can see that some students such as Mert and Anonymous 1 have very large ranges whereas James and Hammaad's ranges are rather small in comparison.

Additionally we made the same plot for the artist popularity variable, ordered in the same way as track popularity, so it is easy to compare. From Spotify's description, the artist popularity variable is determined by the track popularity so it should be a similar graph. However, when you compare, you can see that all ranges are smaller and in general they are on the higher end of the popularity variable. This could be explained by a few reasons, maybe artist popularity distributions in general are higher, or students could be listening to low popularity tracks but from artists that do have very high popularity tracks, or it could be a factor of the time difference. Perhaps the tracks that students listened to in 2017 are no longer very popular, but the artist has new tracks in 2018 that have brought the artist's popularity variable up.


```r
mytheme <- theme_minimal(14) +
    theme(plot.title = element_text(hjust = 0.5))

# Boxplots of track popularity for each student, ordered by highest median track popularity to lowest
box_indie <- ggplot(class_df, aes(x=fct_reorder(Name, track_popularity), y=class_df$track_popularity)) +
                geom_boxplot(fill="#CCCCFF") +
                scale_x_discrete("") +
                labs(y="Track Popularity", x="DSI-er", title="Who is the most indie \n and the most mainstream?") +
                coord_flip() +
                mytheme
box_indie
```

![](FinalReport_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
# Boxplots ordered the same as track_popularity (for comparison) but showing artist popularity
ggplot(class_df, aes(fct_reorder(Name, track_popularity), artist_popularity)) +
  geom_boxplot(fill="#CCCCFF") +
  labs(y="Artist Popularity", x="DSI-er", title="Does this look the same when showing \n artist popularity?") +
  coord_flip() +
  mytheme
```

![](FinalReport_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

The next few graphs try to use the survey inputs to identify any trends within those survey answers. While the number of tracks in our dataset is sizeable (2400), they only come from 24 students, and when separating that into categories based on survey answers, it is hard to really identify any trends. We were mainly interested in the gender variable and the age variable. For gender, the distribution in the dataset is 8 females and 16 males. For age, the distribution in the dataset is age 21: 1, age 22: 4, age 23: 4, age 24: 5, age 25: 5, age 26: 2, age 28: 2. Therefore while the graphs using age are still interesting, it is hard to note any general trends, since if you're talking about 21 year olds it is really just that 1 specific person. Additionally the ages are all in the 21-28 range, so we wouldn't expect a huge amount of difference in their listening habits, but you can see some distinctions.

Below we made a mosaic plot of age and track popularity, which is binned into 4 quartiles since the original number is likely a percentile. You can see from the mosaic plot that there isn't much of a relationship between age and listening to popular or not popular tracks. It looks like 22 year olds listen to the most popular tracks, but this could also be because there is only 1 21 year old and at least 4 22 year olds. Therefore, nothing conculsive can really be said about age and popularity. If given more time, we could collect a wider range of samples and test this again.




```r
library(ggmosaic)
```

```
## 
## Attaching package: 'ggmosaic'
```

```
## The following object is masked from 'package:GGally':
## 
##     happy
```

```r
ggplot(df) +
  geom_mosaic(aes(x = product(age), fill=track_popularity), na.rm=FALSE)
```

![](FinalReport_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
  
Next, we wanted to explore the album data we were able to pull and see if there are any trends based on the year the album was released and therefore when the song was released. 

First, we explored album dates in relation to gender of the student. Below is a bar graph of the number of plays of the track and the decade the song was released, faceted by gender. You can see that the distribution looks similar but there is a little cluster of songs released in the 70s for the males - classic rock songs, no doubt. Since there are a lot more males than females in our dataset, we graphed the same thing again in terms of percentages. In that next graph you can now see that the females in our group have over 80% of their songs coming out of the 2010s decade. So it seems like everyone likes more recent music, but especially DSI females!




```r
# Bar chart with counts
ggplot(class_df_grouped, aes(x=album_decade, y=Freq)) +
  geom_histogram(fill="thistle", stat="identity") +
  facet_wrap(~gender)
```

![](FinalReport_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
# Bar chart with percentages - you can really see the percentage of females listening to recent music is high
ggplot(class_df_grouped, aes(x=album_decade, y=Percent)) +
  geom_histogram(fill="thistle", stat="identity") +
  facet_wrap(~gender) +
  labs(xlab='Decade of Album Release', title='Everyone likes more recent music, but especially DSI Females!')
```

![](FinalReport_files/figure-html/unnamed-chunk-23-2.png)<!-- -->

Since the majority of the songs came out of alubms in the 2000s we wanted to do a deeper dive into the 2000s to see if there are any trends within years of the 2000s.

In terms of gender, the distributions of the 2000s look similarly skewed. However, males have fatter tails in terms of having a higher percentage from 2000 to 2012, so while they are listening to more recent music, maybe they like to listen to throwback songs a little more than females. Additionally, now that we are zooming into the 2000s you can see that of the 2010s the majority of the songs are from 2016-2017, so either songs that came out the year of the playlist or songs that came out the year right before it. 

We would also like to note that there are some album release dates in 2018, even though this was a playlist of top songs played in 2017. This is due to the fact that the way Spotify has tracked their album release dates, it was easiest to take the release date from the actual album, whereas it might have shown up on the playlist because the single came out in 2017, but the album did not come out until 2018.




```r
# Male/Female distributions are pretty similar
ggplot(year2000_grouped, aes(x=album_year, y=Percent)) +
  geom_histogram(fill="thistle", stat="identity") +
  facet_wrap(~gender) +
  labs(xlab='Year of Album Release', title='Similar distributions, but males like to throwback a little more')
```

![](FinalReport_files/figure-html/unnamed-chunk-25-1.png)<!-- -->

Since the gender distributions were similar, even though age cannot be generalized, we wanted to see if age had any trends with album release year. The below graphs are faceted by age and you can see that generally as you increase in age, the skew to 2016-2017 is less and less extreme and students listen to more and more songs released in the early 2000s. In order to better compare, we made a plot with density curves of each age. Here you can see similar trends, but you can really see that 26 year olds have the lowest density around 2016-2017 and are much more evenly spread out than the others. You can also see that 21 and 22 year olds have the lowest percentages of songs from 2000 to about 2007.




```r
# Bar chart with percentages
ggplot(na.omit(year2000_grouped), aes(x=album_year, y=Percent)) +
  geom_histogram(fill="thistle", stat="identity") +
  facet_wrap(~age) +
  labs(xlab='Year of Album Release', title='Year')
```

![](FinalReport_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
# Density curves
ggplot(year2000clean, aes(x = album_year, color=Student_Age)) +
  geom_density() +
  ggtitle("Age/Year of album distribution") +
  labs(x = "Year of Album Release", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](FinalReport_files/figure-html/unnamed-chunk-27-2.png)<!-- -->

We were curious to see how all these factors: age, track popularity, and album release date come together, so we plotted them on a scatterplot with date on the x-axis and popularity on the y-axis. The points are colored by the age of the student. In accordance with the previous plots, most of the songs are clustered around 2016-2017, and they range the full spectrum of track popularity with a big cluster in the 30-75 range and a small clusterin the 0-15 range. Generally, it looks like if younger students listen to older songs they at least have a track popularity of 40 or above. The only students that listen to tracks released before 1991 and are below 40 in popularity are 24 and 25 year olds. You can see this even better in the interactive and subsetted graph described below.

In order to see specifics of these outliers, and to better visualize the graph, we subset the data so it only includes the top 25 songs in each person's playlist, presumably these are their top 25 favorite songs of 2017. Then, we used plotly and displayed the track name and artist name so we could see what exactly these points represented. The oldest song in this subset is "I Fall in Love too Easily" by Chet Baker with a popularity level of 62. The highest track popularity for a song that came out before 2000 is 81 and that is for "Under the Bridge" by Red Hot Chili Peppers. The highest track popularity overall is 89 and is the song Jocelyn Flores by XXXTENTACION. The lowest track popularity for a song that came out before 2000 is 0 and is the song "Come on Home" by Joan Armatrading. These outliers provide an interesting area for further exploration; given more time, we would have liked to analyze the audio features of these songs, and determine what, if anything, differentiates them from other songs in the playlist. 


```r
splot_theme <- theme_classic(12) + theme(plot.title = element_text(hjust = .5))
library(viridis)
```

```
## Loading required package: viridisLite
```

```r
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
# Scatterplot with all songs comparing album release date and track popularity, colored by age
class_df_clean <- na.omit(subset(class_df, select=c(album_date, age, track_popularity)))
Age <- as.factor(class_df_clean$age)
ggplot(class_df_clean, aes(album_date, track_popularity)) + 
    geom_point(aes(color= Age), alpha = .7, size = 2, stroke=0) +
    scale_color_viridis_d() +
    splot_theme + 
    labs(title= 'How does album release date relate to track popularity?', subtitle = "All Tracks",
         x = 'Album Release Date', y = 'Track Popularity Levels')
```

![](FinalReport_files/figure-html/unnamed-chunk-28-1.png)<!-- -->

```r
# Same as above, but with only the top 25 songs per student's playlist
top50_df <- na.omit(subset(class_df, rank <=25, select=c(album_date, age, track_popularity, track_name, artist_name)))
Age <- as.factor(top50_df$age)
g<- ggplot(top50_df, aes(album_date, track_popularity, text=paste("Song:",track_name, "Artist:",artist_name))) + 
    geom_point(aes(color = Age), alpha = .7, size = 2, stroke=0) +
    scale_color_viridis_d() +
    splot_theme + 
    labs(title= 'How does album release date relate to track popularity?', subtitle = "Top 50 Tracks of Each Playlist",
         x = 'Album Release Date', y = 'Track Popularity Levels')

# Made interactive so we can see what those outlier songs are that were released a long time ago but have high track popularity
# Hover over points to see what song it is and the artist in addition to date of release and track pouplarity
ggplotly(g)
```

<!--html_preserve--><div id="htmlwidget-90a4e054a8d235cb8b85" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-90a4e054a8d235cb8b85">{"x":{"data":[{"x":[17179,17442,17200,16385,17200,17144,16385,16717,17032,17284,17200,15706,17347,15918,17228,17200,17417,16385,16588,14873,15803,16755,17374,15370,17284],"y":[10,30,48,45,51,67,43,0,48,70,48,35,0,38,61,40,42,54,40,53,53,0,7,39,38],"text":["Age: 21<br />album_date: 2017-01-13<br />track_popularity: 10<br />Song: On Hold Artist: The xx","Age: 21<br />album_date: 2017-10-03<br />track_popularity: 30<br />Song: I Wish I Knew Artist: Sharon Van Etten","Age: 21<br />album_date: 2017-02-03<br />track_popularity: 48<br />Song: Kora Sings Artist: Sampha","Age: 21<br />album_date: 2014-11-11<br />track_popularity: 45<br />Song: Texas Reznikoff Artist: Mitski","Age: 21<br />album_date: 2017-02-03<br />track_popularity: 51<br />Song: Reverse Faults Artist: Sampha","Age: 21<br />album_date: 2016-12-09<br />track_popularity: 67<br />Song: Another Day Of Sun - From \"La La Land\" Soundtrack Artist: La La Land Cast","Age: 21<br />album_date: 2014-11-11<br />track_popularity: 43<br />Song: I Will Artist: Mitski","Age: 21<br />album_date: 2015-10-09<br />track_popularity:  0<br />Song: All the Way Down Artist: Kelela","Age: 21<br />album_date: 2016-08-19<br />track_popularity: 48<br />Song: Char Artist: Crystal Castles","Age: 21<br />album_date: 2017-04-28<br />track_popularity: 70<br />Song: Quit (feat. Ariana Grande) Artist: Cashmere Cat","Age: 21<br />album_date: 2017-02-03<br />track_popularity: 48<br />Song: Under Artist: Sampha","Age: 21<br />album_date: 2013-01-01<br />track_popularity: 35<br />Song: With Me Artist: Cashmere Cat","Age: 21<br />album_date: 2017-06-30<br />track_popularity:  0<br />Song: Saw You In A Dream Artist: The Japanese House","Age: 21<br />album_date: 2013-08-01<br />track_popularity: 38<br />Song: Because Dreaming Costs Money, My Dear Artist: Mitski","Age: 21<br />album_date: 2017-03-03<br />track_popularity: 61<br />Song: The End of the World Artist: Sharon Van Etten","Age: 21<br />album_date: 2017-02-03<br />track_popularity: 40<br />Song: Timmy's Prayer Artist: Sampha","Age: 21<br />album_date: 2017-09-08<br />track_popularity: 42<br />Song: Green Line Artist: Princess Nokia","Age: 21<br />album_date: 2014-11-11<br />track_popularity: 54<br />Song: First Love / Late Spring Artist: Mitski","Age: 21<br />album_date: 2015-06-02<br />track_popularity: 40<br />Song: Sleep Sound Artist: Jamie xx","Age: 21<br />album_date: 2010-09-21<br />track_popularity: 53<br />Song: One Day Artist: Sharon Van Etten","Age: 21<br />album_date: 2013-04-08<br />track_popularity: 53<br />Song: Life Round Here Artist: James Blake","Age: 21<br />album_date: 2015-11-16<br />track_popularity:  0<br />Song: 風月 Artist: 黃齡","Age: 21<br />album_date: 2017-07-27<br />track_popularity:  7<br />Song: Comin Out Strong Artist: Future","Age: 21<br />album_date: 2012-01-31<br />track_popularity: 39<br />Song: Bag of Bones Artist: Mitski","Age: 21<br />album_date: 2017-04-28<br />track_popularity: 38<br />Song: Europa Pools (feat. Kacy Hill) Artist: Cashmere Cat"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(68,1,84,1)","opacity":0.7,"size":7.55905511811024,"symbol":"circle","line":{"width":0,"color":"rgba(68,1,84,1)"}},"hoveron":"points","name":"21","legendgroup":"21","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[17270,17144,17270,17144,17270,-396,212,16920,17270,17144,8439,17270,15814,13084,16514,17270,69,6209,17144,15110,15340,-3790,17270,14610,8035,17025,17088,16231,15706,17466,17312,16955,15898,17452,17142,16675,17193,17270,14711,17039,16218,17277,17358,16850,16948,16672,16545,17060,17088,16955,16955,17228,16955,17634,17179,17243,16955,17207,17317,17130,16647,17137,16948,17228,17130,16570,17011,17137,16378,14610,16570,17095,17137,17270,17361,16897,17130,17270,17130,17228,15814,13277,16850,17137,17130,17109,16717,16962,11877,16828,8453,17333,7936,17221,17018,17243,16948,10750,16413,17347],"y":[50,67,53,59,46,69,62,3,42,57,38,45,74,37,43,63,64,40,58,47,5,66,63,72,47,75,51,76,46,8,51,35,51,5,39,71,74,9,25,40,17,53,22,57,42,0,61,84,43,48,57,79,51,51,77,2,44,70,53,83,71,80,7,67,80,78,85,59,46,64,52,80,7,74,8,68,83,84,70,79,74,75,37,80,76,77,61,73,79,71,80,70,81,74,82,2,70,74,80,69],"text":["Age: 22<br />album_date: 2017-04-14<br />track_popularity: 50<br />Song: Still Feel Like Your Man Artist: John Mayer","Age: 22<br />album_date: 2016-12-09<br />track_popularity: 67<br />Song: Another Day Of Sun - From \"La La Land\" Soundtrack Artist: La La Land Cast","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 53<br />Song: Moving On and Getting Over Artist: John Mayer","Age: 22<br />album_date: 2016-12-09<br />track_popularity: 59<br />Song: Epilogue - From \"La La Land\" Soundtrack Artist: Justin Hurwitz","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 46<br />Song: Helpless Artist: John Mayer","Age: 22<br />album_date: 1968-12-01<br />track_popularity: 69<br />Song: For Once In My Life Artist: Stevie Wonder","Age: 22<br />album_date: 1970-08-01<br />track_popularity: 62<br />Song: Signed, Sealed, Delivered (I'm Yours) Artist: Stevie Wonder","Age: 22<br />album_date: 2016-04-29<br />track_popularity:  3<br />Song: Lovely Day Artist: Kymaera","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 42<br />Song: Love on the Weekend Artist: John Mayer","Age: 22<br />album_date: 2016-12-09<br />track_popularity: 57<br />Song: Herman’s Habit - From \"La La Land\" Soundtrack Artist: Justin Hurwitz","Age: 22<br />album_date: 1993-02-08<br />track_popularity: 38<br />Song: On a Clear Day You Can See Forever Artist: The Peddlers","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 45<br />Song: Emoji of a Wave Artist: John Mayer","Age: 22<br />album_date: 2013-04-19<br />track_popularity: 74<br />Song: Get Lucky (feat. Pharrell Williams & Nile Rodgers) - Radio Edit Artist: Daft Punk","Age: 22<br />album_date: 2005-10-28<br />track_popularity: 37<br />Song: Moon Blue Artist: Stevie Wonder","Age: 22<br />album_date: 2015-03-20<br />track_popularity: 43<br />Song: I'll Be There - Single Version Artist: CHIC","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 63<br />Song: You're Gonna Live Forever in Me Artist: John Mayer","Age: 22<br />album_date: 1970-03-11<br />track_popularity: 64<br />Song: Our House Artist: Crosby, Stills, Nash & Young","Age: 22<br />album_date: 1987-01-01<br />track_popularity: 40<br />Song: Pick Yourself Up Artist: Nat King Cole","Age: 22<br />album_date: 2016-12-09<br />track_popularity: 58<br />Song: Faith - From \"Sing\" Original Motion Picture Soundtrack Artist: Stevie Wonder","Age: 22<br />album_date: 2011-05-16<br />track_popularity: 47<br />Song: Black (feat. Norah Jones) Artist: Danger Mouse","Age: 22<br />album_date: 2012-01-01<br />track_popularity:  5<br />Song: Anything Could Happen Artist: Ellie Goulding","Age: 22<br />album_date: 1959-08-17<br />track_popularity: 66<br />Song: Blue in Green Artist: Miles Davis","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 63<br />Song: Rosie Artist: John Mayer","Age: 22<br />album_date: 2010-01-01<br />track_popularity: 72<br />Song: All Of The Lights Artist: Kanye West","Age: 22<br />album_date: 1992-01-01<br />track_popularity: 47<br />Song: L-O-V-E Artist: Nat King Cole","Age: 22<br />album_date: 2016-08-12<br />track_popularity: 75<br />Song: Swang Artist: Rae Sremmurd","Age: 22<br />album_date: 2016-10-14<br />track_popularity: 51<br />Song: Bling Blaww Burr (feat. Young Dolph) Artist: Gucci Mane","Age: 22<br />album_date: 2014-06-10<br />track_popularity: 76<br />Song: Or Nah (feat. The Weeknd, Wiz Khalifa and DJ Mustard) - Remix Artist: Ty Dolla $ign","Age: 22<br />album_date: 2013-01-01<br />track_popularity: 46<br />Song: Ocho Cinco Artist: French Montana","Age: 22<br />album_date: 2017-10-27<br />track_popularity:  8<br />Song: Rake It Up Artist: Yo Gotti","Age: 22<br />album_date: 2017-05-26<br />track_popularity: 51<br />Song: Bucket List Artist: Gucci Mane","Age: 22<br />album_date: 2016-06-03<br />track_popularity: 35<br />Song: All Night Artist: Juicy J","Age: 22<br />album_date: 2013-07-12<br />track_popularity: 51<br />Song: Count up (Bonus) Artist: Speaker Knockerz","Age: 22<br />album_date: 2017-10-13<br />track_popularity:  5<br />Song: Curve (feat. The Weeknd) Artist: Gucci Mane","Age: 22<br />album_date: 2016-12-07<br />track_popularity: 39<br />Song: Ain't Heard 'Bout You Artist: DJ Lucky L","Age: 22<br />album_date: 2015-08-28<br />track_popularity: 71<br />Song: Acquainted Artist: The Weeknd","Age: 22<br />album_date: 2017-01-27<br />track_popularity: 74<br />Song: T-Shirt Artist: Migos","Age: 22<br />album_date: 2017-04-14<br />track_popularity:  9<br />Song: HUMBLE. Artist: Kendrick Lamar","Age: 22<br />album_date: 2010-04-12<br />track_popularity: 25<br />Song: Atlanta Zoo (feat. Ludacris) Artist: Gucci Mane","Age: 22<br />album_date: 2016-08-26<br />track_popularity: 40<br />Song: Need a Lighter (feat. 21 Savage) Artist: Lightshow","Age: 22<br />album_date: 2014-05-28<br />track_popularity: 17<br />Song: Aint for None (feat. King Louie) Artist: Dreezy","Age: 22<br />album_date: 2017-04-21<br />track_popularity: 53<br />Song: Gilligan Artist: DRAM","Age: 22<br />album_date: 2017-07-11<br />track_popularity: 22<br />Song: Dlk Will Kill You Music Presents: So up to Par Artist: Gucci Mane","Age: 22<br />album_date: 2016-02-19<br />track_popularity: 57<br />Song: Law Artist: Yo Gotti","Age: 22<br />album_date: 2016-05-27<br />track_popularity: 42<br />Song: Zanzibar Artist: Belly","Age: 22<br />album_date: 2015-08-25<br />track_popularity:  0<br />Song: God Said Trap (King Trappy III) Artist: IDK","Age: 22<br />album_date: 2015-04-20<br />track_popularity: 61<br />Song: U Mad Artist: Vic Mensa","Age: 22<br />album_date: 2016-09-16<br />track_popularity: 84<br />Song: goosebumps Artist: Travis Scott","Age: 22<br />album_date: 2016-10-14<br />track_popularity: 43<br />Song: Intro: Fuck 12 Artist: Gucci Mane","Age: 22<br />album_date: 2016-06-03<br />track_popularity: 48<br />Song: Breaking News Artist: Juicy J","Age: 22<br />album_date: 2016-06-03<br />track_popularity: 57<br />Song: I’m So Humble Artist: The Lonely Island","Age: 22<br />album_date: 2017-03-03<br />track_popularity: 79<br />Song: Shape of You Artist: Ed Sheeran","Age: 22<br />album_date: 2016-06-03<br />track_popularity: 51<br />Song: Equal Rights Artist: The Lonely Island","Age: 22<br />album_date: 2018-04-13<br />track_popularity: 51<br />Song: Made In China Artist: Higher Brothers","Age: 22<br />album_date: 2017-01-13<br />track_popularity: 77<br />Song: Paris Artist: The Chainsmokers","Age: 22<br />album_date: 2017-03-18<br />track_popularity:  2<br />Song: Fake Love Artist: Drake","Age: 22<br />album_date: 2016-06-03<br />track_popularity: 44<br />Song: Turn Up The Beef Artist: The Lonely Island","Age: 22<br />album_date: 2017-02-10<br />track_popularity: 70<br />Song: Light Artist: San Holo","Age: 22<br />album_date: 2017-05-31<br />track_popularity: 53<br />Song: WeChat Artist: Higher Brothers","Age: 22<br />album_date: 2016-11-25<br />track_popularity: 83<br />Song: Starboy Artist: The Weeknd","Age: 22<br />album_date: 2015-07-31<br />track_popularity: 71<br />Song: Back To Back Artist: Drake","Age: 22<br />album_date: 2016-12-02<br />track_popularity: 80<br />Song: Redbone Artist: Childish Gambino","Age: 22<br />album_date: 2016-05-27<br />track_popularity:  7<br />Song: Say It (feat. Tove Lo) Artist: Flume","Age: 22<br />album_date: 2017-03-03<br />track_popularity: 67<br />Song: Castle on the Hill Artist: Ed Sheeran","Age: 22<br />album_date: 2016-11-25<br />track_popularity: 80<br />Song: I Feel It Coming Artist: The Weeknd","Age: 22<br />album_date: 2015-05-15<br />track_popularity: 78<br />Song: Sugar Artist: Maroon 5","Age: 22<br />album_date: 2016-07-29<br />track_popularity: 85<br />Song: Closer Artist: The Chainsmokers","Age: 22<br />album_date: 2016-12-02<br />track_popularity: 59<br />Song: California Artist: Childish Gambino","Age: 22<br />album_date: 2014-11-04<br />track_popularity: 46<br />Song: Lost You Artist: Zeds Dead","Age: 22<br />album_date: 2010-01-01<br />track_popularity: 64<br />Song: Gorgeous Artist: Kanye West","Age: 22<br />album_date: 2015-05-15<br />track_popularity: 52<br />Song: Leaving California Artist: Maroon 5","Age: 22<br />album_date: 2016-10-21<br />track_popularity: 80<br />Song: Rockabye (feat. Sean Paul & Anne-Marie) Artist: Clean Bandit","Age: 22<br />album_date: 2016-12-02<br />track_popularity:  7<br />Song: iSpy Artist: KYLE","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 74<br />Song: LOYALTY. FEAT. RIHANNA. Artist: Kendrick Lamar","Age: 22<br />album_date: 2017-07-14<br />track_popularity:  8<br />Song: All Night Artist: The Vamps","Age: 22<br />album_date: 2016-04-06<br />track_popularity: 68<br />Song: Broccoli Artist: DRAM","Age: 22<br />album_date: 2016-11-25<br />track_popularity: 83<br />Song: Starboy Artist: The Weeknd","Age: 22<br />album_date: 2017-04-14<br />track_popularity: 84<br />Song: HUMBLE. Artist: Kendrick Lamar","Age: 22<br />album_date: 2016-11-25<br />track_popularity: 70<br />Song: Secrets Artist: The Weeknd","Age: 22<br />album_date: 2017-03-03<br />track_popularity: 79<br />Song: Shape of You Artist: Ed Sheeran","Age: 22<br />album_date: 2013-04-19<br />track_popularity: 74<br />Song: Get Lucky (feat. Pharrell Williams & Nile Rodgers) - Radio Edit Artist: Daft Punk","Age: 22<br />album_date: 2006-05-09<br />track_popularity: 75<br />Song: Dani California Artist: Red Hot Chili Peppers","Age: 22<br />album_date: 2016-02-19<br />track_popularity: 37<br />Song: Can't Stop Artist: Lake Street Dive","Age: 22<br />album_date: 2016-12-02<br />track_popularity: 80<br />Song: Redbone Artist: Childish Gambino","Age: 22<br />album_date: 2016-11-25<br />track_popularity: 76<br />Song: Die For You Artist: The Weeknd","Age: 22<br />album_date: 2016-11-04<br />track_popularity: 77<br />Song: This Girl (Kungs Vs. Cookin' On 3 Burners) - Kungs Vs. Cookin' On 3 Burners Artist: Kungs","Age: 22<br />album_date: 2015-10-09<br />track_popularity: 61<br />Song: Back Pocket Artist: Vulfpeck","Age: 22<br />album_date: 2016-06-10<br />track_popularity: 73<br />Song: Famous Artist: Kanye West","Age: 22<br />album_date: 2002-07-09<br />track_popularity: 79<br />Song: Can't Stop Artist: Red Hot Chili Peppers","Age: 22<br />album_date: 2016-01-28<br />track_popularity: 71<br />Song: Needed Me Artist: Rihanna","Age: 22<br />album_date: 1993-02-22<br />track_popularity: 80<br />Song: Creep Artist: Radiohead","Age: 22<br />album_date: 2017-06-16<br />track_popularity: 70<br />Song: Feel It Still Artist: Portugal. The Man","Age: 22<br />album_date: 1991-09-24<br />track_popularity: 81<br />Song: Under The Bridge Artist: Red Hot Chili Peppers","Age: 22<br />album_date: 2017-02-24<br />track_popularity: 74<br />Song: Slide (feat. Frank Ocean & Migos) Artist: Calvin Harris","Age: 22<br />album_date: 2016-08-05<br />track_popularity: 82<br />Song: Let Me Love You Artist: DJ Snake","Age: 22<br />album_date: 2017-03-18<br />track_popularity:  2<br />Song: Fake Love Artist: Drake","Age: 22<br />album_date: 2016-05-27<br />track_popularity: 70<br />Song: All Night (feat. Knox Fortune) Artist: Chance the Rapper","Age: 22<br />album_date: 1999-06-08<br />track_popularity: 74<br />Song: Scar Tissue Artist: Red Hot Chili Peppers","Age: 22<br />album_date: 2014-12-09<br />track_popularity: 80<br />Song: No Role Modelz Artist: J. Cole","Age: 22<br />album_date: 2017-06-30<br />track_popularity: 69<br />Song: Feels Artist: Calvin Harris"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(68,58,131,1)","opacity":0.7,"size":7.55905511811024,"symbol":"circle","line":{"width":0,"color":"rgba(68,58,131,1)"}},"hoveron":"points","name":"22","legendgroup":"22","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[16559,15124,15089,14862,16071,14754,16360,15992,15089,13879,16801,16112,16570,16196,16287,16976,16752,17060,16920,16633,16829,16391,16203,16976,16588,17585,17305,16969,16647,16559,17340,17220,17046,17333,17410,17662,16948,15958,16071,16633,16682,17403,15856,17375,16556,17060,17245,17123,17193,13669,17270,17326,10896,10227,17345,16378,17158,16878,7355,-4383,14975,17270,17067,16427,12920,15951,2922,16378,14497,17207,13149,12418,15607,14245,12500,16949,16740,16436,16927,17113,16860,17067,16381,16997,16927,17403,17258,16886,17018,16745,17060,16962,17249,16955,17151,17368,16479,16927,17368],"y":[57,38,35,0,55,12,3,7,38,23,55,25,29,22,32,22,37,31,24,31,45,57,29,16,5,54,85,73,72,69,7,81,75,70,46,31,66,59,64,79,63,89,59,63,46,26,36,73,62,66,67,67,62,65,54,37,37,33,46,68,56,61,44,34,65,51,59,42,75,44,42,77,57,38,66,34,28,42,51,15,20,52,1,49,55,18,40,39,41,50,84,43,47,48,60,64,48,64,71],"text":["Age: 23<br />album_date: 2015-05-04<br />track_popularity: 57<br />Song: Drag Artist: Day Wave","Age: 23<br />album_date: 2011-05-30<br />track_popularity: 38<br />Song: Red Paper Lanterns Artist: Maybeshewill","Age: 23<br />album_date: 2011-04-25<br />track_popularity: 35<br />Song: Our Perfect Disease Artist: The Wombats","Age: 23<br />album_date: 2010-09-10<br />track_popularity:  0<br />Song: Bumpy Ride Artist: Within Runs","Age: 23<br />album_date: 2014-01-01<br />track_popularity: 55<br />Song: All Over Artist: CRUISR","Age: 23<br />album_date: 2010-05-25<br />track_popularity: 12<br />Song: Limelight Artist: Faded Paper Figures","Age: 23<br />album_date: 2014-10-17<br />track_popularity:  3<br />Song: Golden Maps Artist: Los Waves","Age: 23<br />album_date: 2013-10-14<br />track_popularity:  7<br />Song: Love Is All I Got Artist: Feed Me","Age: 23<br />album_date: 2011-04-25<br />track_popularity: 38<br />Song: Techno Fan Artist: The Wombats","Age: 23<br />album_date: 2008-01-01<br />track_popularity: 23<br />Song: No You Didn't, No You Don't Artist: Courteeners","Age: 23<br />album_date: 2016-01-01<br />track_popularity: 55<br />Song: Absolutely Artist: Ra Ra Riot","Age: 23<br />album_date: 2014-02-11<br />track_popularity: 25<br />Song: Singularity Artist: Hollow & Akimbo","Age: 23<br />album_date: 2015-05-15<br />track_popularity: 29<br />Song: The Sunshine of Your Youth Artist: Cheerleader","Age: 23<br />album_date: 2014-05-06<br />track_popularity: 22<br />Song: I See You Artist: The Horrors","Age: 23<br />album_date: 2014-08-05<br />track_popularity: 32<br />Song: Not the End of the World (Even as We Know It) Artist: Faded Paper Figures","Age: 23<br />album_date: 2016-06-24<br />track_popularity: 22<br />Song: Kid Who Stays in the Picture Artist: Hot Hot Heat","Age: 23<br />album_date: 2015-11-13<br />track_popularity: 37<br />Song: Light Up Artist: Mutemath","Age: 23<br />album_date: 2016-09-16<br />track_popularity: 31<br />Song: Statues in a Gallery Artist: Kishi Bashi","Age: 23<br />album_date: 2016-04-29<br />track_popularity: 24<br />Song: Steer the Canyon Artist: Big Black Delta","Age: 23<br />album_date: 2015-07-17<br />track_popularity: 31<br />Song: Nothing at All Artist: Day Wave","Age: 23<br />album_date: 2016-01-29<br />track_popularity: 45<br />Song: What Am I Becoming? Artist: POP ETC","Age: 23<br />album_date: 2014-11-17<br />track_popularity: 57<br />Song: Happy Idiot Artist: TV On The Radio","Age: 23<br />album_date: 2014-05-13<br />track_popularity: 29<br />Song: The Ballad of Mr. Steak Artist: Kishi Bashi","Age: 23<br />album_date: 2016-06-24<br />track_popularity: 16<br />Song: Conversations With Myself Artist: Drowners","Age: 23<br />album_date: 2015-06-02<br />track_popularity:  5<br />Song: Star Sickness Artist: Miaoux Miaoux","Age: 23<br />album_date: 2018-02-23<br />track_popularity: 54<br />Song: Lay It On Me Artist: Vance Joy","Age: 23<br />album_date: 2017-05-19<br />track_popularity: 85<br />Song: I Like Me Better Artist: Lauv","Age: 23<br />album_date: 2016-06-17<br />track_popularity: 73<br />Song: Cómo Te Atreves Artist: Morat","Age: 23<br />album_date: 2015-07-31<br />track_popularity: 72<br />Song: Ocean Drive Artist: Duke Dumont","Age: 23<br />album_date: 2015-05-04<br />track_popularity: 69<br />Song: Shots - Broiler Remix Artist: Imagine Dragons","Age: 23<br />album_date: 2017-06-23<br />track_popularity:  7<br />Song: Amor Con Hielo Artist: Morat","Age: 23<br />album_date: 2017-02-23<br />track_popularity: 81<br />Song: Stay (with Alessia Cara) Artist: Zedd","Age: 23<br />album_date: 2016-09-02<br />track_popularity: 75<br />Song: Capsize Artist: FRENSHIP","Age: 23<br />album_date: 2017-06-16<br />track_popularity: 70<br />Song: Feel It Still Artist: Portugal. The Man","Age: 23<br />album_date: 2017-09-01<br />track_popularity: 46<br />Song: Burn It Down Artist: Daughter","Age: 23<br />album_date: 2018-05-11<br />track_popularity: 31<br />Song: Attention Artist: Charlie Puth","Age: 23<br />album_date: 2016-05-27<br />track_popularity: 66<br />Song: Sex Artist: Cheat Codes","Age: 23<br />album_date: 2013-09-10<br />track_popularity: 59<br />Song: Strong Artist: London Grammar","Age: 23<br />album_date: 2014-01-01<br />track_popularity: 64<br />Song: Bad Habit Artist: The Kooks","Age: 23<br />album_date: 2015-07-17<br />track_popularity: 79<br />Song: The Less I Know The Better Artist: Tame Impala","Age: 23<br />album_date: 2015-09-04<br />track_popularity: 63<br />Song: Technicolour Beat Artist: Oh Wonder","Age: 23<br />album_date: 2017-08-25<br />track_popularity: 89<br />Song: Jocelyn Flores Artist: XXXTENTACION","Age: 23<br />album_date: 2013-05-31<br />track_popularity: 59<br />Song: Modern Jesus Artist: Portugal. The Man","Age: 23<br />album_date: 2017-07-28<br />track_popularity: 63<br />Song: Issues Artist: Julia Michaels","Age: 23<br />album_date: 2015-05-01<br />track_popularity: 46<br />Song: Hit Me Like a Drum Artist: Parov Stelar","Age: 23<br />album_date: 2016-09-16<br />track_popularity: 26<br />Song: Weak Artist: AJR","Age: 23<br />album_date: 2017-03-20<br />track_popularity: 36<br />Song: Issues Artist: Walk Off the Earth","Age: 23<br />album_date: 2016-11-18<br />track_popularity: 73<br />Song: How Far I'll Go Artist: Auli'i Cravalho","Age: 23<br />album_date: 2017-01-27<br />track_popularity: 62<br />Song: No Lie - Sam Feldt Remix Artist: Sean Paul","Age: 23<br />album_date: 2007-06-05<br />track_popularity: 66<br />Song: To Build A Home Artist: The Cinematic Orchestra","Age: 23<br />album_date: 2017-04-14<br />track_popularity: 67<br />Song: XXX. FEAT. U2. Artist: Kendrick Lamar","Age: 23<br />album_date: 2017-06-09<br />track_popularity: 67<br />Song: 2U Artist: David Guetta","Age: 23<br />album_date: 1999-11-01<br />track_popularity: 62<br />Song: Sleep Now In the Fire Artist: Rage Against The Machine","Age: 23<br />album_date: 1998-01-01<br />track_popularity: 65<br />Song: Sugar Artist: System Of A Down","Age: 23<br />album_date: 2017-06-28<br />track_popularity: 54<br />Song: Turbo Killer Artist: Carpenter Brut","Age: 23<br />album_date: 2014-11-04<br />track_popularity: 37<br />Song: Begin Again Artist: Knife Party","Age: 23<br />album_date: 2016-12-23<br />track_popularity: 37<br />Song: Setting Fires - Vanic Remix Artist: The Chainsmokers","Age: 23<br />album_date: 2016-03-18<br />track_popularity: 33<br />Song: Conquer the Galaxy Artist: Delta Heavy","Age: 23<br />album_date: 1990-02-20<br />track_popularity: 46<br />Song: Home For A Rest Artist: Spirit Of The West","Age: 23<br />album_date: 1958-01-01<br />track_popularity: 68<br />Song: Come Fly With Me - Remastered Artist: Frank Sinatra","Age: 23<br />album_date: 2011-01-01<br />track_popularity: 56<br />Song: The Motto Artist: Drake","Age: 23<br />album_date: 2017-04-14<br />track_popularity: 61<br />Song: BLOOD. Artist: Kendrick Lamar","Age: 23<br />album_date: 2016-09-23<br />track_popularity: 44<br />Song: Cinema (Skrillex Remix) - LUCA LUSH Flip Artist: Benny Benassi","Age: 23<br />album_date: 2014-12-23<br />track_popularity: 34<br />Song: Peer Gynt Suite No. 1, Op. 46 : IV. In the Hall of the Mountain King Artist: Edvard Grieg","Age: 23<br />album_date: 2005-05-17<br />track_popularity: 65<br />Song: Violent Pornography Artist: System Of A Down","Age: 23<br />album_date: 2013-09-03<br />track_popularity: 51<br />Song: 115 - Remaster Artist: Treyarch","Age: 23<br />album_date: 1978-01-01<br />track_popularity: 59<br />Song: Rasputin Artist: Boney M.","Age: 23<br />album_date: 2014-11-04<br />track_popularity: 42<br />Song: Give It Up Artist: Knife Party","Age: 23<br />album_date: 2009-09-10<br />track_popularity: 75<br />Song: Uprising Artist: Muse","Age: 23<br />album_date: 2017-02-10<br />track_popularity: 44<br />Song: Love Stuck Artist: Mother Mother","Age: 23<br />album_date: 2006-01-01<br />track_popularity: 42<br />Song: O Magnum Mysterium: O magnum mysterium Artist: Morten Lauridsen","Age: 23<br />album_date: 2004-01-01<br />track_popularity: 77<br />Song: Mr. Brightside Artist: The Killers","Age: 23<br />album_date: 2012-09-24<br />track_popularity: 57<br />Song: Follow Me Artist: Muse","Age: 23<br />album_date: 2009-01-01<br />track_popularity: 38<br />Song: Ghosts 'n' Stuff Artist: deadmau5","Age: 23<br />album_date: 2004-03-23<br />track_popularity: 66<br />Song: Time Is Running Out Artist: Muse","Age: 23<br />album_date: 2016-05-28<br />track_popularity: 34<br />Song: France Artist: Opal","Age: 23<br />album_date: 2015-11-01<br />track_popularity: 28<br />Song: Drown Artist: DonMonique","Age: 23<br />album_date: 2015-01-01<br />track_popularity: 42<br />Song: Suede Jaw Artist: 808INK","Age: 23<br />album_date: 2016-05-06<br />track_popularity: 51<br />Song: LEAVE ME ALONE Artist: KAYTRANADA","Age: 23<br />album_date: 2016-11-08<br />track_popularity: 15<br />Song: Haunt U (feat. Russ Liquid) Artist: Thriftworks","Age: 23<br />album_date: 2016-02-29<br />track_popularity: 20<br />Song: I Just Want to Live Artist: Lorine Chia","Age: 23<br />album_date: 2016-09-23<br />track_popularity: 52<br />Song: I'M DEAD Artist: Duckwrth","Age: 23<br />album_date: 2014-11-07<br />track_popularity:  1<br />Song: 212 Artist: Azealia Banks","Age: 23<br />album_date: 2016-07-15<br />track_popularity: 49<br />Song: Girls @ (feat. Chance the Rapper) Artist: Joey Purp","Age: 23<br />album_date: 2016-05-06<br />track_popularity: 55<br />Song: BULLETS Artist: KAYTRANADA","Age: 23<br />album_date: 2017-08-25<br />track_popularity: 18<br />Song: XO TOUR Llif3 Artist: Lil Uzi Vert","Age: 23<br />album_date: 2017-04-02<br />track_popularity: 40<br />Song: Steve Harvey (feat. Yung Gravy) Artist: Engelwood","Age: 23<br />album_date: 2016-03-26<br />track_popularity: 39<br />Song: Lunch ' N ' Breakfast Original remix Artist: AtariJones","Age: 23<br />album_date: 2016-08-05<br />track_popularity: 41<br />Song: Worship Artist: Duke Dumont","Age: 23<br />album_date: 2015-11-06<br />track_popularity: 50<br />Song: Yap Artist: Bluestaeb","Age: 23<br />album_date: 2016-09-16<br />track_popularity: 84<br />Song: goosebumps Artist: Travis Scott","Age: 23<br />album_date: 2016-06-10<br />track_popularity: 43<br />Song: Costa Rica Artist: Phlake","Age: 23<br />album_date: 2017-03-24<br />track_popularity: 47<br />Song: Herside Story Artist: Hare Squead","Age: 23<br />album_date: 2016-06-03<br />track_popularity: 48<br />Song: GAZI Artist: A.CHAL","Age: 23<br />album_date: 2016-12-16<br />track_popularity: 60<br />Song: By Design Artist: Kid Cudi","Age: 23<br />album_date: 2017-07-21<br />track_popularity: 64<br />Song: Pothole Artist: Tyler, The Creator","Age: 23<br />album_date: 2015-02-13<br />track_popularity: 48<br />Song: Time Artist: N'to","Age: 23<br />album_date: 2016-05-06<br />track_popularity: 64<br />Song: YOU'RE THE ONE Artist: KAYTRANADA","Age: 23<br />album_date: 2017-07-21<br />track_popularity: 71<br />Song: Who Dat Boy Artist: Tyler, The Creator"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(49,104,142,1)","opacity":0.7,"size":7.55905511811024,"symbol":"circle","line":{"width":0,"color":"rgba(49,104,142,1)"}},"hoveron":"points","name":"23","legendgroup":"23","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[293,12283,16682,-2579,-267,11323,9707,11989,12745,10957,14747,16168,63,11576,13781,8286,2557,16349,3571,12318,-2411,1772,10592,16349,15340,5734,15340,16242,15257,11100,17340,15601,13098,11877,15706,9983,17340,16976,17317,11388,16581,17228,11995,14418,16969,17340,16242,15852,11833,17270,17193,17347,17270,15340,8874,16948,17270,17403,16948,13928,17060,16948,16510,17221,17452,17193,15340,13928,15157,15340,16962,9021,14975,17273,17073,16829,16570,16790,17480,15964,17263,17144,16976,16892,17144,17144,16436,16370,15964,17000,16436,2191,7670,17011,15964,17333,16976,17263,17235,16941,17109,16756,17277,-17,14245,17284,17410,14245,16371,17102,17410,16969,17200,17228,16490,16203,17648,17354,1977,17403,17200,13675],"y":[44,11,42,18,63,51,61,15,33,50,49,0,2,39,43,61,0,0,35,48,56,27,50,0,28,15,30,51,61,71,10,68,66,79,14,60,30,78,45,69,23,70,57,20,72,19,49,50,23,81,77,20,84,70,63,66,61,18,69,60,84,74,60,74,8,78,67,70,60,54,73,77,59,80,71,61,82,52,13,49,76,67,64,79,64,65,20,69,37,62,73,80,5,85,51,38,40,2,40,53,14,52,2,11,36,61,60,22,47,21,20,53,0,28,29,58,7,43,33,54,9,6],"text":["Age: 24<br />album_date: 1970-10-21<br />track_popularity: 44<br />Song: If Not for You Artist: Bob Dylan","Age: 24<br />album_date: 2003-08-19<br />track_popularity: 11<br />Song: Red Morning Light Artist: Kings of Leon","Age: 24<br />album_date: 2015-09-04<br />track_popularity: 42<br />Song: Dream 3 (in the midst of my life) Artist: Max Richter","Age: 24<br />album_date: 1962-12-10<br />track_popularity: 18<br />Song: Fly Me To The Moon Artist: Frank Sinatra","Age: 24<br />album_date: 1969-04-09<br />track_popularity: 63<br />Song: Girl from the North Country (with Johnny Cash) Artist: Bob Dylan","Age: 24<br />album_date: 2001-01-01<br />track_popularity: 51<br />Song: We’re Going To Be Friends Artist: The White Stripes","Age: 24<br />album_date: 1996-07-30<br />track_popularity: 61<br />Song: What I Got Artist: Sublime","Age: 24<br />album_date: 2002-10-29<br />track_popularity: 15<br />Song: About A Girl Artist: Nirvana","Age: 24<br />album_date: 2004-11-23<br />track_popularity: 33<br />Song: The Afterglow Artist: John Frusciante","Age: 24<br />album_date: 2000-01-01<br />track_popularity: 50<br />Song: Apple Blossom Artist: The White Stripes","Age: 24<br />album_date: 2010-05-18<br />track_popularity: 49<br />Song: Everlasting Light Artist: The Black Keys","Age: 24<br />album_date: 2014-04-08<br />track_popularity:  0<br />Song: Fanfare Artist: John Frusciante","Age: 24<br />album_date: 1970-03-05<br />track_popularity:  2<br />Song: I Wonder Artist: Rodríguez","Age: 24<br />album_date: 2001-09-11<br />track_popularity: 39<br />Song: Mississippi Artist: Bob Dylan","Age: 24<br />album_date: 2007-09-25<br />track_popularity: 43<br />Song: Ballad Of The Beaconsfield Miners Artist: Foo Fighters","Age: 24<br />album_date: 1992-09-08<br />track_popularity: 61<br />Song: September Artist: Earth, Wind & Fire","Age: 24<br />album_date: 1977-01-01<br />track_popularity:  0<br />Song: Woncha Come On Home Artist: Joan Armatrading","Age: 24<br />album_date: 2014-10-06<br />track_popularity:  0<br />Song: Dismissed Artist: Justin Hurwitz","Age: 24<br />album_date: 1979-10-12<br />track_popularity: 35<br />Song: What Makes You Think You're the One - 2015 Remaster Artist: Fleetwood Mac","Age: 24<br />album_date: 2003-09-23<br />track_popularity: 48<br />Song: Woo Hoo Artist: The 5.6.7.8's","Age: 24<br />album_date: 1963-05-27<br />track_popularity: 56<br />Song: A Hard Rain's A-Gonna Fall Artist: Bob Dylan","Age: 24<br />album_date: 1974-11-08<br />track_popularity: 27<br />Song: Killer Queen - Remastered 2011 Artist: Queen","Age: 24<br />album_date: 1999-01-01<br />track_popularity: 50<br />Song: Victoria - EP Version Artist: John Mayer","Age: 24<br />album_date: 2014-10-06<br />track_popularity:  0<br />Song: Snare Liftoff - I Want To Be One Of The Greats Artist: J.K. Simmons","Age: 24<br />album_date: 2012-01-01<br />track_popularity: 28<br />Song: Radioactive Artist: Imagine Dragons","Age: 24<br />album_date: 1985-09-13<br />track_popularity: 15<br />Song: Part-Time Lover Artist: Stevie Wonder","Age: 24<br />album_date: 2012-01-01<br />track_popularity: 30<br />Song: Demons Artist: Imagine Dragons","Age: 24<br />album_date: 2014-06-21<br />track_popularity: 51<br />Song: Runaway Artist: Ed Sheeran","Age: 24<br />album_date: 2011-10-10<br />track_popularity: 61<br />Song: I Follow Rivers Artist: Lykke Li","Age: 24<br />album_date: 2000-05-23<br />track_popularity: 71<br />Song: The Real Slim Shady Artist: Eminem","Age: 24<br />album_date: 2017-06-23<br />track_popularity: 10<br />Song: Whatever It Takes Artist: Imagine Dragons","Age: 24<br />album_date: 2012-09-18<br />track_popularity: 68<br />Song: Breezeblocks Artist: alt-J","Age: 24<br />album_date: 2005-11-11<br />track_popularity: 66<br />Song: Hung Up Artist: Madonna","Age: 24<br />album_date: 2002-07-09<br />track_popularity: 79<br />Song: Can't Stop Artist: Red Hot Chili Peppers","Age: 24<br />album_date: 2013-01-01<br />track_popularity: 14<br />Song: Run Boy Run Artist: Woodkid","Age: 24<br />album_date: 1997-05-02<br />track_popularity: 60<br />Song: Pink Artist: Aerosmith","Age: 24<br />album_date: 2017-06-23<br />track_popularity: 30<br />Song: Thunder Artist: Imagine Dragons","Age: 24<br />album_date: 2016-06-24<br />track_popularity: 78<br />Song: Sucker For Pain (with Wiz Khalifa, Imagine Dragons, Logic & Ty Dolla $ign feat. X Ambassadors) Artist: Lil Wayne","Age: 24<br />album_date: 2017-05-31<br />track_popularity: 45<br />Song: Something About Us - Recorded at Spotify Studios NYC Artist: Saint Motel","Age: 24<br />album_date: 2001-03-07<br />track_popularity: 69<br />Song: Harder Better Faster Stronger Artist: Daft Punk","Age: 24<br />album_date: 2015-05-26<br />track_popularity: 23<br />Song: Gangsta's Paradise (feat. L.V.) Artist: Coolio","Age: 24<br />album_date: 2017-03-03<br />track_popularity: 70<br />Song: Eraser Artist: Ed Sheeran","Age: 24<br />album_date: 2002-11-04<br />track_popularity: 57<br />Song: Cry Me a River Artist: Justin Timberlake","Age: 24<br />album_date: 2009-06-23<br />track_popularity: 20<br />Song: Digital Love Artist: Bronze Radio Return","Age: 24<br />album_date: 2016-06-17<br />track_popularity: 72<br />Song: Dark Necessities Artist: Red Hot Chili Peppers","Age: 24<br />album_date: 2017-06-23<br />track_popularity: 19<br />Song: Believer Artist: Imagine Dragons","Age: 24<br />album_date: 2014-06-21<br />track_popularity: 49<br />Song: Take It Back Artist: Ed Sheeran","Age: 24<br />album_date: 2013-05-27<br />track_popularity: 50<br />Song: Demons Artist: The National","Age: 24<br />album_date: 2002-05-26<br />track_popularity: 23<br />Song: Without Me Artist: Eminem","Age: 24<br />album_date: 2017-04-14<br />track_popularity: 81<br />Song: DNA. Artist: Kendrick Lamar","Age: 24<br />album_date: 2017-01-27<br />track_popularity: 77<br />Song: Bad and Boujee (feat. Lil Uzi Vert) Artist: Migos","Age: 24<br />album_date: 2017-06-30<br />track_popularity: 20<br />Song: Mask Off Artist: Future","Age: 24<br />album_date: 2017-04-14<br />track_popularity: 84<br />Song: HUMBLE. Artist: Kendrick Lamar","Age: 24<br />album_date: 2012-01-01<br />track_popularity: 70<br />Song: Radioactive Artist: Imagine Dragons","Age: 24<br />album_date: 1994-04-19<br />track_popularity: 63<br />Song: N.Y. State of Mind Artist: Nas","Age: 24<br />album_date: 2016-05-27<br />track_popularity: 66<br />Song: All We Got (feat. Kanye West & Chicago Children's Choir) Artist: Chance the Rapper","Age: 24<br />album_date: 2017-04-14<br />track_popularity: 61<br />Song: BLOOD. Artist: Kendrick Lamar","Age: 24<br />album_date: 2017-08-25<br />track_popularity: 18<br />Song: XO TOUR Llif3 Artist: Lil Uzi Vert","Age: 24<br />album_date: 2016-05-27<br />track_popularity: 69<br />Song: Same Drugs Artist: Chance the Rapper","Age: 24<br />album_date: 2008-02-19<br />track_popularity: 60<br />Song: Flume Artist: Bon Iver","Age: 24<br />album_date: 2016-09-16<br />track_popularity: 84<br />Song: goosebumps Artist: Travis Scott","Age: 24<br />album_date: 2016-05-27<br />track_popularity: 74<br />Song: No Problem (feat. Lil Wayne & 2 Chainz) Artist: Chance the Rapper","Age: 24<br />album_date: 2015-03-16<br />track_popularity: 60<br />Song: Wesley's Theory Artist: Kendrick Lamar","Age: 24<br />album_date: 2017-02-24<br />track_popularity: 74<br />Song: Slide (feat. Frank Ocean & Migos) Artist: Calvin Harris","Age: 24<br />album_date: 2017-10-13<br />track_popularity:  8<br />Song: I Get The Bag (feat. Migos) Artist: Gucci Mane","Age: 24<br />album_date: 2017-01-27<br />track_popularity: 78<br />Song: Slippery (feat. Gucci Mane) Artist: Migos","Age: 24<br />album_date: 2012-01-01<br />track_popularity: 67<br />Song: Poetic Justice Artist: Kendrick Lamar","Age: 24<br />album_date: 2008-02-19<br />track_popularity: 70<br />Song: Skinny Love Artist: Bon Iver","Age: 24<br />album_date: 2011-07-02<br />track_popularity: 60<br />Song: HiiiPower Artist: Kendrick Lamar","Age: 24<br />album_date: 2012-01-01<br />track_popularity: 54<br />Song: Birthday Song Artist: 2 Chainz","Age: 24<br />album_date: 2016-06-10<br />track_popularity: 73<br />Song: Famous Artist: Kanye West","Age: 24<br />album_date: 1994-09-13<br />track_popularity: 77<br />Song: Juicy Artist: The Notorious B.I.G.","Age: 24<br />album_date: 2011-01-01<br />track_popularity: 59<br />Song: Love You Like A Love Song Artist: Selena Gomez & The Scene","Age: 24<br />album_date: 2017-04-17<br />track_popularity: 80<br />Song: Despacito - Remix Artist: Luis Fonsi","Age: 24<br />album_date: 2016-09-29<br />track_popularity: 71<br />Song: All We Know Artist: The Chainsmokers","Age: 24<br />album_date: 2016-01-29<br />track_popularity: 61<br />Song: Dangerously Artist: Charlie Puth","Age: 24<br />album_date: 2015-05-15<br />track_popularity: 82<br />Song: Stressed Out Artist: Twenty One Pilots","Age: 24<br />album_date: 2015-12-21<br />track_popularity: 52<br />Song: Love Yourself vs F*CK Yourself - Love Yourself Response Artist: Yo Preston","Age: 24<br />album_date: 2017-11-10<br />track_popularity: 13<br />Song: Look What You Made Me Do Artist: Taylor Swift","Age: 24<br />album_date: 2013-09-16<br />track_popularity: 49<br />Song: 李白 Artist: Ronghao Li","Age: 24<br />album_date: 2017-04-07<br />track_popularity: 76<br />Song: Something Just Like This Artist: The Chainsmokers","Age: 24<br />album_date: 2016-12-09<br />track_popularity: 67<br />Song: Another Day Of Sun - From \"La La Land\" Soundtrack Artist: La La Land Cast","Age: 24<br />album_date: 2016-06-24<br />track_popularity: 64<br />Song: 告白氣球 Artist: Jay Chou","Age: 24<br />album_date: 2016-04-01<br />track_popularity: 79<br />Song: 7 Years Artist: Lukas Graham","Age: 24<br />album_date: 2016-12-09<br />track_popularity: 64<br />Song: Someone In The Crowd - From \"La La Land\" Soundtrack Artist: Emma Stone","Age: 24<br />album_date: 2016-12-09<br />track_popularity: 65<br />Song: City Of Stars - From \"La La Land\" Soundtrack Artist: Ryan Gosling","Age: 24<br />album_date: 2015-01-01<br />track_popularity: 20<br />Song: See You Again (feat. Charlie Puth) Artist: Wiz Khalifa","Age: 24<br />album_date: 2014-10-27<br />track_popularity: 69<br />Song: Blank Space Artist: Taylor Swift","Age: 24<br />album_date: 2013-09-16<br />track_popularity: 37<br />Song: 太坦白 Artist: Ronghao Li","Age: 24<br />album_date: 2016-07-18<br />track_popularity: 62<br />Song: 演員 Artist: Joker Xue","Age: 24<br />album_date: 2015-01-01<br />track_popularity: 73<br />Song: Sunset Lover Artist: Petit Biscuit","Age: 24<br />album_date: 1976-01-01<br />track_popularity: 80<br />Song: Hotel California - Remastered Artist: Eagles","Age: 24<br />album_date: 1991-01-01<br />track_popularity:  5<br />Song: Don't Cry (Original) Artist: Guns N' Roses","Age: 24<br />album_date: 2016-07-29<br />track_popularity: 85<br />Song: Closer Artist: The Chainsmokers","Age: 24<br />album_date: 2013-09-16<br />track_popularity: 51<br />Song: 模特 Artist: Ronghao Li","Age: 24<br />album_date: 2017-06-16<br />track_popularity: 38<br />Song: Stand By Me - Acoustic Artist: Matt Johnson","Age: 24<br />album_date: 2016-06-24<br />track_popularity: 40<br />Song: 床邊故事 Artist: Jay Chou","Age: 24<br />album_date: 2017-04-07<br />track_popularity:  2<br />Song: Ran Artist: Future Islands","Age: 24<br />album_date: 2017-03-10<br />track_popularity: 40<br />Song: Name For You Artist: The Shins","Age: 24<br />album_date: 2016-05-20<br />track_popularity: 53<br />Song: Fill in the Blank Artist: Car Seat Headrest","Age: 24<br />album_date: 2016-11-04<br />track_popularity: 14<br />Song: Give Up Artist: American Wrestlers","Age: 24<br />album_date: 2015-11-17<br />track_popularity: 52<br />Song: Have Mercy Artist: Eryn Allen Kane","Age: 24<br />album_date: 2017-04-21<br />track_popularity:  2<br />Song: Cavers Artist: Maple & Beech","Age: 24<br />album_date: 1969-12-15<br />track_popularity: 11<br />Song: Color Him Father Artist: The Winstons","Age: 24<br />album_date: 2009-01-01<br />track_popularity: 36<br />Song: Run, Run, Run Artist: Ann Peebles","Age: 24<br />album_date: 2017-04-28<br />track_popularity: 61<br />Song: Die Young Artist: Sylvan Esso","Age: 24<br />album_date: 2017-09-01<br />track_popularity: 60<br />Song: oh baby Artist: LCD Soundsystem","Age: 24<br />album_date: 2009-01-01<br />track_popularity: 22<br />Song: A Love Vibration Artist: Ann Peebles","Age: 24<br />album_date: 2014-10-28<br />track_popularity: 47<br />Song: Had Ten Dollaz Artist: Cherry Glazerr","Age: 24<br />album_date: 2016-10-28<br />track_popularity: 21<br />Song: Simulation Artist: Tkay Maidza","Age: 24<br />album_date: 2017-09-01<br />track_popularity: 20<br />Song: Little of Your Love - Wookie Remix Artist: HAIM","Age: 24<br />album_date: 2016-06-17<br />track_popularity: 53<br />Song: Your Best American Girl Artist: Mitski","Age: 24<br />album_date: 2017-02-03<br />track_popularity:  0<br />Song: Come on, I'm Waiting Artist: Communions","Age: 24<br />album_date: 2017-03-03<br />track_popularity: 28<br />Song: Strange or Be Forgotten Artist: Temples","Age: 24<br />album_date: 2015-02-24<br />track_popularity: 29<br />Song: Soubour Artist: Songhoy Blues","Age: 24<br />album_date: 2014-05-13<br />track_popularity: 58<br />Song: Coffee Artist: Sylvan Esso","Age: 24<br />album_date: 2018-04-27<br />track_popularity:  7<br />Song: New Religion Artist: Gold Connections","Age: 24<br />album_date: 2017-07-07<br />track_popularity: 43<br />Song: Want You Back Artist: HAIM","Age: 24<br />album_date: 1975-06-01<br />track_popularity: 33<br />Song: These Dreams Artist: Jim Croce","Age: 24<br />album_date: 2017-08-25<br />track_popularity: 54<br />Song: Up All Night Artist: The War On Drugs","Age: 24<br />album_date: 2017-02-03<br />track_popularity:  9<br />Song: White Noise Artist: Ella Vos","Age: 24<br />album_date: 2007-06-11<br />track_popularity:  6<br />Song: D.A.N.C.E. Artist: Justice"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(33,144,140,1)","opacity":0.7,"size":7.55905511811024,"symbol":"circle","line":{"width":0,"color":"rgba(33,144,140,1)"}},"hoveron":"points","name":"24","legendgroup":"24","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[17375,15593,13200,17375,14799,17375,17179,17333,16071,15706,14245,13200,17144,15649,17263,17179,17144,17067,13584,13200,15340,13514,17067,16252,14488,16724,17410,17074,17186,17340,17403,16871,16745,16927,17340,17410,17074,17221,16745,16341,16945,16633,17437,17347,17228,17361,17340,17347,17221,16341,17033,16526,15260,5458,7670,16668,8247,-5114,11092,15340,9496,9794,4748,16436,16473,17273,12157,15260,17872,15407,17033,17865,12418,10227,2922,17284,16948,2486,17340,17284,15841,16976,2922,10957,16266,17375,14321,17243,16532,17179,16654,17200,15544,16864,17263,16828,25,17298,9969,17228,17144,17144,17144,13514,17228,17186,15957,12418,16556,15777,17228,16898,16745,17844,16461,17074,13662,16510,16567,16941,16416,16773,17018],"y":[43,67,70,0,45,56,65,19,53,59,48,63,65,35,60,59,67,50,59,71,34,70,59,41,38,40,49,55,6,42,67,46,36,42,63,53,54,53,18,67,40,25,32,76,58,63,64,16,39,67,72,54,50,62,16,32,54,62,61,61,47,35,22,15,19,34,57,50,61,25,74,63,25,49,54,3,79,67,20,2,31,63,67,56,31,43,43,64,55,77,55,64,51,0,55,60,56,57,64,86,67,63,62,78,73,72,63,51,59,28,81,71,75,64,67,56,74,44,42,82,23,66,76],"text":["Age: 25<br />album_date: 2017-07-28<br />track_popularity: 43<br />Song: Noche De Lluvia Artist: José Madero","Age: 25<br />album_date: 2012-09-10<br />track_popularity: 67<br />Song: Angels Artist: The xx","Age: 25<br />album_date: 2006-02-21<br />track_popularity: 70<br />Song: Mardy Bum Artist: Arctic Monkeys","Age: 25<br />album_date: 2017-07-28<br />track_popularity:  0<br />Song: Noche De Brujas Artist: José Madero","Age: 25<br />album_date: 2010-07-09<br />track_popularity: 45<br />Song: Time (From \"Inception\") Artist: Hans Zimmer","Age: 25<br />album_date: 2017-07-28<br />track_popularity: 56<br />Song: Everything Now Artist: Arcade Fire","Age: 25<br />album_date: 2017-01-13<br />track_popularity: 65<br />Song: On Hold Artist: The xx","Age: 25<br />album_date: 2017-06-16<br />track_popularity: 19<br />Song: Me Rehúso Artist: Danny Ocean","Age: 25<br />album_date: 2014-01-01<br />track_popularity: 53<br />Song: Born Slippy (Nuxx) Artist: Underworld","Age: 25<br />album_date: 2013-01-01<br />track_popularity: 59<br />Song: Alive Artist: Empire of the Sun","Age: 25<br />album_date: 2009-01-01<br />track_popularity: 48<br />Song: Rome Artist: Phoenix","Age: 25<br />album_date: 2006-02-21<br />track_popularity: 63<br />Song: A Certain Romance Artist: Arctic Monkeys","Age: 25<br />album_date: 2016-12-09<br />track_popularity: 65<br />Song: City Of Stars - From \"La La Land\" Soundtrack Artist: Ryan Gosling","Age: 25<br />album_date: 2012-11-05<br />track_popularity: 35<br />Song: A Veces Ni Eso Artist: La Bien Querida","Age: 25<br />album_date: 2017-04-07<br />track_popularity: 60<br />Song: La Magia Artist: Little Jesus","Age: 25<br />album_date: 2017-01-13<br />track_popularity: 59<br />Song: Say Something Loving Artist: The xx","Age: 25<br />album_date: 2016-12-09<br />track_popularity: 67<br />Song: Another Day Of Sun - From \"La La Land\" Soundtrack Artist: La La Land Cast","Age: 25<br />album_date: 2016-09-23<br />track_popularity: 50<br />Song: New Song Artist: Warpaint","Age: 25<br />album_date: 2007-03-12<br />track_popularity: 59<br />Song: All My Friends Artist: LCD Soundsystem","Age: 25<br />album_date: 2006-02-21<br />track_popularity: 71<br />Song: When The Sun Goes Down Artist: Arctic Monkeys","Age: 25<br />album_date: 2012-01-01<br />track_popularity: 34<br />Song: Money Trees Artist: Kendrick Lamar","Age: 25<br />album_date: 2007-01-01<br />track_popularity: 70<br />Song: Paper Planes Artist: M.I.A.","Age: 25<br />album_date: 2016-09-23<br />track_popularity: 59<br />Song: Bailando Solo Artist: Los Bunkers","Age: 25<br />album_date: 2014-07-01<br />track_popularity: 41<br />Song: Noches de Verano Artist: Los Espiritus","Age: 25<br />album_date: 2009-09-01<br />track_popularity: 38<br />Song: Abigail Artist: PXNDX","Age: 25<br />album_date: 2015-10-16<br />track_popularity: 40<br />Song: Do You Go Up - Original Mix Artist: Khai","Age: 25<br />album_date: 2017-09-01<br />track_popularity: 49<br />Song: Mad Love Artist: The Script","Age: 25<br />album_date: 2016-09-30<br />track_popularity: 55<br />Song: The Shortchange Artist: Thomston","Age: 25<br />album_date: 2017-01-20<br />track_popularity:  6<br />Song: Electric (feat. Khalid) Artist: Alina Baraz","Age: 25<br />album_date: 2017-06-23<br />track_popularity: 42<br />Song: Two People Artist: Jaymes Young","Age: 25<br />album_date: 2017-08-25<br />track_popularity: 67<br />Song: Get You (feat. Kali Uchis) Artist: Daniel Caesar","Age: 25<br />album_date: 2016-03-11<br />track_popularity: 46<br />Song: Loud Places (Live from Spotify London) Artist: HONNE","Age: 25<br />album_date: 2015-11-06<br />track_popularity: 36<br />Song: Coney Island Stroll Artist: Slow Hands","Age: 25<br />album_date: 2016-05-06<br />track_popularity: 42<br />Song: Heartburn Artist: Wafia","Age: 25<br />album_date: 2017-06-23<br />track_popularity: 63<br />Song: Feel Something Artist: Jaymes Young","Age: 25<br />album_date: 2017-09-01<br />track_popularity: 53<br />Song: No Man is an Island Artist: The Script","Age: 25<br />album_date: 2016-09-30<br />track_popularity: 54<br />Song: Window Seat (feat. Wafia) Artist: Thomston","Age: 25<br />album_date: 2017-02-24<br />track_popularity: 53<br />Song: way it goes Artist: Hippo Campus","Age: 25<br />album_date: 2015-11-06<br />track_popularity: 18<br />Song: Phonograph Love Artist: Slow Hands","Age: 25<br />album_date: 2014-09-28<br />track_popularity: 67<br />Song: I'll Be Good Artist: Jaymes Young","Age: 25<br />album_date: 2016-05-24<br />track_popularity: 40<br />Song: Meet in the Middle Artist: Ta-ku","Age: 25<br />album_date: 2015-07-17<br />track_popularity: 25<br />Song: The Less I Know The Better Artist: Tame Impala","Age: 25<br />album_date: 2017-09-28<br />track_popularity: 32<br />Song: Wishing Wells Artist: Miki Ratsula","Age: 25<br />album_date: 2017-06-30<br />track_popularity: 76<br />Song: Feels (feat. Pharrell Williams, Katy Perry & Big Sean) Artist: Calvin Harris","Age: 25<br />album_date: 2017-03-03<br />track_popularity: 58<br />Song: Lying Together Artist: FKJ","Age: 25<br />album_date: 2017-07-14<br />track_popularity: 63<br />Song: Rain Artist: The Script","Age: 25<br />album_date: 2017-06-23<br />track_popularity: 64<br />Song: Infinity Artist: Jaymes Young","Age: 25<br />album_date: 2017-06-30<br />track_popularity: 16<br />Song: ILYSB Artist: LANY","Age: 25<br />album_date: 2017-02-24<br />track_popularity: 39<br />Song: epitaph Artist: Hippo Campus","Age: 25<br />album_date: 2014-09-28<br />track_popularity: 67<br />Song: Habits Of My Heart Artist: Jaymes Young","Age: 25<br />album_date: 2016-08-20<br />track_popularity: 72<br />Song: Self Control Artist: Frank Ocean","Age: 25<br />album_date: 2015-04-01<br />track_popularity: 54<br />Song: Perahu Kertas Artist: Maudy Ayunda","Age: 25<br />album_date: 2011-10-13<br />track_popularity: 50<br />Song: Naughty Artist: Matilda the Musical Original Cast","Age: 25<br />album_date: 1984-12-11<br />track_popularity: 62<br />Song: Hallelujah Artist: Leonard Cohen","Age: 25<br />album_date: 1991-01-01<br />track_popularity: 16<br />Song: Say Anything - 2014 Remaster Artist: X JAPAN","Age: 25<br />album_date: 2015-08-21<br />track_popularity: 32<br />Song: All That Artist: Carly Rae Jepsen","Age: 25<br />album_date: 1992-07-31<br />track_popularity: 54<br />Song: 海闊天空 Artist: Beyond","Age: 25<br />album_date: 1956-01-01<br />track_popularity: 62<br />Song: I Fall In Love Too Easily - Vocal Version Artist: Chet Baker","Age: 25<br />album_date: 2000-05-15<br />track_popularity: 61<br />Song: Lucky Artist: Britney Spears","Age: 25<br />album_date: 2012-01-01<br />track_popularity: 61<br />Song: Super Rich Kids Artist: Frank Ocean","Age: 25<br />album_date: 1996-01-01<br />track_popularity: 47<br />Song: Dream A Little Dream Of Me Artist: Ella Fitzgerald","Age: 25<br />album_date: 1996-10-25<br />track_popularity: 35<br />Song: I'd Be Surprisingly Good For You Artist: Madonna","Age: 25<br />album_date: 1983-01-01<br />track_popularity: 22<br />Song: 月亮代表我的心 Artist: Teresa Teng","Age: 25<br />album_date: 2015-01-01<br />track_popularity: 15<br />Song: Cool for the Summer Artist: Demi Lovato","Age: 25<br />album_date: 2015-02-07<br />track_popularity: 19<br />Song: Look Away Artist: Crissi Cochrane","Age: 25<br />album_date: 2017-04-17<br />track_popularity: 34<br />Song: Despacito - Remix Artist: Luis Fonsi","Age: 25<br />album_date: 2003-04-15<br />track_popularity: 57<br />Song: 十年 Artist: Eason Chan","Age: 25<br />album_date: 2011-10-13<br />track_popularity: 50<br />Song: When I Grow Up Artist: Matilda the Musical Original Cast","Age: 25<br />album_date: 2018-12-07<br />track_popularity: 61<br />Song: (They Long To Be) Close To You Artist: Carpenters","Age: 25<br />album_date: 2012-03-08<br />track_popularity: 25<br />Song: Cupid Artist: Girl's Day","Age: 25<br />album_date: 2016-08-20<br />track_popularity: 74<br />Song: Ivy Artist: Frank Ocean","Age: 25<br />album_date: 2018-11-30<br />track_popularity: 63<br />Song: Can't Take My Eyes Off You Artist: Frankie Valli","Age: 25<br />album_date: 2004-01-01<br />track_popularity: 25<br />Song: Somewhere Only We Know Artist: Keane","Age: 25<br />album_date: 1998-01-01<br />track_popularity: 49<br />Song: 好心分手 Artist: Candy Lo","Age: 25<br />album_date: 1978-01-01<br />track_popularity: 54<br />Song: New York Groove Artist: Ace Frehley","Age: 25<br />album_date: 2017-04-28<br />track_popularity:  3<br />Song: Radio Artist: Sylvan Esso","Age: 25<br />album_date: 2016-05-27<br />track_popularity: 79<br />Song: Work from Home Artist: Fifth Harmony","Age: 25<br />album_date: 1976-10-22<br />track_popularity: 67<br />Song: Night Moves Artist: Bob Seger","Age: 25<br />album_date: 2017-06-23<br />track_popularity: 20<br />Song: Thunder Artist: Imagine Dragons","Age: 25<br />album_date: 2017-04-28<br />track_popularity:  2<br />Song: Die Young Artist: Sylvan Esso","Age: 25<br />album_date: 2013-05-16<br />track_popularity: 31<br />Song: This Must Be the Place Artist: Miles Fisher","Age: 25<br />album_date: 2016-06-24<br />track_popularity: 63<br />Song: Down Artist: Marian Hill","Age: 25<br />album_date: 1978-01-01<br />track_popularity: 67<br />Song: The Gambler Artist: Kenny Rogers","Age: 25<br />album_date: 2000-01-01<br />track_popularity: 56<br />Song: Any Other Name Artist: Thomas Newman","Age: 25<br />album_date: 2014-07-15<br />track_popularity: 31<br />Song: Mission Statement Artist: \"Weird Al\" Yankovic","Age: 25<br />album_date: 2017-07-28<br />track_popularity: 43<br />Song: Creature Comfort Artist: Arcade Fire","Age: 25<br />album_date: 2009-03-18<br />track_popularity: 43<br />Song: Röyksopp Forever Artist: Röyksopp","Age: 25<br />album_date: 2017-03-18<br />track_popularity: 64<br />Song: Madiba Riddim Artist: Drake","Age: 25<br />album_date: 2015-04-07<br />track_popularity: 55<br />Song: Fool for Love Artist: Lord Huron","Age: 25<br />album_date: 2017-01-13<br />track_popularity: 77<br />Song: Paris Artist: The Chainsmokers","Age: 25<br />album_date: 2015-08-07<br />track_popularity: 55<br />Song: This Is The Day Artist: The The","Age: 25<br />album_date: 2017-02-03<br />track_popularity: 64<br />Song: (No One Knows Me) Like the Piano Artist: Sampha","Age: 25<br />album_date: 2012-07-23<br />track_popularity: 51<br />Song: Obedear Artist: Purity Ring","Age: 25<br />album_date: 2016-03-04<br />track_popularity:  0<br />Song: Your Life In The End Artist: Prince Rama","Age: 25<br />album_date: 2017-04-07<br />track_popularity: 55<br />Song: The One Artist: The Chainsmokers","Age: 25<br />album_date: 2016-01-28<br />track_popularity: 60<br />Song: Desperado Artist: Rihanna","Age: 25<br />album_date: 1970-01-26<br />track_popularity: 56<br />Song: Baby Driver Artist: Simon & Garfunkel","Age: 25<br />album_date: 2017-05-12<br />track_popularity: 57<br />Song: Hard Times Artist: Paramore","Age: 25<br />album_date: 1997-04-18<br />track_popularity: 64<br />Song: Ocean Man Artist: Ween","Age: 25<br />album_date: 2017-03-03<br />track_popularity: 86<br />Song: Shape of You Artist: Ed Sheeran","Age: 25<br />album_date: 2016-12-09<br />track_popularity: 67<br />Song: Another Day Of Sun - From \"La La Land\" Soundtrack Artist: La La Land Cast","Age: 25<br />album_date: 2016-12-09<br />track_popularity: 63<br />Song: City Of Stars - From \"La La Land\" Soundtrack/Pier Artist: Ryan Gosling","Age: 25<br />album_date: 2016-12-09<br />track_popularity: 62<br />Song: Mia & Sebastian’s Theme - From \"La La Land\" Soundtrack Artist: Justin Hurwitz","Age: 25<br />album_date: 2007-01-01<br />track_popularity: 78<br />Song: Bleeding Love Artist: Leona Lewis","Age: 25<br />album_date: 2017-03-03<br />track_popularity: 73<br />Song: What Do I Know? Artist: Ed Sheeran","Age: 25<br />album_date: 2017-01-20<br />track_popularity: 72<br />Song: Hasta el Amanecer Artist: Nicky Jam","Age: 25<br />album_date: 2013-09-09<br />track_popularity: 63<br />Song: Boneless Artist: Steve Aoki","Age: 25<br />album_date: 2004-01-01<br />track_popularity: 51<br />Song: Lonely - Old Version Artist: Akon","Age: 25<br />album_date: 2015-05-01<br />track_popularity: 59<br />Song: Big Girls Cry Artist: Sia","Age: 25<br />album_date: 2013-03-13<br />track_popularity: 28<br />Song: Suit & Tie Artist: Tyler Ward","Age: 25<br />album_date: 2017-03-03<br />track_popularity: 81<br />Song: Galway Girl Artist: Ed Sheeran","Age: 25<br />album_date: 2016-04-07<br />track_popularity: 71<br />Song: Ain't Your Mama Artist: Jennifer Lopez","Age: 25<br />album_date: 2015-11-06<br />track_popularity: 75<br />Song: Secret Love Song Artist: Little Mix","Age: 25<br />album_date: 2018-11-09<br />track_popularity: 64<br />Song: Good Nights (feat. Mascolo) Artist: Whethan","Age: 25<br />album_date: 2015-01-26<br />track_popularity: 67<br />Song: Seve - Radio Edit Artist: Tez Cadey","Age: 25<br />album_date: 2016-09-30<br />track_popularity: 56<br />Song: Toxic Artist: Alex & Sierra","Age: 25<br />album_date: 2007-05-29<br />track_popularity: 74<br />Song: Irreplaceable Artist: Beyoncé","Age: 25<br />album_date: 2015-03-16<br />track_popularity: 44<br />Song: Fly Artist: June Marieezy","Age: 25<br />album_date: 2015-05-12<br />track_popularity: 42<br />Song: Gene Takes a Drink Artist: Michael Gordon","Age: 25<br />album_date: 2016-05-20<br />track_popularity: 82<br />Song: Side To Side Artist: Ariana Grande","Age: 25<br />album_date: 2014-12-12<br />track_popularity: 23<br />Song: You've Got a Friend In Me (From \"Toy Story\") Artist: Kids R Us Band","Age: 25<br />album_date: 2015-12-04<br />track_popularity: 66<br />Song: YOUTH Artist: Troye Sivan","Age: 25<br />album_date: 2016-08-05<br />track_popularity: 76<br />Song: Middle Artist: DJ Snake"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(53,183,121,1)","opacity":0.7,"size":7.55905511811024,"symbol":"circle","line":{"width":0,"color":"rgba(53,183,121,1)"}},"hoveron":"points","name":"25","legendgroup":"25","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[16395,14547,17133,14192,13948,17398,17229,15513,17025,13948,15247,17106,16014,17152,17148,16981,17405,17148,17405,17021,16885,16234,17021,15427,17192,16773,17179,16925,16574,16990,14245,17137,17277,17137,16720,17165,14975,16881,17291,14610,17258,17187,16934,17270,15756,16773,13423,17130,13404],"y":[3,56,59,58,53,32,56,53,62,54,52,7,66,64,2,35,32,1,40,7,62,5,7,0,55,74,43,59,62,43,72,7,0,50,47,51,54,9,72,66,41,52,58,9,56,71,74,83,52],"text":["Age: 26<br />album_date: 2014-11-21<br />track_popularity:  3<br />Song: GOOD BOY Artist: GD X TAEYANG","Age: 26<br />album_date: 2009-10-30<br />track_popularity: 56<br />Song: Fairy tale Artist: Yoga Lin","Age: 26<br />album_date: 2016-11-28<br />track_popularity: 59<br />Song: BERMUDA TRIANGLE (feat. Crush & DEAN) Artist: Zico","Age: 26<br />album_date: 2008-11-09<br />track_popularity: 58<br />Song: 說好的幸福呢 Artist: Jay Chou","Age: 26<br />album_date: 2008-03-10<br />track_popularity: 53<br />Song: 蒲公英的約定 Artist: Jay Chou","Age: 26<br />album_date: 2017-08-20<br />track_popularity: 32<br />Song: SEARCH (feat. Car the garden, ZICO) Artist: Hangzoo","Age: 26<br />album_date: 2017-03-04<br />track_popularity: 56<br />Song: Heartbeat Artist: Suran","Age: 26<br />album_date: 2012-06-22<br />track_popularity: 53<br />Song: Unrequited Artist: Yoga Lin","Age: 26<br />album_date: 2016-08-12<br />track_popularity: 62<br />Song: Stand By Me Artist: Florence + The Machine","Age: 26<br />album_date: 2008-03-10<br />track_popularity: 54<br />Song: 彩虹 Artist: Jay Chou","Age: 26<br />album_date: 2011-09-30<br />track_popularity: 52<br />Song: 依然愛你 Artist: Leehom Wang","Age: 26<br />album_date: 2016-11-01<br />track_popularity:  7<br />Song: PLAYING WITH FIRE Artist: BLACKPINK","Age: 26<br />album_date: 2013-11-05<br />track_popularity: 66<br />Song: Legacy Artist: Eminem","Age: 26<br />album_date: 2016-12-17<br />track_popularity: 64<br />Song: Beautiful Artist: Crush","Age: 26<br />album_date: 2016-12-13<br />track_popularity:  2<br />Song: LOSER Artist: BIGBANG","Age: 26<br />album_date: 2016-06-29<br />track_popularity: 35<br />Song: Machine Gun (feat. MINO) Artist: Kush","Age: 26<br />album_date: 2017-08-27<br />track_popularity: 32<br />Song: Filament Artist: Nucksal","Age: 26<br />album_date: 2016-12-13<br />track_popularity:  1<br />Song: BANG BANG BANG Artist: BIGBANG","Age: 26<br />album_date: 2017-08-27<br />track_popularity: 40<br />Song: Red Sun Artist: Hangzoo","Age: 26<br />album_date: 2016-08-08<br />track_popularity:  7<br />Song: BOOMBAYAH Artist: BLACKPINK","Age: 26<br />album_date: 2016-03-25<br />track_popularity: 62<br />Song: D (Half Moon) Artist: DEAN","Age: 26<br />album_date: 2014-06-13<br />track_popularity:  5<br />Song: 눈,코,입(Eyes, Noes, Lips) Artist: TAEYANG","Age: 26<br />album_date: 2016-08-08<br />track_popularity:  7<br />Song: WHISTLE Artist: BLACKPINK","Age: 26<br />album_date: 2012-03-28<br />track_popularity:  0<br />Song: Fantastic Baby Artist: BIGBANG","Age: 26<br />album_date: 2017-01-26<br />track_popularity: 55<br />Song: Cave Me In Artist: Gallant","Age: 26<br />album_date: 2015-12-04<br />track_popularity: 74<br />Song: oui Artist: Jeremih","Age: 26<br />album_date: 2017-01-13<br />track_popularity: 43<br />Song: Feel Something Artist: Black Coast","Age: 26<br />album_date: 2016-05-04<br />track_popularity: 59<br />Song: Weight in Gold Artist: Gallant","Age: 26<br />album_date: 2015-05-19<br />track_popularity: 62<br />Song: Make You Feel Artist: Alina Baraz","Age: 26<br />album_date: 2016-07-08<br />track_popularity: 43<br />Song: Moon Tattoo Artist: Sofi Tukker","Age: 26<br />album_date: 2009-01-01<br />track_popularity: 72<br />Song: Best I Ever Had Artist: Drake","Age: 26<br />album_date: 2016-12-02<br />track_popularity:  7<br />Song: Disorder Artist: RKCB","Age: 26<br />album_date: 2017-04-21<br />track_popularity:  0<br />Song: Too Crazy to Love You Artist: Donny","Age: 26<br />album_date: 2016-12-02<br />track_popularity: 50<br />Song: Caves - Samuraii Remix Artist: Haux","Age: 26<br />album_date: 2015-10-12<br />track_popularity: 47<br />Song: Haunted Artist: Stwo","Age: 26<br />album_date: 2016-12-30<br />track_popularity: 51<br />Song: Not Me (feat. Two Feet) Artist: MELVV","Age: 26<br />album_date: 2011-01-01<br />track_popularity: 54<br />Song: Headlines Artist: Drake","Age: 26<br />album_date: 2016-03-21<br />track_popularity:  9<br />Song: Orbit Artist: SŸDE","Age: 26<br />album_date: 2017-05-05<br />track_popularity: 72<br />Song: Bellyache - Marian Hill Remix Artist: Billie Eilish","Age: 26<br />album_date: 2010-01-01<br />track_popularity: 66<br />Song: Love Don't Change Artist: Jeremih","Age: 26<br />album_date: 2017-04-02<br />track_popularity: 41<br />Song: Wait (Kidswaste Remix) Artist: Kidswaste","Age: 26<br />album_date: 2017-01-21<br />track_popularity: 52<br />Song: Different Skies Artist: Shoffy","Age: 26<br />album_date: 2016-05-13<br />track_popularity: 58<br />Song: Landed On Mars Artist: Atlas Bound","Age: 26<br />album_date: 2017-04-14<br />track_popularity:  9<br />Song: HUMBLE. Artist: Kendrick Lamar","Age: 26<br />album_date: 2013-02-20<br />track_popularity: 56<br />Song: Bless Those Tired Eyes Artist: Clem Leek","Age: 26<br />album_date: 2015-12-04<br />track_popularity: 71<br />Song: Planez Artist: Jeremih","Age: 26<br />album_date: 2006-10-02<br />track_popularity: 74<br />Song: When You Were Young Artist: The Killers","Age: 26<br />album_date: 2016-11-25<br />track_popularity: 83<br />Song: Starboy Artist: The Weeknd","Age: 26<br />album_date: 2006-09-13<br />track_popularity: 52<br />Song: LoveStoned / I Think She Knows Interlude Artist: Justin Timberlake"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(143,215,68,1)","opacity":0.7,"size":7.55905511811024,"symbol":"circle","line":{"width":0,"color":"rgba(143,215,68,1)"}},"hoveron":"points","name":"26","legendgroup":"26","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[16724,16752,14932,17319,12485,16588,8229,9131,16752,15772,14932,9131,15505,13514,15772,16469,12344,12784,12053,11982,10505,14932,13514,14935,15505,17375,17284,14439,17388,16899,17284,13312,17375,17525,16591,17375,16913,17242,16581,17284,15340,17683,17375,16071,14791,15908,17388,17375,17360,15639],"y":[62,69,70,26,60,62,38,58,53,47,65,48,9,61,52,63,63,39,72,70,11,68,46,55,6,29,58,47,37,62,52,31,13,0,73,14,47,3,18,44,17,36,2,66,0,26,33,5,56,70],"text":["Age: 28<br />album_date: 2015-10-16<br />track_popularity: 62<br />Song: Mis Sentimientos Artist: Los Angeles Azules","Age: 28<br />album_date: 2015-11-13<br />track_popularity: 69<br />Song: Hasta la Raíz Artist: Natalia Lafourcade","Age: 28<br />album_date: 2010-11-19<br />track_popularity: 70<br />Song: Muerte En Hawaii Artist: Calle 13","Age: 28<br />album_date: 2017-06-02<br />track_popularity: 26<br />Song: Adeline Artist: alt-J","Age: 28<br />album_date: 2004-03-08<br />track_popularity: 60<br />Song: Todo se transforma Artist: Jorge Drexler","Age: 28<br />album_date: 2015-06-02<br />track_popularity: 62<br />Song: Soy Yo Artist: Bomba Estéreo","Age: 28<br />album_date: 1992-07-13<br />track_popularity: 38<br />Song: Las Batallas Artist: Café Tacvba","Age: 28<br />album_date: 1995-01-01<br />track_popularity: 58<br />Song: Pies Descalzos, Sueños Blancos Artist: Shakira","Age: 28<br />album_date: 2015-11-13<br />track_popularity: 53<br />Song: Mi Lugar Favorito Artist: Natalia Lafourcade","Age: 28<br />album_date: 2013-03-08<br />track_popularity: 47<br />Song: Won't You Come Over Artist: Devendra Banhart","Age: 28<br />album_date: 2010-11-19<br />track_popularity: 65<br />Song: Latinoamérica Artist: Calle 13","Age: 28<br />album_date: 1995-01-01<br />track_popularity: 48<br />Song: Quiero Artist: Shakira","Age: 28<br />album_date: 2012-06-14<br />track_popularity:  9<br />Song: Verde Más Allá Artist: Jenny And The Mexicats","Age: 28<br />album_date: 2007-01-01<br />track_popularity: 61<br />Song: Bolero Falaz Artist: Aterciopelados","Age: 28<br />album_date: 2013-03-08<br />track_popularity: 52<br />Song: Mi Negrita Artist: Devendra Banhart","Age: 28<br />album_date: 2015-02-03<br />track_popularity: 63<br />Song: Querida Artist: Juan Gabriel","Age: 28<br />album_date: 2003-10-19<br />track_popularity: 63<br />Song: Putita Artist: Babasónicos","Age: 28<br />album_date: 2005-01-01<br />track_popularity: 39<br />Song: Tijuana Makes Me Happy Artist: Nortec Collective","Age: 28<br />album_date: 2003-01-01<br />track_popularity: 72<br />Song: Eres Artist: Café Tacvba","Age: 28<br />album_date: 2002-10-22<br />track_popularity: 70<br />Song: La flaca Artist: Jarabe De Palo","Age: 28<br />album_date: 1998-10-06<br />track_popularity: 11<br />Song: Bongo Bong Artist: Manu Chao","Age: 28<br />album_date: 2010-11-19<br />track_popularity: 68<br />Song: La Vuelta Al Mundo Artist: Calle 13","Age: 28<br />album_date: 2007-01-01<br />track_popularity: 46<br />Song: Florecita Rockera Artist: Aterciopelados","Age: 28<br />album_date: 2010-11-22<br />track_popularity: 55<br />Song: Bloodbuzz Ohio Artist: The National","Age: 28<br />album_date: 2012-06-14<br />track_popularity:  6<br />Song: Me Voy a Ir Artist: Jenny And The Mexicats","Age: 28<br />album_date: 2017-07-28<br />track_popularity: 29<br />Song: More Than You Know Artist: Axwell /\\ Ingrosso","Age: 28<br />album_date: 2017-04-28<br />track_popularity: 58<br />Song: Something Just Like This - Alesso Remix Artist: The Chainsmokers","Age: 28<br />album_date: 2009-07-14<br />track_popularity: 47<br />Song: Whatever You Like - Single Version Artist: Anya Marina","Age: 28<br />album_date: 2017-08-10<br />track_popularity: 37<br />Song: Without You (feat. Sandro Cavazza) Artist: Avicii","Age: 28<br />album_date: 2016-04-08<br />track_popularity: 62<br />Song: Wildcard (feat. Sidnie Tipton) Artist: KSHMR","Age: 28<br />album_date: 2017-04-28<br />track_popularity: 52<br />Song: Something Just Like This - Don Diablo Remix Artist: The Chainsmokers","Age: 28<br />album_date: 2006-06-13<br />track_popularity: 31<br />Song: Anything, Anything (I'll Give You) Artist: Lucky Boys Confusion","Age: 28<br />album_date: 2017-07-28<br />track_popularity: 13<br />Song: I Love You Artist: Axwell /\\ Ingrosso","Age: 28<br />album_date: 2017-12-25<br />track_popularity:  0<br />Song: NeverLand Artist: Zayion McCall","Age: 28<br />album_date: 2015-06-05<br />track_popularity: 73<br />Song: Runaway (U & I) Artist: Galantis","Age: 28<br />album_date: 2017-07-28<br />track_popularity: 14<br />Song: Sun Is Shining Artist: Axwell /\\ Ingrosso","Age: 28<br />album_date: 2016-04-22<br />track_popularity: 47<br />Song: Neverland Artist: Carson Key","Age: 28<br />album_date: 2017-03-17<br />track_popularity:  3<br />Song: Back To Me Artist: KSHMR","Age: 28<br />album_date: 2015-05-26<br />track_popularity: 18<br />Song: Heroes (we could be) Artist: Alesso","Age: 28<br />album_date: 2017-04-28<br />track_popularity: 44<br />Song: Something Just Like This - R3hab Remix Artist: The Chainsmokers","Age: 28<br />album_date: 2012-01-01<br />track_popularity: 17<br />Song: Clarity Artist: Zedd","Age: 28<br />album_date: 2018-06-01<br />track_popularity: 36<br />Song: Wake Me Up - Radio Edit Artist: Avicii","Age: 28<br />album_date: 2017-07-28<br />track_popularity:  2<br />Song: Renegade Artist: Axwell /\\ Ingrosso","Age: 28<br />album_date: 2014-01-01<br />track_popularity: 66<br />Song: If I Lose Myself - Alesso vs OneRepublic Artist: OneRepublic","Age: 28<br />album_date: 2010-07-01<br />track_popularity:  0<br />Song: Whatever You Like Artist: This Rigid Empire","Age: 28<br />album_date: 2013-07-22<br />track_popularity: 26<br />Song: Ladi Dadi (feat. Wynter Gordon) - Tommy Trash Remix Artist: Steve Aoki","Age: 28<br />album_date: 2017-08-10<br />track_popularity: 33<br />Song: Lonely Together (feat. Rita Ora) Artist: Avicii","Age: 28<br />album_date: 2017-07-28<br />track_popularity:  5<br />Song: Something New Artist: Axwell /\\ Ingrosso","Age: 28<br />album_date: 2017-07-13<br />track_popularity: 56<br />Song: Something Just Like This - Tokyo Remix Artist: Coldplay","Age: 28<br />album_date: 2012-10-26<br />track_popularity: 70<br />Song: Sweet Nothing (feat. Florence Welch) Artist: Calvin Harris"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(253,231,37,1)","opacity":0.7,"size":7.55905511811024,"symbol":"circle","line":{"width":0,"color":"rgba(253,231,37,1)"}},"hoveron":"points","name":"28","legendgroup":"28","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":46.2864259028643,"r":7.97011207970112,"b":43.8356164383562,"l":40.6475716064757},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":15.9402241594022},"title":"How does album release date relate to track popularity?","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":19.1282689912827},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-6263.3,19021.3],"tickmode":"array","ticktext":["1960","1980","2000","2020"],"tickvals":[-3653,3652,10957,18262],"categoryorder":"array","categoryarray":["1960","1980","2000","2020"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.98505603985056,"tickwidth":0.724555643609193,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.724555643609193,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"Album Release Date","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":15.9402241594022},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-4.45,93.45],"tickmode":"array","ticktext":["0","25","50","75"],"tickvals":[0,25,50,75],"categoryorder":"array","categoryarray":["0","25","50","75"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.98505603985056,"tickwidth":0.724555643609193,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":12.7521793275218},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.724555643609193,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"Track Popularity Levels","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":15.9402241594022},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":2.06156048675734,"font":{"color":"rgba(0,0,0,1)","family":"","size":12.7521793275218},"y":0.905511811023622},"annotations":[{"text":"Age","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":15.9402241594022},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"e900c7345dc":{"colour":{},"x":{},"y":{},"text":{},"type":"scatter"}},"cur_data":"e900c7345dc","visdat":{"e900c7345dc":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->

### V. Executive Summary

In this project, our aim was to comprehensively characterize our DSI cohort's music listening preferences using every student's "Your Top Songs 2017" playlist on Spotify. In this section, we present a summary of our most interesting findings. In order to adequately sum up our class' listening habits, we consider two questions. First, what are the unique listening habits of the cohort as a whole? And second, how diverse is the cohort's taste in music, that is, how do individuals within the class differ from one another? 

**What are the unique listening habits of the cohort as a whole?**

To summarize the unique features of the DSI cohort, we compared the audio features of the songs most popular within the DSI to 2017's overall most popular songs. For most variables, including energy, danceability, and valence (i.e. musical happiness), DSI songs ran the gamut from high to low, and were not particularly differentiated. There was however, a marked difference in loudness, a variable Spotify describes as "primary psychological correlate of physical strength". The chart below compares the loudness distribution of songs listened to by the DSI cohort to that of 2017's most popular songs, both nationally and globally. The results are stark: the DSI as a whole seems to love loud music. Both the 50 most popular DSI songs and all DSI songs show a higher level of loudness than the displayed benchmarks.

<img src="FinalReport_files/figure-html/unnamed-chunk-29-1.png" style="display: block; margin: auto;" />

Another key way in which DSI students differ from the global population is their genre preferences. The chart below plots how popular a given genre was within the DSI, and compares it how popular that genre was among all Spotify users (using the Top 100 Tracks playlist). We see that though pop is the most popular type of music for both groups, it is much less common within the DSI. Rap and hip-hop are similarly less popular among DSI students. Conversely, rock genres, especially modern rock, tend to be much more popular among our class than among all Spotify users.

<img src="FinalReport_files/figure-html/unnamed-chunk-30-1.png" style="display: block; margin: auto;" />

**How diverse is the DSI cohort's musical taste?**

We now focus our attention inwards, and investigate the similarities and differences between individuals in the DSI. An obvious first step is to determine how differentiated each playlist is. To do so, we plot the heat map below, in which the color of each cell represents the number of songs (as a percentage of the total playlist) the user on the x axis has in common with the user on the y axis. Note that for the sake of contrast, we omit cells on the diagonal (a playlist has theoretically has 100% overlap with itself, but this makes it hard to see other patterns).  

<img src="FinalReport_files/figure-html/unnamed-chunk-31-1.png" style="display: block; margin: auto;" />

Though a few users (e.g. Lea, Hammaad, Yimin) have a lot in common, most students exhibit limited overlap. Moreover, some users (e.g. Eric, Shadi) have almost no songs in common with other DSI students.  The next chart offers a potential explanation for this trend. We plot track popularity (a Spotify-defined measure of how popular a given song is) for each track in a user's playlist. We can think of the median of this boxplot as a measure of how "indie" each student is. We note that users with a large amount of overlap in the previous graph (e.g. Lea, Hammaad) are less indie, and tend to listen to more popular music. On the other hand, users with less in common (e.g. Eric) are the most indie, and listen to some fairly obscure songs.
	
<img src="FinalReport_files/figure-html/unnamed-chunk-32-1.png" style="display: block; margin: auto;" />

One final lens through which to view diversity is in terms of consistency: how uniform is each student's preferences? A good way to analyze this is by determining the proportion of a user's playlist that comes from his or her favorite genre. The chart below displays this metric for each student in the DSI. While most users seem to enjoy a variety of genres, some are extremely consistent. Tom is an example of the latter: almost 90% of his songs are classified as rap!

<img src="FinalReport_files/figure-html/unnamed-chunk-33-1.png" style="display: block; margin: auto;" />

Overall, we conclude that though there are some unifying features, the DSI cohort's taste in music is remarkably varied. This diversity is multi-faceted: there are over 1000 artists and 600 genres represented in the combined playlist that formed our data set. A direct consequence of this fact is that it is hard to know what music to expect at a gathering of DSI students: it depends entirely on who is in charge. Perhaps it's Lea, keeping people entertained with a steady stream of Top 40 hits, or Eric, diving into depths of indie, or Tom, bumping the latest Kendrick Lamar song. Just be prepared, for no matter who controls the music: it might get loud!

### VI. Interactive Component

Link to the Interactive Component (Hosted on Block Builder): https://bl.ocks.org/daniellesu/bda312bb5a71159d3bc7483c278ddd96/070d7b9a58c3813c5c624997856a1e88376faeb2

For our interactive component, we decided to make a game where the users can gain general knowledge about the audio features of a song as well as interesting trends of popular songs in DSI 2017 playlists. We used D3 to build this interactive component.  

Specifically, we made a DSI 2017 Spotify playlist with the top 10 songs out of all 2400 songs we collected from users. In this game, a user is asked to play a song from the DSI 2017 playlist, select the song from the dropdown list, and guess the values of eight audio features by dragging the bars (between 0.0 and 1.0) in a bar plot where each bar corresponds to one audio feature. Once the user finishes guessing, s/he can click "Submit", and the actual values and the median values (of all 2400 songs) of those audio features will display in the bar plot. The user can then click "Play Again", select another song from the dropdown list, and repeat the guessing process.

The instructions/rules of the game and the audio features are explained on the webpage, so that the users can easily navigate through the page and play the game. We also made some adjustments to the data to facilitate this game. Particularly, we normalized the values of three audio features, loudness, tempo, and track popularity, so that all the values fall between 0.0 and 1.0 (inclusive), and the bars of all audio features are on the same scale.

There are also several things that we attempted and would like to improve in the future. A small caveat in the technical execution is that when the user drags the bar up and down, if the bar is dragged below 0.0 by accident and the user lets go of the click, the user wouldn't be able to drag it back up. However, you are able to click "Play Again" to refresh the screen and get all the bars back. Another issue is if you keep clicking "Submit" over and over again, it will keep appending new bars with the actual values - if we had more time we would edit this so that wouldn't happen. Additionally, if you do not select a song and you click submit, the bars will fade, in the future we would add a warning that says you have not selected a song and would not let the bars change. If we had more time, we would try to display the numbers of the actual values and the median above the bars. It would also be interesting to calculate a metric that describes how close a user's guesses are to the actual values, indicating how strong the user's music sense is and display that as a result.

We also considered universal design when we created this interactive component. We wanted to make sure that our game is accessible to users with color vision deficiency (CVD). Therefore, we used Color Oracle, a color deficiency simulator, to test our interactive component on all 3 types of color deficiency. In the end, we selected the colors of the bars in such a way that the bars with actual values are still differentiable from the colors of the bars with a user's guesses. Furthermore, we have the transition of the bars with actual values go from bottom to top so that the users can see them coming up and see where they are stopping in relation to the bars with their guesses. With this design, users with CVD would have no problem playing our game. However, we could not make this game accessible to users with hearing impairment, since it asks the user to guess values after listening to a song. In the future, we would like to find an alternative way for those users to enjoy the game.

## VII. Conclusion

The main limitation of our project is that we didn't collect as much data from our classmates as we would have liked. We ended up collecting playlist information from 24 users (including ourselves). We attribute the lack of responses in part to the fact that Spotify was not accessible to everyone in 2017. For example, Spotify is not supported in China or India. This was not absolutely detrimental to our project. However, because there are still 100 songs per playlist and people's music taste varied enough that we had a large number of songs to analyze.
  
Given more time, we would have also analyzed Spotify users' "Your Wrapped 2018", which is the exact same concept as the "Your Top Songs 2017" playlist. We did not do this as the 2018 playlist was recently released. We wanted to do this to be able to analyze if people's tastes had changed over time and if so, in what ways.
  
Through this project, we learned a lot about how to collect data through an API and use it in R. We also learned how to combine data from different sources as we had to combine the survey data with what we could get from the API. We learned more about our cohort and their propensity for loud music.
