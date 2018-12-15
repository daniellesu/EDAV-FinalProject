library(spotifyr)
# input your own spotify client id and spotify client secret to be able to use Spotify's API
Sys.setenv(SPOTIFY_CLIENT_ID = '')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '')

access_token <- get_spotify_access_token()

# reading in and changing the column names
setwd("~/Columbia/EDAV/FinalProject/EDAV-FinalProject")
survey_data <- read_csv("StudentResponses.csv", col_names = TRUE)
names(survey_data) <- c("Timestamp", "email", "uni", "age", "gender", "nationality", "location2017", "undergrad", "useFrequency", "username", "playlistID", "otherPlatform", "consent")


# Deepak gave us the wrong playlist so we are going to delete his row
survey_data[which(survey_data$uni == 'dr2998'),] <- NA

library(stringr)
pattern <- '^spotify:user:(\\S*)$|^https://open\\.spotify\\.com/user/([^?]*)'
matches <- str_match(survey_data$username, pattern)
match1 <- matches[,2]
match2 <- matches[,3]

user <- c()

for (i in seq_along(match1)){
  if(!is.na(match1[i])){
    user <- c(user, match1[i])
  }
  else if(!is.na(match2[i])){
    user <- c(user, match2[i])
  }
  else{
    user <- c(user, NA)
  }
}

survey_data$cleanUser <- user

pattern2 <- '^spotify:user:[\\S]*:playlist:(\\S*)$|^https:\\/\\/open\\.spotify\\.com\\/playlist\\/(\\S*)$|^https:\\/\\/open\\.spotify\\.com\\/user\\/spotify\\/playlist\\/(\\S*)[\\?].*$'
playlist_matches <- str_match(survey_data$playlistID, pattern2)
playlist_matches1 <- playlist_matches[,2]
playlist_matches2 <- playlist_matches[,3]
playlist_matches3 <- playlist_matches[,4]

playlist_id <- c()

for (i in seq_along(playlist_matches1)){
  if(!is.na(playlist_matches1[i])){
    playlist_id <- c(playlist_id, playlist_matches1[i])
  }
  else if(!is.na(playlist_matches2[i])){
    playlist_id <- c(playlist_id, playlist_matches2[i])
  }
  else if(!is.na(playlist_matches3[i])){
    playlist_id <- c(playlist_id, playlist_matches3[i])
  }
  else{
    playlist_id <- c(playlist_id, NA)
  }
}

survey_data$cleanPlaylist <- playlist_id

# cleaning up city
for (i in seq_along(survey_data$location2017)){
  if(!is.na(survey_data$location2017[i])){
    if(survey_data$location2017[i] == 'bangalore, India'){
      survey_data$location2017[i] = 'Bangalore, India'
    }
    if(survey_data$location2017[i] == 'NYC' || survey_data$location2017[i] == 'NYC, USA'){
      survey_data$location2017[i] = 'New York, USA'
    }
    if(survey_data$location2017[i] == 'Boston' || survey_data$location2017[i] == 'Boston,USA'){
      survey_data$location2017[i] = 'Boston, USA'
    }
  }
}

# cleaning up undergrad
for (i in seq_along(survey_data$undergrad)){
  if(!is.na(survey_data$undergrad[i])){
    if(survey_data$undergrad[i] == 'Itam' || survey_data$undergrad[i] == 'Instituto Tecnologico Autonomo de Mexico'){
      survey_data$undergrad[i] = 'ITAM'
    }
    if(survey_data$undergrad[i] == 'Cornell'){
      survey_data$undergrad[i] = 'Cornell University'
    }
    if(survey_data$undergrad[i] == 'Vellore institute of technology' || survey_data$undergrad[i] == 'Vellore Institute of Technology, India'){
      survey_data$undergrad[i] = 'Vellore Institute of Technology'
    }
  }
}

# creating an empty data frame where everyone's playlist information will be stored
class_playlists = data.frame()

for (i in seq_along(survey_data$cleanUser)){
  if(!is.na(survey_data$cleanUser[i]) && !is.na(survey_data$cleanPlaylist)){
    # print(survey_data$cleanUse[i])
    temp <- get_playlist_audio_features(survey_data$cleanUser[i], survey_data$cleanPlaylist[i])
    temp$user_id <- survey_data$cleanUser[i]
    temp$location2017 <- survey_data$location2017[i]
    temp$nationality <- survey_data$nationality[i]
    temp$undergrad <- survey_data$undergrad[i]
    temp$age <- survey_data$age[i]
    temp$useFrequency <- survey_data$useFrequency[i]
    class_playlists <- rbind(class_playlists, temp)
  }
}

lea <- get_playlist_audio_features('leac750', '37i9dQZF1E9UKwJS1D9012')
danielle <- get_playlist_audio_features('danielle219', '37i9dQZF1E9Lke9WJ01eT7')
hammaad <- get_playlist_audio_features('1256140081', '37i9dQZF1E9ECwVUJQFrgP')
kelly <- get_playlist_audio_features('22ugjl3xvvzgwtbwi3ubpt72a', '37i9dQZF1E9VRPVezelVUk')

lea$undergrad <- 'McGill University'
hammaad$undergrad <- 'Yale University'
kelly$undergrad <- 'University of Rochester'
danielle$undergrad <- 'Duke University'

lea$location2017 <- 'Montreal, CA'
hammaad$location2017 <- 'Boston, USA'
kelly$location2017 <- 'New York, USA'
danielle$location2017 <- 'New York, USA'

lea$nationality <- 'France'
hammaad$nationality <- 'India'
kelly$nationality <- 'China'
danielle$nationality <- 'United States'

lea$age <- 22
hammaad$age <- 24
kelly$age <- 24
danielle$age <- 24

lea$useFrequency <- 3
hammaad$useFrequency <- 5
kelly$useFrequency <- 3
danielle$useFrequency <- 5

lea$user_id <- 'lea'
hammaad$user_id <- 'hammaad'
kelly$user_id <- 'kelly'
danielle$user_id <- 'danielle'

class_playlists <- rbind(class_playlists, lea)
class_playlists <- rbind(class_playlists, hammaad)
class_playlists <- rbind(class_playlists, kelly)
class_playlists <- rbind(class_playlists, danielle)

# writing all music data to csv
# write.csv(class_playlists, file = "ClassMusicData.csv")
