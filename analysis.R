rm(list=ls())

library(tidyverse)
library(spotifyr)
library(genius)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(textdata)
library(viridis)

##https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/


id <- '84b02976826b416e97f4f09c29fc0d22'
secret <- 'c71f59b2f660411195f882664dc9d068'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token(client_id=id,
                                         client_secret=secret)

df_orig <- get_artist_audio_features('twenty one pilots')
df <- df_orig

artist_albums <- tribble(
  ~artist, ~album,
  "Twenty One Pilots", "Vessel",  
  "Twenty One Pilots", "Trench",
  "Twenty One Pilots", "Blurryface",
  "Twenty One Pilots", "Twenty One Pilots"
)


artist_albums_orig <- artist_albums %>%
  add_genius(artist, album)


artist_albums <- artist_albums_orig %>% 
  group_by(track_title) %>% 
  mutate(lyric = paste(lyric, collapse = " ")) %>% 
  distinct(track_title, .keep_all = TRUE) %>% 
  left_join(df, by = c("track_title" = "track_name")) %>% 
  distinct(track_title, .keep_all = TRUE) %>% 
  select(-line)



tidy_lyric1 <- artist_albums %>% 
  unnest_tokens(word, lyric) 

tidy_lyric2 <- tidy_lyric1 %>% 
  anti_join(rbind(rbind(stop_words[1]), "da", "eh", "yeah", "ooh", "na", "bah", 'la', "nah", "bou", "uh", "dema", "ow", "em"), by = "word")

tidy_lyric2$word[tidy_lyric2$word == "dema"] <- "demons"



## gets counts of words
word_counts <- tidy_lyric2 %>%
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

wordcloud(words = word_counts$word, freq = word_counts$n,
          rot.per=0.35, colors=brewer.pal(8, "Dark2"),
          min.freq = 5, max.words=200, random.order=FALSE)


## total number of unique words in each song
tidy_lyric2 %>% 
  group_by(track_title, album) %>% 
  summarise(n_unique_words = n_distinct(word)) %>% 
  arrange(desc(n_unique_words))




## density plot of valence for each album (can copy this for other parameters)
df %>%
  filter(album_name == c("Vessel", "Trench", "Blurryface", "Twenty One Pilots")) %>%
  ggplot(aes(x = valence, fill = "pink")) +
  geom_density(scale = 0.9) +
  facet_grid(album_name~.)



## bottom 5 songs, lowest valence score (can copy this for other parameters)
df %>%
  select(track_name, valence) %>%
  distinct(track_name, .keep_all = TRUE) %>%
  arrange(desc(valence)) %>%
  top_n(-5)








### this is a good one, make the plot prettier; valence and tempo
df %>%
  filter(album_name == c("Vessel", "Trench", "Blurryface", "Twenty One Pilots")) %>%
  arrange(album_release_year) %>% 
  ggplot() +
  geom_density(aes(x = valence*200), fill = "red", alpha = 0.5) +
  geom_density(aes(x = tempo), fill = "yellow", alpha = 0.5) +
  scale_x_continuous(limits=c(0, 200), 
                     sec.axis = sec_axis(~ . *1/200, name = "valence")) +
  facet_grid(album_name~.) +
  scale_fill_discrete(labels = c("valence", "tempo")) +
  theme_light()




# #### Word sentiment ----------
# word_sentiment <- get_sentiments("nrc")
# 

# negative <- word_sentiment %>% 
#   filter(sentiment == "negative") %>% 
#   mutate(neg = T)
# 
# positive <- word_sentiment %>% 
#   filter(sentiment == "positive") %>% 
#   mutate(neg = F)
# 
# neg_pos <- negative %>% 
#   bind_rows(positive)
# 
# tidy_lyric_sentiment <- tidy_lyric2 %>% 
#   left_join(neg_pos, by = "word") %>% 
#   drop_na(neg)




