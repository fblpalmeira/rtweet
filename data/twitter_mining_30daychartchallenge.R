# load library rtweet
library(rtweet)

# get timelines for rihanna and katyperry
tweets <- get_timeline(c("30DayChartChall"),
                       n = 3200)

# load tweets from data file
tweets <- readRDS("tweets.rds")

###### Inspect Tweets ########
# load tidyverse library
library(tidyverse)

# inspect data frame
summary(tweets)
glimpse(tweets)

# count tweets per screen_name
tweets %>% 
  count(screen_name)

# when was the first tweet created and the last tweet created
tweets %>%
  group_by(screen_name) %>%
  summarise(begin = min(created_at),
            end = max(created_at))

#View(tweets)

# create new clean data frame with just original tweets
original_tweets <- tweets %>%
  filter(!is_retweet)

#View(original_tweets)

# create a selected dataframe
original_tweets_selected <- original_tweets %>%
  select(screen_name, text, source,
         favorite_count, retweet_count)

#View(original_tweets_selected)

# load stringi library
library(stringi)

# extract non-ascii characters from texts (i.e., actual tweets)
original_tweets_selected$code <- stri_match_all(original_tweets_selected$text,
                                                regex = '[^[:ascii:]]')

# unnest code list
tweets_unnest <- original_tweets_selected %>%
  unnest(code)

#View(tweets_unnest)

# look the rtweet emojis dataframe
emojis

# combine our tweest_unnest and emojis data frames
tweets_unnest <- left_join(tweets_unnest,
                           emojis)

write_tsv(emojis, "emojis.tsv")
emojis <- read_tsv("emojis.tsv")

#View(emojis)

# clean tweets_unnest by removing description that is NA
clean_emoji_tweets <- tweets_unnest %>%
  filter(!is.na(description))

#View(clean_emoji_tweets)

############ COUNT EMOJIS ############
clean_emoji_tweets %>%
  count(screen_name, code, description) %>%
  arrange(-n) %>%
  View()

# count emojis per tweet
clean_emoji_tweets %>%
  count(screen_name, text) %>%
  arrange(-n)

# top 5 emojis per screen_name
clean_emoji_tweets %>%
  count(screen_name, code, description) %>%
  arrange(-n) %>%
  group_by(screen_name) %>%
  top_n(5)

################# COLLOCATIONS ####################
library(tidytext)

tokenized_tweets <- clean_emoji_tweets %>%
  unnest_tokens(word, text)

View(tokenized_tweets)

tokenized_tweets %>%
  count(screen_name, code, word) %>%
  arrange(-n)

# clean word column
to_remove <- c("https", "t.co", "amp",
               stopwords::stopwords())

library(stopwords)
#install.packages("stopwords")

tokenized_tweets$word %in% to_remove

`%notin%` <- Negate(`%in%`)

tokenized_tweets_clean <- tokenized_tweets %>%
  filter(word %notin% to_remove)

# count emoji and word per screen_name
tokenized_tweets_clean %>%
  count(screen_name, code, word) %>%
  arrange(-n)

png(file = "30DayChartChall.png", width = 800, height = 700)

# plot the emoji word count per screen_name
tokenized_tweets_clean %>%
  count(screen_name, code, description, word) %>%
  arrange(-n) %>%
  group_by(screen_name) %>%
  top_n(20) %>%
  ggplot(aes(x = word, y = n)) +
  geom_label(aes(label = description)) +
  coord_flip() +
  facet_wrap(~screen_name, scales = "free")

dev.off()

##########################

library(magick)
library(magrittr) 

# Call back the plot
plot <- image_read("30DayChartChall.png")
plot2<-image_annotate(plot, "Visualization by @fblpalmeira
                      Data: @30DayChartChall | Image credit: @30DayChartChall", 
                      color = "gray", size = 12, 
                      location = "10+50", gravity = "southeast")
# And bring in a logo
i1 <- image_read("https://pngset.com/images/double-exclamation-mark-emoji-for-facebook-email-u0026-sms-id-emoji-two-exclamation-marks-logo-symbol-trademark-text-transparent-png-2720386.png") 
out1<-image_composite(plot2,image_scale(i1,"x40"), offset = "+530+30")

i2 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png")
out2<-image_composite(out1,image_scale(i2,"x40"), offset = "+230+70")

i3 <- image_read("https://pngset.com/images/double-exclamation-mark-emoji-for-facebook-email-u0026-sms-id-emoji-two-exclamation-marks-logo-symbol-trademark-text-transparent-png-2720386.png") 
out3<-image_composite(out2,image_scale(i3,"x40"), offset = "+220+115")

i4 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png")
out4<-image_composite(out3,image_scale(i4,"x40"), offset = "+230+160")

i5 <- image_read("https://images.emojiterra.com/google/android-11/512px/1f389.png") 
out5<-image_composite(out4,image_scale(i5,"x50"), offset = "+670+190")

i6 <- image_read("https://images.emojiterra.com/google/android-11/512px/1f389.png") 
out6<-image_composite(out5,image_scale(i6,"x50"), offset = "+230+230")

i7 <- image_read("https://images.emojiterra.com/google/android-nougat/512px/26a0.png") 
out7<-image_composite(out6,image_scale(i7,"x40"), offset = "+230+280")

i8 <- image_read("https://images.emojiterra.com/twitter/512px/1f38a.png")
out8<-image_composite(out7,image_scale(i8,"x40"), offset = "+230+325")

i9 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png")
out9<-image_composite(out8,image_scale(i9,"x40"), offset = "+535+325")

i10 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png")
out10<-image_composite(out9,image_scale(i10,"x40"), offset = "+230+365")

i11 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png") 
out11<-image_composite(out10,image_scale(i11,"x40"), offset = "+230+415")

i12 <- image_read("https://pngset.com/images/double-exclamation-mark-emoji-for-facebook-email-u0026-sms-id-emoji-two-exclamation-marks-logo-symbol-trademark-text-transparent-png-2720386.png") 
out12<-image_composite(out11,image_scale(i12,"x40"), offset = "+530+415")

i13 <- image_read("https://images.emojiterra.com/google/android-11/512px/1f389.png") 
out13<-image_composite(out12,image_scale(i13,"x50"), offset = "+220+455")

i14 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png")
out14<-image_composite(out13,image_scale(i14,"x40"), offset = "+230+500")

i15 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png") 
out15<-image_composite(out14,image_scale(i15,"x40"), offset = "+230+545")

i16 <- image_read("https://images.emojiterra.com/google/android-11/512px/1f389.png") 
out16<-image_composite(out15,image_scale(i16,"x50"), offset = "+530+570")

i17 <- image_read("https://images.emojiterra.com/google/android-10/512px/1f4e2.png")
out17<-image_composite(out16,image_scale(i17,"x40"), offset = "+230+620")

i18 <- image_read("https://pbs.twimg.com/profile_images/1488963523363356674/zvAtX9CG_400x400.jpg") 
out18<-image_composite(out17,image_scale(i18,"x120"), offset = "+650+30")

image_browse(out18)

# And overwrite the plot without a logo
image_write(out18, "30DayChartChall2.png")

citation(package="rtweet")

