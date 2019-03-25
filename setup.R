# Loads Libraries.
packages <- c("twitteR", "igraph")

# Checks for packages
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Specify keys
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

# Initialize twitter API
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Specify hashtags and dates. Dates must be a vector > 1 of dates in "YYYY-MM-DD" format.  Hashtags much be a character variable, with multiple hashtags seperated by +
ACIS_2019_dates <- c("2019-03-19", "2019-03-20", "2019-03-21", "2019-03-22", "2019-03-23", "2019-03-24")
ACIS_2019_hashtags <- "#ACIS2019 + acis2019"

ACIS_2019_tweets <- get_conference_tweets(ACIS_2019_dates, ACIS_2019_hashtags)
ACIS_2019_connections <- get_conference_connections(ACIS_2019_tweets)
ACIS_2019_profiles <- classify_ACIS_tweeters(ACIS_2019_connections)
ACIS_2019_graph <- build_conference_network(ACIS_2019_profiles, ACIS_2019_connections, "acisboston")
visualize_ACIS_by_field(ACIS_2019_graph, ACIS_2019_profiles, "ACIS_2019_by_color.png", "#ACIS2019 Twitter Connections (retweets, replies and mentions)", "Each node represents a twitter user, colored by discipline (automatically generated from twitter bio)")
visualize_conf_scaled(ACIS_2019_graph, "ACIS_2019_scaled.png", "#ACIS2019 Twitter Connections (retweets, replies and mentions)", "Each node represents a twitter user, scaled for number of tweets on the conference hashtag")
