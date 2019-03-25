# Specify hashtags and dates. Dates must be a vector > 1 of dates in "YYYY-MM-DD" format.  Hashtags much be a character variable, with multiple hashtags seperated by +
ACIS_2019_dates <- c("2019-03-19", "2019-03-20", "2019-03-21", "2019-03-22", "2019-03-23", "2019-03-24")
ACIS_2019_hashtags <- "#ACIS2019 + acis2019"

# Sample workflow for calling tweets, analyzing connections, calling and classifying twitter profiles, and graphing the conference twitter network.

# Call tweets
ACIS_2019_tweets <- get_conference_tweets(ACIS_2019_dates, ACIS_2019_hashtags)

# Make connections
ACIS_2019_connections <- get_conference_connections(ACIS_2019_tweets)

# Get and classify profiles (for ACIS2019 only)
ACIS_2019_profiles <- classify_ACIS_tweeters(ACIS_2019_connections)

# Create graph network objects, dropping the conference twitter handle
ACIS_2019_graph_color <- build_ACIS_network(ACIS_2019_profiles, ACIS_2019_connections, "acisboston")
ACIS_2019_graph_scaled <- build_conference_network(ACIS_2019_connections, "acisboston")

# Visualize graphs
visualize_ACIS_by_field(ACIS_2019_graph_color, ACIS_2019_profiles, "ACIS_2019_by_color.png", "#ACIS2019 Twitter Connections (retweets, replies and mentions)", "Each node represents a twitter user, colored by discipline (automatically generated from twitter bio)")
visualize_conf_scaled(ACIS_2019_graph_scaled, "ACIS_2019_scaled.png", "#ACIS2019 Twitter Connections (retweets, replies and mentions)", "Each node represents a twitter user, scaled for number of tweets on the conference hashtag")
