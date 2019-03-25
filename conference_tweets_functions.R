# Given a range of dates (must be a vector), and at least one hashtag (as a character), returns a dataframe containing all unique tweets
get_conference_tweets <- function(dates, hashtags) {
  conf_tweets <- as.data.frame(matrix(ncol = 16, nrow = 0))
  names(conf_tweets) <- c("text", "favorited", "favoriteCount", "replyToSN", "created", "truncated", "replyToSID", "id", "replyToUID", "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted", "longitude", "latitude")
  total_loops <- length(dates)-1
  for(date_ndx in 1:total_loops) {
    tw <- searchTwitter(hashtags, n = 174, since = dates[date_ndx], until = dates[date_ndx + 1])
    tw_df <- twListToDF(tw)
    conf_tweets <- rbind(conf_tweets, tw_df)
  }
  conf_tweets <- conf_tweets[which(is.na(conf_tweets$text) == FALSE), ]
  conf_tweets <- unique(conf_tweets)
  # Return
  return(conf_tweets)
}

# Get Connections
get_conference_connections <- function(conference_tweets_df) {
  individual_connection <- as.data.frame(matrix(ncol = 3, nrow = 1))
  names(individual_connection) <- c("person_tweeting", "person_mentioned", "tweet_ID")
  
  connections <- as.data.frame(matrix(ncol = 3, nrow = 0))
  names(connections) <- c("person_tweeting", "person_mentioned", "tweet_ID")
  
  for (tweet_ndx in 1:nrow(conference_tweets_df)) {
    tweet <- conference_tweets_df$text[tweet_ndx]
    tweet_words <- strsplit(tweet, " ")[[1]]
    all_mentions_boolean <- grepl("^@", tweet_words)
    all_mentions <- tweet_words[all_mentions_boolean]
    if (length(all_mentions) != 0) {
      for (mention_ndx in 1:length(all_mentions)) {
        mention <- all_mentions[mention_ndx]
        mention <- gsub("^@", "", mention)
        if(grepl("[[:punct:]]$", mention)) {
          mention <- gsub("[[:punct:]]$", "", mention)
        }
        if(grepl("[[:punct:]]s$", mention)) {
          mention <- gsub("[[:punct:]]s$", "", mention)
        }
        individual_connection$person_tweeting[1] <- ACIS_2019_tweets$screenName[tweet_ndx]
        individual_connection$person_mentioned[1] <- mention 
        individual_connection$tweet_ID[1] <- ACIS_2019_tweets$id[tweet_ndx]
        connections <- rbind(connections, individual_connection)
      } 
    }
    else {
      individual_connection$person_tweeting[1] <- ACIS_2019_tweets$screenName[tweet_ndx]
      individual_connection$person_mentioned[1] <- NA
      individual_connection$tweet_ID[1] <- ACIS_2019_tweets$id[tweet_ndx]
      connections <- rbind(connections, individual_connection)
    }
  }
  # Drop all tweets for which there is no connection
  connections <- connections[which(is.na(connections$person_mentioned) == FALSE), ]
  return(connections)
}

# This function requires a dataframe which contains usernames and fields, and a dataframe which contains tweets that feature mentions, retweets or replies.  It can also include an account to drop (i.e. the conference twitter account) as a string
build_conference_network <- function(conference_profiles, conference_connections, account_to_drop) {
  # Call in twitter connections data and merge connections and profiles
  conf_edge_list <- merge(conference_connections, conference_profiles, by.x = "person_mentioned", by.y = "screenName", all.x = TRUE)
  useful_EL_metadata_1 <- c("person_tweeting", "person_mentioned", "field", "tweet_ID")
  conf_edge_list <- conf_edge_list[useful_EL_metadata_1]
  names(conf_edge_list) <- c("person_tweeting", "person_mentioned", "mentioned_field", "tweet_ID")
  
  conf_edge_list <- merge(conf_edge_list, conference_profiles, by.x = "person_tweeting", by.y = "screenName", all.x = TRUE)
  useful_EL_metadata_2 <- c("person_tweeting", "field", "person_mentioned", "mentioned_field", "tweet_ID")
  conf_edge_list <- conf_edge_list[useful_EL_metadata_2]
  names(conf_edge_list) <- c("person_tweeting", "tweeting_field", "person_mentioned", "mentioned_field", "tweet_ID")
  
  # Drops conference hashtag, if necessary
  if(account_to_drop != "") {
    conf_edge_list <- conf_edge_list[which(conf_edge_list$person_tweeting != account_to_drop), ]
    conf_edge_list <- conf_edge_list[which(conf_edge_list$person_mentioned != account_to_drop), ]
  }
  
  # Limits edge list to tweeter and mentioned
  sparse_metadata <-c("person_tweeting", "person_mentioned") 
  sparse_conf_edge_list <- conf_edge_list[sparse_metadata]
  
  # Creates an adjacency matrix
  conf_graph_df <- graph.data.frame(sparse_conf_edge_list)
  conf_adjacency <- get.adjacency(conf_graph_df, sparse = FALSE)
  diag(conf_adjacency) <- NA
  
  # Creates a graph adjacency object
  conf_graph <- graph.adjacency(conf_adjacency, mode = "undirected", weighted = NULL, diag = FALSE)
  return(conf_graph)
}

visualize_conf_scaled <- function(graph_object, file_name, graph_title, sub_title) {
  # Scale this differently
  V(graph_object)$size=(degree(graph_object) - min(degree(graph_object)))/(max(degree(graph_object))-min(degree(graph_object)))*10
  
  png(file=file_name, width=2500, height=2500, res=300)
  plot(graph_object, layout=layout_with_graphopt(graph_object, charge = 0.0001, mass = 60, spring.length = 10, spring.constant = 5), 
       edge.width=.25,
       vertex.color = "#999999",
       vertex.frame.color = "#999999",
       vertex.label=V(graph_object)$name,
       vertex.label.cex = .3,
       asp = 1,
       main = graph_title,
       sub = sub_title
  )
  dev.off()
}