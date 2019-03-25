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
