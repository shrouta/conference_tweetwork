# Classifies ACIS Tweeters
classify_ACIS_tweeters <- function(connections_df) {
  # Capture users from tweets database
  ACIS_users <- cbind(as.list(connections_df$person_tweeting), as.list(connections_df$person_mentioned))
  ACIS_users <- unique(ACIS_users)
  ACIS_profiles <- twListToDF(lookupUsers(ACIS_users, includeNA = FALSE))
  
  # Strip to useful metadata
  useful_ACIS_profile_metadata <-  c("screenName", "name", "description")
  ACIS_profiles <- ACIS_profiles[useful_ACIS_profile_metadata]
  row.names(ACIS_profiles) <- NULL
  
  # For loop that checks tweeter's discipline (adapt this as necessary)
  art_history_keywords <- c("art", "history", "historian")
  history_keywords <- c("history", "historian", "twitterstorian", "hist")
  irish_language_keywords <- c("ghaeilge", "gaeilge", "ceoltóir", "gael", "irish")
  irish_studies_keywords <- c("irish", "studies")
  irish_studies_orgs <- c("bais", "irishmods", "conference")
  library_keywords <- c("library", "archive", "archives", "archival", "librarian", "archivist", "resources")
  lit_keywords <- c("english", "literature", "lit", "poetry", "poetics", "poetic", "englishatul", "modernist", "modernism", "uccenglish")
  music_keywords <- c("soundscape", "soundscapes", "music", "ethnomusicologist")
  press_keywords <- c("press", "editor", "publisher")
  theatre_keywords <- c("theatre", "theater", "performance")
  institution_keywords <- c("framingham", "heritage", "ecrchat", "university", "universities")
  
  
  for(profile_ndx in 1:nrow(ACIS_profiles)) {
    profile_text <- tolower(ACIS_profiles$description[profile_ndx])
    profile_text <- gsub("[[:punct:]]", "", profile_text)
    profile_text <- gsub('[0-9]+', '', profile_text)
    profile_text <- strsplit(as.character(profile_text), " ")[[1]]
    if (all(irish_studies_keywords %in% profile_text) == TRUE) {
      ACIS_profiles$field[profile_ndx] <- "Irish_Studies"
    }
    else if (TRUE %in% (irish_studies_orgs %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Irish_Studies"
    }
    else if (all(c("art", "history") %in% profile_text) == TRUE) {
      ACIS_profiles$field[profile_ndx] <- "Art_History"
    }
    else if (all(c("art", "historian") %in% profile_text) == TRUE) {
      ACIS_profiles$field[profile_ndx] <- "Art_History"
    }
    else if (TRUE %in% (lit_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Literature"
    }
    else if (TRUE %in% (history_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "History"
    }
    else if (TRUE %in% (library_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Library"
    }
    else if (TRUE %in% (theatre_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Theatre"
    }
    else if (TRUE %in% (music_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Music"
    }
    else if (TRUE %in% (irish_language_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Gaeilge"
    }
    else if (TRUE %in% (press_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Press"
    }
    else if (TRUE %in% (institution_keywords %in% profile_text)) {
      ACIS_profiles$field[profile_ndx] <- "Institution"
    }  
    else {
      ACIS_profiles$field[profile_ndx] <- "Unknown"
    }
  }
  return(ACIS_profiles)
}

visualize_ACIS_by_field <- function(graph_object, conference_profiles, file_name, graph_title, sub_title) {
  # Converts profile info to factors
  conference_profiles$field <- as.factor(conference_profiles$field)
  
  V(graph_object)$field = as.character(conference_profiles$field[match(V(graph_object)$name, conference_profiles$screenName)])
  
  V(graph_object)$color <- V(graph_object)$field
  V(graph_object)$color <- gsub("Press", "#fdb462", V(graph_object)$color)
  V(graph_object)$color <- gsub("Art_History", "#ccebc5", V(graph_object)$color)
  V(graph_object)$color <- gsub("Gaeilge", "#80b1d3", V(graph_object)$color)
  V(graph_object)$color <- gsub("History", "#ffffb3", V(graph_object)$color)
  V(graph_object)$color <- gsub("Institution", "#bc80bd", V(graph_object)$color)
  V(graph_object)$color <- gsub("Irish_Studies", "#bebada", V(graph_object)$color)
  V(graph_object)$color <- gsub("Library", "#fb8072", V(graph_object)$color)
  V(graph_object)$color <- gsub("Literature", "#8dd3c7", V(graph_object)$color)
  V(graph_object)$color <- gsub("Music", "#fccde5", V(graph_object)$color)
  V(graph_object)$color <- gsub("Theatre", "#b3de69", V(graph_object)$color)
  V(graph_object)$color <- gsub("Unknown", "#888888", V(graph_object)$color)
  
  png(file=file_name, width=2500, height=2500, res=150)
  plot(graph_object, rescale = T, layout=layout.fruchterman.reingold(graph_object), 
       edge.width=1,
       vertex.label=NA,
       vertex.size = 4,
       vertex.label.cex = .25,
       asp = .8,
       main = graph_title,
       sub = sub_title
  )
  legend("topleft", legend = c("Academic Press", "Art History", "Gaeilge", "History", "Institution (college, university, etc)", "Irish Studies", "Libraries and Archives", "Literature", "Music", "Theatre", "Unknown"), col = c("#fdb462", "#ccebc5", "#80b1d3", "#ffffb3", "#bc80bd", "#bebada", "#fb8072", "#8dd3c7", "#fccde5", "#b3de69", "#888888"), pch = 15, cex = 1.5, title = "Discipline (derived from Twitter bio)"
  )
  dev.off()
}