# User prompt-based decision tree for bathymetric image geomorphic classification scheme

#' Classify images to database
#'
#' @description Prompt user with options for bathymetric image classification
#'
#' @param db NULL or a suitably formatted database. If NULL, it will fetch the most recently time-stamped file in the database directory.
#' @param database_directory Folder containing databse file
#' @param photo_directory NULL or directory where photos are stored. Not currently used for anything.
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_sort
#'
#' @export
#'
classify_images <- function(db = NULL,
                            database_directory,
                            photo_directory = NULL){
  ## Test vars (remove when not testing internally)
  # db = NULL
  # database_directory = database_dir
  # photo_directory = photo_dir
  ## INITIAL DATABASE LOADING
  if(is.null(db)){
    # Find most recent file in database directory and load it in
    db_list <- list.files(database_dir, full.names = T) %>% str_sort(., decreasing = T)
    this_file <- db_list[1]
    db_top <- read.csv(file = this_file)
  } else {
    db_top = db
  }
  ## Filename preparation for in-loop writing
  current_fname <- unlist(lapply(strsplit(trim_path(this_file),"[.]"),"[[",1))
  #
  raw_fname <- unlist(lapply(strsplit(trim_path(current_fname),"[-]"),"[[",1))
  current_TS <- format(Sys.time(),"%Y-%m-%d-%H-%M-%S")
  new_fname <- paste0(raw_fname,"-",current_TS,'.csv')
  ## Which rows are to be done?
  rows_to_do <- which(is.na(db_top$complete))
  images_to_do <- db_top$SourceFile[rows_to_do]
  ## Question_strings
  terrain <- c("Flat","Sloped","Cliff","Break")
  talus <- c("Yes","No")
  substrate <- c("Consolidated","Unconsolidated")
  # Tree 1, consolidated
  structure_t1 <- c("Stratified","Blocky")
  faulting <- c("Yes","No")
  # Tree 2, unconsolidated
  structure_t2 <- c("Boulder","Cobble","Gravel","Sand","Silt")
  structure_t2b <- c(NA,"Boulder","Cobble","Gravel","Sand","Silt")
  erosion <- c("Scour/Scarp","Creep","None")
  bedforms <- c("Yes","No")
  bioturbation <- c("Yes","No")
  ## START LOOP HERE
  it_list <- vector('list', length = length(images_to_do))
  this_db <- db_top
  for(i in seq_along(it_list)){
    # i = 1
    # Initiate relevant vars for this loop.
    this_row <- this_db[rows_to_do[i],]
    this_image <- images_to_do[i]
    message("Decision tree for: ", this_image)
    ## classifying
    # Question 1; validity
    Q1_validity <- menu(title = "Is this image useable?", c("Yes","No")) %>% as.numeric()
    # If 1 (yes), proceed. If 2 (no), end this iteration.
    if(Q1_validity == 1){
      # Update db
      this_row$useable = Q1_validity
      ## QUESTION 2: Terrain
      Q2_terrain <- menu(title = "What is the terrain?", terrain) %>% as.numeric()
      this_row$Terrain <- terrain[Q2_terrain]
      ## QUESTION 3: TALUS DEPOSIT
      Q3_talus <- menu(title = "Is there a talus deposit?", talus) %>% as.numeric()
      this_row$Talus <- talus[Q3_talus]
      ## QUESTION 4: BEDROCK CONSOLIDATION
      Q4_substrate <- menu(title = "Is the bedrock substrate consolidated or unconsolidated?", substrate) %>% as.numeric()
      if(Q4_substrate == 1){
        # Consolidated; tree 1
        this_row$Substrate <- substrate[Q4_substrate]
        ## QUESTION 5, TREE 1: STRUCTURE
        Q5_t1 <- menu(title = "What is the structure?", structure_t1) %>% as.numeric()
        this_row$Structure_primary <- structure_t1[Q5_t1]
        ## QUESTION 6, TREE 1: FAULTING
        Q6_t1 <- menu(title = "Is there faulting?", faulting) %>% as.numeric()
        this_row$Faulting <- faulting[Q6_t1]
        ## TREE 1 FINITO
      } else {
        # Unconsolidated; tree 2
        this_row$Substrate <- substrate[Q4_substrate]
        ## QUESTION 5, TREE 2: STRUCTURE (PRIMARY)
        Q5_t2 <- menu(title = "What is the primary structure?",
                      structure_t2) %>% as.numeric()
        this_row$Structure_primary <- structure_t2[Q5_t2]
        ## QUESTION 6, TREE 2: STRUCTURE (SECONDARY)
        Q6_t2b <- menu(title = "What is the secondary structure?",
                       structure_t2b) %>% as.numeric()
        this_row$Structure_secondary <- structure_t2b[Q6_t2b]
        ## QUESTION 7, TREE 2: EROSION
        Q7_t2 <- menu(title = "What sort of erosion is there?",
                      erosion) %>% as.numeric()
        this_row$Erosion <- erosion[Q7_t2]
        ## QUESTION 8, TREE 2: BEDFORMS
        Q8_t2 <- menu(title = "Are there bedforms present?",
                      bedforms) %>% as.numeric()
        this_row$Bedforms <- bedforms[Q8_t2]
        ## QUESTION 9, TREE 2: BIOTURBATION
        Q9_t2 <- menu(title = "Is there bioturbation?",
                      bedforms) %>% as.numeric()
        this_row$Bioturbation <- bioturbation[Q9_t2]
      }
      # Other; both trees
      message("Is there any other information for this image? Write in the console and press enter:")
      other_resp <- readline()
      if(other_resp == ""){
        this_row$Other <- NA
      } else {
        this_row$Other <- other_resp
      }
      # Add completion
    } else {
      this_row$useable <- 0
    }
    # Update complete
    this_row$complete <- 1
    # add row to iterated DB
    this_db[rows_to_do[i],] <- this_row
    # Write
    write.csv(this_db,paste0(database_directory,new_fname), row.names = F)
    message("Image ",this_image," complete.")
  }
}
