# Image classification decision tree in R for AJ

# Last updated 2024-09-24
# Created by Matt Harris
# m.harris@gns.cri.nz
# https://www.github.com/MRPHarris

# Written and run in:
#   Rstudio v2024.04.2
#   R v4.4.0

# Prompts user with a decision tree containing options for broad
# classification of bathymetric images. Answers are written to a database.

# Database is saved as a new .csv (at the start of each function call),
# then iteratively overwritten as each image is classifed.

# Potential to add a bash script wrapper that opens target image files
# in an image viewer software of some sort.

##### SCRIPT SETUP (ALWAYS RUN) #####

# Load packages required by this script.
# install.packages("pacman")

## Load required packages
library(pacman)
pacman::p_load(tidyverse,dplyr,magrittr,here)

# All folders are specified relative to the project directory
proj_dir <- paste0(here(),"/")

# Photos and database files are stored within the data folder
data_dir <- paste0(proj_dir,"data/")
photo_dir <- paste0(data_dir,"photos/")
database_dir <- paste0(data_dir,"database/")

# Functions are stored within the function dir for cleanliness
fn_dir <- paste0(proj_dir,"R/")

##### Functions #####

## Get functions from package if using for package testing.
fn_list <- list.files(fn_dir,full.names = T) %>% as.list()
invisible(lapply(fn_list, source, verbose = F))
rm(fn_list)

##### Decision Tree #####

classify_images(
  db = NULL,
  database_directory = database_dir,
  photo_directory = photo_dir
)

##### MISC/OLD CODE #####

# Files
# - list of all files
# - evolving list or previous of processed data

# Take inventory

# List all photos in photo dir
photo_list <- list.files(photo_dir, full.names = T)

# List all databases
db_list <- list.files(database_dir, full.names = T) %>% str_sort(., decreasing = T)
this_file <- db_list[1]

# Open most recent database
this_db <- read.csv(file = this_file)

# Get the name of the image to be analysed next
images_in_db <- this_db$SourceFile
images_in_photodir_long <- list.files(photo_dir, full.names = T)
images_in_photodir_short <- list.files(photo_dir, full.names = F)

# Test for matches
images_in_db %in% images_in_photodir_short

# Isolate images that we have
images_for_analysis <- images_in_photodir_long[images_in_db %in% images_in_photodir_short]

