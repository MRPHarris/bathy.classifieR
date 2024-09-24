# Non-exported functions

#' Trim path text from a filename
#'
#' @description Remove all text text prior to a filename from the supplied string. Operates by trimming all text before the last '/' in a string.
#'
#' @noRd
#'
trim_path <- function(filenames){
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    trimmed_filenames <- vector(mode = "character", length = length(filenames))
    for(f in seq_along(filenames)){
      filename_trimmed <- unlist(strsplit(filenames[f],"/"))[length(unlist(strsplit(filenames[f],"/")))]
      trimmed_filenames[f] <- filename_trimmed
    }
    trimmed_filenames
  } else if(length(filenames) == 1){
    filename <- filenames
    filename_trimmed <- unlist(strsplit(filename,"/"))[length(unlist(strsplit(filename,"/")))]
    filename_trimmed
  } else{
    message("Empty object; no path to trim. Filenames may be missing.")
  }
}
