#' Trim whitespaces from a string
#'
#' This function removes whitespaces from the beginning and end of a string
#'
#' @param x The string to be stripped
#' @return A string without whitespaces at the beginning and end
trim = function(x) {
    gsub("^\\s+|\\s+$", "", x)
}

#' Helper function that loads a specific file and returns the variables therein
#'
#' @param filename The path to the RData file
#' @return Returns the content of the RData file as data frame
load.RData = function(filename){
    load(filename)
    data.frame(get(ls()[ls() != "filename"]))
}
