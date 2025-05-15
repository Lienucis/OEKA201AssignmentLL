# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

cword <- function(file_path) {
  # Read the content of the file
  text <- tolower(readLines(file_path, warn = FALSE))
  
  # Split the text into words
  words <- unlist(strsplit(text, "\\s+"))
  
  # Count the number of words
  num_words <- length(words)
  
  return(num_words)
}


