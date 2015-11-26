#' LIX Readability measure
#' =======================
#' 
#' Calculates the LIX readability measure from values
#' already computed from the text.
#' 
#' **Input**
#' - Number of word in the text
#' - Number of periods (including ":" etc.)
#' - Number of words with > 6 letters
#' 
#' 
my.LIX = function(words, periods, longwords)
{
  LIX = (words / periods) + (100 * longwords / words)
  return(LIX)
}