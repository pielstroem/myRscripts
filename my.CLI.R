#' Coleman-Liau index of readibility
#' =================================
#' 
#' Calculates the Coleman-Liau Index of Readibility from values
#' already computed from the text.
#' 
#' **Input**
#' - L Average number of letters per 100 words
#' - S Average number of sentences per 100 words
#' 
my.CLI = function(L, S)
{
  CLI = 0.0588 * L - 0.296 * S - 15.8
  return(CLI)
}