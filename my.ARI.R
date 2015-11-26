#' Automated Readability Index
#' ===========================
#' 
#' Computes the 'Automated Readibility Index' from measures
#' already computed from the text.
#' 
#' **Input**
#' - Text length in characters
#' - Text length in words
#' - Text length in sentences
my.ARI = function(characters, words, sentences)
{
  ARI = 4.71 * (characters / words) + 0.5 * (words / sentences) - 21.43
  return(ARI)
}
