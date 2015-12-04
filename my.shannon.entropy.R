#' Calculate Shannon entropy from a vector of propabilities
#' =======================================================
#' 
#' **Input**
#' - Vector of **probabilities**
#' 
#' **ToDo**
#' - Automated computation of propability vectors from frequency vectors
#' 
my.shannon.entropy = function(x, normalized = F)
{
  x = x[x != 0 & is.na(x) == F]
  if(normalized == F)
  {H = -sum(x * log(x))}
  if(normalized == T)
  {H = -sum( (x * log(x)) / log(length(x)) )}  
  return(H)
}