#' Transpose to stylo-readable data frame
#' ======================================
#' 
my.transpose.to.stylo = function(df)
{
  matrix = as.matrix(df[2:11]) # 2:11 is where the numeric data is
  matrix = t(matrix)
  stylo.df = data.frame(matrix)
  colnames(stylo.df) = df$text
  return(stylo.df)
}
