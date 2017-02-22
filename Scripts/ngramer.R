ngramer <- function(tokens,n)
{
#Make n-grams from output of the tokenize.R script
  n_grams <- list()
  i <- 1
  while( i<= length(tokens))
  {
    n_grams[[i]] <- array()
    j <- 1
    while(j <= (length(tokens[[i]])-(n-1) ))
    {
      n_grams [[i]][j]<- paste(tokens[[i]][j:(j+(n-1))],collapse = " ")
      j <- j+1
    } 
    i <- i+1
  }
  
  n_grams
}