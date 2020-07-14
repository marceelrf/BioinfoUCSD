PatternMatching <- function(Pattern, Genome, toPy = c(TRUE,FALSE)){
  require(stringr,quietly = T)
  
  position <- NULL
  fp <- str_length(Genome)-str_length(Pattern)+1
  for(i in 1:(fp)){
    if(str_sub(Genome,start = i,end = i+str_length(Pattern)-1) == Pattern){
      
      position <- append(position, i)
    }
  }
  
  if(toPy == TRUE){
    position <- position - 1
  }
  return(position)
}
