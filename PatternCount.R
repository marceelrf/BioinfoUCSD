PatternCount <- function(Text, Pattern){
  require(stringr,quietly = TRUE)
  contar <- 0
  for(i in 1:(str_length(Text)-str_length(Pattern)+1)){
    if(str_sub(Text,start = i,end = i+str_length(Pattern)-1) == Pattern){
      contar <- contar +1
    }
  }
  return(contar)
}
