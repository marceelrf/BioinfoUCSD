FrequentWords <- function(Text,k){
  require(stringr,quietly = TRUE)
  FrequentPatterns <- NULL
  contar <- NULL
  for(i in 1:(str_length(Text)-k)){
    Pattern <- str_sub(Text,start = i,end = i+k-1)
    contar[i] <- PatternCount(Text,Pattern)
  }
  
  maxContar <- max(contar)
  
  for(i in 1:(str_length(Text)-k)){
    if(contar[i] == maxContar){
      FrequentPatterns <- append(FrequentPatterns,str_sub(Text,start = i,end = i+k-1))
    }
  }
  FrequentPatterns <- FrequentPatterns[!duplicated(FrequentPatterns)]
  #Falta remover as duplicatas
  return(FrequentPatterns)
  #return(contar)
}
