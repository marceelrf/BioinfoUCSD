ClumpFinding <- function(Text,k,L,t){
  require(stringr)
  
  kmer <- NULL
  kmers <- NULL
  for(i in 1:((str_length(Text)-L+1))){
    contar <- 0
    Text2 <- str_sub(Text,start = i,end = i+L-1)
    #print(Text2)
    for(j in 1:(str_length(Text2)-k+1)){
      kmer <- str_sub(Text2,start = j,end = j+k-1)
      contar <- PatternCount(Text2,kmer)
      if(contar >= t){
        kmers <- append(kmers, kmer)
      }
      
    }
  }
  kmers <- kmers[!duplicated(kmers)]
  return(kmers)
}
