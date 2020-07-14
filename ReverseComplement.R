ReverseComplement <- function(Text){
  require(stringr,quietly = T)
  strand <- NULL
  
  for(i in 1:(str_length(Text))){
    strand[i] <- ifelse(str_sub(Text,start = i,end = i) == "A",
                        yes = "T",
                        no = ifelse(str_sub(Text,start = i,end = i) == "T",
                                    yes = "A",
                                    no = ifelse(str_sub(Text,start = i,end = i) == "G",
                                                yes = "C",
                                                no = "G")))
  }
  strand <- rev(strand)
  strand <- str_c(strand,collapse = "")
  return(strand)
}
