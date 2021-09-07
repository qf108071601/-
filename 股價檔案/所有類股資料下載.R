source(file = "C:/Users/Lai/Desktop/ªÑ»ùÀÉ®×/getstock.R")

seq= c(1:18,20:31) %>% as.character() 
seq[1:9] = paste0("0",seq[1:9])

type1 = str_sub(seq,start = 1,end = 1)
type2 = str_sub(seq,start = 2,end = 2)

d = Sys.Date() %>% gsub("(-)","",.) %>% as.numeric()-1


I1 = lapply(1:15, function(a){
  getstock(date = d,type1 = type1[a],type2 = type2[a]) %>% as.data.frame()
})

I2 = lapply(16:30, function(a){
  getstock(date = d,type1 = type1[a],type2 = type2[a]) %>% as.data.frame()
})

I = c(I1,I2)
