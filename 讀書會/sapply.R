data(iris)
kkk = {}
kkk = sapply(1:150,function(x){
  if (iris$Sepal.Width[x]>2.5){
    kkk[x] = as.character(iris$Species[x])
  }  
  else{
    kkk[x] = "fuck"
  }
})
