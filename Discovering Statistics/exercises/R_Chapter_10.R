for(i in 1:10){
  print(i)
}

# ---------------------------------------------

fruit <- c("apple", "banana", "pomegranate")
fruit.length <- rep(NA, length(fruit))
names(fruit.length) <- fruit

for(i in fruit){
  fruit.length[i] <- nchar(i)
}
fruit.length

# ---------------------------------------------

x <- 1
while(x <= 5){
  print(x)
  x <- x + 1
}

# ---------------------------------------------

for(i in 1:10){
  if(i ==3){
    next
  }
  print(i)
}

for(i in 1:10){
  if(i ==4){
    break
  }
  print(i)
}