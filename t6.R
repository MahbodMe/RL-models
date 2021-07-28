


test <- function(X){
  test3 <<- TRUE
  example <- matrix(FALSE, 3, 1)
  for (i in 1:3)
    example[i,1] <<- TRUE
  return (example)
}
result <- mclapply(1:4,test, mc.preschedule = FALSE, mc.cores = 2)

(as.matrix(unlist(result[1:4])))

test4 <- abind(as.matrix(unlist(result[1])), as.matrix(unlist(result[2])), 
               as.matrix(unlist(result[3])), as.matrix(unlist(result[4])), along = 3)
test <- array(NA, c(3,1,4))
for (i in 1:4){
  test[,,i] <- as.matrix(unlist(result[i])) 
}


test4 <- abind(as.matrix(unlist(result[test5])), along = 3)


test5 <- rep(1:4, 1)


test.env <- new.env()
assign("example", FALSE, envir = test.env)



test2 <- function(y){
  ann <- xy
  return(ann*y)
}

test <- function(X){
  y <- rep(NA, 8)
  for (i in 1:8){
    xy <<- 2
    y[i] <- test2(X)
  }
  return(y)
}
mclapply(1:4, test, mc.preschedule = FALSE, mc.cores = 2)
get("example", envir = test.env)


remove(y)
y <- 2
y
y <- rm()


