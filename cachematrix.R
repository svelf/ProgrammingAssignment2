# The results are tested based on matrix MA1 (square matrix), N1(inverse of MA1) and I2 (which is the identity m.)
# As a test N1 %*% MA1 should equal the Identity
MA1 <- matrix(c(1/3, -1/6, -1, 3/4), nrow = 2, ncol = 2)
N1 <- matrix(c(9,2,12,4), nrow = 2, ncol = 2) # Inverse-i.e. result of solve(MA1)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
# TEST<-  N1 %*% MA1 = I2


###Testing the functions with real Data 
nn<- cacheSolve(MA1)

myMatrix_object <- makeCachematrix(nn) # The result of this should be equivalent to N1 and N1 %*% MA1 = I2

TEST<- myMatrix_object%*%MA1 
print(TEST==I2)

################ Functions 

makeCachematrix <- function(x, ...) {
  #This function is required to retrieve the inverse form an object of type 
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) # At this point the inv function gets executed 
  x$setinv(m)
  m
}


cacheSolve <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    # Function set resets value with a new vector
    x <<- y
    m <<- NULL
    # Clears any value that has been stored by an earlier running of the function
  }
  get <- function() x  #Retrieve the value of the matrix inputted, in our example 
  #when typing nn$get() you should get the matrix MA1
  setinv <- function(solve) m <<- solve 
  getinv <- function() m #Retrieve the value of m if stored gets it directly
  #which should be NULL in the first iteration
  list(set = set, # gives the name 'set' to the set() function defined above
       get = get, # gives the name 'get' to the get() function defined above
       setinv = setinv,  #gives the name ’setinv'' to the setinv() function defined above
       getinv = getinv   # gives the name ‘getinv' to the getinv() function defined above
  )
}
