## Esta funcion almacena en caché el inverso de una matriz

##  makeCacheMatrix crea una matriz  que almacena su inversa en caché
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function(){
    x
  }  
  setinverse <- function (inverse) s <<- inverse
  getinverse <- function () s
  list(set = set , get = get ,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calcula la inversa de la matriz makeCacheMatrix y si se ha calculado 
## antes, entonces se recuperará el inverso del caché 
cacheSolve <- function(x,...) {
  s <- x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  dat <- x$get()
  s <- solve(dat,...)
  x$setinverse(s)
  s
}
## Para probar si la matriz funciona 
s <- matrix(rnorm(20),4,4)
m1 <- makeCacheMatrix(s)
cacheSolve(m1)
