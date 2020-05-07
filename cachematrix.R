##this fuction caclculate an inverse of a matrix and retrive the result from cache
##this assignment gives us a rough illustration of how lexical scoping works

##the first makeCacheMatrix stores a matrix and its inverse
#it used set and get fuction to set values and retrive values

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  set<-function(mat){
    x<<-mat
    i<<-NULL
  }
  get<-function() x
  setInv<-function(inv)
    i<-inv
  getInv<-function() i
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}

##without this second function the previous function is incomplete because this function
##actually calculate the inverse

cacheSolve<-function(x,...){
  i<-x$getInv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  if(length(data)%%sqrt(length(data))==0)
   i<-solve(data,...)
  x$setInv(i)
  i
  }
