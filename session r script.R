#session script free boot camp#

#numeric
 x = 10.5       # assign a decimal value
 x             # print the value of x 
 class(x)       # print the class name of x 
 k <- 1 
  k              # print the value of k 
  class(k)       # print the class name of k 
  is.integer(k)  # is k an integer? 
  
  #integer 
  
  y <- as.integer(3) 
   y              # print the value of y
   
   class(y)       # print the class name of y 
   is.integer(y)  # is y an integer? 
   y=8L        #another way to assign integer
   
  a<- as.integer(3.14)   #always give greatest integer
   a
   as.integer("payel")   #gives NA value
   as.integer(TRUE)   #logical operator true(value) =1
as.integer(FALSE)     #logical operator false(value)=2
  

#logical operator 
x <- 1;y<-2
            # sample values 
 z <- x> y       # is x larger than y? 
 z              # print the logical value 
 class(z)       # print the class name of z 
 TRUE+FALSE
TRUE+TRUE


#character 
x <- as.character(3.14) 
x              # print the character string 
 class(x)       # print the class name of x 
 
 fname <- "Joe"; lname <-"Smith"
 
 paste(fname, lname)  #Two character values can be concatenated with the paste function
 
 sprintf("%s has %d dollars", "Sam", 100) 
 
 #vector
 c(2, 3, 5) 
 c(TRUE, FALSE, TRUE, FALSE, FALSE)  #c-combining function 
 c("aa", "bb", "cc", "dd", "ee")
 
n<- c(2, 3, 5) 
s<- c("aa", "bb", "cc", "dd", "ee") 
c(n,s)
 t<-c(n,s)
 class(t)
 class(n)
 class(s)
 #c>n>l
#vector arithmatic

a <- c(1,3, 5, 7) 
 b <- c(1, 2, 4, 8)
 5 * a            #multiply 5 with vector a
 a + b  
 a - b 
 a * b 
 a / b 

#indexing of vector
 s<- c("aa", "bb", "cc", "dd", "ee")  
 s[3]
s[-3]
s[6] 
s[c(2, 5, 3)]  #to check multiple elements 
s[1:4]       # range (start:end)

1:100

20:50

#named vector members


v <- c(1,2) 
names(v) <- c("First", "Last") 
v

#matrix

A <- matrix(c(2, 4, 3, 1, 5, 7),nrow=2,ncol=3,byrow = TRUE) # the data elements 
A
 A <- matrix(c(2,4,3,1,5,7), nrow = 2,ncol=3)
 
A                      # print the matrix 
A[2, 3]                ## element at 2nd row, 3rd column 
A[2, ]                 # the 2nd row
A[ ,3]                 # the 3rd column 
A[ ,c(1,3)]            # the 1st and 3rd columns
dimnames(A) <- list( c("row1", "row2"), c("col1", "col2", "col3"))
A
#matrix construction 
B = matrix( c(2, 4, 3, 1, 5, 7),nrow=3, ncol=2)
B
t(B)          # transpose of B 


#combining matrix
C = matrix(c(7, 4, 2), nrow=3,  ncol=1) 
C
B
cbind(B, C)              #bind by column

D = matrix( c(6, 2), nrow=1, ncol=2) 
D
B
rbind(B, D)               #bind by row

#list

n = c(2, 3, 5) 
 s = c("aa", "bb", "cc", "dd", "ee") 
 b = c(TRUE, FALSE, TRUE, FALSE, FALSE) 
 x <- list(n, s, b, 3)   # x contains copies of n, s, b
 class(x)
x
x[4]  
x[c(2,4)]
class(x)  
x=c(n,s,b,3)  
class(x)  


#data frame used to store data tables 

n = c(2, 3, 5) 
 s = c("aa", "bb", "cc") 
 b = c(TRUE, FALSE, TRUE) 
 df = data.frame(n, s, b)       # df is a data frame

data.frame(mtcars) 

 sf<-data.frame(mtcars)        #example of in build data frame 
 mtcars[1, 2] 
 nrow(mtcars)                 #number of rows in a data frame   
ncol(sf)                      #number of column in a data frame   
head(sf)                      #by default shows 6 col 
tail(sf)
head(sf,8)                   #to increase the number of heads 
mtcars[,"am"]                 #to view a particular column  
mtcars[c("mpg", "hp")] 

mtcars[c(3, 24),] 
L = mtcars$am == 0 









