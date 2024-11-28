

oj <- read.csv("F:/Assinment for batch 4 by Ankit Sir/oj.csv",header=TRUE)

View(oj)          #to view the data frame 
names(oj)         #give the column names of the data set (can be used in vectors also)
colnames(oj)      #same out put but only true for the dataframe 
str(oj)           #structure of a dataset 

head(oj) #top 6      #first 6 rows(by default) 
head(oj,10)          #customize 

tail(oj,4) #last 6

summary(oj)

oj$brand <- as.character(oj$brand)      #gives output as character(change in summary presentation)
oj$brand <- as.factor(oj$brand)         #gives output as factors 

table(oj$store) #count per observation

#select week, count(*) from oj group by week

table(oj$brand)

oj[1,3] # data.frame[row , column]

#oj[ row          , column  ]

#technique to trim your data ##filtering of rows and columns 

oj[c(1,12,38,256) ,c(1,3,6) ] #combination = c()


oj[1:5,c("brand","feat")] # BRAND!=brand

#select columns from table name where (filter the row)

dat <- oj[oj$brand=="tropicana", ] #only show the rows having brabd name as dominicks

summary(dat)
levels(dat$brand)
dim(dat)            #dimension of a data frame 

table(dat$brand)

dat1 <- oj[oj$brand=="dominicks" | oj$brand=="tropicana", ] #" | " pipeline or operator 

# select * from oj where brand in ("dominicks","tropicana")

head(dat1,4) #top 6 by default

table(dat1$brand)

dim(dat1)

dat2 <- oj[oj$brand=="tropicana" & oj$feat==1, ]


dim(dat2)


d1 <- oj[oj$brand == "dominicks" | oj$brand == "minute.maid" & oj$feat == 0,]

#select * from oj where brand = "dominicks" or brand = "minute.maid" and feat =0
dim(d1)

table(d1$feat,d1$brand)

d2 <- oj[(oj$brand == "dominicks" | oj$brand == "minute.maid") & oj$feat == 0,]
dim(d2)
#select * from oj where brand in (minute.maid,dominicks) and feat =0

table(d2$feat,d2$brand)

#(A or B) & c -> 

dim(d2)
#write.csv(dataset name, "path")

x <- 17:33
x
x>26
which(x > 26)    #gives index from where my true value starts 

index <- which(oj$brand=="dominicks") #from 221 row brand dominicks start
head(index)

dat2 <- oj[which(oj$brand=="dominicks") , ]   #gives us the row having brand dominicks 

dat31 <- subset(oj,oj$brand=="crest")  #observe the result 

dat31 <- subset(oj,oj$brand=="crest",select=c(logmove,INCOME)) #keep keyword in SAS
colnames(dat31)

dat32 <- subset(oj,oj$brand=="crest",select=c(-week,-brand)) #drop keyword in SAS
colnames(dat32)

dat4 <- oj[ , c("brand","feat")] # 2 columns

dat4.1 <- oj[,c(4:6)]       # filtering of columns based on index of column

which(colnames(oj) =="feat" )  #index of "feat" in the columns that oj contains 

dat4.2 <- oj[,-c(3:5)]       #drop of column numbers from 3 to 5 (3 4 5)

colnames(dat4.2)

dat5 <- oj[oj$brand=="dominicks" & oj$feat==0,c("week","store")]
head(dat5)


# mysplit <- split(oj, oj$brand)
# summary(mysplit)

colnames(oj)
oj$logInc <- log(oj$INCOME)       #  creating a new variable(want to take log of my income and store that)
colnames(oj)

oj$flag <- ifelse(oj$brand=="tropicana", 1,0)  #if tropicana then 1 else o
colnames(oj)
dim(oj)
table(oj$flag)

#sorting 
            #1  2   3 4 5
numbers <- c(8,100,-4,5,10)
order(numbers)

numbers[order(numbers,decreasing = TRUE)]

dat5.1 <- oj[order(oj$store,decreasing = TRUE), ]  #week first least then increasing 

numbers[order(numbers)]

# aggregate function in sql min, max, sum, avg, count, SD

agg <- aggregate(oj$price,list(oj$brand),FUN=mean) 
agg
#try with week/store in place of brand and check result 
 
#need to remeber the numeric variable on which you want to apply the function, the groups on basis segrigation will be done                 
                  
# aggregate(numeric variable, by what?, by which function)

#select dept, avg(age) from file group by dept

agg

class(agg)
# family of apply function ...apply sapply lapply tapply(tapply=table apply)
#tapply(numeric variable, on what basis(class), function)

tap <- tapply(oj$price,oj$brand, mean) #pivot equivalent in R
tap                                   #same as aggregator function but in a different way

tap1 <- tapply(oj$price, INDEX = list(oj$store,oj$brand), mean)
 #if we want mean for more than one variable at a time 
tap1
#Merging and Join

df1 <- data.frame(CustomerId=c(1,2,3,4,5,6,
                              7,8,9,10,11,12),
                 Product=c("Toaster","Radio","TV",
                           "Toaster","Radio","TV",
                           "Toaster","Radio","TV",
                           "Toaster","Radio","TV"))

df2 <- data.frame(CustomerId=c(1,9,3,18,5,6,7,8),
                 State=c("Alabama","Ohio","California",
                         "Nebraska","Alabama","Ohio",
                         "New Jersey","Iowa"))


merge(x = df1, y = df2, by = "CustomerId", all = TRUE) #full join by customer id
merge(x = df1, y = df2, by = "CustomerId", all.x=TRUE) #left join as df1
merge(x = df1, y = df2, by = "CustomerId", all.y=TRUE) #right join as df2
merge(x = df1, y = df2, by = "CustomerId") #inner join by the common numbers 




#---------------Apply family------------------------
#https://www.guru99.com/r-apply-sapply-tapply.html

# apply() takes Data frame or matrix as an input and gives output in vector, 
# list or array. 
# apply() Function is primarily used to avoid explicit uses of loop constructs.
# It is the most basic of all collections can be used over a matrice.


# apply(X, MARGIN, FUN)
# Here:
#   -x: an array or matrix
# -MARGIN:  take a value or range between 1 and 2 to define 
# where to apply the function:
# -MARGIN=1`: the manipulation is performed on rows
# -MARGIN=2`: the manipulation is performed on columns
# -MARGIN=c(1,2)` the manipulation is performed on rows and columns
# -FUN: tells which function to apply. 
# Built functions like mean, median, sum, min, max 
# and even user-defined functions can be applied>

m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum)   #matrix, along rows/columns ,function we want to apply 
a_m1

# lapply() function is useful for performing operations on list objects 
# and returns a list object of same length of original set. 
# lappy() returns a list of the similar length as input list object,
# each element of which is the result of applying FUN to the corresponding 
# element of list. 
# lapply() takes list, vector or data frame as input and gives output in list.

# lapply(X, FUN)
# Arguments:
#   -X: A vector or an object
# -FUN: Function applied to each element of x

movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower)   #print the movies in lower case 
str(movies_lower)

movies_lower <-unlist(lapply(movies,tolower)) # to unlist and get the output in form of vector 
str(movies_lower)

# sapply() function takes list, vector or data frame as input and gives 
# output in vector or matrix. It is useful for operations on list objects
# and returns a list object of same length of original set.
# sapply() function does the same job as lapply() function but returns a vector.

# sapply(X, FUN)
# Arguments:
#   -X: A vector or an object
# -FUN: Function applied to each element of x

dt <- cars
lmn_cars <- lapply(dt, min)   #min of speed and distance 
smn_cars <- sapply(dt, min)   #same output in differenet format 
lmn_cars
smn_cars

lmxcars <- lapply(dt, max)
smxcars <- sapply(dt, max)
lmxcars
smxcars


# tapply() computes a measure (mean, median, min, max, etc..) 
# or a function for each factor variable in a vector. 
# It is a very useful function that lets you create a subset of a vector and 
# then apply some functions to each of the subset.

# tapply(X, INDEX, FUN = NULL)
# Arguments:
#   -X: An object, usually a vector
# -INDEX: A list containing factor
# -FUN: Function applied to each element of x

data(iris)
iris
tapply(iris$Sepal.Width, iris$Species, median)


#thank you
