#COURSE TOPIC MODELLING UNIVERSITAT DE BARCELONA FEB 2021

#R TUTORIAL

#R Installation:
#1.Download and install R by going to https://cloud.r-project.org/
#2.Download and install RStudio Desktop (Free) at https://rstudio.com/products/rstudio/download/
#(RStudio is an integrated development environment (IDE), an interface with many convenient features and tools)

#R packages extend the functionality of R by providing additional functions, data, and documentation
#R package installation:
#1.Go to menu Tools -> Install Packages, type package name and install it or
#2.type: install.packages("package_name") in the console pane
#3 To load the package after installation, type: library(package_name) in the console pane 
#"CRAN task views" (https://cran.r-project.org/) classify packages by topic



.libPaths() #Get library locations containing R packages

library() #Get the list of all the packages installed

search()  #Get all packages currently loaded in the R environment


#TOOLS TO LEARN R:
#Help: ?function or help(function) or ??"general" or help.search("general")

#To display examples: example(function)

#You can then find out which functions it provides by typing one of:
#library(help = "package_name") or help(package = "package_name")
#vignette("np",package="np")
#vignette(all=TRUE) # vignettes for all packages on the computer

#Textbooks, documentation and free tutorials.
#Textbooks:
#1.An Introduction to R (2020): https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf
#2.Hands-On Programming with R: https://rstudio-education.github.io/hopr/
#3.R for Data Science: https://r4ds.had.co.nz/

#Free tutorials:
#1. https://www.datamentor.io/r-programming/
#2. https://www.tutorialspoint.com/r/index.htm
#3. https://www.learnbyexample.org/r-introduction/
#4. https://www.statmethods.net/
#5. https://www.r-exercises.com/2016/07/22/start-here-to-learn-r/

#Documentation and R communities:
#1.Documentation for packages and functions: https://www.rdocumentation.org/
#2.Documentation for packages and functions: https://rdrr.io/
#3.R-BLOGGERS (news and tutorials): https://www.r-bloggers.com/
#4.StackOverflow (Q&A): https://stackoverflow.com/questions/tagged/r
#5.Ask Google

#R is an object oriented programming language
#IN R, EVERYTHING THAT EXISTS IS AN OBJECT. EVERYTHING THAT HAPPENS IS A FUNCTION CALL

#R Data Structure:
5+2

#Create R objects:

x <- 5
y <- 7

z <- y - x

z

myString <- "Hello, World!"

print(myString)

#List of reserved words:
?reserved

#To delete a variable:
rm(myString)

ls() #to see all created objects

rm(list = ls()) ##to remove all created objects

#Vectors are the simplest R objects, an ordered list of primitive R objects of the same type
#The data types can be logical, integer, double, character, complex or raw

x <- c(1, 5, 4, 9, 0) ## Atomic vector of type double

is.vector(x)

View(x)

typeof(x)

mode(x) #alternative to typeof function

class(x)

length(x)

mean(x) #mean function

int <- c(2L, 4L, 6L) # Atomic vector of type integer

typeof(int)

comp <- c(1 + 1i, 1 + 2i, 1 + 3i) # Atomic vector of type complex

typeof(comp)

y <- c("unversitat", "de", "barcelona") #Atomic vector of type character 

typeof(y)

length(y)

logic <- c(TRUE, FALSE, TRUE) #Atomic vector of type logical 

typeof(logic)

z <- c(1, 5.4, TRUE, "hello")

typeof(z) #all vector elements are converted to character

length(z)

x <- 1:7; x

y <- 1:7; y

z <- x + y; z  #element-wise operation

#Attribute: info (metadata) that you can attach to an atomic vector (or any R object)
#Attributes are like a "named list of vectors" that can be attached to any object
#Vectors can be atomic vectors (elements of the same type) or list (elements of different types)

x <- 1:10

attributes(x)

#The most common attributes (with specific functions) for atomic vectors are: names, dimensions (dim) and classes

die <- 1:6

names(die)

names(die) <- c("one", "two", "three", "four", "five", "six") #assign names

die #names are displayed with values

names(die)

attributes(die) #display attributes all at once

attr(die, "names")  #display and assign attributes

attr(die, "names") <- c("one", "two", "three", "four", "five", "six")

attr(die, "ub") <- "Universitat de Barcelona"

attributes(die)

names(die) <- NULL #to remove names attribute 

#An atomic vector becomes an n-dimensional array by assigning dimensions attribute with dim:

dim(die) <- c(2, 3) #dimension attribute

die

typeof(die)

class(die)

#Many R functions (generic) will specifically look for an object’s class attribute, and
#then handle the object in a predetermined way based on the attribute

class("Hello")

class(5)

#Date values
#Dates are represented as the number of days since 1970-01-01

today <- Sys.Date(); today # print today's date

typeof(today)  #number of days

class(today) # class "Date"

format(today, format="%B %d %Y")

#To see date and time formats:
help(strptime)

# use as.Date( ) to convert strings to dates (numeric)

mydates <- as.Date(c("2007-06-22", "2004-02-13"))   #format can be defined in the function

# number of days between 6/22/07 and 2/13/04
days <- mydates[1] - mydates[2]

date() #returns the current date and time

now <- Sys.time(); now #returns the current date and time (seconds since 12:00 AM 01/01/1970)

typeof(now)

class(now)  #classes "POSIXct" and "POSIXt"  

unclass(now) #to remove the time class

timeDate <- as.POSIXct("2015-10-19 10:15")   #convert character data to date and time (format can be set)

str(timeDate)

#The lubridate package provides functions for reading, manipulating, and doing arithmetic with dates in R

#Elements of a vector can be accessed using vector indexing.
#The vector used for indexing can be integer, logical or character vector.

x <- c(0, 2, 4, 6, 8, 10)

x[3]           # access 3rd element

x[c(2, 4)]     # access 2nd and 4th element

x[-1]          # access all but 1st element

x[c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)]  #access elements in TRUE position

x > 4

x[x > 4]  # filtering vectors based on conditions

x <- c("first"=3, "second"=0, "third"=9)

names(x)

x["second"]  #access element through its name

x[c("first", "third")]

#Vectors can be modified through assignment (main difference from STATA):

x <- 1:8

x[2] <- 0; x        # modify 2nd element

x[x<5] <- 5; x   # modify elements less than 5

x <- x[1:4]; x      # truncate x to first 4 elements (overwrite x)

#To delete a vector, assign NULL

x <- NULL; x

#Matrix is a two dimensional vector:

a <- matrix(1:9, nrow = 3, ncol = 3); a

View(a)

class(a)

attributes(a)

#Other ways to create a matrix:

b <- cbind(c(1,2,3),c(4,5,6)); b

c <- rbind(c(1,2,3),c(4,5,6)); c

x <- 1:6

dim(x) <- c(2,3)

#Name columns and rows
colnames(a) <- c("C1","C2","C3")

rownames(a) <- c("R1","R2","R3")

dimnames(a)

#We can access elements of a matrix using the square bracket [ indexing method 
#Elements can be accessed as var[row, column]. Here rows and columns are vectors (integer, logical or character):

a <- matrix(1:9, nrow = 3, ncol = 3)

a[c(1,2),c(2,3)]    # select rows 1 & 2 and columns 2 & 3  

a[c(3,2),]    # leaving column field blank will select entire columns

a[a>5]    # select elements greater than 5

#Factor is a data structure for categorical data

status <- factor(c("single", "married", "married", "single"))

#Factor levels can be manually set even though they are not included in the list:

status <- factor(c("single", "married", "married", "single"), levels = c("single", "married", "divorced"))

typeof(status)

attributes(status)

levels(status)

str(status) #factors are stored as integer vectors; levels are stored in a character vector and individual elements indices

as.character(status) #convert a factor to a character string


#List (a type of vector) is a data structure having components of mixed data types

x <- list("a" = 2.5, "b" = TRUE, "c" = 1:3)

typeof(x)

length(x)

#List structure with str function:

str(x)  #a, b and c are called "tags" (element names)

x <- list(2.5,TRUE,1:3); x #list without tags

#Access components of a list:
x <- list("name" = "diego", "age" = 20, "speaks" = c("Catalan", "Italian", "English"))

x[c(1:2)]    # index using integer vector

x[-2]        # using negative integer to exclude second component

x[c(T,F,F)]  # index using logical vector

x[c("age","speaks")]    # index using character vector

#Indexing with [ gives a sublist. To retrieve the content, use [[

x["age"]

typeof(x["age"])    # single [ returns a list

x[["age"]]    # double [[ returns the content

typeof(x[["age"]])

#An alternative to [[ is the $ operator

x$name    # same as x[["name"]]

x$a      # partial matching, same as x$ag or x$age

#Modify a list through assignment:

x[["name"]] <- "Josep"
x

#Add components through assignment:

x[["vegetarian"]] <- TRUE
x

#Delete components by assigning NULL:

x[["age"]] <- NULL

str(x)

#Data frame is a special case of a list with class "data.frame", which has each component of equal length

df <- data.frame("SN" = 1:2, "Age" = c(21,15), "Name" = c("John","Dora"))

str(df) # Name is of type factor as data.frame() converts character vector into factor

df <- data.frame("SN" = 1:2, "Age" = c(21,15), "Name" = c("John", "Dora"), stringsAsFactors = FALSE)

str(df) #stringsAsFactors=FALSE avoid conversion of character vector into factor

names(df) #gives the name of each variable

summary(df) #gives some very basic summary statistics for each variable

head(df) #shows the first few rows

tail(df)  #shows the last few rows

attributes(df) 

class(df)

ncol(df)

nrow(df)

length(df)    # returns length of the list, same as ncol()

#Functions of like read.table(), read.csv(), read.delim(), read.fwf() also read data into a data frame

#Components of data frame can be accessed like a "list" or like a "matrix"

#Data frames can be modified through reassignment

#Rows and columns can be added by using the rbind() and cbind() function, respectively
#Columns can be added through simple list-like assignments:

df$State <- c("EN","SP"); df

df$State <- NULL; df

#Display and edit data frame like in STATA or Excel:

View(df)

newdf <- edit(df) # edit copy and save as a new object (don't edit in place)

View(newdf)       #to display edited data frame
  
fix(df)           # edit in place

View(df)

mydat <- edit(data.frame()) #to enter data manually (unlikely)

#datasets already available in R:
library(help = "datasets")

#Data frames can me manipulated using either base R functions or functions developed in other packages (e.g., tidyverse)

#Tidyverse (https://www.tidyverse.org/) is a collection of R packages designed for data science sharing a common philosophy

#Install the complete tidyverse with:

install.packages("tidyverse")






