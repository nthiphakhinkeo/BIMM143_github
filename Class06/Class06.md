# Class06
Nicholas Thiphakhinkeo A17686679

\#every fxn in R has atleast 3 things \# - a **name**, one or more input
**arguments**, The **body**, where

``` r
add <- function(x,y) {x+y}
```

``` r
add(x=c(10,1,1,10),y=1)
```

    [1] 11  2  2 11

``` r
mean(c(10,10,NA), na.rm=T)
```

    [1] 10

## \#Q1. Write a function grade() to determine an overall grade from a vector of student homework assignment scores dropping the lowest single score. If a student misses a homework (i.e. has an NA value) this can be used as a score to be potentially dropped. Your final function should be adquately explained with code comments and be able to work on an example class gradebook such as this one in CSV format: “https://tinyurl.com/gradeinput” \[3pts\]

``` r
student1 <- c(100, 100, 100, 100, 100, 100, 100, 90)
student2 <- c(100, NA, 90, 90, 90, 90, 97, 80)
student3 <- c(90, NA, NA, NA, NA, NA, NA, NA)
```

# Student 1 Avg

``` r
mean(student1)
```

    [1] 98.75

# Student 2 Avg

``` r
mean(student2, na.rm=T)
```

    [1] 91

# Student 3

``` r
mean(student3, na.rm=T)
```

    [1] 90

# Subtracting Value by Eye

``` r
mean(student1[-8])
```

    [1] 100

# Min Function

``` r
min.ind <- which.min(student1)
mean(student1[-min.ind])
```

    [1] 100

``` r
min.ind <- which.min(student2)
mean(student2[-min.ind], na.rm=F)
```

    [1] NA

# Making NAs Zero

``` r
student2[ is.na(student2) ] = 0
student2
```

    [1] 100   0  90  90  90  90  97  80

# Finds min value bf getting mean

``` r
grade <- function(x) {
  x[ is.na(x) ] <- 0
  mean(x[-which.min(x)])}
```

# Question 1: Writing Grade() Function

``` r
gradebook <- read.csv("https://tinyurl.com/gradeinput",row.names=1)
head(gradebook)
```

              hw1 hw2 hw3 hw4 hw5
    student-1 100  73 100  88  79
    student-2  85  64  78  89  78
    student-3  83  69  77 100  77
    student-4  88  NA  73 100  76
    student-5  88 100  75  86  79
    student-6  89  78 100  89  77

``` r
ans <- apply(gradebook,1,grade)
```

# Question 2: Highest Scoring Student

``` r
which.max(ans)
```

    student-18 
            18 

``` r
ans[which.max(ans)]
```

    student-18 
          94.5 

# Question 3: Which was toughest HW Assignment

``` r
masked_gradebook <- gradebook
masked_gradebook[ is.na(masked_gradebook)] = 0
apply(masked_gradebook,2,mean)
```

      hw1   hw2   hw3   hw4   hw5 
    89.00 72.80 80.80 85.15 79.25 

# Grade2

# Q4: Cor()

``` r
x <- c(100,90,80,100) 
y <- c(100,90,81,99)
z <- c(100,80,90,100)
cor(x,z)
```

    [1] 0.6363636

``` r
cor(ans,masked_gradebook$hw5)
```

    [1] 0.6325982

``` r
apply(masked_gradebook, 2, cor, y=ans)
```

          hw1       hw2       hw3       hw4       hw5 
    0.4250204 0.1767780 0.3042561 0.3810884 0.6325982 
