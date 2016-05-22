########################################
### get.installed.package.list() returns a character vector of installed
### packages. base::search() would enumerate which are loaded/attached.
get.installed.package.list <- function() {
    package_frame <- installed.packages()
    package_list <- package_frame[, 1]
    return(unique(package_list))
}
### old.package.list gets the list of packages already installed.
### new.package.list gets the list of packages needed in the script.
### the next bits check one list against the other, install what
### isn't there already, and load the lot.
old.package.list <- get.installed.package.list()
new.package.list <-
    c("ggplot2",
      "base")
for (i in new.package.list) {
    if (i %in% old.package.list == FALSE) {
        install.packages(i , dependencies = TRUE)
    }
    require(i , character.only = TRUE)
}
### done installing packages.
########################################

list.files()

unzip("activity.zip")
activity <- read.csv("activity.csv")
head(activity)

qplot(activity$steps , geom = "histogram")
qplot(na.omit(activity$steps) , geom = "histogram")

?aggregate
stepsPerDay <-
    aggregate(steps ~ date ,
              data = activity ,
              FUN = sum ,
              na.rm = TRUE)
str(stepsPerDay)
qplot(as.numeric(stepsPerDay$steps) ,
      geom = "histogram" ,
      bins = 30)

meanSteps <- mean(stepsPerDay$steps)
medianSteps <- median(stepsPerDay$steps)

?paste
paste("Mean is" , meanSteps , ";" , "median is" , medianSteps , sep = " ")

qplot(as.numeric(stepsPerDay$steps) ,
      geom = "histogram" ,
      bins = 20)

head(activity)
str(activity)

stepsPerInterval <- aggregate(steps ~ interval ,
                              data = activity ,
                              FUN = mean ,
                              na.rm = TRUE)
#stepsPerInterval$inverval <- as.factor(stepsPerInterval$interval)
?qplot

qplot(
    interval ,
    steps ,
    data = stepsPerInterval ,
    geom = "line" ,
    xlab = "5-minute interval" ,
    ylab = "Average steps"
) + geom_vline( xintercept = 835 , color = "red" ) 


p <- ggplot(stepsPerInterval , aes(interval , steps)) + geom_line() + 
            geom_vline( xintercept = 835 , color = "red" ) 

p

stepsPerInterval[ which.max(stepsPerInterval$steps) , ]



most <- stepsPerInterval[ which.max(stepsPerInterval$steps) , ]
print( paste( "The highest average number of steps is " , eval(most$steps) , 
              ", occuring at interval " , eval(most$interval) , "." , sep = ""))



missing <- sum( is.na( activity$steps ))
paste( "The total number of rows containing missing data is " , missing , "." ,
       sep = "" )



all.days <- data.frame( "date" = unique( activity$date ))
all.days.steps <- merge( all.days , stepsPerDay , all = TRUE)
just.steps <- all.days.steps$steps
just.steps

for( i in 1:length(just.steps) ) {
    if( is.na( just.steps[i]) == T ) { 
        just.steps[i] <- mean( c( just.steps[i-1] , just.steps[i+1]) , 
                               na.rm = T )}
    
}
just.steps

stepsPerDay.corrected <- cbind( all.days , just.steps )
avgStepsPerDay.corrected <- mean( just.steps )

### EVERYTHING ABOVE HERE IS IN THE FIRST PASS RMD FILE AS OF
### 2016-05-22 00:03
### SAVE FILES, GIT PUSH.

activity.corrected <- activity

for( i in 1:nrow(activity.corrected) ) {
    day <- activity.corrected[ i , 'date' ]
    steps <- stepsPerDay.corrected[ stepsPerDay.corrected$date == day , "just.steps" ]
    ratio <- steps / avgStepsPerDay.corrected
    interval.steps <- stepsPerInterval[ stepsPerInterval$interval == 
                                        activity.corrected$interval[i], 'steps']
    if( is.na(activity.corrected[ i , 'steps']) == TRUE ) {
        activity.corrected[i,'steps'] <- ratio * interval.steps
    }
}

















