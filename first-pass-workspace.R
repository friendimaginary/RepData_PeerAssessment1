list.files()

unzip( "activity.zip" )
activity <- read.csv( "activity.csv" )
head(activity)

install.packages( 'ggplot2' )
require( ggplot2 )

qplot( activity$steps , geom = "histogram" )
qplot( na.omit(activity$steps) , geom = "histogram" )

?aggregate
stepsPerDay <- aggregate( steps ~ date , data = activity , FUN = sum , na.rm = TRUE )
str( stepsPerDay )
qplot( as.numeric(stepsPerDay$steps) , geom = "histogram" , bins = 30)

meanSteps <- mean( stepsPerDay$steps )
medianSteps <- median( stepsPerDay$steps )

?paste
paste( "Mean is" , meanSteps , ";" , "median is" , medianSteps , sep = " " )

