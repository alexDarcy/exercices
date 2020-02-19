library(ggplot2)
input <- read.table("input.csv", sep=",")
hull <- read.table("hull.csv", sep=",")
p <- ggplot(NULL, aes(x=V1, y=V2)) + geom_path(data=hull) + geom_point(data=input)
