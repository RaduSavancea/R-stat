setwd('~/R project')

#========================================================
#Aufgabe (a)
#========================================================
set.seed(1)
#generating the normal distributed samples
x=rnorm(100) 
sum(x)
sd(x)
mean(x)

set.seed(2)
y=rnorm(100)
sum(y)
sd(y)
mean(y)
write.table(cbind(x,y), file = "Output.txt")

xval=c(mean(x),sd(x),sum(x))
xdata=c(x,xval)
yval=c(mean(y),sd(y),sum(y))
ydata=c(y,yval)
write.table(cbind(xdata,ydata), file = "Output2.txt")

#========================================================
#Aufgabe (b)
#========================================================

#Creating the dataframe with the ranodmly distributed samples
data_frame=data.frame(x=c(rnorm(10,0,2),rnorm(15,1,1)), group=c(rep("Group1",10),rep("Group2",15)))
write.table(data_frame, sep="&",file="my.data.txt",row.names = FALSE)

#========================================================
#Aufgabe (c)
#========================================================

data2 <-read.table(file = "my.data.txt", sep="&", header=TRUE)
data2$group <- as.character(data2$group)
#Modifying the second group with Group 3
data2[data2 == "Group2"] <- "Group3"
data2$group <- as.factor(data2$group)
data2
write.table(data2, sep="&",file="my.data.txt",row.names = FALSE)

#========================================================
#Aufgabe (d)
#========================================================

#Function that returns the value of the distribution function of the binomial distribution

my.pbinom <- function(q,size,prob){
  res <- 0
  for(i in 1:q){
    res <- res+choose(size,i)*(prob^i)*((1-prob)^(size-i))
  }
  return(res)
}

#Quantile function 
my.qbinom <- function(q,size,prob,perc){
  return(uniroot(function (q,size,prob) my.pbinom(q,size,prob)-perc,interval=c(0,10),size=10,prob=0.5))
}

my.pbinom(5,10,0.5)
my.qbinom(5,10,0.5,0.5)$root

#========================================================
#Aufgabe (e)
#========================================================

data(iris)
par(mfrow=c(1,3))
mi<-min(iris$Sepal.Width)
ma<-max(iris$Sepal.Width)
for(i in unique(iris$Species)){
  boxplot(subset(iris,Species==i)$Sepal.Width,ylim=c(mi,ma), col="red", ylab="Width", xlab=i,main= paste(i, "Sepal Width", sep=" ")) 
}
boxplot(Sepal.Width~Species, data=iris, col="red", main="Sepal Width Boxplot")

#========================================================
#Aufgabe (f)
#========================================================

exams <- data.frame(passed = c(72, 58),failed = c(10, 20), row.names = c("Professor X", "Professor Y"))
exams
exams.by.students <- data.frame(passed = c(rep(1,130), rep(0,30)),Professor = c(rep("X",72),rep("Y",58),rep("X",10),rep("Y",20)))
exams.by.students

#Here we are creating the frequency table
table(exams.by.students$passed)
table(exams.by.students$Professor)

#========================================================
#Aufgabe (g)
#========================================================

exams_mat <- as.matrix(exams)
exams_mat

pdf(file="Exams.pdf",width=6,height=6)
postscript(file="Exams.pdf",width=6,height=6)
par(mfrow=c(1,2))
pie(exams_mat[1,], labels="Professor X",col=c("white","black"),radius=2)
legend("topleft",c("passed","failed"), cex=1, fill=c("white","black"))
pie(exams_mat[2,], labels="Professor Y",col=c("white","black"),radius=2)
dev.off()

#========================================================
#Aufgabe (h)
#========================================================

par(mfrow=c(1,1))
data(swiss)
EducationLevel <- swiss$Education
Fertility <- swiss$Fertility
plot(EducationLevel,Fertility)
length(Fertility)
ignore <-45
#identify(EducationLevel,Fertility)

EducationLevel <- c(EducationLevel[1:ignore-1],EducationLevel[ignore+1:length(EducationLevel)]) 
Fertility <- c(Fertility[1:ignore-1],Fertility[ignore+1:length(Fertility)]) 
plot(EducationLevel,Fertility)


?swiss

