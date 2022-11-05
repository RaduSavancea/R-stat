


setwd('~/R project')



##### Day 2 ####

###Exercise 1
City <- c("LosAngeles","Phoenix","SanDiego","SanFrancisco","Seattle", "LosAngeles","Phoenix","SanDiego","SanFrancisco","Seattle")
OnTime <- c(497, 221, 212, 503, 1841, 694, 4840, 383, 320, 201)
Delayed <- c(62, 12, 20, 102, 305, 117, 415, 65, 129, 61)
df <- data.frame(City = City,
                 OnTime = OnTime,
                 Delayed=Delayed, Airline = c(rep("AlaskaAirlines", times=5), rep("AmericaWest", times=5)))
# the dataframe is saved in df variable

df 

###Exercise 2

# We want to create a new dataframe with the percentages in order to see better Simpsons Paradox.
# Here we are interested to see which airline was more precise (that means had not so many delays )

Alaska_Total_Percent <- sum(OnTime[1:5])/(sum(OnTime[1:5]) + sum(Delayed[1:5]))
America_Total_Percent <- sum(OnTime[6:10])/(sum(OnTime[6:10]) + sum(Delayed[6:10]))

Percent_Alaska = vector()
for (i in 1:5) {
  Percent_Alaska <- append(Percent_Alaska, OnTime[i]/(OnTime[i] + Delayed[i]))

}

Percent_Alaska

Percent_America = vector()
for (i in 6:10) {
  Percent_America <- append(Percent_America, OnTime[i]/(OnTime[i] + Delayed[i]))
  
}

Percent_America

df_percent <- data.frame(Percent_Alaska = c(Percent_Alaska, Alaska_Total_Percent), 
                 Percent_America = c(Percent_America, America_Total_Percent) 
                )

df_percent
rownames(df_percent) <- c("LosAngeles","Phoenix","SanDiego","SanFrancisco","Seattle", "Total")

df_percent
# Here we can clearly see how the Simpson's Paradox arises. Alaska airlines had better pecentages for all of the cities listed
# but in total the percentage was smaller as for America (this can be clearly seen in the constructed dataframe)

### Exercise 3
data2 <- read.csv("winequality-white.csv", sep=";")
colnames(data2)
length(data2$residual.sugar)

#adding the column
data2$good <- with(data2, ifelse(quality > 5, "1", "0"))

good_wine_sugar = data2$residual.sugar[data2$good == 1]
good_wine_acid = data2$volatile.acidity[data2$good == 1]

plot(good_wine_sugar, good_wine_acid)

bad_wine_sugar = data2$residual.sugar[data2$good == 0]
bad_wine_acid = data2$volatile.acidity[data2$good == 0]

plot(bad_wine_sugar, bad_wine_acid)

prop_good_wine_sugar <- c(mean(good_wine_sugar),
median(good_wine_sugar),
IQR(good_wine_sugar),
min(good_wine_sugar),
max(good_wine_sugar),
sd(good_wine_sugar))

prop_good_wine_acid <- c(mean(good_wine_acid),
                          median(good_wine_acid),
                          IQR(good_wine_acid),
                          min(good_wine_acid),
                          max(good_wine_acid),
                          sd(good_wine_acid))

prop_bad_wine_sugar <- c(mean(bad_wine_sugar),
                         median(bad_wine_sugar),
                         IQR(bad_wine_sugar),
                         min(bad_wine_sugar),
                         max(bad_wine_sugar),
                         sd(bad_wine_sugar))

prop_bad_wine_acid <- c(mean(bad_wine_acid),
                         median(bad_wine_acid),
                         IQR(bad_wine_acid),
                         min(bad_wine_acid),
                         max(bad_wine_acid),
                         sd(bad_wine_acid))
table_prop <- data.frame(Sugar_Good_Wine = prop_good_wine_sugar,
                         Acid_Good_Wine = prop_good_wine_acid,
                         Sugar_Bad_Wine = prop_bad_wine_sugar,
                         Acid_Good_Wine = prop_bad_wine_acid)
                        

rownames(table_prop) <- c("Mean", "Median", "IQR", "Min", "Max", "SD")

table_prop

####Day 3 #####

###Exercise 1

delay_percent <- 1 - df_percent
delay_percent
LA <- as.numeric(as.vector(df_percent[1,]))


barplot(height = as.matrix(delay_percent),                       # Grouped barplot using Base R
        beside = TRUE)
cities = c(rep("LosAngeles", times=2), rep("Phoenix", times=2),rep("SanDiego", times=2),rep("SanFrancisco", times=2), rep("Seattle ", times=2))
company=rep(c("AlaskaAir", "America"), times=5 )
percent=c(0.11091234, 0.14426634, 0.05150215, 0.07897241, 0.08620690 , 0.14508929, 0.16859504, 0.28730512, 0.14212488    ,  0.23282443)

delay_remastered = data.frame(Cities=cities, Company=company, Percent=percent)


library(ggplot2)
ggplot(delay_remastered, aes(Cities, Percent, fill = Company)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

delay_remastered

subset(delay_remastered, Company=='AlaskaAir')


ggplot(subset(delay_remastered, Company=='AlaskaAir') , aes(x = "", y = Percent, fill = Cities)) +
  geom_col(color = "black") +
  geom_text(aes(label = Percent),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  scale_fill_brewer() +
  theme_bw()


ggplot(subset(delay_remastered, Company=='America') , aes(x = "", y = Percent, fill = Cities)) +
  geom_col(color = "black") +
  geom_text(aes(label = Percent),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  scale_fill_brewer() +
  theme_bw()

#Histogram for good weins
sugar_good_hist <- ggplot(subset(data2, good==1) , aes(x=residual.sugar)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 1, bins=70)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
sugar_good_hist

#histogram for bad weins
sugar_bad_hist <- ggplot(subset(data2, good==0) , aes(x=residual.sugar)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 1, bins=70)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
sugar_bad_hist

#box plot for both types of wine
sugar_box <- ggplot(data2, aes(x=good, y=residual.sugar)) + geom_boxplot()
sugar_box

#ecdf plot for both types of wine
sugar_ecdf <- ggplot(data2, aes(x = residual.sugar)) + stat_ecdf(aes(color = good,linetype = good), 
            geom = "step", size = 1.5) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(y = "CDF")
sugar_ecdf

sugar_good_hist <- ggplot(subset(data2, good==1) , aes(x=residual.sugar)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 1, bins=70)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
sugar_good_hist

#histogram for bad weins
acid_bad_hist <- ggplot(subset(data2, good==0) , aes(x=volatile.acidity)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.05, bins=20)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
acid_bad_hist

acid_good_hist <- ggplot(subset(data2, good==1) , aes(x=volatile.acidity)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.05, bins=20)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
acid_good_hist

#box plot for both types of wine
acid_box <- ggplot(data2, aes(x=good, y=volatile.acidity)) + geom_boxplot()
acid_box

#ecdf plot for both types of wine
acid_ecdf <- ggplot(data2, aes(x = volatile.acidity)) + stat_ecdf(aes(color = good,linetype = good), 
                                                                 geom = "step", size = 1.5) + scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(y = "CDF")
acid_ecdf

S

####Day 4####
#Histogram for all weines
#Histogram for good weins
all_pH_hist <- ggplot((data2) , aes(x=pH)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.02, bins=70)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
all_pH_hist



#Histogram for good weins
pH_good_hist <- ggplot(subset(data2, good==1) , aes(x=pH)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.02, bins=70)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
pH_good_hist

#histogram for bad weins
pH_bad_hist <- ggplot(subset(data2, good==0) , aes(x=pH)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.02, bins=70)+
  geom_density(alpha=.2, fill="#FF6666")+title("Plot of length \n by dose")
pH_bad_hist

## QQ Plots

ggplot(data2, aes(sample = pH)) +
  stat_qq() +
  stat_qq_line()+ggtitle("All Wines")

ggplot(subset(data2, good==1), aes(sample = pH)) +
  stat_qq() +
  stat_qq_line()+ggtitle("Good Wines")

ggplot(subset(data2, good==0), aes(sample = pH)) +
  stat_qq() +
  stat_qq_line()+ggtitle("Bad Wines")

## PP Plot

standard_data2 <- (data2$pH - mean(data2$pH)) / sd(data2$pH)
probDist <- pnorm(standard_data2)

plot(ppoints(length(standard_data2)), sort(probDist), main = "PP All Weins", xlab = "Observed Probability", ylab = "Expected Probability")
abline(0,1)

standard_data2_good <- (subset(data2, good==1) $pH - mean(data2$pH)) / sd(data2$pH)
probDist2 <- pnorm(standard_data2_good)

plot(ppoints(length(standard_data2_good)), sort(probDist2), main = "PP Good Wine", xlab = "Observed Probability", ylab = "Expected Probability")
abline(0,1)

standard_data2_bad <- (subset(data2, good==0) $pH - mean(data2$pH)) / sd(data2$pH)
probDist3 <- pnorm(standard_data2_bad)

plot(ppoints(length(standard_data2_bad)), sort(probDist3), main = "PP Bad Wine", xlab = "Observed Probability", ylab = "Expected Probability")
abline(0,1)

