#Data cleaning, analysis and modelling 

########### Xuehan #####################             
str(Satisfaction_Survey)
summary(Satisfaction_Survey)
# use summary() to view the distribution of the dataset.
# Then I found that there are four variables
# (Satisfaction, Departure Delay in Minutes,Arrival Delay in Minutes, Flight time in mintues) have NA value. 
# So we need to analyse these NA value to decide whether we need to remove them.
s <- Satisfaction_Survey
s$Number_of_years <- s$`Year of First Flight`-2003
s$`Flight date` <- NULL
s$`Airline Code` <- NULL
s$`Orgin City` <- NULL
s$`Origin State` <- NULL
s$`Destination City` <- NULL
s$`Destination State` <- NULL
s$`Scheduled Departure Hour` <- NULL
s$`Year of First Flight` <- NULL
# Removing NA values from the Satisfaction column
s <- s[-which(is.na(s$Satisfaction)),] # remove NA row 

# separate data to flight canceled and not cancelled.

s_cancelled <- subset(s, s$`Flight cancelled` == "Yes")
s_cancelled$`Departure Delay in Minutes` <- NULL
s_cancelled$`Flight time in minutes` <- NULL
s_cancelled$`Arrival Delay in Minutes` <- NULL
summary(s_cancelled)
#remove 3 variables in flight cancelled dataset:Departure Delay in Minutes, arrival Delay in Minutes,Flight time in minutes
s_not_cancelled <- subset(s, s$`Flight cancelled` == "No")
S_not_cancelled1 <- s_not_cancelled
s_not_cancelled <-s_not_cancelled[-which(is.na(s_not_cancelled$`Flight time in minutes`)),] 
s_not_cancelled <- s_not_cancelled[,-ncol(s_not_cancelled)+1]
summary(s_not_cancelled)

########### Advait #####################
# Creating boxplots to view outliers
#% of Flight with other Airlines
summary(s_not_cancelled$`% of Flight with other Airlines`)
boxplot(s_not_cancelled$`% of Flight with other Airlines`, main="Boxplot of % of Flight with other Airlines", ylab="Percentage")


#Shopping Amount at Airport
summary(s_not_cancelled$`Shopping Amount at Airport`)
boxplot(s_not_cancelled$`Shopping Amount at Airport`, ylim=c(0,2), main="Boxplot of Shopping amount at Airport", ylab="US Dollars")


#Eating and Drinking at Airport
summary(s_not_cancelled$`Eating and Drinking at Airport`)
boxplot(s_not_cancelled$`Eating and Drinking at Airport`, main="Boxplot of eating and drinking at Airport", ylab="US Dollars")

#Arrival Delay in Minutes
summary(s_not_cancelled$`Arrival Delay in Minutes`)
boxplot(s_not_cancelled$`Arrival Delay in Minutes`, main="Boxplot of Arrival delay in minutes", ylab="Minutes")

#Number of other Loyalty cards
summary(s_not_cancelled$`No. of other Loyalty Cards`)
boxplot(s_not_cancelled$`No. of other Loyalty Cards`, main="Boxplot of No. of other loyalty cards", ylab="Number of cards")

#Number of Flight distance
summary(s_not_cancelled$`Flight Distance`)
boxplot(s_not_cancelled$`Flight Distance`, main="Boxplot of Flight Distance", ylab="Miles")

#Number of years
summary(s_not_cancelled$Number_of_years)
boxplot(s_not_cancelled$Number_of_years, main="Boxplot of Number of years", ylab="Years")

#### Z-score Normalization ####
# select numeric variables
SZ <- s_not_cancelled 
SZ$Age <- as.numeric(paste(SZ$Age))
SZ$`Price Sensitivity` <- as.numeric(paste(SZ$`Price Sensitivity`))
SZ$`No of Flights p.a.` <- as.numeric(paste(SZ$`No of Flights p.a.`))
SZ$`No. of other Loyalty Cards` <- as.numeric(paste(SZ$`No. of other Loyalty Cards`))
SZ$`Departure Delay in Minutes` <- as.numeric(paste(SZ$`Departure Delay in Minutes`))
SZ$`Flight time in minutes` <- as.numeric(paste(SZ$`Flight time in minutes`))
SZ$`Flight Distance` <- as.numeric(paste(SZ$`Flight Distance`))
SZ$`% of Flight with other Airlines` <- as.numeric(paste(SZ$`% of Flight with other Airlines`))
SZ$`Arrival Delay in Minutes` <- as.numeric(paste(SZ$`Arrival Delay in Minutes`))
SZ$`Shopping Amount at Airport` <- as.numeric(paste(SZ$`Shopping Amount at Airport`))
SZ$`Eating and Drinking at Airport` <- as.numeric(paste(SZ$`Eating and Drinking at Airport`))

library("dplyr")
Z <- select_if(SZ, is.numeric)
Z$`Day of Month` <- NULL
Z <- scale(Z)
Z <- cbind(Z,select_if(SZ, is.character),select_if(SZ, is.integer))
# Z is the normalization result and use this dataset to build linear model

########## Removing Outliers ################
Final_Dataset <- Z

ul1 <- quantile(Final_Dataset$Age,0.975)
ll1 <- quantile(Final_Dataset$Age,0.025)
Z[Z$Age<ll1,]$Age <- ll1
Z[Z$Age>ul1,]$Age <- ul1

ul2 <- quantile(Final_Dataset$`Price Sensitivity`,0.975)
ll2 <- quantile(Final_Dataset$`Price Sensitivity`,0.025)
Z[Z$`Price Sensitivity`<ll2,]$`Price Sensitivity` <- ll2
Z[Z$`Price Sensitivity`>ul2,]$`Price Sensitivity` <- ul2

ul3 <- quantile(Final_Dataset$`No of Flights p.a.`,0.975)
ll3 <- quantile(Final_Dataset$`No of Flights p.a.`,0.025)
Z[Z$`No of Flights p.a.`<ll3,]$`No of Flights p.a.` <- ll3
Z[Z$`No of Flights p.a.`>ul3,]$`No of Flights p.a.` <- ul3

ul4 <- quantile(Final_Dataset$`No. of other Loyalty Cards`,0.975)
ll4 <- quantile(Final_Dataset$`No. of other Loyalty Cards`,0.025)
Z[Z$`No. of other Loyalty Cards`<ll4,]$`No. of other Loyalty Cards` <- ll4
Z[Z$`No. of other Loyalty Cards`>ul4,]$`No. of other Loyalty Cards` <- ul4

ul5 <- quantile(Final_Dataset$`Shopping Amount at Airport`,0.975)
ll5 <- quantile(Final_Dataset$`Shopping Amount at Airport`,0.025)
Z[Z$`Shopping Amount at Airport`<ll5,]$`Shopping Amount at Airport` <- ll5
Z[Z$`Shopping Amount at Airport`>ul5,]$`Shopping Amount at Airport` <- ul5

ul6 <- quantile(Final_Dataset$`Eating and Drinking at Airport`,0.975)
ll6 <- quantile(Final_Dataset$`Eating and Drinking at Airport`,0.025)
Z[Z$`Eating and Drinking at Airport`<ll6,]$`Eating and Drinking at Airport` <- ll6
Z[Z$`Eating and Drinking at Airport`>ul6,]$`Eating and Drinking at Airport` <- ul6

ul7 <- quantile(Final_Dataset$`Departure Delay in Minutes`,0.975)
ll7 <- quantile(Final_Dataset$`Departure Delay in Minutes`,0.025)
Z[Z$`Departure Delay in Minutes`<ll7,]$`Departure Delay in Minutes` <- ll7
Z[Z$`Departure Delay in Minutes`>ul7,]$`Departure Delay in Minutes` <- ul7

ul8 <- quantile(Final_Dataset$`Arrival Delay in Minutes`,0.975)
ll8 <- quantile(Final_Dataset$`Arrival Delay in Minutes`,0.025)
Z[Z$`Arrival Delay in Minutes`<ll8,]$`Arrival Delay in Minutes` <- ll8
Z[Z$`Arrival Delay in Minutes`>ul8,]$`Arrival Delay in Minutes` <- ul8

ul9 <- quantile(Final_Dataset$`Flight time in minutes`,0.975)
ll9 <- quantile(Final_Dataset$`Flight time in minutes`,0.025)
Z[Z$`Flight time in minutes`<ll9,]$`Flight time in minutes` <- ll9
Z[Z$`Flight time in minutes`>ul9,]$`Flight time in minutes` <- ul9

ul10 <- quantile(Final_Dataset$`Flight Distance`,0.975)
ll10 <- quantile(Final_Dataset$`Flight Distance`,0.025)
Z[Z$`Flight Distance`<ll10,]$`Flight Distance` <- ll10
Z[Z$`Flight Distance`>ul10,]$`Flight Distance` <- ul10

ul11 <- quantile(Final_Dataset$Number_of_years,0.975)
ll11 <- quantile(Final_Dataset$Number_of_years,0.025)
Z[Z$Number_of_years<ll11,]$Number_of_years <- ll11
Z[Z$Number_of_years>ul11,]$Number_of_years <- ul11

ul12 <- quantile(Final_Dataset$`% of Flight with other Airlines`,0.975)
ll12 <- quantile(Final_Dataset$`% of Flight with other Airlines`,0.025)
Z[Z$`% of Flight with other Airlines`<ll12,]$`% of Flight with other Airlines` <- ll12
Z[Z$`% of Flight with other Airlines`>ul12,]$`% of Flight with other Airlines` <- ul12

#### correlation chart####
str(s_not_cancelled)
install.packages("corrplot")
library(corrplot)
newdfs <- select_if(Z, is.numeric)
newdfs
newdfs$`Day of Month` <- NULL
str(newdfs)
col1 <- colorRampPalette(c("#002627","#084447","white","#0762a2","#29516d"))
res = cor(newdfs)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90, col=col1(200))


####### linear regression model  ##########
# We remove % of flight with other airlines, No. of other loyalty cards, Flight distance, departure delay in minutes due to multicollinearity issues as they are highly correlated with other explanatory variables

Z$`No. of other Loyalty Cards` <- NULL
Z$`Flight Distance` <- NULL
Z$`Departure Delay in Minutes` <- NULL
attach(Z)
jitter(Z$Age)
jitter(Z$`Price Sensitivity`)
jitter(Z$`Shopping Amount at Airport`)
jitter(Z$`Eating and Drinking at Airport`)
jitter(Z$`No of Flights p.a.`)
jitter(Z$`Flight time in minutes`)
jitter(Z$Number_of_years)
jitter(Z$`Day of Month`)
jitter(Z$`Arrival Delay in Minutes`)

l1 = lm(formula = Z$Satisfaction ~ Z$Age+Z$`Price Sensitivity`+Z$`Shopping Amount at Airport`+Z$`Eating and Drinking at Airport`+Z$`Day of Month`+Z$`Arrival Delay in Minutes`+Z$`Flight time in minutes`+Z$`No of Flights p.a.`+Z$`Airline Status`+Z$Gender+Z$`Type of Travel`+Z$Class+Z$`Airline Name`+Z$Number_of_years+Z$`% of Flight with other Airlines, data = Z)
summary(l1)

###### Optimizing the model by removing bad predictors ##############
# Removed eating and drinking at airport
# Removed flight time in minutes
# Removed day of month
# Removed % of flight with other airlines

l2 = lm(formula = Z$Satisfaction ~ Z$Age+Z$`Price Sensitivity`+Z$`Shopping Amount at Airport`+Z$`Arrival Delay in Minutes`+Z$`No of Flights p.a.`+Z$`Airline Status`+Z$Gender+Z$`Type of Travel`+Z$Class+Z$`Airline Name`+Z$Number_of_years, data = Z)
summary(l2)

# Removed airline names. However Adjusted R-squared goes down. So, keeping Airline names
l3 = lm(formula = Z$Satisfaction ~ Z$Age+Z$`Price Sensitivity`+Z$`Shopping Amount at Airport`+Z$`Arrival Delay in Minutes`+Z$`No of Flights p.a.`+Z$`Airline Status`+Z$Gender+Z$`Type of Travel`+Z$Class+Z$Number_of_years, data = Z)
summary(l3)






####Association rules####
C <- s_not_cancelled1
C$`Departure Delay in Minutes` <- NULL
C$`No. of other Loyalty Cards`<- NULL
C$`Flight Distance` <- NULL
C$`Day of Month` <- NULL
C$`Eating and Drinking at Airport` <- NULL
C$`Airline Name` <- NULL
C$`Flight cancelled` <- NULL
C$`Arrival Delay greater 5 Mins` <- NULL

C$Satisfaction[C$Satisfaction >= 4] <- 'Satisfaction'
C$Satisfaction[C$Satisfaction < 4 & C$Satisfaction >=3] <- 'Average'
C$Satisfaction[C$Satisfaction < 3] <- 'Dissatisfaction'
C$Satisfaction = as.factor(C$Satisfaction)

C$Age[C$Age >= 55] = 'Elder'
C$Age[C$Age > 30 & C$Age<=55 ] = 'MiddleAgedpeople'
C$Age[C$Age<= 30] = 'Youngpeople'

C$`Arrival Delay in Minutes`[C$`Arrival Delay in Minutes`>5]  ='delay'
C$`Arrival Delay in Minutes`[C$`Arrival Delay in Minutes`<=5] ='notdelay'

category <- function(vec){
  q <- quantile(vec, c(0.33, 0.67))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return(vBuckets)
}
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("grid")
#install.packages("digest")
library(arules)
library(arulesViz)
library(grid)

# Coerce the Survey data frame into a sparse transactions matrix :
C$Satisfaction <- as.factor(C$Satisfaction)
C$Age <- as.factor(C$Age)
C$`Price Sensitivity` <- as.factor(category(C$`Price Sensitivity`))
C$`No of Flights p.a.` <- as.factor(category(C$`No of Flights p.a.`))
C$`% of Flight with other Airlines` <- as.factor(category(C$`% of Flight with other Airlines`))
C$`Shopping Amount at Airport` <- as.factor(category(C$`Shopping Amount at Airport`))
C$`Arrival Delay in Minutes` <- as.factor(C$`Arrival Delay in Minutes`)
C$`Flight time in minutes`<- as.factor(category(C$`Flight time in minutes`))
C$`Airline Status`<- as.factor(C$`Airline Status`)
C$Gender <- as.factor(C$Gender)
C$`Type of Travel` <- as.factor(C$`Type of Travel`)
C$Class <- as.factor(C$Class)
C$Number_of_years <- as.factor(category(C$Number_of_years))
View(C)

CX <- as(C,"transactions")

itemFrequencyPlot(CX,support=0.05,cex.names=0.5)

#predict happy customers (as defined by their overall satisfaction >=4).
ruleset <- apriori(CX,parameter=list(support=0.1, confidence=0.3),appearance = list(rhs="Satisfaction=Satisfaction",default="lhs"))
plot(ruleset,jitter=0) 
goodrules <- ruleset[quality(ruleset)$lift > 1.5]
inspect(goodrules)
top.lift <- sort(goodrules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10))  




###############SVM################
###### not use z score  ########
library("dplyr")
S <- s_not_cancelled
dim(S)
S$Satisfaction[S$Satisfaction >= 4] <- 'Satisfaction'
S$Satisfaction[S$Satisfaction < 4] <- 'Dissatisfaction'



randomindex = sample(1:nrow(S))
cutpoint = floor(2*nrow(S)/3)
traindata = S[randomindex[1:cutpoint],]
testdata = S[randomindex[(cutpoint+1):nrow(S)],]
dim(traindata)
str(S)
#install.packages("kernlab")
library(kernlab)
svmOutput <- ksvm(Satisfaction ~Class+`Type of Travel`+`Airline Status`+Age+`Arrival Delay in Minutes`+`Price Sensitivity`+`Shopping Amount at Airport`+Gender,data=traindata, kernel= "rbfdot", kpar = "automatic", C = 50, cross = 3, prob.model = TRUE)
svmOutput
svmPred <- predict(svmOutput, testdata, type = "votes")
dim(svmPred)
dim(testdata)
compTable <- data.frame(data.frame(testdata$Satisfaction=="happy",svmPred[1,]))
table(compTable)
res <- table(compTable)
#Calculate an error rate
errorRate <- (res[1,1]+res[2,2])/(sum(res))
errorRate




##################ggplot
#install.packages("ggplot2")
library(ggplot2)
as <- s_not_cancelled
######3 level for satisfaction
as$Satisfaction[as$Satisfaction >= 4] <- 'Satisfaction'
as$Satisfaction[as$Satisfaction < 4 & as$Satisfaction >=3 ] <- 'Average'
as$Satisfaction[as$Satisfaction < 3] <- 'Dissatisfaction'
as$Satisfaction = as.factor(as$Satisfaction)

######5 level for satisfaction
#as$Satisfaction <- factor(ceiling(as$Satisfaction))    for 5 color coding

########GGplot Tasks

# Airline Status
g <- ggplot(as, aes(as$`Airline Status`))
g + geom_bar(aes(fill=as$Satisfaction), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="satisfaction analysis", 
       subtitle="Airline Status")

# Type of Travel
g <- ggplot(as, aes(as$`Type of Travel`))
g + geom_bar(aes(fill=as$Satisfaction), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="satisfaction analysis", 
       subtitle="Type of Travel")



# Gender
g <- ggplot(as, aes(Gender))
g + geom_bar(aes(fill=Satisfaction), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="satisfaction analysis", 
       subtitle="Gender")

# Airline Status
g <- ggplot(as, aes(`Airline Status`))
g + geom_bar(aes(fill=Satisfaction), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="satisfaction analysis", 
       subtitle="Airline Status")

# Type of Travel
g <- ggplot(as, aes(`Type of Travel`))
g + geom_bar(aes(fill=Satisfaction), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="satisfaction analysis", 
       subtitle="Type of Travel")

# Price Sensitivity
g <- ggplot(as, aes(`Price Sensitivity`))
g + geom_bar(aes(fill=Satisfaction), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="satisfaction analysis", 
       subtitle="Price Sensitivity")



# Airline Name
#install.packages('ggthemes')
library(ggthemes)
options(scipen = 999)  # turns of scientific notations like 1e+40
# X Axis Breaks and Labels 
brks <- seq(-15000000, 15000000, 5000000)
lbls = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")
# Plot
freqtable <- table(s_not_cancelled$`Airline Name`)
freq <-as.data.frame(sort(freqtable))
s_not_cancelled$`Airline Name` <- factor(s_not_cancelled$`Airline Name`, levels = freq$Var1)
satisfaction <- as$Satisfaction
ggplot(s_not_cancelled, aes(x = `Airline Name`, y = Satisfaction, fill = satisfaction)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes
  labs(title="Airline Company") +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")  # Color palette



####Age and Satisfaction
ageS <- ggplot(as, aes(Age)) + scale_fill_manual(values=c("Satisfaction"="#DFE1E3", "Dissatisfaction"="#798E9E", "Average"="#02353F"))

ageS + geom_histogram(aes(fill=Satisfaction), binwidth = 4, col="black", size=1) +ggtitle("Satisfaction vs Age")
ageS + geom_histogram(aes(fill=Satisfaction), bins=5, col="black", size=.1) +ggtitle("Satisfaction vs Age")

#####arrival delay vs Satisfaction##############

adS <- ggplot(as, aes(`Arrival Delay in Minutes`)) + scale_fill_brewer()
adS + geom_histogram(aes(fill=Satisfaction), binwidth = 20, col="black", size=1) +ggtitle("Satisfaction vs Arrival delay in Minutes")
adS + geom_histogram(aes(fill=Satisfaction), bins=10, col="black", size=.1) +ggtitle("Satisfaction vs Arrival delay in Minutes")


Ardy96 <- quantile(as$`Arrival Delay in Minutes`,0.96)
Sati_Ardy <- ggplot(as[as$`Arrival Delay in Minutes`<Ardy96,],aes(x=`Arrival Delay in Minutes`))
Sati_Ardy <- Sati_Ardy + geom_histogram(aes(fill=Satisfaction),position = "dodge", bins = 30)
Sati_Ardy <- Sati_Ardy + ggtitle("Satisfaction versus Arrival Delay")
Sati_Ardy


###########class and satisfaction
as$Class <- factor(as$Class, levels = c("Eco","Eco Plus","Business"))


cs <- ggplot(as, aes(Class))
cs + geom_bar(aes(fill=Satisfaction), col="black", width = 0.6) + theme(axis.text.x = element_text(angle=0, vjust=0.6)) + scale_fill_manual(values=c("Average"="#DFE1E3", "Satisfaction"="#798E9E", "Dissatisfaction"="#02353F"))

#############Day of month######
as1 = s_not_cancelled
as1$Satisfaction = as.factor(as1$Satisfaction)
dss <- ggplot(as1, aes(`Day of Month`)) +  scale_fill_brewer(palette = "Spectral")

dss + geom_histogram(aes(fill=Satisfaction), binwidth = 1, col="black", size=1) +ggtitle("Satisfaction vs Day of Month")

#############shopping

ss <- ggplot(as, aes(`Shopping Amount at Airport`)) +  scale_fill_brewer(palette = "Spectral")

ss + geom_histogram(aes(fill=Satisfaction), binwidth = 4, col="black", size=1) +ggtitle("Satisfaction vs Day of Shopping Amount")



##########decision tree######
dt = s_not_cancelled
dt$Satisfaction[dt$Satisfaction >= 4] <- 'Satisfaction'
dt$Satisfaction[dt$Satisfaction < 4 & dt$Satisfaction >=3 ] <- 'Average'
dt$Satisfaction[dt$Satisfaction <= 2] <- 'Dissatisfaction'
set.seed(2000)
install.packages("rpart")
library(rpart)
randomindex1 = sample(1:nrow(dt), size = 10000)
cutpoint1 = floor(2*10000/3)
traindata1 = dt[randomindex1[1:cutpoint1],]
testdata1 = dt[cutpoint1+1:10000,]
dtree<-rpart(Satisfaction ~ Class +`Type of Travel`+`Airline Status`+ Age+ `Arrival Delay in Minutes` +`Price Sensitivity`+`Shopping Amount at Airport`+Gender
             ,data=traindata1, method="class", parms=list(split="information"))
#Class+`Type of Travel`+`Airline Status`+Age+`Arrival Delay in Minutes`+`Price Sensitivity`+`Shopping Amount at Airport`+Gender
printcp(dtree)
tree<-prune(dtree,cp=0.0125)
opar<-par(no.readonly = T)
par(mfrow=c(1,2))
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(dtree,branch=1,type=4, fallen.leaves=T,cex=0.7, sub="Decision Tree for Airline Satisfaction")
rpart.plot(tree,branch=1, type=4,fallen.leaves=T,cex=0.7, sub="Decision Tree for Airline Satisfaction")
par(opar)
dev.off()
predtree<-predict(tree,newdata=testdata1,type="class")   
table(testdata1$Satisfaction,predtree,dnn=c("real one","predict one"))    






