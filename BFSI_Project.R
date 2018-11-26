#Saptarshi_Banerjee_Group_Elect_BFSI_Capstone_Project
#Set working Directory

#--**--**CredX-Acquisition Analytics**--**--

#loading libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)

#loading all the files and data understanding, since the file has blanks : replaccing as NA
demographic <- read.csv("Demographic_data.csv", na.strings="", stringsAsFactors = FALSE)
creditBureau <- read.csv("Credit_Bureau_data.csv", na.strings="", stringsAsFactors = FALSE)

# Checking structure of demographic dataset 
summary(demographic)
str(demographic) # 71295 obs. of  12 variables:

# Checking structure of creditBureau data dataset 
summary(creditBureau)
str(creditBureau) #71295 obs. of  19 variables:

# check if Application.ID is indeed a primary key or not

setdiff(demographic$Application.ID,creditBureau$Application.ID) # integer(0)

#-----Removing Duplicates----
#removing duplicate application ids from the data sets
#checking with rows are duplicated
demographic[duplicated(demographic$Application.ID),]
creditBureau[duplicated(creditBureau$Application.ID),]
#Same data sets have the application id being duplicated (i.e. 765011468, 653287861 and 671989187)
#for e.g. lets take App id 765011468 - belongs to a 57 yrs old male and to a 38 year old female.
#Since we do not know whose data corresponds in the Credit Bureau data set. 
#imputing these three application id's 'completely' from both data sets

demographic <- demographic[which(demographic$Application.ID != 765011468),]
demographic <- demographic[which(demographic$Application.ID != 653287861),]
demographic <- demographic[which(demographic$Application.ID != 671989187),]

creditBureau <- creditBureau[which(creditBureau$Application.ID != 765011468),]
creditBureau <- creditBureau[which(creditBureau$Application.ID != 653287861),]
creditBureau <- creditBureau[which(creditBureau$Application.ID != 671989187),]

#all duplicated rows have been removed
demographic[duplicated(demographic$Application.ID),] # 0 rows
creditBureau[duplicated(creditBureau$Application.ID),] # 0 rows

#checking the no. of rows in each dataset

nrow(demographic) #71289
nrow(creditBureau) #71289

#merging the demographic and credit bureau datasets

customerData <- merge(creditBureau, demographic, by = "Application.ID")

nrow(customerData) #71289
ncol(customerData) #30 # merge has happened correctly

#checking if performance tags are the same
index_Diff_perf <-customerData[which(customerData$Performance.Tag.y != customerData$Performance.Tag.x),]
index_Diff_perf # all performance tags are the same hence imputing Performance.Tag.x column
customerData <- customerData[,-19]

#****Data Cleaning****
#1. Performance tag
summary(customerData$Performance.Tag.y) # 1425 rows have NA's in performance tag, assumption this is rejected population
customerData_subset_NA_Target <- customerData[is.na(customerData$Performance.Tag.y),]#saving the NA_perf tag in a seperate Date Frame
customerData <- customerData[!is.na(customerData$Performance.Tag.y),]
summary(customerData$Performance.Tag.y) #all NA's in Performance tag have been imputed

#default rate is 2947/(2947+66917) = 0.04218195

###################################################################################################

colnames(customerData)[29] <- "performance"

write.csv(customerData, "customerData1.csv", row.names = FALSE)
write.csv(customerData_subset_NA_Target, "customerData_na_subset.csv", row.names = FALSE)

#___________________
#******Data Cleaning & EDA*****************

customerData <- read.csv("customerData1.csv")
str(customerData)

#deduplication-not required, already checked

##################################################################################################

# Treating invalid values, any variable having more than 15% of data points
# missing is not eligible for imputation hence it makes sense to compute
# and drop those variables

missing_values <- customerData %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='red') +
  coord_flip()

#Since the no of NA / Missing values are only less than 1 % in Gender, No.of.Dependents,, Marital status, Residence, Profession - Imputing the same  
#Education has 1.6 % NA / Missing values - Imputing the same  

# Writing a function "plot_response" to do the same task for each variable

plot_performance <- function(cat_var, var_name){
  a <- aggregate(performance~cat_var, customerData, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_performance <- cbind(a, count)
  
  colnames(agg_performance) <- c(var_name, "performance_rate","No.of_Prospect")
  agg_performance[, 2] <- format(round(agg_performance[, 2], 3))
  
  ggplot(agg_performance, aes(agg_performance[, 1], count, label = performance_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

#________________________________________________________________________________
#******Numerical Variables - Missing Value and outlier Treatment********

#imputing missing values
abcd<-as.data.frame(sapply(customerData, function(x) sum(is.na(x))))
abcd

#Variable - Income (numeric) : 25 vales at 0 and 79 at -0.5 : Imputing them
table(customerData$Income,customerData$performance)
summary(customerData$Income)
customerData$Income[which(customerData$Income ==-0.5 )]<-0
customerData <- customerData[!customerData$Income ==0,] #25 vales at 0 and 79 at -0.5 : Imputing them

boxplot(customerData$Income)
ggplot(customerData,aes(Income))+geom_histogram()
quantile(customerData$Income, seq(0,1,0.01)) # no Outliers

#Variable - Avgas.CC.Utilization.in.last.12.months - has 1021 NA - No action - # business problem indicates 
# that na's indicate that person does not have a CC, replacing the NA values with Zero
customerData$Avgas.CC.Utilization.in.last.12.months [which(is.na(customerData$Avgas.CC.Utilization.in.last.12.months))]<-0

table(customerData$Avgas.CC.Utilization.in.last.12.months,customerData$performance)
summary(customerData$Avgas.CC.Utilization.in.last.12.months)

boxplot(customerData$Avgas.CC.Utilization.in.last.12.months) # data has outliers on the higher slide
ggplot(customerData,aes(Avgas.CC.Utilization.in.last.12.months))+geom_histogram() # data is very unevenly distributed accross x

plot_performance(customerData$Avgas.CC.Utilization.in.last.12.months, "Avgas.CC.Utilization.in.last.12.months")

# Outlier treatment capping at 95% - 99
quantile(customerData$Avgas.CC.Utilization.in.last.12.months, seq(0,1,0.01), na.rm = TRUE)
customerData[(which(customerData$Avgas.CC.Utilization.in.last.12.months>99)),]$Avgas.CC.Utilization.in.last.12.months <- 99
# Zero trades opened has lowest defaults, as the no of trades increases, so does do the defaults

#Variable - No.of.months.in.current.residence (numeric)
table(customerData$No.of.months.in.current.residence,customerData$performance)
summary(customerData$No.of.months.in.current.residence)

boxplot(customerData$No.of.months.in.current.residence)
ggplot(customerData,aes(No.of.months.in.current.residence))+geom_histogram() # data is highly squeued in 1st bucket
quantile(customerData$No.of.months.in.current.residence, seq(0,1,0.01)) # data has not outliers

#Variable - No.of.months.in.current.company (numeric)
table(customerData$No.of.months.in.current.company,customerData$performance)
summary(customerData$No.of.months.in.current.company)

boxplot(customerData$No.of.months.in.current.company)
ggplot(customerData,aes(No.of.months.in.current.company))+geom_histogram()
quantile(customerData$No.of.months.in.current.company, seq(0,1,0.01))
# So, capping the No.of.months.in.current.company at 99% ile = 74
customerData[(which(customerData$No.of.months.in.current.company>74)),]$No.of.months.in.current.company <- 74

# Outstanding.Balance (numeric) : has 272 na values : assuming that the NA's indicate no outstanding balance
#These 272 are common in Presence.of.open.home.loan, Avgas.CC.Utilization.in.last.12.months & Outstanding.Balance : Hence imputing them

table(customerData$Outstanding.Balance,customerData$performance)
summary(customerData$Outstanding.Balance)
customerData$Outstanding.Balance [which(is.na(customerData$Outstanding.Balance))]<-0

boxplot(customerData$Outstanding.Balance)
ggplot(customerData,aes(Outstanding.Balance))+geom_histogram(bins = 10)
quantile(customerData$Outstanding.Balance, seq(0,1,0.01), na.rm = TRUE)

# So, capping the Outstanding.Balance at 97% ile = 3857929.00
customerData[(which(customerData$Outstanding.Balance>3857929.00)),]$Outstanding.Balance <- 3857929.00

#Variable - Total.No.of.Trades (numeric)
table(customerData$Total.No.of.Trades,customerData$performance)
summary(customerData$Total.No.of.Trades)

boxplot(customerData$Total.No.of.Trades)
ggplot(customerData,aes(Total.No.of.Trades))+geom_histogram()

# So, capping the Total.No.of.Trades at 90% ile = 20
quantile(customerData$Total.No.of.Trades, seq(0,1,0.01), na.rm = TRUE)
customerData[(which(customerData$Total.No.of.Trades>20)),]$Total.No.of.Trades <- 20

# Variable - Age (numeric) - # there are 19 values with age 0 and one with -3 and 23 NA Values
table(customerData$Age, customerData$performance)
summary(customerData$Age)

customerData$Age[which(customerData$Age ==-3 )]<-0
customerData <- customerData[!customerData$Age ==0,] 

boxplot(customerData$Age)
ggplot(customerData,aes(Age))+geom_histogram()

# Let's check the outlier in the variables 
quantile(customerData$Age,seq(0,1,0.01)) # no outliers

# Binning the age variable and store it into "binning.age".
#customerData$binning.age <- as.factor(cut(customerData$Age, breaks = c(14, 30,35,40,45, 50, 55,70)))
#we can also bin as c(14,35,40,45, 50, 55,70))) Q is are we ready to sque our x because our Y is so 

#_____Correlation Plot for Numeric Variables_____

library(corrplot)
corr_df <-customerData[,c(8,16,17,19,23,27,28,29)]
corrs = round(cor(corr_df, use = "pairwise.complete.obs"), 2)
corrplot(corrs, method = "color", type = "lower", order = "FPC", tl.cex=0.6 )

#_____________________________________________________________________________________________
#*****************Categorical Variables**************

# Education - 113 NA's : 5 had defaulted : replacing with "others"
# Checking the levels of the Education
table(customerData$Education,customerData$performance)
plot_performance(customerData$Education, "Education")
customerData$Education[which(is.na(customerData$Education))] <- "Others" # clubbing the 118 NA's with others

# Gender - has 2 NA's : None Defaulted : Imputed 2
table(customerData$Gender,customerData$performance)
summary(customerData$Gender)
customerData <- customerData[!is.na(customerData$Gender),] # removing the NA's
plot_performance(customerData$Gender, "Gender")

# Marital.Status has 5 NA's : None Defaulted : Imputed 5
colnames(customerData)[21] <- "Marital.Status" # Shorterning the Colname
table(customerData$Marital.Status,customerData$performance)
summary(customerData$Marital.Status)
customerData <- customerData[!is.na(customerData$Marital.Status),] # removing the NA's
plot_performance(customerData$Marital.Status, "Marital.Status")

# No.of.dependents has 2 NA's : None Defaulted : Imputed 2
table(customerData$No.of.dependents,customerData$performance)
summary(customerData$No.of.dependents)
customerData <- customerData[!is.na(customerData$No.of.dependents),] # removing the 2 NA's
plot_performance(customerData$No.of.dependents, "No.of.dependents")

# Profession has 13 NA : None Defaulted - Imputing 13
table(customerData$Profession,customerData$performance)
summary(customerData$Profession)
customerData <- customerData[!is.na(customerData$Profession),] # removing the 13 NA's
plot_performance(customerData$Profession, "Profession")

# Type.of.residence has 8 na's : None defaulted - Clubbing with "Others"
table(customerData$Type.of.residence,customerData$performance)
summary(customerData$Type.of.residence)
customerData$Type.of.residence[which(is.na(customerData$Type.of.residence))] <- "Others" # clubbing the 8 NA's with others

plot_performance(customerData$Type.of.residence, "Type.of.residence")

#_____________________________________________________
# Presence.of.open.auto.loan 
table(customerData$Presence.of.open.auto.loan,customerData$performance)
summary(customerData$Presence.of.open.auto.loan)
plot_performance(customerData$Presence.of.open.auto.loan, "Presence.of.open.auto.loan")
# having an Auto loan significantly reduced the chances of default

# Presence.of.open.home.loan has 272 na's : Indicate that they do not have Home loans
#replacing the NA values with Zero : Zero is also the mode of the class
customerData$Presence.of.open.home.loan [which(is.na(customerData$Presence.of.open.home.loan))]<-0
table(customerData$Presence.of.open.home.loan,customerData$performance)
summary(customerData$Presence.of.open.home.loan)
plot_performance(customerData$Presence.of.open.home.loan, "Presence.of.open.home.loan")
# having a Home loan significantly reduced the chances of default

# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
table(customerData$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,customerData$performance)
summary(customerData$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
# So, capping the No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. at 98% ile = 13
quantile(customerData$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., seq(0,1,0.01), na.rm = TRUE)
customerData[(which(customerData$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>13)),]$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 13

plot_performance(customerData$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")
# Predictably people with Zero inquiries have lowest defaults, as the no of inquiries increases, so does do
# the defaults

# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
table(customerData$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,customerData$performance)
summary(customerData$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)

# So, capping the No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. at 98% ile = 7
quantile(customerData$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., seq(0,1,0.01), na.rm = TRUE)
customerData[(which(customerData$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>7)),]$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 7

plot_performance(customerData$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
# Predictably people with Zero inquiries have lowest defaults, as the no of inquiries increases, so do
# the defaults

# No.of.PL.trades.opened.in.last.6.months
table(customerData$No.of.PL.trades.opened.in.last.6.months,customerData$performance)
summary(customerData$No.of.PL.trades.opened.in.last.6.months)
# So, capping the No.of.PL.trades.opened.in.last.6.months at 99% ile = 5
quantile(customerData$No.of.PL.trades.opened.in.last.6.months, seq(0,1,0.01), na.rm = TRUE)
customerData[(which(customerData$No.of.PL.trades.opened.in.last.6.months>5)),]$No.of.PL.trades.opened.in.last.6.months <- 5

plot_performance(customerData$No.of.PL.trades.opened.in.last.6.months, "No.of.PL.trades.opened.in.last.6.months")
# Zero trades opened has lowest defaults, as the no of PL trades increases, so does do the defaults

# No.of.trades.PL.opened.in.last.12.months
table(customerData$No.of.PL.trades.opened.in.last.12.months,customerData$performance)
summary(customerData$No.of.PL.trades.opened.in.last.12.months)
# So, capping the No.of.PL.trades.opened.in.last.6.months at 98% ile = 8
quantile(customerData$No.of.PL.trades.opened.in.last.12.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.PL.trades.opened.in.last.12.months>8)),]$No.of.PL.trades.opened.in.last.12.months <- 8

plot_performance(customerData$No.of.PL.trades.opened.in.last.12.months, "No.of.PL.trades.opened.in.last.12.months")
# Zero trades opened has lowest defaults, as the no of PL trades increases, so does do the defaults

# No.of.trades.opened.in.last.12.months
table(customerData$No.of.trades.opened.in.last.12.months,customerData$performance)
summary(customerData$No.of.trades.opened.in.last.12.months)
# So, capping the No.of.trades.opened.in.last.12.months at 98% ile = 19
quantile(customerData$No.of.trades.opened.in.last.12.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.trades.opened.in.last.12.months>19)),]$No.of.trades.opened.in.last.12.months <- 19

plot_performance(customerData$No.of.trades.opened.in.last.12.months, "No.of.trades.opened.in.last.12.months")
# As the no of trades increases, so does do the defaults

# No.of.trades.opened.in.last.6.months : Has one NA -  imputing
table(customerData$No.of.trades.opened.in.last.6.months,customerData$performance)
summary(customerData$No.of.trades.opened.in.last.6.months)
customerData <- customerData[!is.na(customerData$No.of.trades.opened.in.last.6.months),] # removing the 1 NA
# So, capping the No.of.trades.opened.in.last.6.months at 94% ile = 6
quantile(customerData$No.of.trades.opened.in.last.6.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.trades.opened.in.last.6.months>6)),]$No.of.trades.opened.in.last.6.months <- 6

plot_performance(customerData$No.of.trades.opened.in.last.6.months, "No.of.trades.opened.in.last.6.months")
# Zero trades opened has lowest defaults, as the no of trades increases, so does do the defaults

#No.of.times.90.DPD.or.worse.in.last.12.months
table(customerData$No.of.times.90.DPD.or.worse.in.last.12.months,customerData$performance)
summary(customerData$No.of.times.90.DPD.or.worse.in.last.12.months)

# So, capping the No.of.times.90.DPD.or.worse.in.last.12.months at 98% ile = 3
quantile(customerData$No.of.times.90.DPD.or.worse.in.last.12.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.times.90.DPD.or.worse.in.last.12.months>3)),]$No.of.times.90.DPD.or.worse.in.last.12.months <- 3

plot_performance(customerData$No.of.times.90.DPD.or.worse.in.last.12.months, "No.of.times.90.DPD.or.worse.in.last.12.months")
# Highy correlated

# No.of.times.60.DPD.or.worse.in.last.12.months
table(customerData$No.of.times.60.DPD.or.worse.in.last.12.months,customerData$performance)
summary(customerData$No.of.times.60.DPD.or.worse.in.last.12.months)

# So, capping the No.of.times.60.DPD.or.worse.in.last.12.months at 97% ile = 3
quantile(customerData$No.of.times.60.DPD.or.worse.in.last.12.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.times.60.DPD.or.worse.in.last.12.months>3)),]$No.of.times.60.DPD.or.worse.in.last.12.months <- 3

plot_performance(customerData$No.of.times.60.DPD.or.worse.in.last.12.months, "No.of.times.60.DPD.or.worse.in.last.12.months")
# Highy correlated

# No.of.times.30.DPD.or.worse.in.last.12.months
table(customerData$No.of.times.30.DPD.or.worse.in.last.12.months,customerData$performance)
summary(customerData$No.of.times.30.DPD.or.worse.in.last.12.months)

# So, capping the No.of.times.30.DPD.or.worse.in.last.12.months at 95% ile = 3
quantile(customerData$No.of.times.30.DPD.or.worse.in.last.12.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.times.30.DPD.or.worse.in.last.12.months>3)),]$No.of.times.30.DPD.or.worse.in.last.12.months <- 3

plot_performance(customerData$No.of.times.30.DPD.or.worse.in.last.12.months, "No.of.times.30.DPD.or.worse.in.last.12.months")
# Highy correlated

# No.of.times.30.DPD.or.worse.in.last.6.months
table(customerData$No.of.times.30.DPD.or.worse.in.last.6.months,customerData$performance)
summary(customerData$No.of.times.30.DPD.or.worse.in.last.6.months)

# So, capping the No.of.times.30.DPD.or.worse.in.last.6.months at 95% ile = 3
quantile(customerData$No.of.times.30.DPD.or.worse.in.last.6.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.times.30.DPD.or.worse.in.last.6.months>3)),]$No.of.times.30.DPD.or.worse.in.last.6.months <- 3

plot_performance(customerData$No.of.times.30.DPD.or.worse.in.last.6.months, "No.of.times.30.DPD.or.worse.in.last.6.months")
# Highy correlated

# No.of.times.60.DPD.or.worse.in.last.6.months
table(customerData$No.of.times.60.DPD.or.worse.in.last.6.months,customerData$performance)
summary(customerData$No.of.times.60.DPD.or.worse.in.last.6.months)

# So, capping the No.of.times.60.DPD.or.worse.in.last.6.months at 98% ile = 3
quantile(customerData$No.of.times.60.DPD.or.worse.in.last.6.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.times.60.DPD.or.worse.in.last.6.months>3)),]$No.of.times.60.DPD.or.worse.in.last.6.months <- 3

plot_performance(customerData$No.of.times.60.DPD.or.worse.in.last.6.months, "No.of.times.60.DPD.or.worse.in.last.6.months")
# Highy correlated

# No.of.times.90.DPD.or.worse.in.last.6.months
table(customerData$No.of.times.90.DPD.or.worse.in.last.6.months,customerData$performance)
summary(customerData$No.of.times.90.DPD.or.worse.in.last.6.months)

# So, capping the No.of.times.90.DPD.or.worse.in.last.6.months at 97% ile = 1
quantile(customerData$No.of.times.90.DPD.or.worse.in.last.6.months, seq(0,1,0.01))
customerData[(which(customerData$No.of.times.90.DPD.or.worse.in.last.6.months>1)),]$No.of.times.90.DPD.or.worse.in.last.6.months <- 1

plot_performance(customerData$No.of.times.90.DPD.or.worse.in.last.6.months, "No.of.times.90.DPD.or.worse.in.last.6.months")
# Highy correlated

#_____________________________________________________

#checking for NA values again
abcd<-as.data.frame(sapply(customerData, function(x) sum(is.na(x))))
abcd

#writing the files to ensure that we have a back up : These are base files for later models

write.csv(customerData, "custDataF.csv", row.names = FALSE)
customerData <-read.csv("custDataF.csv", stringsAsFactors = FALSE)

#--------------Data Prep for Logistic Regression---------
#Normalising Continuous variables
customerData$No.of.times.90.DPD.or.worse.in.last.6.months <- scale(customerData$No.of.times.90.DPD.or.worse.in.last.6.months)
customerData$No.of.times.60.DPD.or.worse.in.last.6.months <- scale(customerData$No.of.times.60.DPD.or.worse.in.last.6.months)
customerData$No.of.times.30.DPD.or.worse.in.last.6.months <- scale(customerData$No.of.times.30.DPD.or.worse.in.last.6.months)
customerData$No.of.times.90.DPD.or.worse.in.last.12.months <- scale(customerData$No.of.times.90.DPD.or.worse.in.last.12.months)
customerData$No.of.times.60.DPD.or.worse.in.last.12.months <- scale(customerData$No.of.times.60.DPD.or.worse.in.last.12.months)
customerData$No.of.times.30.DPD.or.worse.in.last.12.months <- scale(customerData$No.of.times.30.DPD.or.worse.in.last.12.months)
customerData$Avgas.CC.Utilization.in.last.12.months <- scale(customerData$Avgas.CC.Utilization.in.last.12.months)
customerData$No.of.trades.opened.in.last.6.months <- scale(customerData$No.of.trades.opened.in.last.6.months)
customerData$No.of.trades.opened.in.last.12.months <- scale(customerData$No.of.trades.opened.in.last.12.months)
customerData$No.of.PL.trades.opened.in.last.6.months <- scale(customerData$No.of.PL.trades.opened.in.last.6.months)
customerData$No.of.PL.trades.opened.in.last.12.months <- scale(customerData$No.of.PL.trades.opened.in.last.12.months)
customerData$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- scale(customerData$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
customerData$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- scale(customerData$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
customerData$Outstanding.Balance <- scale(customerData$Outstanding.Balance)
customerData$Total.No.of.Trades <- scale(customerData$Total.No.of.Trades)
customerData$Age <- scale(customerData$Age)
customerData$No.of.dependents <- scale(customerData$No.of.dependents)
customerData$Income <- scale(customerData$Income)
customerData$No.of.months.in.current.residence <- scale(customerData$No.of.months.in.current.residence)
customerData$No.of.months.in.current.company <- scale(customerData$No.of.months.in.current.company)

#creating a dataframe of categorical variables
customerData_cat <- customerData[ , c(20,21,24,25,26)]
#converting categorical to factor
customerData_cat <-data.frame(sapply(customerData_cat, function(x) factor(x)))
str(customerData_cat)
#creating dummy variable for factor attributes
dummies <- data.frame(sapply(customerData_cat, function(x) data.frame(model.matrix(~x-1,data=customerData_cat))[,-1]))

#final data set for Logistic Regression
customerData_final <- cbind(customerData[ , -c(20,21,24,25,26)], dummies)
View(customerData_final) # 69717 obs of 36 variables

#Splitting the data between train and test
library(caTools)
set.seed(100)
indices <- sample.split(customerData_final$performance, SplitRatio = 0.7)
train <- customerData_final[indices,]
test <- customerData_final[!(indices),]

#________________________logistic Regression______________
# initial model
model_1 <-glm(performance~.,data=train, family="binomial")
summary(model_1) # AIC = 16460...36 coeff...nullDev 17084....resDev16388

#Stepwise selection
library(MASS)
model_2 <-stepAIC(model_1, direction = "both") 
summary(model_2) # AIC 16435.58...15 coeff...nullDev 17084....resDev16388
library(car)
vif(model_2)

#removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.: VIF 4.5 & pvalue 0.055906

model_3 <- glm(formula = performance ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + Total.No.of.Trades + Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Gender + Education.xMasters, 
               family = "binomial", data = train)
summary(model_3) # AIC 16437...14 coeff...nullDev 17084....resDev16409
vif(model_3)

# removing No.of.PL.trades.opened.in.last.6.months

model_4 <- glm(formula = performance ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + Total.No.of.Trades + Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Gender + Education.xMasters, 
               family = "binomial", data = train)
summary(model_4)  # AIC 16443...14 coeff...nullDev 17084....resDev16417
vif(model_4)

# removing Total.No.of.Trades

model_5 <- glm(formula = performance ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Gender + Education.xMasters, 
               family = "binomial", data = train)
summary(model_5)  # AIC 16448...13 coeff...nullDev 17084....resDev16424
vif(model_5) # VIF for all variables is less tha 2.74, hence will only look at pvalues now

# removing Gender

model_6 <- glm(formula = performance ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company + Education.xMasters, 
               family = "binomial", data = train)
summary(model_6)  # AIC 16448...12 coeff...nullDev 17084....resDev16426

# removing Education.xMasters

model_7 <- glm(formula = performance ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + Income + No.of.months.in.current.residence + 
                 No.of.months.in.current.company, 
               family = "binomial", data = train)
summary(model_7)  # AIC 16448...09 coeff...nullDev 17084....resDev16428

# removing No.of.months.in.current.residence

model_8 <- glm(formula = performance ~ No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + Income + 
                 No.of.months.in.current.company, 
               family = "binomial", data = train)
summary(model_8)  # AIC 16450...08 coeff...nullDev 17084....resDev16432

# Taking model_8 as the final model as all coeff have a P-value of less than 0.05
final_model <- model_8

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(final_model, newdata = test[, -1], type = "response")
summary(predictions_logit)
quantile(predictions_logit, seq(0,1,0.01))
#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 0.036.

predicted_response <- factor(ifelse(predictions_logit >= 0.036, "yes", "no"))
test$response <- factor(ifelse(test$performance == 1, "yes", "no"))

summary(predicted_response)
summary(test$response)
# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response , positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.014 to 0.96 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.014,0.096,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.014,0.096,length=5),seq(0.014,0.96,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff # 0.04464646

# Let's choose a cutoff value of 12% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.04464646, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc #0.6231413
sens #0.617214 
spec #0.6234026 

# ROC study
library(pROC)
auc <- roc(test$response, predictions_logit)
print(auc) #Area under the curve : 0.6704
plot(roc(test$response, predictions_logit, direction="<"),
     col="yellow", lwd=3, main="Logistic Regression ROC")

#Application Score Card
PDO<-20
BaseScore<-400
Odds<-10
 
#Calculating Factor & Offset
 
Factor=PDO/log(2)
Offset=BaseScore-(Factor*log(Odds))
Offset # 333.5614
Factor # 28.8539

test$prob <- predictions_logit # saving the predictions (probabity of Good) from GLM on the test data
summary(test$prob) # probability has a very narrow range
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01446 0.02203 0.03638 0.04235 0.05522 0.16173 
write.csv(test, "test.csv", row.names = FALSE)
test$score <- 333.5614 + (28.8539 * log(test$prob/(1-test$prob))) # Calc the risk score
summary(test$score) # scores gave a very narrow range
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#211.7   224.1   239.0   239.2   251.6   286.1 
test$score [which(test$prob ==0.04464646)]
hist(test$score, title (main="Application_Score_Card",xlab="Risk_Score"))

# ----Model Building - :- Random forest

# Loading library 
library(randomForest)
# Loading data 
customerData <- read.csv("customerData1.csv")# taking he base file before logistic Regression EDA
#removing NA's : NA's are a small percentage of the data : More over tried different iterations with replacing the values
#however, it only leads to decrease in accuracy
customerData <- na.omit(customerData) 
#Variable - Income (numeric) : 25 vales at 0 and 79 at -0.5 : Imputing them
customerData$Income[which(customerData$Income ==-0.5 )]<-0
customerData <- customerData[!customerData$Income ==0,] #25 vales at 0 and 79 at -0.5 : Imputing them
# Variable - Age (numeric) - # there are 19 values with age 0 and one with -3 and 23 NA Values
customerData$Age[which(customerData$Age ==-3 )]<-0
customerData <- customerData[!customerData$Age ==0,]

colnames(customerData)[21] <- "Marital.Status" # Shorterning the Colname

customerDataRF <- customerData

#converting all the numerical columns to numbers and Categorical to Factors
numbercol <-c("No.of.times.90.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.6.months", "No.of.times.30.DPD.or.worse.in.last.6.months", "No.of.times.90.DPD.or.worse.in.last.12.months", "No.of.times.60.DPD.or.worse.in.last.12.months",
              "No.of.times.30.DPD.or.worse.in.last.12.months", "Avgas.CC.Utilization.in.last.12.months", "No.of.trades.opened.in.last.6.months",
              "No.of.trades.opened.in.last.12.months", "No.of.PL.trades.opened.in.last.6.months", "No.of.PL.trades.opened.in.last.12.months", "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.", "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
              "Outstanding.Balance", "Total.No.of.Trades", "Age", "No.of.dependents", "Income", "No.of.months.in.current.residence", "No.of.months.in.current.company")
factorcol <-c("Presence.of.open.home.loan", "Presence.of.open.auto.loan", "Gender", "Marital.Status", "Education", "Profession", "Type.of.residence")
customerDataRF [,numbercol] <- lapply (numbercol, function(x) as.numeric(as.character(customerDataRF[,x])))
customerDataRF [,factorcol] <- lapply (factorcol, function(x) as.factor(as.character(customerDataRF[,x])))
customerDataRF$performance <- as.factor (customerDataRF$performance)

#Splitting data into 70/30 for training RF
set.seed(100)
indices <- sample.split(customerDataRF$performance, SplitRatio = 0.70)
train_rf <- customerDataRF[indices, ]
test_rf <- customerDataRF[!indices, ]
nrow(train_rf)/nrow(customerDataRF) # 0.6999942
nrow(test_rf)/nrow(customerDataRF) # 0.3000058

#Running the RF algorithm
cust_rf <- randomForest(performance ~., data = train_rf[-1], proximity = FALSE, ntree = 100, do.trace = TRUE, mtry = 5, na.action = na.omit)

#tuning the Random Forest Model
mtry <- tuneRF(train_rf[-1],train_rf$performance, ntreeTry=100, stepFactor=1,improve=0.1, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

final_rf <-randomForest(performance ~., data = train_rf[-1], mtry=best.m, importance=TRUE,ntree=200, do.trace = TRUE, na.action = na.omit)
print(final_rf)

#Predicting on test data
rf_pred <- predict(cust_rf, test_rf[, c(-1,-29)], type = "prob")
summary(rf_pred)
str(summary(rf_pred))
test_rf$response <- factor(ifelse(test_rf$performance == 1, "yes", "no"))
summary(rf_pred)
quantile(rf_pred[, 2],seq(0,1,0.01)) # 90% range of Prob of "1" (goods) is between 0.0100 & 0.1500

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(0.01,0.15,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.25,length=5),seq(0,0.25,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.056)]
cutoff_rf
# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= 0.04111111, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 30], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1] #0.6290323
# Specificity 
conf_forest$byClass[2] #0.5771417
# Accuracy 
conf_forest$overall[1] #0.5793311 

#____________Tuning Random Forest____________

# Final RF important variables
importance(cust_rf, type=NULL, class=NULL, scale=TRUE)
varImpPlot(cust_rf)

# ROC study
library(pROC)
auc <- roc(test_rf$response, rf_pred[, 2])
print(auc) #Area under the curve : 0.6262
plot(roc(test_rf$response, rf_pred[, 2], direction="<"),
     col="yellow", lwd=3, main="Random Forest ROC")

#Application Score Card
PDO<-20
BaseScore<-400
Odds<-10

#Calculating Factor & Offset

Factor=PDO/log(2)
Offset=BaseScore-(Factor*log(Odds))
Offset # 333.5614
Factor # 28.8539

test_rf$prob <- rf_pred[, 2] # saving the predictions (probabity of Good) from GLM on the test data
summary(test_rf$prob) # probability has a very narrow range between 0 to 0.34

test_rf$score <- 333.5614 + (28.8539 * log(test_rf$prob/(1-test_rf$prob))) # Calc the risk score
summary(test_rf$score) # scores gave a very narrow range 201.0 to 314.4

write.csv(test_rf, "test_rf.csv", row.names = FALSE)
hist(test_rf$score, main="Application_Score_Card",xlab="Risk_Score")


#_______________SMOTE + GLM MOdel_____________
library(randomForest)
# Loading data 
customerData <- read.csv("customerData1.csv")# taking he base file before logistic Regression EDA
#removing NA's : NA's are a small percentage of the data : More over tried different iterations with replacing the values
#however, it only leads to decrease in accuracy
customerData <- na.omit(customerData) 
#Variable - Income (numeric) : 25 vales at 0 and 79 at -0.5 : Imputing them
customerData$Income[which(customerData$Income ==-0.5 )]<-0
customerData <- customerData[!customerData$Income ==0,] #25 vales at 0 and 79 at -0.5 : Imputing them
# Variable - Age (numeric) - # there are 19 values with age 0 and one with -3 and 23 NA Values
customerData$Age[which(customerData$Age ==-3 )]<-0
customerData <- customerData[!customerData$Age ==0,]

colnames(customerData)[21] <- "Marital.Status" # Shorterning the Colname

customerDataRF <- customerData

#converting all the numerical columns to numbers and Categorical to Factors
numbercol <-c("No.of.times.90.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.6.months", "No.of.times.30.DPD.or.worse.in.last.6.months", "No.of.times.90.DPD.or.worse.in.last.12.months", "No.of.times.60.DPD.or.worse.in.last.12.months",
              "No.of.times.30.DPD.or.worse.in.last.12.months", "Avgas.CC.Utilization.in.last.12.months", "No.of.trades.opened.in.last.6.months",
              "No.of.trades.opened.in.last.12.months", "No.of.PL.trades.opened.in.last.6.months", "No.of.PL.trades.opened.in.last.12.months", "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.", "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
              "Outstanding.Balance", "Total.No.of.Trades", "Age", "No.of.dependents", "Income", "No.of.months.in.current.residence", "No.of.months.in.current.company")
factorcol <-c("Presence.of.open.home.loan", "Presence.of.open.auto.loan", "Gender", "Marital.Status", "Education", "Profession", "Type.of.residence")
customerDataRF [,numbercol] <- lapply (numbercol, function(x) as.numeric(as.character(customerDataRF[,x])))
customerDataRF [,factorcol] <- lapply (factorcol, function(x) as.factor(as.character(customerDataRF[,x])))
customerDataRF$performance <- as.factor (customerDataRF$performance)

set.seed(100)
indices <- sample.split(customerDataRF$performance, SplitRatio = 0.70)
train_rf <- customerDataRF[indices, ]
test_rf <- customerDataRF[!indices, ]
nrow(train_rf)/nrow(customerDataRF) #  0.6999942
nrow(test_rf)/nrow(customerDataRF) #  0.3000058

print(table(customerDataRF$performance)) # 65680 Zero's and 2892 Ones
print(prop.table(table(customerDataRF$performance))) # 4% (0.04217465) of the data is for default : Minority class


#Let's create extra positive observations using SMOTE. We set perc.over = 100 to double the quantity of 
#positive cases, and set perc.under=200 to keep half of what was created as negative cases.

library(DMwR)
train_rf$performance <- as.factor(train_rf$performance)
trainSplit <- SMOTE(performance ~ ., train_rf, perc.over = 100, perc.under=200)
trainSplit$performance <- as.numeric(trainSplit$performance)

prop.table(table(trainSplit$performance)) # the data has now been evenly split (50:50) between 0 and 1

#We then train using the SMOTE'd training set and predict using the same testing set as used 
#before on the non-SMOTE'd training set to ensure we're comparing apples-to-apples:

cust_rf <- randomForest(performance ~., data = train_rf[-1], proximity = FALSE, ntree = 100, do.trace = TRUE, mtry = 5, na.action = na.omit)
rf_pred <- predict(cust_rf, test_rf[, c(-1,-29)], type = "prob")
test_rf$response <- factor(ifelse(test_rf$performance == 1, "yes", "no"))
summary(rf_pred)
quantile(rf_pred[, 2],seq(0,1,0.01)) # actual range of Prob of "1" is between 0.0100 & 0.1700
#_________________________________

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.17,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 
#library(caret)
#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.17,length=5),seq(0,0.17,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.1)]
cutoff_rf
# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= 0.04070707, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 30], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1] #Get a value of 0.6198157 after SMOTE+RF Vs the earlier value of only RF 0.6290323
# Specificity 
conf_forest$byClass[2] #Get a value of 0.5720158 after SMOTE+RF Vs the earlier value of only RF 0.5771417 
# Accuracy 
conf_forest$overall[1] #Get a value of 0.5740327 after SMOTE+RF Vs the earlier value of only RF 0.5771417

#SMOTE : did not help on this data set

#____________________Score Card + GLM________________

# Traditional Credit Scoring Using Logistic Regression

#loading the libraries
library(data.table)
library(scorecard)

#loading the data
customerData <- read.csv("customerData1.csv")
str(customerData)

# information values
custdata_info_value = iv(customerData, y = "performance")
summary(custdata_info_value)
custdata_info_value$variable <- as.factor(custdata_info_value$variable)
ggplot(custdata_info_value, aes(x=variable,y=info_value)) + geom_col() +coord_flip()

#___________scorecard________

# filter variable via missing rate, iv, identical value rate
# we removed the non significant variables(10 variables)
dt_sel = var_filter(customerData, "performance")

# woe binning ------
bins = woebin(dt_sel, "performance")
dt_woe = woebin_ply(dt_sel, bins)

# Logistic Regression ------creating model 1 with all selected variables
m = glm(performance ~ ., family = "binomial", data = dt_woe)
summary(m) #AIC 23341...Null Dev 24427...Res Dev 23303

# Select a formula-based model by AIC
m_step = step(m, direction="both", trace=FALSE)
m = eval(m_step$call)
summary(m) #AIC 23323...Null Dev 24427...Res Dev 23307...7 coeff

# Taking this as the final model : Variable "No.of.times.90.DPD.or.worse.in.last.12.months_woe" shows as 
# not-Significant, however EDA shows it is highly significant, hence not removing it

# predicted probability
dt_pred = predict(m, type='response', dt_woe)
summary(dt_pred) # values range between 0.01179 and 0.10273

# performance
# ks & roc plot
perf_eva(dt_woe$performance, dt_pred) # KS 0.2742 : Gini 0.3384 : AUC 0.6741

# Creating Application scorecard
card = scorecard(bins, m, points0 = 400, odds0 = 1/9, pdo = 20,
                 basepoints_eq0 = FALSE)

# Creating Application scorecard on only total score
score1 = scorecard_ply(customerData, card)
hist(score1$score, main = "Application Score Card", xlab = "Scores")

# Creating Application scorecard on credit score for both total and each variable
score2 = scorecard_ply(customerData, card, only_total_score = F)

#________________________________________________
# Comparing on rejected population
dt_test <- read.csv("customerData_na_subset.csv")

# converting Rejected dataset into woe values
test = woebin_ply(dt_test, bins)

# predicting proability on rejected population
test_pred = predict(m, type='response', test)

# credit score, only_total_score = TRUE
test_score = scorecard_ply(dt_test, card)
hist(test_score$score, main = "Application Score Card", xlab = "Scores")
boxplot(score1$score, test_score$score, main="Score Comparison : Approved Vs Rejected",xlab = "Approved                               Rejected")

# Example III # credit score, only_total_score = FALSE
train_score2 = scorecard_ply(customerData, card, only_total_score=FALSE)
test_score2 = scorecard_ply(dt_test, card, only_total_score=FALSE)

psi1 = perf_psi(
  score = list(train = train_score2, test = test_score2),
  label = list(train = dt_woe$performance , test = dt_test$performance)
)
psi1$psi # psi dataframe
psi1$pic # pic of score distribution

## End

#___________________

# filter variable via missing rate, iv, identical value rate
dt_sel = var_filter(germancredit, "creditability")

# woe binning ------
bins = woebin(dt_sel, "creditability")

dt_woe = woebin_ply(dt_sel, bins)

# glm ------
m1 = glm( creditability ~ ., family = "binomial", data = dt_woe)
summary(m1)

# Select a formula-based model by AIC
m_step = step(m1, direction="both", trace=FALSE)
m2 = eval(m_step$call)
summary(m2)

# predicted proability
dt_pred = predict(m2, type='response', dt_woe)

# performance ------
# Example I # only ks & auc values
perf_eva(dt_woe$creditability, dt_pred, show_plot=FALSE)

# Example II # ks & roc plot
perf_eva(dt_woe$creditability, dt_pred)

# Example III # ks, lift, roc & pr plot
perf_eva(dt_woe$creditability, dt_pred, type = c("ks","lift","roc","pr"))

## End
#_______________________

##------------perf_psi----------

# filter variable via missing rate, iv, identical value rate
dt_sel = var_filter(germancredit, "creditability")
# breaking dt into train and test ------
dt_list = split_df(dt_sel, "creditability", ratio = 0.6, seed=21)
dt_train = dt_list$train; dt_test = dt_list$test
# woe binning ------
bins = woebin(dt_train, "creditability")
# converting train and test into woe values
train = woebin_ply(dt_train, bins)
test = woebin_ply(dt_test, bins)
# glm ------
m1 = glm(creditability ~ ., family = "binomial", data = train)
summary(m1)

# Select a formula-based model by AIC
m_step = step(m1, direction="both", trace=FALSE)
m2 = eval(m_step$call)
summary(m2)

# predicted proability
train_pred = predict(m2, type='response', train)
test_pred = predict(m2, type='response', test)

# # ks & roc plot
# perf_eva(train$creditability, train_pred, title = "train")
# perf_eva(test$creditability, test_pred, title = "test")

## scorecard
card = scorecard(bins, m2)

# credit score, only_total_score = TRUE
train_score = scorecard_ply(dt_train, card)
test_score = scorecard_ply(dt_test, card)

# Example I # psi
psi = perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$creditability, test = test$creditability)
)
psi$psi # psi dataframe
psi$pic # pic of score distribution

# Example II # specifying score range
psi_s = perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$creditability, test = test$creditability),
  x_limits = c(200, 750),
  x_tick_break = 50
)
# Example III # credit score, only_total_score = FALSE
train_score2 = scorecard_ply(dt_train, card, only_total_score=FALSE)
test_score2 = scorecard_ply(dt_test, card, only_total_score=FALSE)
psi
psi2 = perf_psi(
  score = list(train = train_score2, test = test_score2),
  label = list(train = train$creditability, test = test$creditability)
)
psi2$psi # psi dataframe
psi2$pic # pic of score distribution
## End
