#Load libraries
library(xlsx)           #for reading data from excel file
library(caret)          #for data splitting function - createDataPartition
library(rpart)          #to build Decision tree regression model
library(randomForest)   #to build random forest regression model 
library(DMwR)           #to calculate regression evaluation statistics
library(ggplot2)        #for visualizations of data
library(gbm)            #to build gradient boosting model

#set working directory
setwd("E:/Edwisor/Project - Employee Absenteeism")

#to check if the working directory is set right
getwd()

#Loading the dataset to model and predict values
df = read.xlsx("Absenteeism_at_work_Project.xls",sheetIndex = 1)

#################################DATA EXPLORATION#############################################
##### Data Exploration or preparation includes,
##1. Identification of variables and their datatypes 
##2. Descriptive statistics
##3. Conversion of data types into required ones
##4. Univariate and Bivariate analysis
##5. Missing Value Analysis
##6. Outlier Analysis
##7. Feature Engineering

########### 1. Identification of variables and their datatypes############

#fetch first five observations from the dataset 
head(df)

##First five rows are displayed from the dataset that contains 21 attributes.

#fetch last five observations from the dataset
tail(df)

##Last row says that there are 740 rows (range index being 1 to 740 rows) in the dataset.

#to get the number of entries or dimensions of the dataset
dim(df)

##Dataset comprises of 740 observations and 21 attributes, out of which one is target variable and rest are independent variables.

#to identify target and predictor variables and their datatypes
str(df)

##*Observation:
##  Most of the attributes are in float datatype including the target attribute (Absenteeism time in hours). Hence, datatype conversion is required for few attributes according to Attribute Information.


################### 2. Descriptive statistics################

#to get the summary statistics of the dataset
summary(df)

##*Initial Observations:

##1. There are null values present in most of the attributes in the dataset.
##2. The dataset contains 36 Individual identification (ID)s of the regular absentees.
##3. Average value for Reason for absence is 23 (Medical consultation) and minimum value is 0 (no such reason).
##4. Average value for Month of absence is 6 (June) and minimum value is 0 (no such month).
##5. Average Day of the week for absence is 4 (Wednesday) and the average season of absence is 3 (Winter).
##6. Minimum distance from residence to work is 5 kms.
##7. Most of the employees are middle aged (35 to 40) and have completed their high school only. 
##8. Although there are employees who have joined recently but majority of the employees are experienced. 
##9. Maximum hours of absent is 120 hours, which is equivalent to 5 full days and 15 working days (assuming 8 hours of work time per day). 

#to get the count of unique values in the dataset
sapply(df, function(x) length(unique(x)))

#to get the unique values of Service_time from the dataset
unique(df$Service.time)

##Minimum and maximum years of experience of employees are 1 and 29 respectively.

################ 3. Conversion of datatypes into the required ones###############

#Datatype conversion
df$Work.load.Average.day. = as.numeric(df$Work.load.Average.day.)       #from character type to numeric
df$Reason.for.absence = as.factor(df$Reason.for.absence)                #Reason for absence can only be one of the 21 categories stratified as per International Code of Diseases (ICD)
df$Month.of.absence = as.factor(df$Month.of.absence)                    #Month of absence can only be 1-12 months. 
df$Day.of.the.week = as.factor(df$Day.of.the.week)                      #Day of the week can only be 1-7 days.
df$Seasons = as.factor(df$Seasons)                                      #Seasons can only be 1-4 seasons.
df$Disciplinary.failure = as.factor(df$Disciplinary.failure)            #Disciplinary failure can only be either (yes=1/ no=0)
df$Education = as.factor(df$Education)                                  #Education can be one of the four(high school (1), graduate (2), postgraduate (3), master and doctor (4))
df$Social.drinker = as.factor(df$Social.drinker)                        #Social drinker can only be either (yes=1/ no=0)
df$Social.smoker = as.factor(df$Social.smoker)                          #Social smoker can only be either (yes=1/ no=0)
df$Pet = as.factor(df$Pet)                                              #Pet denotes the number of pets which can be 0 - <10 (maximum value for example)
df$Son = as.factor(df$Son)                                              #Son denotes the number of children which can be 0 - <10 (maximum value for example)

#Check datatypes of attributes after datatype conversion
str(df)

#Get numeric and categorical attributes from dataset
num_data = df[, sapply(df, function(x) is.numeric(x))]
num_cols = colnames(num_data)
cat_data = df[, sapply(df, function(x) is.factor(x))]
cat_cols = colnames(cat_data)
print("Numeric cols are:")  
print(num_cols)
print("Categorical cols are:") 
print(cat_cols)

####Extreme value Analysis

##Based on the basic understanding of the data from Descriptive statistics, it is obvious that there are outliers present in the dataset which are incorrectly entered or measured data. Hence, we can remove those values.

#fetch rows where absenteeism is zero
df[which(df$Absenteeism.time.in.hours ==0),]

##It is obvious that Absenteeism_time_in_hours can't be zero. we can remove the observations as they are incorrectly entered or recorded.

#Drop rows where Absenteeism_time_in_hours is zero
df = df[-which(df$Absenteeism.time.in.hours ==0),]

#Fetch rows where Month_of_absence is zero
df[which(df$Month.of.absence ==0),]

#Replace zero value of Month of absence by 10
df$Month.of.absence[df$Month.of.absence==0] = 10

#fetch rows where Reason for absence is zero
df[which(df$Reason.for.absence ==0),]

##Reason for absence can't be zero and hence zero values can be replaced by category - 26 (unjustified absence).

#Replace zero values of Reason for absence by category - 26
df$Reason.for.absence[df$Reason.for.absence==0] = 26


##################### 4. Univariate and Bivariate analysis#####################

#####Univariate analysis

##Univariate analysis - Transportation_expense
hist(df$Transportation.expense,main="Distribution of transportation expense",xlab="Transportation expense",col="darkmagenta")

##Most of the employees incur maximum transportation expense in the range of 175 - 200, which is nearly half of the maximum fare.

#Univariate analysis - Distance_from_Residence_to_Work
hist(df$Distance.from.Residence.to.Work,main="Distribution of distance from residence to work",xlab="Distance from residence to work",col="blue")

##Most of the employees travel more than 25 kms from their residence to reach their work place.

#Univariate analysis - Reason_for_absence
ggplot(df, aes(factor(df$Reason.for.absence))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = "Reason for absence",y = NULL)+ggtitle("Distribution of Reaason for absence")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
           vjust = -0.2, geom = "text", position = "identity", color ="red")

##The top four of them cover 50% of the reasons for absence
##1. Medical consultation
##2. Dental consultation
##3. Physiotherapy
##4. Diseases of the musculoskeletal system and connective tissue
##The unjusitified absence amounts to 4.5% of the total.


#Univariate analysis - Seasons
ggplot(df, aes(factor(df$Seasons))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = "Seasons",y = NULL)+ggtitle("Distribution of Seasons")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.2, geom = "text", position = "identity", color ="red")


##Highest number of absentees are recored in the Season 4 (Winter) followed by Season 2 (Autumn).

#Univariate analysis - Month_of_absence
ggplot(df, aes(factor(df$Month.of.absence))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = "Month of absence",y = NULL)+ggtitle("Distribution of month of absence")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.2, geom = "text", position = "identity", color ="red")


##Highest percentage of absentees are recorded in the month of March followed by February and October, which comes under Winter and Autumn seasons respectively.

#Univariate analysis - Day_of_the_week
ggplot(df, aes(factor(df$Day.of.the.week))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = "Day of the week",y = NULL)+ggtitle("Distribution of day of the week")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.2, geom = "text", position = "identity", color ="red")

##Highest percentage of absentees are recorded on start of the week, Mondays followed by Wednesdays. 

#Univariate analysis - Social_drinker
ggplot(df, aes(factor(df$Social.drinker))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = "Social drinking with 1 = Yes and 0 = No",y = NULL)+ggtitle("Distribution of social drinking status of employees")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.2, geom = "text", position = "identity", color ="red")

#Univariate analysis - Social_smoker
ggplot(df, aes(factor(df$Social.smoker))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = "Social smoking with 1 = Yes and 0 = No",y = NULL)+ggtitle("Distribution of social smoking status of employees")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.2, geom = "text", position = "identity", color ="red")

##Thus, most of the employees have the habit of social drinking but very few employees have the habit of social smoking.

#Univariate analysis - Number of children
ggplot(df, aes(factor(df$Son))) +
  geom_bar(fill="dodgerblue3")+
  labs(x = "Number of children",y = NULL)+ggtitle("Distribution of number of children")+
  stat_count(aes(label = paste(round(prop.table(..count..) * 100,2), "%", sep = "")),
             vjust = -0.2, geom = "text", position = "identity", color ="red")

##Most of the employees are married with 0 or 1 child. Thus, they have dependents/children to take care of and their presence might be required in case if their child/dependent fall sick.

##Bivariate analysis

#Bivariate analysis - Distance_from_Residence_to_Work and Transportation_expense
agg <- aggregate(df$Transportation.expense, by = list(df$Distance.from.Residence.to.Work),  FUN = mean)
#Transpose dataframe to matrix
aggmat <- t(agg[-1])
#Create column names
colnames(aggmat) <- agg[,1]
#barplot
barplot(aggmat,xlab = "Distance from Residence to Work",ylab = "Transportation expense",main = "Distribution of Average transportation expense by distance from Residence to Work")

##This plot shows us that distance from residence to work and transportation expense are weakly correlated and there is no information on the mode of transport, peak hours, traffic factors; But,the general hypothesis is that transportation expense increases with distance from residence to work. 

#Bivariate analysis - Age and Service_time
agg <- aggregate(df$Service.time, by = list(df$Age),  FUN = mean)
#Transpose dataframe to matrix
aggmat <- t(agg[-1])
#Create column names
colnames(aggmat) <- agg[,1]
#barplot
barplot(aggmat,xlab = "Age",ylab = "Service time",main = "Distribution of Average service time of the employees by their age")

##This plot clearly tells us that Age and Service time are positively correlated and it is obvious that service time of a person increases with his age. 

#Bivariate analyis - Age and Work_load_Average/day
agg <- aggregate(df$Work.load.Average.day., by = list(df$Age),  FUN = mean)
#Transpose dataframe to matrix
aggmat <- t(agg[-1])
#Create column names
colnames(aggmat) <- agg[,1]
#barplot
barplot(aggmat,xlab = "Age",ylab = "Work load average per day",main = "Distribution of Average work load per day by age of the employees")

##The work load seems to be same irrespective of the age.

#Bivariate analysis - Age and Distance_from_Residence_to_Work
agg <- aggregate(df$Distance.from.Residence.to.Work, by = list(df$Age),  FUN = mean)
#Transpose dataframe to matrix
aggmat <- t(agg[-1])
#Create column names
colnames(aggmat) <- agg[,1]
#barplot
barplot(aggmat,xlab = "Age",ylab = "Distance from Residence to Work",main = "Distribution of Average Distance from Residence to Work by age")

##Another hypothesis is that aged employees might stay closer to the office. But, values after age 33 are not significant to compare as hypothesis fails here after age 33.

#Bivariate analysis - Age and Hit_target
agg <- aggregate(df$Hit.target, by = list(df$Age),  FUN = mean)
#Transpose dataframe to matrix
aggmat <- t(agg[-1])
#Create column names
colnames(aggmat) <- agg[,1]
#barplot
barplot(aggmat,xlab = "Age",ylab = "Hit target",main = "Distribution of Average Hit target by age")

##Hit target seems to be same irrespective of the age of the employees.

#Bivariate analysis - Social drinking and Hit_target
ggplot(df, aes(x = df$Social.drinker, fill = df$Hit.target)) + geom_bar(position = "dodge",fill="dodgerblue3")+ labs(x = "Social drinking with 1 = Yes and 0 = No",y = "Count of target hit")+ggtitle("Distribution of total target hit by Social drinking status of the employee")

##It is interesting to observe that employees with social drinking habit have total target hit value more than non drinkers. Thus, their drinking status doesn't have much impact on their target hit. 

#Bivariate analysis - Social drinking and Disciplinary failure
ggplot(data=df, aes(x=df$Disciplinary.failure, fill=Social.drinker)) + geom_bar(position = "fill")+ggtitle("Distribution of count of Disciplinary failure by Social drinking status of the employees")+xlab("Disciplinary failure with 1 = Yes and 0 = No")

##Thus, it is clear that social drinking doesn't have any effect on disciplinary failure in employees.

##Bivariate analysis - Work load Average/day and Month of absence
agg <- aggregate(df$Work.load.Average.day., by = list(df$Month.of.absence),  FUN = mean)
#Transpose dataframe to matrix
aggmat <- t(agg[-1])
#Create column names
colnames(aggmat) <- agg[,1]
#barplot
barplot(aggmat,xlab = "Work load average per day",ylab = "Month of absence", main = "Distribution of Work load Average/day by Month of absence")

##Work load average/day seems to be higher in the month of January followed by June. It is almost in the same range for rest of the months.

#Bivariate analysis - Work_load_Average/day and Hit_target
agg <- aggregate(df$Hit.target, by = list(df$Work.load.Average.day.),  FUN = mean)
#Transpose dataframe to matrix
aggmat <- t(agg[-1])
#Create column names
colnames(aggmat) <- agg[,1]
#barplot
barplot(aggmat,xlab = "Work load average per day",ylab = "Hit target", main = "Distribution of Average Hit target by work load average/day")

##Thus, average target hit is same for almost all the work load average/day.


##################### 5. Missing value Analysis#####################
#to get the sum of null/missing values in  the dataset
sum(is.na(df))

#create dataframe to analyze missing values in training dataset
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_count"
missing_val$Missing_percentage = (missing_val$Missing_count/nrow(df)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1,3)]
missing_val

##Since the percentage of missing values of the attributes in the dataset is less, we can impute the missing values statistically.
##Imputation of missing values in the dataset: 
##Imputation is a method to fill in the missing values with estimated ones. Mean / Mode / Median imputation is one of the most frequently used methods. It consists of replacing the missing data for a given attribute by the mean or median (quantitative attribute) or mode (qualitative attribute) of all known values of that variable.

##Since, the attributes are numerical and categorical, we can opt for Mean/Median/Mode and KNN methods for imputation.
##To find the apt method out of all,
##* Remove any random value from the column missing and replace with NA.
##* Impute the null value with methods one by one.
##* Select the method with output nearly equal to the original value.

#Imputing missing values - continuous attributes
#Choosing a random value from Weight attribute to replace it as NA
act_val = df$Weight[20]

#Replace actual value with NA
df$Weight[20] = NA

#Mean imputation
mean_val = mean(df$Weight, na.rm=TRUE)
df$Weight[20] = NA

#Median imputation
median_val = median(df$Weight, na.rm=TRUE)
df$Weight[20] = NA

#KNN imputation using k = 3, 5, 7 values
df_knn3 = knnImputation(df, k = 3)
knn3_val = df_knn3$Weight[20]
df_knn5 = knnImputation(df, k = 5)
knn5_val = df_knn5$Weight[20]
df_knn7 = knnImputation(df, k = 7)
knn7_val = df_knn7$Weight[20]

#Substitute with actual value
df$Weight[20] = act_val

#Imputing missing values - categorical attributes

#Choosing a random value from Education attribute to replace it as NA
act_cat_val = df$Education[83]

#Replace actual value with NA
df$Education[83] = NA

#Mode inputation
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

gm = getmode(df$Education)
mode_val = df$Education[83] 
df$Education[83] = NA

#Substitute with actual value
df$Education[83] = act_cat_val

#Results of Imputation

print("###### Imputing missing values for continuous attribute - example ######")
print(paste0("Actual value is: ", act_val))
print(paste0("Mean value is:" ,as.integer(mean_val)))
print(paste0("Median value is:", as.integer(median_val)))
print(paste0("KNN value for k = 3 is:" , as.integer(knn3_val)))
print(paste0("KNN value for k = 5 is:" , as.integer(knn5_val)))
print(paste0("KNN value for k = 7 is:" , as.integer(knn7_val)))

print("###### Imputing missing values for categorical attribute - example ######")
print(paste0("Actual value is:", (act_cat_val)))
print(paste0("Mode value is:" , (mode_val)))
print(paste0("KNN value for k = 3 is:" , df_knn3$Education[83]))
print(paste0("KNN value for k = 5 is:" , df_knn5$Education[83]))
print(paste0("KNN value for k = 7 is:" , df_knn7$Education[83]))

##Thus, from the above methods of imputation, we can choose KNN method for imputation with k = 3.

#Impute missing values with k = 3
df = knnImputation(df, k = 3)

#to check for missing values after imputation
sum(is.na(df))

##Thus, there are no missing values present in the dataset. 


####################### 6. Outlier Analysis#####################

##Boxplot analysis

#Plot boxplot to identify outliers in the dataset
boxplot(df[,c('Transportation.expense','Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target')])
boxplot(df[,c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours')])
boxplot(df[,c('Work.load.Average.day.')])

#Outlier analysis using boxplot method
print("******** Outlier Analysis ********")
for (i in num_cols){
  print(" ")
  print(paste0("***** " , i , " *****"))
  q = quantile(df[,i],c(0.25,0.75))
  iqr1 = q[2]-q[1]
  min1 = q[1]-1.5*iqr1
  max1 = q[2]+1.5*iqr1
  print("Minimum and Maximum values are:")
  print(min1)
  print(max1)
  minlen = length(df[,i][which(df[,i]<min1)])
  maxlen = length(df[,i][which(df[,i]>max1)])
  print(paste0("Outliers - " , sum(minlen,maxlen)))
}

#to replace outlier values with NA
print("***** After Replacing Outlier values with NA *****")
for (i in num_cols){
  print(" ")
  print(paste0("***** " , i , " *****"))
  q = quantile(df[,i],c(0.25,0.75))
  iqr1 = q[2]-q[1]
  min1 = q[1]-1.5*iqr1
  max1 = q[2]+1.5*iqr1
  df[,i][df[,i]<min1] = NA
  df[,i][df[,i]>max1] = NA
  minlen = length(df[,i][which(df[,i]<min1)])
  maxlen = length(df[,i][which(df[,i]>max1)])
  print(paste0("Outliers - " , sum(minlen,maxlen)))
}

#to impute NA values with KNN method using k = 3
df = knnImputation(df, k = 3)

#to check if the outliers have been imputed
boxplot(df[,c('Transportation.expense','Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target')])
boxplot(df[,c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours')])
boxplot(df[,c('Work.load.Average.day.')])

##Thus, there are no outliers present in the dataset. 

##############################  Data Visualization ##############################################

##Data visualization helps us to understand the relationship between features. Here, to understand how each independent feature is related to the target feature, we need to perform visualization on data.

#Distance from Residence to Work Vs Absenteeism time in hours
dist = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Distance.from.Residence.to.Work), FUN=mean)
ggplot(data = dist, aes(x = dist$Category, y = dist$x)) + geom_col(fill="dodgerblue3") + ggtitle("Average Absenteeism time in hours by distance")+xlab("Distance from Residence to Work (km)")+ylab("Absenteeism time in hours")

##General hypothesis is that distance from residence to work will have an impact on absenteeism. Here, average hours of absent remains the same irrespective of the distance from residence to work of the employees. There is concentration of more leaves where the distance of residence from work is between 10-30 kms.

#Work load Average/day Vs Absenteeism time in hours
wl = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Work.load.Average.day.), FUN=mean)
ggplot(data = wl, aes(x = dist$Category, y = dist$x)) + geom_col(fill="dodgerblue3") + ggtitle("Average Absenteeism time in hours by distance")+xlab("Distance from Residence to Work (km)")+ylab("Absenteeism time in hours")

##Thus, average hours of absent remains same irrespective of the work load average/day.

#Service time Vs Absenteeism time in hours
st = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Service.time), FUN=mean)
ggplot(data = st, aes(x = st$Category, y = st$x)) + geom_col(fill="dodgerblue3") + ggtitle("Average Absenteeism time in hours by distance")+xlab("Service time")+ylab("Absenteeism time in hours")

##Thus, the employees with service years > 8 tend to take more leaves.

#Disciplinary failure Vs Absenteeism time in hours
disp = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Disciplinary.failure), FUN=sum)
ggplot(data = disp, aes(x = disp$Category, y = disp$x)) + geom_col(fill="dodgerblue3") + ggtitle("Total Absenteeism time in hours by Disciplinary failure in Employees")+xlab("Disciplinary failure with 1 = Yes and 0 = No")+ylab("Absenteeism time in hours")

##Thus, it is observed that employees with no disciplinary failure have the highest absent hours in total.

#Age of the employees Vs Absenteeism time in hours
age = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Age), FUN=mean)
ggplot(data = age, aes(x = age$Category, y = age$x)) + geom_col(fill="dodgerblue3") + ggtitle("Average Absenteeism time in hours by Age of the Employees")+xlab("Age of the employees (in yrs)")+ylab("Absenteeism time in hours")

##Hypothesis is that as the age increases, employees tend to take more leave compared to others due to health issues. Here, people over 45+ years of age tends to take less leaves compared to others.

#Number of children Vs Absenteeism time in hours
son = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Son), FUN=sum)
ggplot(data = son, aes(x = son$Category, y = son$x)) + geom_col(fill="dodgerblue3") + ggtitle("Total Absenteeism time in hours by Children")+xlab("Number of children")+ylab("Absenteeism time in hours")

##It is interesting to note that employees with no issues have highest absenteeism. This is followed by employees with 1 child and it might be due to the fact that there would be noone else to take care of the child if it falls sick and one parent has to stay back with the kid. In case of 3 or 4 children, there is a high chance that older siblings take care of younger ones. 

#Number of pets Vs Absenteeism time in hours
pet = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Pet), FUN=sum)
ggplot(data = pet, aes(x = pet$Category, y = pet$x)) + geom_col(fill="dodgerblue3") + ggtitle("Total Absenteeism time in hours by Pet")+xlab("Number of pets")+ylab("Absenteeism time in hours")

##It is to be noted that the employees with no pet or 1 pet are frequent absentees.

#Reason for absence Vs Absenteeism time in hours
Reasons = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Reason.for.absence), FUN=sum)
ggplot(data = Reasons, aes(x = Reasons$Category, y = Reasons$x)) + geom_col(fill="dodgerblue3") + ggtitle("Total Absenteeism time in hours by Reason for absence")+xlab("Reason for absence")+ylab("Absenteeism time in hours")

##Reasons and Remedies/Suggestions to reduce absenteeism:

##The top four reasons for absence are:
##1. Diseases of the musculoskeletal system and connective tissue
##2. Injury, poisoning and certain other consequences of external causes
##3. Medical consultation
##4. Unjustified absence

##Remedies/Suggestions to reduce absenteeism:
##* Musculoskeletal system disease is the major reason of absenteeism. Bad working posture and high workload are possible reasons for the high incidence of musculoskeletal disease. Company should conduct a study on the working postures of people and come up with ergonomic workplace design. Company should try to optimize workload keeping in mind occupational health of working people.
##* Injury, poisoning and certain other consequences of external causes can be the consequences of the bad and unsafe working environment. Safety of the employees should be the major concern of the company.
##* Medical and dental consultation can be brought down by optimizing workloads and adapting to employee friendly workplace and conducting more programs and medical camps to create awareness on importance of physical and mental wellness among employees. 
##* Unjustified absence is too high.Company should try to reduce high workloads and set up measures so that employees don't feel work stress and they get to discuss work related problems and pressures.
##* Counselling sessions are to be conducted on regular basis to employees to get their feedbacks and help them in their work and personal issues if any

#Month of absence Vs Absenteeism time in hours
months = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Month.of.absence), FUN=sum)
ggplot(data = months, aes(x = months$Category, y = months$x)) + geom_col(fill="dodgerblue3") + ggtitle("Total Absenteeism time in hours by Month of absence")+xlab("Month of absence")+ylab("Absenteeism time in hours")

##We can infer that maximum hours of absent are recorded in the month of March followed by July. we can check the reason for absence in these months to analyse further.

names(months) = c("Month of absence","Total hours of absent")
months$Percent_of_total_hours_of_absent = (months$`Total hours of absent`/sum(months$`Total hours of absent`)) * 100
months = months[order(-months$Percent_of_total_hours_of_absent),]
rownames(months) = NULL
months

#Reasons for absence in March month

mar_absent = aggregate(df$Absenteeism.time.in.hours,by=list(df$Reason.for.absence,df$Month.of.absence==3), FUN=sum)
mar_absent = mar_absent[-which(mar_absent$Group.2 ==FALSE),]
mar_absent$Group.2 = NULL
names(mar_absent) = c("Reason for absence","Total hours of absent")
mar_absent = mar_absent[order(-mar_absent$`Total hours of absent`),]
rownames(mar_absent) = NULL
mar_absent

##The top four reasons for absence are:
##1. Diseases of the musculoskeletal system and connective tissue
##2. Patient follow-up 
##3. Injury, poisoning and certain other consequences of external causes
##4. Dental consultation

#Reasons for absence in July month
july_absent = aggregate(df$Absenteeism.time.in.hours,by=list(df$Reason.for.absence,df$Month.of.absence==7), FUN=sum)
july_absent = july_absent[-which(july_absent$Group.2 ==FALSE),]
july_absent$Group.2 = NULL
names(july_absent) = c("Reason for absence","Total hours of absent")
july_absent = july_absent[order(-july_absent$`Total hours of absent`),]
rownames(july_absent) = NULL
july_absent

##The top four reasons for absence are:
##1. Patient follow-up
##2. Unjustified absence
##3. Certain infectious and parasitic diseases
##4. Injury, poisoning and certain other consequences of external causes
##5. Unjustified absence might be due to personal reason(vacation with family) as it is holiday season.

#Seasons Vs Absenteeism time in hours
seasons = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Seasons), FUN=sum)
ggplot(data = seasons, aes(x = seasons$Category, y = seasons$x)) + geom_col(fill="dodgerblue3") + ggtitle("Total Absenteeism time in hours by Seasons")+xlab("Seasons")+ylab("Absenteeism time in hours")

##Winter season has highest absenteeism followed by Summer. It is also to be noted that March and July month has highest absenteeism. 

#Day of the week Vs Absenteeism time in hours
dow = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Day.of.the.week), FUN=sum)
ggplot(data = dow, aes(x = dow$Category, y = dow$x)) + geom_col(fill="dodgerblue3") + ggtitle("Total Absenteeism time in hours by day of the week")+xlab("Day of the week")+ylab("Absenteeism time in hours")

##Highest percentage of absenteeism is recorded on start of the week, Mondays followed by Tuesdays which can be due to the fact that most people travel over weekend and they tend to extend their holiday plan till monday/tuesday. 

#Effect on Absenteeism by Social drinking/smoking in employees
drink_smoke = aggregate(df$Absenteeism.time.in.hours,by=list(df$Social.drinker==1,df$Social.smoker==0), FUN=sum)
labels <- c("Social smoker","Social drinker & smoker", "Non drinker/smoker", "Social drinker")
piepercent<- round(100*drink_smoke$x/sum(drink_smoke$x), 1)
pie(drink_smoke$x, labels=piepercent, main = "Absenteeism by Social Drinkers/Smokers", col = rainbow(length(drink_smoke$x)))
legend("topright", c("Social smoker","Social drinker & smoker", "Non drinker/smoker", "Social drinker"), cex = 0.8,
       fill = rainbow(length(drink_smoke$x)))

##Looks like 56% of Social drinkers and interestingly 36% of Non drinkers/smokers are regular absentees.

####################### 7. Feature Engineering####################

##Feature selection:

##Correlation Analysis:

##The relationship between numerical variables in the dataset can be found using correlation matrix.
##Hypothesis of Correlation Analysis:
##1. There should be low or no correlation between independent numerical variables.
##2. There should be high correlation between target and independent numerical variables.

#to find the correlation between variables in the dataset
corrgram::corrgram(num_data,upper.panel=corrgram::panel.pie, main = "Correlation Plot")

##Here, none of the independent attributes are highly correlated with the target and with other each other.

##Chi-Square Test: 
##This test is used to derive the statistical significance of relationship between the categorical variables in the dataset. It returns probability for the computed chi-square distribution with the degree of freedom.
##Hypothesis of Chi-Square test:
##1. Null Hypothesis: The null hypothesis of the Chi-Square test is that no relationship exists on the categorical variables in the population; they are independent.
##2. Alternate Hypothesis: The alternate hypothesis of the Chi-Square test is that there exists relationship between the categorical variables in the population; they are not independent.
##If p-value is less than 0.05 then we reject the null hypothesis. And if p-value is greater than 0.05 then we accept the null hypothesis.

# Chi-Square test to find relationship among categorical attributes
pval = c()
for(i in cat_cols){
  for(j in cat_cols){
    chi2 = chisq.test(df[,i],df[,j])
    pval = c(pval,chi2$p.value)
  }
}
length(pval)#100
#converting pval to matrix m1
m1 = matrix(pval,ncol=10)
m1
#Converting m1 to dataframe chi_df
chi_df = data.frame(m1)
#Setting row names to catcols
row.names(chi_df) = cat_cols
#Setting column names to catcols
colnames(chi_df) = cat_cols
chi_df

##We can observe that p value of Seasons vs Month_of_absence is 0.000000e+00 which means that it is highly dependent on Month of absence.

#Drop seasons as it is dependent on Month of absence
df$Seasons = NULL

##ANOVA: 
##Analysis Of Variance or ANOVA is used to find the relationship between independent categorical variables and numerical variable. This can be dependent or independent.
##Hypothesis of ANOVA testing:
##1. Null Hypothesis: Mean of all categories in a variable are same and numerical variable doesn't depend on it.
##2. Alternate Hypothesis: Mean of at least one category in a variable is different and numerical variable depends on it. 
##If p-value is less than 0.05 then we reject the null hypothesis. And if p-value is greater than 0.05 then we accept the null hypothesis.

#ANOVA test
aov_results = aov(Absenteeism.time.in.hours ~ Reason.for.absence + Month.of.absence + Day.of.the.week + Disciplinary.failure + Education + Son + Social.drinker + Social.smoker + Pet,data = df)
summary(aov_results)

##Thus, all the attributes except Social smoker are significant to the target. Hence, we can consider all of the attributes given above.

#Drop Social_Smoker as it is not significant to the target
df$Social.smoker = NULL

df_selected = df

##Since our dataset consists of numerical and categorical features, we need to hot encode each category of categorical variables to perform data modelling.

cat_data = df[, sapply(df, function(x) is.factor(x))]
cat_cols = colnames(cat_data)

#Hot encoding of categorical variables
dmy <- dummyVars(" ~ .", data = df)
df1 = data.frame(predict(dmy,newdata = df))
df1$Reason.for.absence.0 = NULL
df1$Month.of.absence.0 = NULL

##Feature Scaling

#to get the distribution of numerical variables for normality check
hist(df1$Transportation.expense)
hist(df1$Distance.from.Residence.to.Work)
hist(df1$Weight)
hist(df1$Height)
hist(df1$Service.time)
hist(df1$Age)
hist(df1$Work.load.Average.day.)
hist(df1$Hit.target)

##Thus, the distributions are not gaussian and hence we have to normalize the data to bring them in a common range.

#Normalization on dataset
for (i in colnames(df1)){
  df1[,i] = (df1[,i] - min(df1[,i]))/(max(df1[,i]) - min(df1[,i]))
}

df_normalized = df1

###############  Applying Machine Learning algorithms  ###################################

##We can split the data into train and test datasets. Training dataset is used for building training model and test dataset is for validating our model. This is done to understand the robustness, accuracy and performance of the model built. 

#Obtain test and training dataset
train.index = caret::createDataPartition(df1$Absenteeism.time.in.hours, p = .80, list = FALSE)
train_data = df1[train.index,]
test_data  = df1[-train.index,]

##Dimensions of training dataset
dim(train_data)

##Dimesions of test dataset
dim(test_data)


################## 1. Linear regression ##################

lm_model = lm(Absenteeism.time.in.hours~.,train_data)
summary(lm_model)
str(test_data)
test_data[,1:73]

predictions_LM = predict(lm_model, test_data[,1:73])

ggplot2::qplot(x = test_data[,74], y = predictions_LM, data = test_data, color = I("blue"), geom = "point")

RMSE = function(act, pred){
  sqrt(mean((act - pred)^2))
}

RMSE(test_data[,74],predictions_LM)

################## 2. Decision Tree ##################

DT_model = rpart(Absenteeism.time.in.hours~ ., data = train_data, method = "anova")

summary(DT_model)

predictions_DT = predict(DT_model, test_data[,1:73])

RMSE = function(act, pred){
  sqrt(mean((act - pred)^2))
}

RMSE(test_data[,74],predictions_DT)


################## 3. Random Forest ###################

rf_model = randomForest(Absenteeism.time.in.hours ~.,data=train_data, importance = TRUE, ntree = 200)

summary(rf_model)

predictions_RF = predict(rf_model, test_data[,1:73])

RMSE = function(act, pred){
  sqrt(mean((act - pred)^2))
}

RMSE(test_data[,74],predictions_RF)

################## 4. Gradient Boosting ###################

gbm_model = gbm(Absenteeism.time.in.hours~.,data = train_data, n.trees = 200, shrinkage = 0.01,interaction.depth = 5)
summary(gbm_model)

predictions_GBM = predict(gbm_model, test_data[,1:73],n.trees = 200)

RMSE(test_data[,74],predictions_GBM)

##Thus, we can choose Gradient Boosting model as it yields better results.

######################### Predict absent hours for the year 2011 #####################################

##Thus, from the data given it is clear that it is past data. Now, we are expected to make project losses every month in 2011 if same trend of absenteeism continues. Thus, assuming that the given data is from year 2010, we can frame the sample dataset for 2011 by increasing the age and service time of the employees and keeping the rest of the data constant assuming there will not be much change to it.

df_data_2011 = df_selected
df_data_2011$Absenteeism.time.in.hours = NULL

#Increment Age and Service time in the dataset
for (i in num_cols){
  if (i == 'Age'){
    df_data_2011[,i] = df_data_2011[,i] + 1.0
  }
   if(i == 'Service.time'){
     df_data_2011[,i] = df_data_2011[,i] + 1.0
   }
}

##Since the data is preprocessed in dataset for 2011, let's continue with the below steps:

#Hot encoding of categorical features
dmy <- dummyVars(" ~ .", data = df_data_2011)
df1_data_2011 = data.frame(predict(dmy,newdata = df_data_2011))
#df1$Reason.for.absence.0 = NULL
#df1$Month.of.absence.0 = NULL

#Normalization on dataset
for (i in colnames(df1_data_2011)){
  df1_data_2011[,i] = (df1_data_2011[,i] - min(df1_data_2011[,i]))/(max(df1_data_2011[,i]) - min(df1_data_2011[,i]))
}

#Predict using GB model
Ypred_2011= predict(gbm_model,df1_data_2011)

#Adding predicted absent hours to the test dataset 
df1_data_2011$Predicted_Absent_hours = Ypred_2011

summary(df_normalized$Absenteeism.time.in.hours)
summary(df1_data_2011$Predicted_Absent_hours)
