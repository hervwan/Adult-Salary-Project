#############################################################
# Getting Data
#############################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Amelia)) install.packages("Amelia", repos = "http://cran.us.r-project.org")

#We Read in the adult_sal.csv file and set it to a data frame called adult.

adult <- read.csv('adult_sal.csv')
head(adult)

#We can notice that the index has been repeated, we drop this column
adult <- select(adult,-X)

head(adult)
str(adult)
summary(adult)


#############################################################
# Data Preparation
#############################################################


#####Grouping
#We can see a lot of colmuns that are categorical factors, 
#and lot of them have too many factors than necessary.


#Type_employer
table(adult$type_employer)

#We see 1836 with a question mark.
# As well a s 2 small group of never-worked and without-pay.
#We will create a group "unemployed" and put them there.
# Same with local government job and State job. As well as Self-employed jobs
group_type <- function(job){
  job <- as.character(job)
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else if(job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
adult$type_employer <- sapply(adult$type_employer,group_type)
table(adult$type_employer)

#Marital
table(adult$marital)
#We will regroup in 3 group: Married, Not Married and Never married

group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

levels(adult$country)
#Grouping countries by continent
#Creating continents as strings vectos 
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- sapply(adult$country,group_country)

#Change country column to region
names(adult)[names(adult)=="country"] <- "region"
#Checking the table
table(adult$region)

#Checking if categorical columns have factor levels and change if necessary
str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$region <- sapply(adult$region,factor)
adult$marital <- sapply(adult$marital,factor)

#############Missing Data

#First any cell with a "?" value will be converted to a NA Value
adult[adult == '?'] <- NA

#Plot MissMap
#This is a heatmap pointing out missing values (NA). 
#This gives a quick glance at how much data is missing, 
#in this case, not a whole lot (relatively speaking)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#We will decide to drop them from the data Frame
# May take awhile
adult <- na.omit(adult)

#Check missmap again
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

##############################################
#Data Analysis
#############################################

#Effect of Age

#Plot histogram Income by Age
adult %>% ggplot(aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1,alpha=0.4, position="dodge") + theme_bw()
adult %>% ggplot() + geom_boxplot(aes(y=age,fill=income),color='black',alpha=0.4) + theme_bw()

mean1 <-adult %>% select(age,income) %>%filter(income=="<=50K") 
mean(mean1$age)
mean2 <-adult %>% select(age,income) %>%filter(income==">50K") 
mean(mean2$age)

#We can see that the age has a big impact on the income through the distribution and box plot
#The average age of people earning more than 50K are 44years old against 36 and a half for those earning less than 50K

#Effect of hours worked per week
adult %>% ggplot(aes(hr_per_week)) + geom_histogram(aes(fill=income),color='black',binwidth=1,alpha=0.4, position="dodge") + theme_bw()
adult %>% ggplot() + geom_boxplot(aes(y=hr_per_week,fill=income),color='black',alpha=0.4) + theme_bw()

#Here, we can see that although the mean is the same, people earning more than 50K tend to work much more than those earning less than 50K.

############Region effect
ggplot(adult,aes(region,group=income)) + geom_bar(aes(y=(..count..)/sum(..count..),fill=income),color='black')+theme_bw()+ geom_text(aes( label = scales::percent(..prop..),  y= ..prop.. ), stat= "count", vjust = -.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  scale_y_discrete(labels = scales::percent)+   facet_grid(~income) 

ggplot(adult,aes(income,group=region)) + geom_bar(aes(y=(..count..)/sum(..count..),fill=region),color='black')+theme_bw()+ geom_text(aes( label = scales::percent(..prop..),  y= ..prop.. ), stat= "count", vjust = -.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  scale_y_discrete(labels = scales::percent)+   facet_grid(~region) 

#For the region effect, we can see that most of the data come from north america. But we can also see that except for latin and south america, 
#the percentage of people earning more than 50K and those that earn less are similar in every region.

############Sex
ggplot(adult,aes(sex,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.4, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#We can see that according to sex, the income might be different. Women tend to earn less.

###########Occupation 
ggplot(adult,aes(occupation,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.4, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#The occupation can also play a big impact as occupation such as Executive managers or professor ( specialist) tend to earn more 

###########Education 
ggplot(adult,aes(education,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.4, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Education also reflect a possible impact. As we can see that around half of those having a bachelor, master or doctorate earn more than 50K.

##########Race
ggplot(adult,aes(race,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.4, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Here, we can see that race could also provide some information. It looks like that Asian and white have a higher probability to earn more than 50K than Black.

##########Marital and relationship
ggplot(adult,aes(marital,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.4, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(adult,aes(relationship,group=income)) + geom_bar(aes(fill=income),color='black',alpha=0.4, position="dodge")+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Here again, Married people tend to earn more than the never married and not married counterpart.


#############################################################
#Model Building
#############################################################

#Now it's time to build a model to classify people into two groups: Above or Below 50k in Salary.

#Logistic Regression is a type of classification model. In classification models, 
#we attempt to predict the outcome of categorical dependent variables, using one or more independent variables. 
#The independent variables can be either categorical or numerical.

#Logistic regression is based on the logistic function,
#which always takes values between 0 and 1. 
#Replacing the dependent variable of the logistic function with a linear
#combination of dependent variables we intend to use for regression,
#we arrive at the formula for logistic regression.


head(adult)

# Split raw data set into train and test set: Validation set will be 10% of the Set
set.seed(101) 
sample <- sample.split(adult$income, SplitRatio = 0.80)

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)
head(test)
str(test)

###################

######Change income for accuracy
Change_income <- function(inc){
  inc <- as.character(inc)
  
  # More than 50K
  if (inc=='>50K'){
    return('1')
  }else{
    #Less than 50K
    return('0')
  }
}
test_ver <- test
test_ver$income <- sapply(test$income,Change_income)


########################Test With only age
Test_Age <- glm(formula = income ~ age, family = binomial(logit), 
                data = train)
test$predicted.income = predict(Test_Age, newdata=test, type="response")

#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
print(paste('Accuracy',format(round(accuracy, 3), nsmall = 2)))

Accuracy_results <- data_frame(method = "Using only age", Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2)))
Accuracy_results %>% knitr::kable()
#########################


########################Test With only type of employer
Test_type_employer <- glm(formula = income ~ type_employer, family = binomial(logit), 
                data = train)
test$predicted.income = predict(Test_type_employer, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                          data_frame(method="Using only type of employer",  
                                     Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

#########################

########################Test With only financial weight
Test_fnlwgt <- glm(formula = income ~ fnlwgt, family = binomial(logit), 
                          data = train)
test$predicted.income = predict(Test_fnlwgt, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using only financial weight",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

#########################

########################Test With only education
Test_education <- glm(formula = income ~ education, family = binomial(logit), 
                   data = train)
test$predicted.income = predict(Test_education, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using only education",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()
#########################


########################Test With only region
Test_region <- glm(formula = income ~ region, family = binomial(logit), 
                      data = train)
test$predicted.income = predict(Test_region, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using only region",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

#########################


########################Test With only sex
Test_sex <- glm(formula = income ~ sex, family = binomial(logit), 
                   data = train)
test$predicted.income = predict(Test_sex, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using only sex",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

#We can see that by using those variables independentely, we have an accuracy ranging around 75%
#########################

###########################
#######Test two variables
###########################

########################Test With age and education
Test_age_education <- glm(formula = income ~ age +education, family = binomial(logit), 
                data = train)
test$predicted.income = predict(Test_age_education, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using age and education",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

#########################


########################Test With region and type of employer
Test_region_typemployer <- glm(formula = income ~ region + type_employer, family = binomial(logit), 
                          data = train)
test$predicted.income = predict(Test_region_typemployer, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using region and type of employer",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

#########################
#We can see that by using two of those variables, we have a similar accuracy ranging around 75%
###########################
#######Test three variables
###########################

########################Test With age and education and sex
Test_age_education_sex <- glm(formula = income ~ age +education+sex, family = binomial(logit), 
                          data = train)
test$predicted.income = predict(Test_age_education_sex, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using age, education and sex",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()
#########################


########################Test With age and financial weight and type of employer
Test_age_financial_employer <- glm(formula = income ~ age +fnlwgt+type_employer, family = binomial(logit), 
                              data = train)
test$predicted.income = predict(Test_age_financial_employer, newdata=test, type="response")
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using age, financial weight and type of employer",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

##############
#Here Again, we see that by using three of those variables, we have a similar accuracy that can go down 73,4%% or go up to 79%



#As the Data Set is quite small, we can use the whole range of variables for the logistic regression
#Use all the features to train a glm() model on the training data set

#######################
#Model with all variables
#######################
model = glm(income ~ ., family = binomial(logit), data = train)

test$predicted.income = predict(model, newdata=test, type="response")

#Print Summary of Model
summary(model)

#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)


######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using every variables",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()

# We have a range of variables at our disposal to include in the model or not
#Can we have a similar accuracy by using less variables? Thus making the model more interpretable ?
####Choose a model by AIC in a Stepwise Algorithm
# function called step(). The step() function iteratively tries to remove predictor 
#variables from the model in an attempt to delete variables that do not significantly add to the fit

new.step.model <- step(model)

test$predicted.income = predict(new.step.model, newdata=test, type="response")

#Print Summary of Model
summary(new.step.model)
#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Using Step algorithm",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()


# With this, we can see that we still use most of the whole range of variables in order to get 84,5% accuracy.We dropped the education_num variable !
#Our final model is thus:
#glm(formula = income ~ age + type_employer + fnlwgt + education + 
#marital + occupation + relationship + race + sex + capital_gain + 
#  capital_loss + hr_per_week + region, family = binomial(logit), 
#data = train)


############################################
#Final Model
#############################################
model =glm(formula = income ~ age + type_employer + fnlwgt + education + 
  marital + occupation + relationship + race + sex + capital_gain + 
    capital_loss + hr_per_week + region, family = binomial(logit), 
  data = train)

test$predicted.income = predict(model, newdata=test, type="response")

#Print Confusion Matrix
table(test$income, test$predicted.income > 0.5)

######### Print Overall Accuracy
fitted.probabilities <- test$predicted.income
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test_ver$income)
accuracy <-1-misClasificError
Accuracy_results <- bind_rows(Accuracy_results,
                              data_frame(method="Final Model ( every variables except education_num)",  
                                         Accuracy = paste('Accuracy =',format(round(accuracy, 3), nsmall = 2))))
Accuracy_results %>% knitr::kable()
#####Recall
print((4248)/(4248+366))
#####Precision
print((4248)/(4248+587))
