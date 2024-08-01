#CIS 9660 Project(Logistic regression)

install.packages("stargazer")
library(stargazer)

#Load data
df<-read.csv("stroke_data.csv",header=T)
fix(df)
dim(df)
summary(df)

#Data cleaning
#remove "other" in gender
table(df$gender)
df<- df[df$gender != "Other",]
dim(df)

#remove N/A in the bmi
length(which(df$bmi=="N/A"))
df<- df[df$bmi != "N/A",]
dim(df)

#check each data type and convert them to the data type we want
summary(df)
df$gender<-as.factor(df$gender)
df$hypertension<-as.factor(df$hypertension)
df$heart_disease<-as.factor(df$heart_disease)
df$ever_married<-as.factor(df$ever_married)
df$work_type<-as.factor(df$work_type)
df$Residence_type<-as.factor(df$Residence_type)
df$bmi<-as.numeric(df$bmi)
df$smoking_status<-as.factor(df$smoking_status)
df$stroke<-as.factor(df$stroke)
str(df)


#Logistic regression
attach(df)
table(stroke)
209/4908
#In this data set, only 4% of people has stroke.

#Regression with all predictors
glm1<-glm(stroke~.-id,data=df,family=binomial)
summary(glm1)

stargazer(glm1, title = "Logistic Regression with All Predictors", align = TRUE)

# The predict() function can be used to predict the probability that the person will have a stroke or not.
glm.probs=predict(glm1,type="response")
# Here we have printed only the first ten probabilities.
glm.probs[1:10]
contrasts(stroke)

glm.pred=rep(0,4908)
# change the value to 1 if glm.probs>0.25(it doesn't have enough predicted 1 if we use 0.5)
glm.pred[glm.probs>.25]=1
# the table() function can be used to produce a confusion matrix in order to determine how many observations were correctly or incorrectly classified
table(glm.pred,stroke)
#the accuracy rate
mean(glm.pred==stroke)
#training error rate
1-mean(glm.pred==stroke)

#precision rate
table(glm.pred,stroke)[2,2]/(table(glm.pred,stroke)[2,2]+table(glm.pred,stroke)[2,1])

#recall rate
table(glm.pred,stroke)[2,2]/(table(glm.pred,stroke)[2,2]+table(glm.pred,stroke)[1,2])


#regression with selected predictors only
glm2<-glm(stroke~gender+age+hypertension+heart_disease+ever_married+Residence_type+avg_glucose_level+smoking_status,data=df,family=binomial)
summary(glm2)

# The predict() function can be used to predict the probability that the person will have a stroke or not.
glm.probs2=predict(glm2,type="response")
# Here we have printed only the first ten probabilities.
glm.probs2[1:10]
contrasts(stroke)

glm.pred2=rep(0,4908)
# change the value to 1 if glm.probs>0.25(it doesn't have enough predicted 1 if we use 0.5)
glm.pred2[glm.probs2>.25]=1
# the table() function can be used to produce a confusion matrix in order to determine how many observations were correctly or incorrectly classified
table(glm.pred2,stroke)
#the accuracy rate
mean(glm.pred2==stroke)
#training error rate
1-mean(glm.pred2==stroke)



#precision rate
table(glm.pred2,stroke)[2,2]/(table(glm.pred2,stroke)[2,2]+table(glm.pred2,stroke)[2,1])
#recall rate
table(glm.pred2,stroke)[2,2]/(table(glm.pred2,stroke)[2,2]+table(glm.pred2,stroke)[1,2])



#create the training and test data using validation set approach
set.seed(11)
train=sample(1:nrow(df), nrow(df)/2)
df.test=df[-train,]
dim(df.test)

#create the DV in the test data
stroke.test=stroke[-train]


#run logistic regression in the training dataset
glm.fits=glm(stroke~.,data=df,family=binomial, subset=train)
summary(glm.fits)

#predicted probabilities of having stroke with test data set
glm.probs=predict(glm.fits,df.test,type="response")

#change the probabilities to value (>0.25 is having stroke) for 2454 observations in the test data
glm.pred=rep(0,2454)
glm.pred[glm.probs>.25]=1

#compare the predicted value of DV with the true value of DV in the test dataset
table(glm.pred,stroke.test)
#calculate the accuracy rate (how much percentage of correct predictions)
mean(glm.pred==stroke.test)
#calculate the test error
mean(glm.pred!=stroke.test)
#precision rate
12/(55+12)
table(glm.pred,stroke.test)[2,2]/(table(glm.pred,stroke.test)[2,2]+table(glm.pred,stroke.test)[2,1])

#recall rate
12/(12+91)
table(glm.pred,stroke.test)[2,2]/(table(glm.pred,stroke.test)[2,2]+table(glm.pred,stroke.test)[1,2])


##redo logistics regression with selected predictors
glm.fits=glm(stroke~gender+age+hypertension+heart_disease+ever_married+Residence_type+avg_glucose_level+smoking_status,data=df,family=binomial, subset=train)
summary(glm.fits)


#predicted probabilities of having stroke for with test dataset with significant predictors only
glm.probs=predict(glm.fits,df.test,type="response")

#change the probabilities to value (>0.5 is having stroke) for 2454 observations in the test data
glm.pred=rep(0,2454)
glm.pred[glm.probs>.25]=1

#compare the predicted value of DV with the true value of DV in the test dataset
table(glm.pred,stroke.test)
#recalculate the accuracy rate (how much percentage of correct predictions)
mean(glm.pred==stroke.test)
#recalculate the test error
mean(glm.pred!=stroke.test)

#recalculate precision rate
#14/(52+14)
table(glm.pred,stroke.test)[2,2]/(table(glm.pred,stroke.test)[2,2]+table(glm.pred,stroke.test)[2,1])

#recalculate recall rate
#14/(89+14)
table(glm.pred,stroke.test)[2,2]/(table(glm.pred,stroke.test)[2,2]+table(glm.pred,stroke.test)[1,2])


stargazer(glm2, title = "Logistic Regression", no.space = TRUE)
stargazer(table(glm.pred2,stroke), title = "Predicted vs Actual Stroke", no.space = TRUE, notes = "\\ {0=} Stroke No, {1=} Stroke Yes, {NA=} Number of Observations")
stargazer(glm.fits, title = "Logistic Regression with Validation Set", no.space = TRUE)
stargazer(table(glm.pred,stroke.test), title = "Predicted vs Actual Stroke with Validation Set", no.space = TRUE, notes = "\\ {0=} Stroke No, {1=} Stroke Yes, {NA=} Number of Observations")


##build a persona based on relative frequency and median of the dataset for each column
table(df$gender) #female (2897), male (2011)
median(df$age) #44
table(df$hypertension) # hypertension_0 (4457), hypertension_1 (451)
table(df$heart_disease) # heartdisease_0 (4665), heartdisease_1 (243)
table(df$ever_married) # no (1704), yes (3204)
table(df$work_type) # children (671), govt (630), never worked (22), private (2810), self-employed (775)
table(df$Residence_type) # rural (2418), urban (2490)
median(avg_glucose_level) #91.68 #mean (105.2974)
median(df$bmi) #28.1 #mean (28.89456)
table(df$smoking_status) # formerly smoked (836), never smoked (1852), smokes (737), unknown (1483)

##predict someone has a stroke (or not) using a fake persona. 
#suppose: predict the probability of having a stroke for a 28 years old male who is single, never smokes, doesn't have heart disease or hypertension and lives in an urban area.
#gender: male, age: 28, hypertension: 0, heart_diesase: 0, ever_married:no, residence_type: urban, glucose_level: 99, smoking_status: never smoked

persona <- data.frame(gender=c("Male"),age=c(28),heart_disease=c("0"),hypertension=c("0"),ever_married=c("No"),Residence_type=c("Urban"),avg_glucose_level=c(99),smoking_status=c("never smoked"))
persona.pred <- predict(glm.fits,persona,type="response")
persona.pred 
#probability is <0.25 cutoff value of having stroke so the model predict this person is less likely have stroke


#retry logistic regression using LOOCV
glm2<-glm(stroke~gender+age+hypertension+heart_disease+ever_married+Residence_type+avg_glucose_level+smoking_status,data=df,family=binomial)
coef(glm2)
library(boot)
cv.err=cv.glm(df,glm2)

cv.err #the LOOCV
cv.err$delta #cross validation error

#accuracy rate for LOOCV
1-cv.err$delta[1]