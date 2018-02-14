# load the data frame
employeeTermData = read.csv("C:/Users/Bhagya Amarasekara/Documents/Bhagya/learning/people analytics/employee churn/MFG10YearTerminationData.csv")
myData = employeeTermData
names(myData)
str(myData)
summary(myData)
# analysing data
statsCount=as.data.frame.matrix(table(myData[,c("STATUS_YEAR", "STATUS")]))
statsCount$TOTAL = statsCount$ACTIVE + statsCount$TERMINATED
statsCount$TermPerc = statsCount$TERMINATED*100/statsCount$TOTAL
mean(statsCount$TermPerc)
library(ggplot2)
p =ggplot(myData, aes(x=BUSINESS_UNIT))
p +geom_bar(aes(fill=STATUS))
terminateddb=myData[myData$STATUS == "TERMINATED",]
p1 = ggplot(terminateddb, aes(x=as.factor(STATUS_YEAR)))
p1 +geom_bar(aes(fill=termtype_desc))
p2 = ggplot(terminateddb, aes(x=as.factor(STATUS_YEAR)))
p2 +geom_bar(aes(fill=termreason_desc))
p3 = ggplot(terminateddb, aes(x=department_name))
p3 +geom_bar(aes(fill=termreason_desc))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(density(terminateddb$age))
rug(terminateddb$age)
library(lattice)
densityplot(~age,groups=STATUS,data=myData,auto.key=TRUE)
densityplot(~length_of_service,groups=STATUS,data=myData,auto.key=TRUE)
bwplot(age~STATUS,data=myData)
bwplot(length_of_service~STATUS,data=myData)
# building the model
#partitioning data 10-fold cross validation, 9 years for training, 1 year for testing
library(rattle)
seedn=42
set.seed(seedn)
myNobs = nrow(myData)
mySample = myTrain = subset(myData,STATUS_YEAR <= 2014)
myValidate = NULL
myTest = subset(myData,STATUS_YEAR == 2014)
myInput <- c("age", "length_of_service", "gender_full","STATUS_YEAR", "BUSINESS_UNIT")
myNumeric <- c("age", "length_of_service", "STATUS_YEAR")
myCategoric <- c("gender_full", "BUSINESS_UNIT")
myTarget <- "STATUS"
myRisk <- NULL
myIdent <- "EmployeeID"
myIgnore <- c("recorddate_key", "birthdate_key", "orighiredate_key", "terminationdate_key", "city_name","job_title", "store_name")
myWeights <- NULL
myTrainingData<-myTrain[c(myInput, myTarget)]
myTestingData<-myTest[c(myInput, myTarget)]
library(rpart)
# decision trees
emp_churn_tree <- rpart(STATUS~.,data = myTrainingData, method = "class")
fancyRpartPlot(emp_churn_tree)
# random forest
library(randomForest)
emp_churn_forrest <- randomForest(STATUS ~.,data = myTrainingData)
emp_churn_forrest
importance(emp_churn_forrest)
# ada boost
library("ada")
emp_churn_ada <- ada(STATUS ~. , data = myTrainingData)
print(emp_churn_ada)
print(sort(names(listAdaVarsUsed(emp_churn_ada))))
print(listAdaVarsUsed(emp_churn_ada))
# SVM
library(kernlab)
emp_churn_svm <- ksvm(STATUS ~.,data = myTrainingData, kernel = "rbfdot", prob.model = TRUE)

