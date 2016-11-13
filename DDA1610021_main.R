
#Setting the Environment
rm(list=ls())
setwd("D:\\PGDDA\\Logistic_Regression")

library("car") 
library("Hmisc")
library("ROCR")
library("caret")

#Loading the Dataset

german_credit <- read.csv("german.csv", header = T , stringsAsFactors = F)

german_credit <- unique(german_credit)
sum(is.na(german_credit))


str(german_credit)
summary(german_credit)

#Plot the Univariate Analysis Graphs

#Plot of Duration.in.month
ggplot(german_credit,aes(german_credit$Duration.in.month, fill= as.factor(german_credit$Default_status))) + geom_histogram( binwidth = 5,position = 'fill') + labs(x='Duration in Month',fill= 'Default Status')

#Plot of Credit.amount
ggplot(german_credit,aes(german_credit$Credit.amount, fill= as.factor(german_credit$Default_status))) + geom_histogram( binwidth = 1000,position = 'fill') + labs(x='Credit Amount',fill= 'Default Status')

#Plot of Age.in.Years
ggplot(german_credit,aes(german_credit$Age.in.Years, fill= as.factor(german_credit$Default_status))) + geom_histogram( binwidth = 10,position = 'fill')+ labs(x='Age in Years',fill= 'Default Status')

#Plot of Job_status
ggplot(german_credit,aes(Job_status, fill= as.factor(german_credit$Default_status))) + geom_bar(position = 'fill') + labs(x='Job Status',fill= 'Default Status')

#Plot of Credit.history
ggplot(german_credit,aes(Credit.history, fill= as.factor(german_credit$Default_status))) + geom_bar(position = 'fill') + labs(x='Credit.history',fill= 'Default Status')


# Outlier Treatment for Variables

boxplot.stats(german_credit$Credit.amount)
german_credit$Credit.amount[german_credit$Credit.amount >= 15000.0] <- 15000.0;

boxplot.stats(german_credit$Age.in.Years)
german_credit$Age.in.Years[german_credit$Age.in.Years >= 64] <- 64;

boxplot.stats(german_credit$Duration.in.month)
german_credit$Duration.in.month[german_credit$Duration.in.month >= 42] <- 42;

german_credit$Age.in.Years <- as.factor(ifelse(german_credit$Age.in.Years <= 20,'0-20',ifelse(german_credit$Age.in.Years<=40,'20-40','40+')))          
german_credit$Credit.amount <- as.factor(ifelse(german_credit$Credit.amount<=5000,'0-5000',ifelse(german_credit$Credit.amount<=10000,'5000-10000','10000+')))
german_credit$Duration.in.month <-as.factor(ifelse(german_credit$Duration.in.month<=12,'0-12',ifelse(german_credit$Duration.in.month<=24,'12-24','24+')))





# Dummy variable creation for Age , Credit amount and Duration in Month

str(german_credit$Age.in.Years)
dummy <- as.data.frame(model.matrix(~Age.in.Years ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

dummy<- as.data.frame(model.matrix(~Credit.amount ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

dummy<- as.data.frame(model.matrix(~Duration.in.month ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)


# Variable Transformation and Dummy variable creation for Status.of.existing.checking.account

german_credit$Status.of.existing.checking.account[german_credit$Status.of.existing.checking.account == "A11"] <- "Lt 0";

german_credit$Status.of.existing.checking.account[german_credit$Status.of.existing.checking.account == "A12"] <- "0-Lt 200"
german_credit$Status.of.existing.checking.account[german_credit$Status.of.existing.checking.account == "A13"] <- "GtEq 200"
german_credit$Status.of.existing.checking.account[german_credit$Status.of.existing.checking.account == "A14"] <- "No Acc"

german_credit$Status.of.existing.checking.account <- as.factor(german_credit$Status.of.existing.checking.account)
dummy<- as.data.frame(model.matrix(~Status.of.existing.checking.account ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Credit.history

german_credit$Credit.history[german_credit$Credit.history == "A30"] <- "No Cred exists"
german_credit$Credit.history[german_credit$Credit.history == "A31"] <- "No Credits at this bank"
german_credit$Credit.history[german_credit$Credit.history == "A32"] <- "Existing Credit paid back" 
german_credit$Credit.history[german_credit$Credit.history == "A33"] <- "Credit Payment Delayed"
german_credit$Credit.history[german_credit$Credit.history == "A34"] <-  "Credit Exists"

german_credit$Credit.history <- as.factor(german_credit$Credit.history) 
dummy<- as.data.frame(model.matrix(~Credit.history ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Purpose

german_credit$Purpose[german_credit$Purpose == "A40"] <- "New Car"
german_credit$Purpose[german_credit$Purpose == "A41"] <- "Used Car"
german_credit$Purpose[german_credit$Purpose == "A42"] <- "furniture/equipment"
german_credit$Purpose[german_credit$Purpose == "A43"] <- "radio/television"
german_credit$Purpose[german_credit$Purpose == "A44"] <- "domestic appliances"
german_credit$Purpose[german_credit$Purpose == "A45"] <- "repairs" 
german_credit$Purpose[german_credit$Purpose == "A46"] <-  "education"
german_credit$Purpose[german_credit$Purpose == "A47"] <- "vacation - does not exist?)"
german_credit$Purpose[german_credit$Purpose == "A48"] <- "retraining"
german_credit$Purpose[german_credit$Purpose == "A49"] <- "busines"
german_credit$Purpose[german_credit$Purpose == "A410"] <- "others"

german_credit$Purpose <- as.factor(german_credit$Purpose)
dummy<- as.data.frame(model.matrix(~Purpose ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Savings.account.bonds

german_credit$Savings.account.bonds[german_credit$Savings.account.bonds == "A61"] <- "Lt 100"
german_credit$Savings.account.bonds[german_credit$Savings.account.bonds == "A62"] <- "100-Lt 500 "
german_credit$Savings.account.bonds[german_credit$Savings.account.bonds == "A63"] <- "500-Lt 1000"
german_credit$Savings.account.bonds[german_credit$Savings.account.bonds == "A64"] <- "GtEq 1000"
german_credit$Savings.account.bonds[german_credit$Savings.account.bonds == "A65"] <- "unknown/no savings account"

german_credit$Savings.account.bonds <- as.factor(german_credit$Savings.account.bonds)
dummy<- as.data.frame(model.matrix(~Savings.account.bonds ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)


# Variable Transformation and Dummy variable creation for Present.employment.since.

german_credit$Present.employment.since.[german_credit$Present.employment.since. == "A72"] <- "Lt 1"
german_credit$Present.employment.since.[german_credit$Present.employment.since. == "A73"] <- "GtEq 1 - Lt 4"
german_credit$Present.employment.since.[german_credit$Present.employment.since. == "A74"] <- "GtEq 4 - Lt 7"
german_credit$Present.employment.since.[german_credit$Present.employment.since. == "A75"] <- "GtEq 7"
german_credit$Present.employment.since.[german_credit$Present.employment.since. == "A71"] <- "unemployed"

german_credit$Present.employment.since. <- as.factor(german_credit$Present.employment.since.)
dummy<- as.data.frame(model.matrix(~Present.employment.since. ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Installment.rate.in.percentage.of.disposable.income

german_credit$Installment.rate.in.percentage.of.disposable.income <- as.numeric(german_credit$Installment.rate.in.percentage.of.disposable.income)
german_credit$Installment.rate.in.percentage.of.disposable.income <- as.factor(german_credit$Installment.rate.in.percentage.of.disposable.income)

dummy<- as.data.frame(model.matrix(~Installment.rate.in.percentage.of.disposable.income ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Personal.status.and.sex

german_credit$Personal.status.and.sex[german_credit$Personal.status.and.sex == "A91"] <-"male:divorced/separated"
german_credit$Personal.status.and.sex[german_credit$Personal.status.and.sex == "A92"] <-"female:divorced/separated/married"
german_credit$Personal.status.and.sex[german_credit$Personal.status.and.sex == "A93"] <-"male:single"
german_credit$Personal.status.and.sex[german_credit$Personal.status.and.sex == "A94"] <-"male:married/widowed"
german_credit$Personal.status.and.sex[german_credit$Personal.status.and.sex == "A95"] <-"female:single"

german_credit$Personal.status.and.sex <- as.factor(german_credit$Personal.status.and.sex)
dummy<- as.data.frame(model.matrix(~Personal.status.and.sex ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Other.debtors...guarantors

german_credit$Other.debtors...guarantors[german_credit$Other.debtors...guarantors == "A101"] <-"none" 
german_credit$Other.debtors...guarantors[german_credit$Other.debtors...guarantors == "A102"] <-"co-applicant"
german_credit$Other.debtors...guarantors[german_credit$Other.debtors...guarantors == "A103"] <-"guarantor"

german_credit$Other.debtors...guarantors <- as.factor(german_credit$Other.debtors...guarantors)
dummy<- as.data.frame(model.matrix(~Other.debtors...guarantors ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Present.residence.since

german_credit$Present.residence.since <-  as.numeric(german_credit$Present.residence.since)
german_credit$Present.residence.since <-  as.factor(german_credit$Present.residence.since)

dummy<- as.data.frame(model.matrix(~Present.residence.since ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)


# Variable Transformation and Dummy variable creation for Property

german_credit$Property[german_credit$Property == "A121"] <- "real estate"
german_credit$Property[german_credit$Property == "A122"] <- "blding soc. sav. agrmt/ Life Insur"
german_credit$Property[german_credit$Property == "A123"] <- "car or other"
german_credit$Property[german_credit$Property == "A124"] <- "unknown /no prop."

german_credit$Property <- as.factor(german_credit$Property)
dummy<- as.data.frame(model.matrix(~Property ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Other.installment.plans

german_credit$Other.installment.plans[german_credit$Other.installment.plans == "A141"] <-"bank" 
german_credit$Other.installment.plans[german_credit$Other.installment.plans == "A142"] <-"store"
german_credit$Other.installment.plans[german_credit$Other.installment.plans == "A143"] <-"none"

german_credit$Other.installment.plans <- as.factor(german_credit$Other.installment.plans)
dummy<- as.data.frame(model.matrix(~Other.installment.plans ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Housing.

german_credit$Housing.[german_credit$Housing. == "A151"] <-"rent" 
german_credit$Housing.[german_credit$Housing. == "A152"] <-"own"
german_credit$Housing.[german_credit$Housing. == "A153"] <-"for free"

german_credit$Housing. <- as.factor(german_credit$Housing.)
dummy<- as.data.frame(model.matrix(~Housing. ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Number.of.existing.credits.at.this.bank.

german_credit$Number.of.existing.credits.at.this.bank. <- as.numeric(german_credit$Number.of.existing.credits.at.this.bank.)
german_credit$Number.of.existing.credits.at.this.bank. <- as.factor(german_credit$Number.of.existing.credits.at.this.bank.)

dummy<- as.data.frame(model.matrix(~Number.of.existing.credits.at.this.bank. ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)

# Variable Transformation and Dummy variable creation for Job_status

german_credit$Job_status[german_credit$Job_status == "A171"] <- "unemployed/unskilled - non-resident"
german_credit$Job_status[german_credit$Job_status == "A172"] <- "unskilled - resident"
german_credit$Job_status[german_credit$Job_status == "A173"] <- "skilled employee / official"
german_credit$Job_status[german_credit$Job_status == "A174"] <- "management/ self-employed/highly qualified employee/ officer"

german_credit$Job_status <- as.factor(german_credit$Job_status)
dummy<- as.data.frame(model.matrix(~Job_status ,data =german_credit))
dummy <- dummy[, -1 ]
german_credit<- cbind(german_credit,dummy)


# Variable Transformation  for More.than.1.people.being.liable.to.provide.maintenance.for
german_credit$More.than.1.people.being.liable.to.provide.maintenance.for <- german_credit$Number.of.people.being.liable.to.provide.maintenance.for. - 1
german_credit$More.than.1.people.being.liable.to.provide.maintenance.for <- as.factor(More.than.1.people.being.liable.to.provide.maintenance.for)

# Variable Transformation  for Telephone.

german_credit$Telephone.[german_credit$Telephone. == "A191"] <- 0 
german_credit$Telephone.[german_credit$Telephone. == "A192"] <- 1
german_credit$Telephone. <- as.numeric(german_credit$Telephone.)

# Variable Transformation  for foreign.worker

german_credit$foreign.worker[german_credit$foreign.worker == "A201"] <- 1 
german_credit$foreign.worker[german_credit$foreign.worker == "A202"] <- 0  
german_credit$foreign.worker <- as.numeric(german_credit$foreign.worker)

#Removing the previous categorical variables
german_credit <- german_credit[, -c(1:18)]

#Separating Train and Test Data
library(caTools)
set.seed(100)
split_german_credit <- sample.split(german_credit$Default_status,SplitRatio = 0.7) 
german_credit_train <- german_credit[split_german_credit,]
german_credit_test <-  german_credit[!(split_german_credit),]

#Model Development

intial_model <- glm(Default_status ~., data = german_credit_train, family = "binomial")
summary(intial_model)
best_model <- step(intial_model, direction = "both" )
summary(best_model);
vif(best_model)


# Remove foreign.worker
model_3 <- glm(formula = Default_status ~ `Credit.amount10000+` + 
                 `Duration.in.month12-24` + `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                 `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                 `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                 Purposeeducation + `PurposeNew Car` + `Savings.account.bondsGtEq 1000` + 
                 `Savings.account.bondsLt 100` + `Present.employment.since.GtEq 4 - Lt 7` + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 `Personal.status.and.sexmale:single` + Other.debtors...guarantorsguarantor + 
                 Present.residence.since2 + Other.installment.plansnone + 
                 Housing.rent, family = "binomial", data = german_credit_train)
summary(model_3)
vif(model_3)

# Remove `Savings.account.bondsGtEq 1000` 
model_4 <- glm(formula = Default_status ~ `Credit.amount10000+` + 
                 `Duration.in.month12-24` + `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                 `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                 `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                 Purposeeducation + `PurposeNew Car` + 
                 `Savings.account.bondsLt 100` + `Present.employment.since.GtEq 4 - Lt 7` + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 `Personal.status.and.sexmale:single` + Other.debtors...guarantorsguarantor + 
                 Present.residence.since2 + Other.installment.plansnone + 
                 Housing.rent, family = "binomial", data = german_credit_train)
summary(model_4)

# Remove `Present.employment.since.GtEq 4 - Lt 7` 
model_5 <- glm(formula = Default_status ~ `Credit.amount10000+` + 
                 `Duration.in.month12-24` + `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                 `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                 `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                 Purposeeducation + `PurposeNew Car` + 
                 `Savings.account.bondsLt 100` + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 `Personal.status.and.sexmale:single` + Other.debtors...guarantorsguarantor + 
                 Present.residence.since2 + Other.installment.plansnone + 
                 Housing.rent, family = "binomial", data = german_credit_train)
summary(model_5)

# Remove Housing.rent 
model_6 <- glm(formula = Default_status ~ `Credit.amount10000+` + 
                 `Duration.in.month12-24` + `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                 `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                 `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                 Purposeeducation + `PurposeNew Car` + 
                 `Savings.account.bondsLt 100` + 
                 Installment.rate.in.percentage.of.disposable.income3 + Installment.rate.in.percentage.of.disposable.income4 + 
                 `Personal.status.and.sexmale:single` + Other.debtors...guarantorsguarantor + 
                 Present.residence.since2 + Other.installment.plansnone 
                 , family = "binomial", data = german_credit_train)
summary(model_6)

# Remove Installment.rate.in.percentage.of.disposable.income3 
model_7 <- glm(formula = Default_status ~ `Credit.amount10000+` + 
                 `Duration.in.month12-24` + `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                 `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                 `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                 Purposeeducation + `PurposeNew Car` + 
                 `Savings.account.bondsLt 100` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                 `Personal.status.and.sexmale:single` + Other.debtors...guarantorsguarantor + 
                 Present.residence.since2 + Other.installment.plansnone 
               , family = "binomial", data = german_credit_train)
summary(model_7)

# Remove Duration.in.month12-24` 
model_8 <- glm(formula = Default_status ~ `Credit.amount10000+` + 
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                 `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                 `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                 Purposeeducation + `PurposeNew Car` + 
                 `Savings.account.bondsLt 100` + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 `Personal.status.and.sexmale:single` + Other.debtors...guarantorsguarantor + 
                 Present.residence.since2 + Other.installment.plansnone 
               , family = "binomial", data = german_credit_train)

summary(model_8)

# Remove `Credit.amount10000+`  
model_9 <- glm(formula = Default_status ~  
                 `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                 `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                 `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                 Purposeeducation + `PurposeNew Car` + 
                 `Savings.account.bondsLt 100` + 
                 Installment.rate.in.percentage.of.disposable.income4 + 
                 `Personal.status.and.sexmale:single` + Other.debtors...guarantorsguarantor + 
                 Present.residence.since2 + Other.installment.plansnone 
               , family = "binomial", data = german_credit_train)
summary(model_9)

# Remove Other.debtors...guarantorsguarantor  
model_10 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc` + `Credit.historyExisting Credit paid back` + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Purposeeducation + `PurposeNew Car` + 
                  `Savings.account.bondsLt 100` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  `Personal.status.and.sexmale:single` + 
                  Present.residence.since2 + Other.installment.plansnone 
                , family = "binomial", data = german_credit_train)
summary(model_10)
vif(model_10)

# Remove `Credit.historyExisting Credit paid back` 
model_11 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Purposeeducation + `PurposeNew Car` + 
                  `Savings.account.bondsLt 100` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  `Personal.status.and.sexmale:single` + 
                  Present.residence.since2 + Other.installment.plansnone 
                , family = "binomial", data = german_credit_train)
summary(model_11)

# Remove Savings.account.bondsLt 100`   
model_12 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Purposeeducation + `PurposeNew Car` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  `Personal.status.and.sexmale:single` + 
                  Present.residence.since2 + Other.installment.plansnone 
                , family = "binomial", data = german_credit_train)
summary(model_12)

# Remove Purposeeducation 
model_13 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                   `PurposeNew Car` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  `Personal.status.and.sexmale:single` + 
                  Present.residence.since2 + Other.installment.plansnone 
                , family = "binomial", data = german_credit_train)
summary(model_13)

# Remove PurposeNew Car
model_14 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  `Personal.status.and.sexmale:single` + 
                  Present.residence.since2 + Other.installment.plansnone 
                , family = "binomial", data = german_credit_train)
summary(model_14)

# Remove Other.installment.plansnone
model_15 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  `Personal.status.and.sexmale:single` + 
                  Present.residence.since2 
                , family = "binomial", data = german_credit_train)
summary(model_15)

# Remove Personal.status.and.sexmale:single
model_16 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Installment.rate.in.percentage.of.disposable.income4 + 
                  Present.residence.since2 
                , family = "binomial", data = german_credit_train)
summary(model_16)

# Remove Present.residence.since2
model_17 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + `Status.of.existing.checking.accountGtEq 200` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Installment.rate.in.percentage.of.disposable.income4 
                  , family = "binomial", data = german_credit_train)
summary(model_17)

# Remove Status.of.existing.checking.accountGtEq 200
model_18 <- glm(formula = Default_status ~  
                  `Duration.in.month24+` + 
                  `Status.of.existing.checking.accountNo Acc`  + 
                  `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank` + 
                  Installment.rate.in.percentage.of.disposable.income4 
                , family = "binomial", data = german_credit_train)
summary(model_18)

# Remove Installment.rate.in.percentage.of.disposable.income4 
model_19 <- glm(formula = Default_status ~  
                     `Duration.in.month24+` + 
                     `Status.of.existing.checking.accountNo Acc`  + 
                     `Credit.historyNo Cred exists` + `Credit.historyNo Credits at this bank`
                     , family = "binomial", data = german_credit_train)
summary(model_19)


#Calculating C-Statistic for Train Data
model_final <- model_17
german_credit_train$predicted_prob = predict(model_final, type = "response") 
rcorr.cens(german_credit_train$predicted_prob,german_credit_train$Default_status)

#Calculating C-Statistic for Test Data
german_credit_test$predicted_prob = predict(model_final, newdata =german_credit_test, type = "response") 
rcorr.cens(german_credit_test$predicted_prob,german_credit_test$Default_status)

#Calculating KS-Statistic for Train Data
model_score <- prediction(german_credit_train$predicted_prob,german_credit_train$Default_status)
model_perf <- performance(model_score, "tpr", "fpr")
plot(model_perf)
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf,"x.values")[[1]])
ks <- max(ks_table) 
ks
which(ks_table == ks)/ nrow(german_credit_train)

#Calculating KS-Statistic for Test Data
model_score <- prediction(german_credit_test$predicted_prob,german_credit_test$Default_status)
model_perf <- performance(model_score, "tpr", "fpr")
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf,"x.values")[[1]])
ks <- max(ks_table) 
ks
which(ks_table == ks)/ nrow(german_credit_test)

#Calculating Accuracy,Sensitivity and Specificity with appropriate threshold for Train and Test data
confusionMatrix(as.numeric(german_credit_train$predicted_prob > 0.8),german_credit_train$Default_status )
confusionMatrix(as.numeric(german_credit_test$predicted_prob > 0.8),german_credit_test$Default_status )

#Calculating Overall Accuracy,Sensitivity and Specificity with appropriate threshold
german_credit_Overall <-rbind(german_credit_train,german_credit_test)

confusionMatrix(as.numeric(german_credit_Overall$predicted_prob > 0.8),german_credit_Overall$Default_status )


