##########Written By Liam Flaherty For ST558 Project 3##########
#####1. Load Data And Required Packages#####
library(plumbr)
library(tidyverse)
library(ranger)
library(glmnet)
library(leaps)
library(caret)

diabetes=read_csv("diabetes.csv", show_col_types = FALSE)
df=diabetes |>        #Age is categorical too, but enough buckets to leave as numeric#
  mutate(Diabetes_binary=factor(Diabetes_binary, levels=c(0,1), labels=c("No_Diabetes", "Diabetes")),
         HighBP=factor(HighBP, levels=c(0,1), labels=c("Reg_BP", "High_BP")),
         HighChol=factor(HighChol, levels=c(0,1), labels=c("Reg_Chol", "High_Chol")),
         CholCheck=factor(CholCheck, levels=c(0,1), labels=c("No_Check", "Check")),
         Smoker=factor(Smoker, levels=c(0,1), labels=c("Non_Smoker", "Smoker")),
         Stroke=factor(Stroke, levels=c(0,1), labels=c("No_Stroke", "Stroke")),
         HeartDiseaseorAttack=factor(HeartDiseaseorAttack, levels=c(0,1), labels=c("No_HeartAttack", "HeartAttack")),
         PhysActivity=factor(PhysActivity, levels=c(0,1), labels=c("No_Activity", "Activity")),
         Fruits=factor(Fruits, levels=c(0,1), labels=c("No_Fruits", "Fruits")),
         Veggies=factor(Veggies, levels=c(0,1), labels=c("No_Veggies", "Veggies")),
         HvyAlcoholConsump=factor(HvyAlcoholConsump, levels=c(0,1), labels=c("No_Alc", "Lots_Of_Alc")),
         AnyHealthcare=factor(AnyHealthcare, levels=c(0,1), labels=c("No_HealthCare", "HealthCare")),
         NoDocbcCost=factor(NoDocbcCost, levels=c(0,1), labels=c("Cost_Not_impactful", "No_Doc_BC_Cost")),
         Diffwalk=factor(HighChol, levels=c(0,1), labels=c("Can_Walk", "Cant_Walk")),
         Sex=factor(Sex, levels=c(0,1), labels=c("Female", "Male")),
         GenHlth=factor(GenHlth, levels=1:5, labels=c("Excellent", "Very_Good", "Good", "Fair", "Poor")),
         Education=factor(Education, levels=1:6, labels=c("None", "Elem", "Some_HS", "HS", "Some_College", "College")),
         Income=factor(Income, levels=1:8, labels=c("None", "Poor", "LC", "LMC","MC","UMC","UC","Rich")))

set.seed(558)
split=createDataPartition(y=df$Diabetes_binary, p=0.7, list=FALSE)
train=df[split,]
test=df[-split,]

log_reg_max=train(Diabetes_binary~ GenHlth+BMI+HighBP+
                    HighChol+HeartDiseaseorAttack+Age+
                    HvyAlcoholConsump+DiffWalk+Income,
                  data=train,
                  method="glm",
                  family="binomial",
                  metric="logLoss",
                  trControl=trainControl(
                    method="cv",
                    number=5,
                    classProbs=TRUE,
                    summaryFunction=mnLogLoss
                  ))



log_reg_max
max_pred=predict(log_reg_max, newdata=test, type="prob")
pred_data_max=data.frame(obs=test$Diabetes_binary, max_pred)
log_loss_max=mnLogLoss(pred_data_max, lev=levels(test$Diabetes_binary))
class_predictions_max=predict(log_reg_max, newdata=test, type="raw")
conf_matrix_max=confusionMatrix(data=class_predictions_max, reference=test$Diabetes_binary)
conf_matrix_max




#####2. Endpoints#####
###2a. Pred ###
#This endpoint should take in any predictors used in your ‘best’ model.#
#You should have default values for each that is the mean of that variable’s values.#
#Below this API put three example function calls to the API that I can easily copy and paste to check that it works.#


###2b. info###
#This endpoint shouldn’t have any inputs. The output should be a message with:#
#Your name#
#A URL for your rendered github pages site#