##########Written By Liam Flaherty For ST558 Project 3##########
#####1. Load Data And Required Packages#####
###1a. Load Packages###
library(plumber)
library(tidyverse)
library(ranger)
library(glmnet)
library(leaps)
library(caret)

###1b. Load in Data###
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

load("log_reg_max.R")
mymodel=coef(log_reg_max$finalModel)



###1c. Get default values###
mcf=function(data, col) {
  data |>
    count(across(all_of(col)), sort=TRUE) |>
    slice(1) |>
    pull(1)
}

dGenHlth=mcf(df, "GenHlth")
dBMI=mcf(df, "BMI")
dHighBP=mcf(df, "HighBP")
dHighChol=mcf(df, "HighChol")
dHeartDiseaseorAttack=mcf(df, "HeartDiseaseorAttack")
dAge=mcf(df, "Age")
dHvyAlcoholConsump=mcf(df, "HvyAlcoholConsump")
dDiffWalk=mcf(df, "DiffWalk")
dIncome=mcf(df, "Income")



#####2. Prediction Endpoints#####
#This endpoint should take in any predictors used in your ‘best’ model.#
#You should have default values for each that is the mean of that variable’s values.#
#Below this API put three example function calls to the API that I can easily copy and paste to check that it works.#

#* @apiTitle Predicting diabetes
#* @apiDescription Using small logistic model to predict the probability of diabetes
#* @param GenHlth General Health (1-5)
#* @param BMI Body Mass Index (numeric)
#* @param HighBP High Blood Pressure (binary)
#* @param HighChol High Cholesterol (binary)
#* @param HeartDiseaseorAttack History of Heart Disease or Attack (binary)
#* @param Age Age (1 through 9 categorical)
#* @param HvyAlcoholConsump Heavy Alcohol Consumption (binary)
#* @param DiffWalk Difficulty Walking (binary)
#* @param Income Income Category (1-8 categorical)
#* @get /pred

function(GenHlth=(dGenHlth),  
         BMI=(dBMI),     
         HighBP=(dHighBP),
         HighChol=(dHighChol), 
         HeartDiseaseorAttack=(dHeartDiseaseorAttack), 
         Age=(dAge),     
         HvyAlcoholConsump=(dHvyAlcoholConsump), 
         DiffWalk=(dDiffWalk), 
         Income=(dIncome)) {
  
  # Convert inputs to appropriate types
  new_data=data.frame(
    GenHlth=(GenHlth),  
    BMI=(BMI),     
    HighBP=(HighBP),
    HighChol=(HighChol), 
    HeartDiseaseorAttack=(HeartDiseaseorAttack), 
    Age=(Age),     
    HvyAlcoholConsump=(HvyAlcoholConsump), 
    DiffWalk=(DiffWalk), 
    Income=(Income)
  )
  
  # Make The Prediction
  pred_prob=predict(model, newdata=new_data, type="prob")
  
  # Output Results
  return(list(
    diabetes_probability=pred_prob[1, "Diabetes"],
    input_data=new_data
  ))
}


###2b. info###
#This endpoint shouldn’t have any inputs. The output should be a message with:#
#Your name#
#A URL for your rendered github pages site#

#* @get /info
function(){
  "Name: Liam Flaherty, Github link: https://github.com/l-flaherty/ST558-Project-3"
}