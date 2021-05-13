# Data-Science-Capstone-Project-IDV-Learners

  TO RUN THE CODE, PLEASE DOWNLOAD the CSV FILE TO YOUR R DEFAULT WORKING DIRECTORY
  #####################################################################################################################################


 For this project, I have chosen the dataset "Pima Indians Diabetes Database" in Kaggle. It is a subset of the orginal dataset from the National Institute of Diabetes and Digestive and Kidney Diseases. All patients in this dataset are females of atleast 21 years old and Pima Indian heritage. 

The dataset contains eight predictor variables - 

1) Pregnancies - Number of times pregnant

2) Gluose - Plasma glucose concentration a 2 hours in an oral glucose tolerance test

3) BloodPressure - Diastolic blood pressure (mm Hg)

4) SkinThickness - Triceps skin fold thickness (mm)

5) Insulin - 2-Hour serum insulin (mu U/ml)

6) BMI - Body mass index (weight in kg/(height in m)^2)

7) DiabetesPedrigreeFunction - Diabetes pedigree function. It is diabetes likelihood function based on family history

8) Age - Age (years)

  The outcome/target variable is a binary variable indicating if a person has diabetes or not. The number of observations is 768.

  The goal is to predict if a female person of Pima Indian heritage has diabetes or not based on certain predictors. This is binary classification machine learning.

  Exploratory data anlysis was done to look at the distributions, correlations and data inconsistencies of the predictors and outcome. Four models were built using machine learning algorthims. The final model chosen is a model fit using k-nearest neighbours algorithm with pregnancies, glucose, blood pressure, skin thickness, insulin, BMI, diabetes pedigree function and age as predictors. The parameter for number of neighbours (k) was tuned using bootstrapping. This model was chosen as it had the highest accuracy and F1 score.The accuracy was 0.7792208 and F1 score was 0.8411215
