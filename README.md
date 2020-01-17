# HR-Analytics
An R shiny application employing Descriptive and Predictive Analytics to estimate the probability of attrition of an employee based on numerous input factors.

The dataset for this analysis was obtained from Kaggle. Additional variables were derived from the existing data contained in 5 files. Elimination of missing values was done through synthetic imputation; categorical variables were one-hot encoded and multicollinear variables along with variables with near-zero variation were dropped. Linear combinations were identified and removed before scaling the data using a min-max normalization method. Important relationships between the predictors and target variables were identified through exploratory data analysis.

Logistic and neural network models were unable to provide good results but XGBoost, which has hyper-parameter tuning capabilities and selects variables based on its importance, proved to be a prudent method to model the data.

With the development of this prediction model, it is possible to predict the probable attrition of an employee. R shiny application provides an interactive interface for stakeholders to see current trends of attrition and to gauge employee’s chance of leaving the company. 
