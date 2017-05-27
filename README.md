# Cricket_ODIs_Match_Win_Prediction

Predicting Chances of a Team Winning an ODI Cricket Match

Steps Involved:
- Web Scrapping Data from Different Cricket Blogs and Data Archives
- Cleaning and Formating Data and Storing it into CSV format
- Three Sets of CSV files, All Team Won/Loose Records, All Team Batting Records, All Team Bowling Records
- Calculating Different Performance metrics, used as Independent variables
- Recursive Feature Elimination (Random Forest CV Evaluation) & Step-wise regression Technique is used for Feature Selection 
- VIF score for removing multicollinearity in multiple Iterations
- Using LogisticRegression Algorithm to Build the model
- AUC, Kappa, Confusion matrix, Type-I/II error for Model Evaluation
- Using K-fold Cross-validation for model validation
- Predicting Winning Probablity for Each Team Playing against Each other in ODI Match

