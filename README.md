# Cricket_ODIs_Match_Win_Prediction

Predicting Chances of a Team Winning an ODI Cricket Match

Steps Involved:
- Web Scrapping Data from Different Cricket Blogs and Data Archives
- Cleaning and Formating Data and Storing it into CSV format
- Three Sets of CSV files, All Team Won/Loose Records, All Team Batting Records, All Team Bowling Records
- These files are used for Calculating Different Performance metrics, used as Independent variables
- Using LogisticRegression Algorithm to Build the model
- Step-wise regression for variable selection in multiple Iterations
- VIF score for removing multicollinearity in multiple Iterations
- AUC, Kappa, Confusion matrix, Type-I/II error for Model Evaluation
- Using K-fold Cross-validation for model validation
- Predicting New Match Probablity for each team

