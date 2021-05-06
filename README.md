# Decision-Stumps-Algorithm
Decision stumps (DS) and boosted decision stumps (BDS) for regression.
Decission stumps are decision trees with one split. 
## Description
I apply Decision stump to the Boston data set to predict medv given lstat and rm. (In other words, medv is the label and lstat
and rm are the attributes).
The data set is split into training and test sets. The test residual sum of squares is given by 
<img src="https://user-images.githubusercontent.com/35296145/117314618-db1aa180-ae7e-11eb-90e1-1e80c3997ac5.PNG" width="15%"></img>


I apply boosting on the decision stump to see if the test MSE is reduced or increased as a result of boosting.
