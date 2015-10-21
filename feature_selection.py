import scipy.io as sio
import numpy as np
from sklearn.feature_selection import RFE
from sklearn.feature_selection import RFECV
from sklearn.svm import SVR
import sys
import csv
from sklearn.metrics import mean_squared_error
from sklearn.datasets import make_friedman1
from sklearn.ensemble import GradientBoostingRegressor

def convert_list_to_matrix(inp, rows, columns):
	'''
	Converts a 1D vector/list into a matrix based on the rows/columns parameters that are passed in.
	'''
	myarray = np.asarray(inp) # converts the input into a numpy array object (Python list -> numpy array)
	matrix = np.resize(myarray, (rows, columns)) # resizes the array as the parameters specify
	return matrix
'''
Testing out the feature selection functionalities on scikit-learn.
Loading a specific matrix file and running functions on them
'''
##Example Usage##
def gradient_boosting(features_values_temp, rows_temp, columns_temp, prediction_values_temp, kernel, threshold):
	#kernel: linear, poly, rbf, sigmoid, precomputed

	rows = 0
	while rows_temp > 0:
		rows = rows + 1
		rows_temp = rows_temp - 1

	columns = 0
	while columns_temp > 0:
		columns = columns + 1
		columns_temp = columns_temp - 1

	features_values = [x for x in features_values_temp]
	prediction_values = [y for y in prediction_values_temp]



	rotated = convert_list_to_matrix(features_values, rows, columns)
	scores = np.array(prediction_values)

	threshold = float(threshold)

	estimator = SVR(kernel=kernel) # try to change to the model for which the test is gonna run (lasso, ridge, etc.)

	 X, y = make_friedman1(n_samples=1200, random_state=0, noise=1.0)
	 X_train, X_test = X[:200], X[200:]
	 y_train, y_test = y[:200], y[200:]
	 est = GradientBoostingRegressor(n_estimators=100, learning_rate=0.1, max_depth=1, random_state=0, loss='ls').fit(X_train, y_train)
	 mean_squared_error(y_test, est.predict(X_test)) 



def feature_selection_with_cv(features_values_temp, rows_temp, columns_temp, prediction_values_temp, kernel, threshold):

	#kernel: linear, poly, rbf, sigmoid, precomputed

	rows = 0
	while rows_temp > 0:
		rows = rows + 1
		rows_temp = rows_temp - 1

	columns = 0
	while columns_temp > 0:
		columns = columns + 1
		columns_temp = columns_temp - 1

	features_values = [x for x in features_values_temp]
	prediction_values = [y for y in prediction_values_temp]



	rotated = convert_list_to_matrix(features_values, rows, columns)
	scores = np.array(prediction_values)

	threshold = float(threshold)

	estimator = SVR(kernel=kernel) # try to change to the model for which the test is gonna run (lasso, ridge, etc.)

	###############START: PLAYING AROUND WITH RECURSIVE FEATURE WITH CROSS VALIDATION FUNCTION.#####################
	#####Seems to be a bit different. RFE (without cross validation) allows us to choose a number of features we're ###
	#####looking for. It seems that cross valdiation chooses the optimal number? So no threshold? Not positive.#######



	 selector = RFECV(estimator, step=1, cv=5)
	 selector = selector.fit(rotated, scores)
	 selector.support_

	 print selector.support_
	 features_used = [i+1 for i, x in enumerate(selector.support_) if x == True] # i+1 b/c matlab starts indexing from 1

	 print features_used

	 features_used = []
	 threshold = selector.score(rotated, scores) ####perhaps if this is the "optimal # of features" we could use this value as
	 											#the RFE threshold value.
	 print "threshold: "
	 print threshold


	################################################################################################
	#######################END: RECURSIVE FEATURE WITH CROSS VALIDATION#############################
	################################################################################################
def feature_selection(features_values_temp, rows_temp, columns_temp, prediction_values_temp, kernel, threshold):

	#kernel: linear, poly, rbf, sigmoid, precomputed

	# for whatever reason, I cannot directly use the parameters that are passed in to run in the feature selection functions
	# because of this, the next several lines are essentially redefining the parameters and storing them in another variable name
	rows = 0
	while rows_temp > 0:
		rows = rows + 1
		rows_temp = rows_temp - 1

	columns = 0
	while columns_temp > 0:
		columns = columns + 1
		columns_temp = columns_temp - 1

	features_values = [x for x in features_values_temp]
	prediction_values = [y for y in prediction_values_temp]
	# end of defining parameters

	rotated = convert_list_to_matrix(features_values, rows, columns)
	scores = np.array(prediction_values)
	threshold = float(threshold)

	estimator = SVR(kernel=kernel)

	#Running binary search to help find the correct features that meet the specified threshold.
	lower_bound = 0
	upper_bound = columns

	while(upper_bound - lower_bound > 1):
		current_selector = (lower_bound + upper_bound)/2
		selector = RFE(estimator, current_selector, step=1)
		selector = selector.fit(rotated, scores)
		if selector.score(rotated, scores) > threshold:
			upper_bound = current_selector
		else:
			lower_bound = current_selector

	 print "second threshold: "
	 print selector.score(rotated, scores)


	# To extract the features that the model used. This value is returned back to matlab
	features_used = [i+1 for i, x in enumerate(selector.support_) if x == True] # i+1 b/c matlab starts indexing from 1
	return features_used

def feature_sorting(features_values_temp, rows_temp, columns_temp, prediction_values_temp, kernel, threshold):
	rows = 0
	while rows_temp > 0:
		rows = rows + 1
		rows_temp = rows_temp - 1

	columns = 0
	while columns_temp > 0:
		columns = columns + 1
		columns_temp = columns_temp - 1

	features_values = [x for x in features_values_temp]
	prediction_values = [y for y in prediction_values_temp]

	rotated = convert_list_to_matrix(features_values, rows, columns)
	# print rotated.shape
	scores = np.array(prediction_values)

	threshold = float(threshold)

	estimator = SVR(kernel=kernel) # try to change to the model for which the test is gonna run (lasso, ridge, etc.)

	selector = RFE(estimator, 0, step=1)
	selector = selector.fit(rotated, scores)
	features_used = [i for i, x in enumerate(selector.support_) if x == True] # i+1 b/c matlab starts indexing from 1

	return selector.ranking_.tolist()


#command line arguments.
if __name__ == '__main__':
	# root_path = sys.argv[1]
	# input_path = sys.argv[2]
	# output_path = sys.argv[3]
	features_values = sys.argv[4]
	prediction_values = sys.argv[5]
	kernel = sys.argv[6]
	threshold = sys.argv[7]


	feature_selection(features_values, 1, 1, prediction_values, kernel, threshold)
s