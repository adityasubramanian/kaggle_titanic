import csv as csv
import numpy as np

csv_file_object = csv.reader(open('../kaggle_data/train.csv', 'rb')) 
header = csv_file_object.next()  # The next() command just skips the 
                                 # first line which is a header
data=[]                          # Create a variable called 'data'.
for row in csv_file_object:      # Run through each row in the csv file,
    data.append(row)             # adding each row to the data variable
data = np.array(data) 	         # Then convert from a list to an array
			         # Be aware that each item is currently
                                 # a string in this format


number_passengers = np.size(data[0::,1].astype(np.float))
number_survived = np.sum(data[0::,1].astype(np.float))
proportion_survivors = number_survived / number_passengers

women_only_stats = data[0::,4] == "female" # This finds where all 
men_only_stats = data[0::,4] != "female"   # This finds where all the 

# Using the index from above we select the females and males separately
women_onboard = data[women_only_stats,1].astype(np.float)     
men_onboard = data[men_only_stats,1].astype(np.float)

# Then we finds the proportions of them that survived
proportion_women_survived = \
                       np.sum(women_onboard) / np.size(women_onboard)  
proportion_men_survived = \
                       np.sum(men_onboard) / np.size(men_onboard) 

# and then print it out
print 'Proportion of women who survived is %s' % proportion_women_survived
print 'Proportion of men who survived is %s' % proportion_men_survived


test_file = open('../csv/test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()

prediction_file = open("genderbasedmodel.csv", "wb")
prediction_file_object = csv.writer(prediction_file)

prediction_file_object.writerow(["PassengerId", "Survived"])
for row in test_file_object:       # For each row in test.csv
    if row[3] == 'female':         # is it a female, if yes then                                       
        prediction_file_object.writerow([row[0],'1'])    # predict 1
    else:                              # or else if male,       
        prediction_file_object.writerow([row[0],'0'])    # predict 0
test_file.close()
prediction_file.close()


# So we add a ceiling
fare_ceiling = 40
# then modify the data in the Fare column to = 39, if it is greater or equal to the ceiling
data[ data[0::,9].astype(np.float) >= fare_ceiling, 9 ] = fare_ceiling - 1.0

fare_bracket_size = 10
number_of_price_brackets = fare_ceiling / fare_bracket_size

# I know there were 1st, 2nd and 3rd classes on board
number_of_classes = 3

# But it's better practice to calculate this from the data directly
# Take the length of an array of unique values in column index 2
number_of_classes = len(np.unique(data[0::,2])) 

# Initialize the survival table with all zeros
survival_table = np.zeros((2, number_of_classes, number_of_price_brackets))

for i in xrange(number_of_classes):       #loop through each class
  for j in xrange(number_of_price_brackets):   #loop through each price bin

    women_only_stats = data[                             
                         (data[0::,4] == "female")   
                       &(data[0::,2].astype(np.float)
                             == i+1)         
                       &(data[0:,9].astype(np.float) 
                            >= j*fare_bracket_size)  
                       &(data[0:,9].astype(np.float) 
                            < (j+1)*fare_bracket_size)
                          , 1]                                                   
 						                                    									


    men_only_stats = data[                            
                         (data[0::,4] != "female")   
                       &(data[0::,2].astype(np.float) 
                             == i+1)                 
                       &(data[0:,9].astype(np.float)  
                            >= j*fare_bracket_size)                
                       &(data[0:,9].astype(np.float) 
                            < (j+1)*fare_bracket_size)    
                          , 1]      