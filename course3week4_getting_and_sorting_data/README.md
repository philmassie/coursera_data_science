README.md
==
This repo was created as a submission for the Coursera Data Science Specialisation, Module 3, Getting and Sorting Data.

the data set for the assignment is available from [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip).

This repo includes the following files:
- `README.md`: Overview of the purpose and workflow of the scripts
- `CodeBook.md`: Overview of data sets, and wrangling steps
- `run_analysis.R`: R script for running the data processing from the raw data to the final tidy data set.

The following data files from the original data set are required to run `run_analysis.R`

- `activity_labels.txt`
- `features.txt`
- `subject_test.txt`
- `subject_train.txt`
- `X_test.txt`
- `X_train.txt`
- `y_test.txt`
- `y_train.txt`

The following libraries are required to implement the script
- `dplyr`
- `reshape2`

Execution
--
Once the data files are in the correct folder, and the required libraries are installed, simply execute the run_analysis.R file. The file concludes with a simple loop experiment to make sure the procedure went according to plan.

Transformations
--
For the purposes of this analysis, from the original dataset, the following files were loaded into R.
- `train/subject_train.txt`: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- `train/X_train.txt`: Training set data.
- `train/y_train.txt`: activity list id's.
- `test/subject_train.txt`: Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- `test/X_test.tx`t: Test set data.
- `test/y_test.txt`: Subject id's.
- `features.txt`: List of all variables.
- `activity_labels.txt`: Links the class labels with their activity name.

A full, ordered list of test subjects was generated by binding the `subject_train.txt` and `subject_test.txt` data sets.

The full ordered training set data were generated by binding the `x_test` and `X_train` data sets.

The full, ordered activity list was generated by binding the `y_train` and `y_test` data sets. Using a left join, these id's were associated with the descriptive activity names from the `activity_lables.txt` file.

The three resulting data sets were then combined via a column bind into a full data set (`data_full`). This data set included subject ID, activity ID, Activity name followed by all the data.

`data_full` was then assigned column names. Apart from the first 3 columns ("subject", "activityid", "activityname") the remaining column names were derived from the features text file.

data_full was then subset to include only the `subject` and `activityname` column, along with the columns whose names contained `mean()` or `std()`

In order to achieve mean values of each variable for each activity and each subject, the data was first melted into long format using `subject` and `activityname` as id variables `data_long`. 

These data were then cast back into wide format and the values meaned `data_wide_means`. 

Finally these wide data were melted again into long format with activity and activity name as id variables `tidy data`.



The brief for the assignment was as follows:
==
### Instructions ###

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
Review criterialess 

    The submitted data set is tidy.
    The Github repo contains the required scripts.
    GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
    The README that explains the analysis files is clear and understandable.
    The work submitted for this project is the work of the student who submitted it.

Getting and Cleaning Data Course Projectless 
--
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Good luck!