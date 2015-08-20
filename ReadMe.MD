## Class Project for "Getting and Cleaning Data"

The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

[http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones )




**Here are the steps that must be performed before running the R script:**

1. Download the zip file from [this URL](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip).
2. Unzip the file and rename the folder as "data".
3. Make sure that the folder "data", and the run_analysis.R script, are both in the current working directory.
4. Use:   `source("run_analysis.R")`   command in RStudio.
** Note that it requires the [reshape2 package](http://cran.r-project.org/web/packages/reshape2/index.html), which can be downloaded from CRAN.
5. After the execution of the previous step, you will find file: tidyData.txt (220 Kb) [it contains data from a data frame called tidy with 180x66 dimension. Check RStudio Environment].

You can read more about the data and the analysis in the `Code Book`.
