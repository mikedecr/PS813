# ----------------------------------------------------
#   Guide for problem set 5, PS 813
#   Michael DeCrescenzo, Feb 26, 2020
# ----------------------------------------------------

# On your computer, you should have a folder for your PS 813 material.
# (If you don't, you should run all of your 813 homework out of one folder!)

# Directly inside your ps813 folder, you need a folder for data. 
# Call it "data".
# Make this folder if you don't have it.

# There is a dataset for Problem Set 5 on Canvas
#   on the main page under "Problem Sets."
# Download this dataset and save it in your ps813/data folder.

# If this data folder is not in its proper place, 
#   and the data are not properly in that folder,
#   the code below WILL NOT WORK.

# Open RStudio using the .Rproj file
#   (that you should have created for this class folder!),
#   which will open RStudio with the working directory 
#   already set to your ps813 folder.


# ---- load packages -----------------------

# packages you need
library("here")
library("tidyverse")


# ---- import data -----------------------

crime <- haven::read_dta(here("data", "Prob_5_data.dta")) 

# the :: syntax says,
#   "use the read_dta() function from the {haven} package"
#   even if we don't library() this package

# if this didn't work: 
# install.packages("haven")
# and then re-run it




# ---- estimate regression -----------------------

# the tools discussed in the previous R script on REGRESSION 
#   will help you here
# I sent this to you 1+ week ago

model <- lm(pcrimer ~ povr, data = crime)

# In particular, you can use broom::glance() and broom::tidy() 
#   to evaluate the model fit and coefficients,
# and broom::augment() to get predictions, residuals, etc.




# ---- adding data to the dataset -----------------------

# The easiest way to do this:
# begin by exporting the data as a CSV file
# MUST include .csv extension in file name

write_csv(crime, here("data", "prob-5-export.csv"))



# add variable to data using Excel or whatever you want.
# Re-save this modified data as a CSV using Excel.




# Then bring the revised data back into R (using read_csv() this time).
# Notice the filename is changed to some hypothetical revised dataset

new_crime <- read_csv(here("data", "prob-5-revised.csv"))


# you estimate a multiple regression using the same lm() function
# only you add multiple independent variables with a plus sign.

new_model <- lm(pcrimer ~ povr + new_variable, data = new_crime)







