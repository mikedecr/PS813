/* This file installs Dave's program,
   creates the covariate data 
   (from a fixed seed in the program; the seed in this file does not matter)
   and then saves the data.

   The R program then pulls the stata data from Github, 
   and simulates the DV according to the user's seed num
*/

net install PS813_EX4, from(https://faculty.polisci.wisc.edu/weimer)
PS813_EX4 9870
save data/probat.dta, replace