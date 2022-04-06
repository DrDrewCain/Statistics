# Statistics486
Compromises of all the codes I've written for Statistical &amp; Computing AppStat (486) at Rutgers University

The written language is in R where we will be computing numerous data sets as well as apply various statistical methods onto statistical applications. 

# Assignment 2 is to create a smooth.spline function using the data NOAA or NOAANew and create a linearfit, and a default fit, perform sqrt transformation, and plot the various qq-plot as required. 

# Assignment 2 requirement-
- Build a function which:
- takes as input a data frame, and the two elements of a frame that you want to model
- Plot the data fit with a smoothing spline with the default cross validation, vs the smoothing spline with 2 degrees of freedom (linear fit).
- Test the difference of fit using the F test
- Plot the normal quantile quantile plots for the residuals
- Perform sqrt transformation on the data only if user wants to perform it (such as inputting a -1 or any value < 0) 
#Run it on the NOAAnew data


# Assignment 3 is to create a bootstrap pointwise confidence band which will fit a smooth.spline function essentially created in assignment 2 into the band
 # Requirement - 
- Build bootstrap pointwise confidence bands for smooth around the default smooth with smooth.spline. 
- add the linear fit as well to see if it falls inside the bands or not.
- To Do * do the bootstrapping without the residuals by taking the x,y pairs at random in bootstrapping process

Assignment 3 gave me quite a headache to figure out because this course was my first time programming in R and I wasn't too familiar with the various techniques and libraries which R contains. Although, I knew that R was similar to Python and C I knew that it was a language that I still was not familiar with. So I spent a lot of time trying to read the various documentations that was widely available to help me figure out the ways to create this bootstrap function. 

# Assignment 4 
 # requirement
- Build a confidence interval estimator for the mean based on the bootstrap (use 10000 =nboot)
- Build a simulator that draws n samples form a lognormal distribution (rlnorm) and builds both the central limit theorem based confidence interval, and compares it to the coverage rate for the bootstrap (confidence interval based on the the bootstrap program). (1000 simulation runs minimum)

- Run it for lognormal (nsim=1000) for n=3,10,30,100, alpha= .1,.05 That’s 8 runs
- Compare the coverage of the 2 confidence intervals with nominal coverage
- Write up the results with a table
- Submit write up on school platform along with code
- 2̶0̶ ̶p̶o̶i̶n̶t̶s̶ ̶e̶x̶t̶r̶a̶ ̶c̶r̶e̶d̶i̶t̶ ̶i̶f̶ ̶c̶a̶n̶ ̶w̶r̶i̶t̶e̶ ̶b̶o̶o̶t̶s̶t̶r̶a̶p̶ ̶t̶o̶ ̶w̶o̶r̶k̶ ̶w̶i̶t̶h̶o̶u̶t̶ ̶l̶o̶o̶p̶s̶!̶
- (Solo) 
- Code contaisn full comments. 

# Assignment 5
# Requirement
- Functionality Automatic model selection:

-      1) With Leaps, Using Cp, for the 1st stage, and PRESS as the 2nd, then plotting the predictions vs the actual values

-      2) With LARS, Using Cp for the 1st stage and CVLARS as the 2nd, then plotting the predictions vs the actual values

- Apply the two approaches to 

       The full Auto data, modeling mpg  with the full 2nd order matrix
       The 3 Auto data sets  modeling mpg with the full 2nd order matrix based on separating by country of origin
Comment on whether the variables might indicate something about different countries design philosophies.

# Assignment 6 
# Requirement 
- Try animate(mystery9) in R. Then try pairs, lattice.
- If anything like an animal appears google the name of that animal with the words “effect”
- Describe what you saw, describe what you found and describe how you think the curse of dimensionality may have kept you from seeing it with animate quickly.

# Assignment 7
# Requirement

- Create a function that takes the output of each smoother in smoother.pck and calculates its press statistic.
- Apply each to the NOAA.new data using the parameters given
- Now create a function that uses a greedy random search to find better values of the adjustable parameters, based on minimizing PRESS

# Assignment 8
# Requirement

- Rewrite the bootstrap program you wrote for assignment 4
- Where you are taking the mean, put in a user provided statistical function
- Everyplace you take a standard deviation, use a different bootstrap program to calculate the standard error of your statistic instead.
- So inside the bootstrap loop in the main program, you call a different bootstrap program to estimate variance.
- Apply both the original bootstrap confidence estimate of the mean, and your new bootstrap using the median as the estimate of location to the data set special.sample - in xy.pck. Provide the 95% confidence intervals.

# Assignment 9
# Requirement

- Constuct an FDR program using the basic approach
- Input : Vector of P values Q value (desired false discovery rate) Are the data independent or not? (T,F input)
- Output
- Graph like that in slide 14
- The Tests that are considered discoveries (“interesting”) from the original order of the p values
- Whether you assumed independence or not. Comment your code
- Test your code with the vector v1<- (c((1e-5*runif(100)),runif(900))) both assuming independent and not independent at Q=.05 
# RIGHTS AND ACADEMIC INTEGRITY 
Compromises of all the codes I've written for Statistical & Computing AppStat (486) at Rutgers University. Please do not copy my code. All work are copy righted and reserved. If you copy my code and submit as your own, you will get in trouble for failure to comply to academic integrity. All codes shown are to be used as reference unless with explicit permission.
