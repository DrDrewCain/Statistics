# Statistics486
Compromises of all the codes I've written for Statistical &amp; Computing AppStat (486) at Rutgers University

The written language is in R where we will be computing numerous data sets as well as apply various statistical methods onto statistical applications. 

# Assignment 2 is to create a smooth.spline function using the data NOAA or NOAANew and create a linearfit, and a default fit, perform sqrt transformation, and plot the various qq-plot as required. 

#Assignment 2 requirement
-Build a function which:
– takes as input a data frame, and the two elements of a frame that you want to model

–Plot the data fit with a smoothing spline with the default cross validation, vs the smoothing spline with 2 degrees of freedom (linear fit).

–Test the difference of fit using the F test

–Plot the normal quantile quantile plots for the residuals
-Perform sqrt transformation on the data only if user wants to perform it (such as inputting a -1 or any value < 0) 
#Run it on the NOAAnew data


# Assignment 3 is to create a bootstrap pointwise confidence band which will fit a smooth.spline function essentially created in assignment 2 into the band
Requirement - 
- Build bootstrap pointwise confidence bands for smooth around the default smooth with smooth.spline. 
- add the linear fit as well to see if it falls inside the bands or not.
- For 20 points extra credit, do the bootstrapping without the residuals by taking the x,y pairs at random in bootstrapping process

Assignment 3 gave me quite a headache to figure out because this course was my first time programming in R and I wasn't too familiar with the various techniques and libraries which R contains. Although, I knew that R was similar to Python and C I knew that it was a language that I still was not familiar with. So I spent a lot of time trying to read the various documentations that was widely available to help me figure out the ways to create this bootstrap function. 

