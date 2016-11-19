#For this problem we will start with a simulation in order to find out how large n needs
#to be for the binomial distribution to be approximated by the normal
#distribution. 

#We will take m samples from the binomial distribution for some n and p.
#1.(4pts.) Let's let p=1/2, use the rbinom function to generate the sample of size m. 
#Add normal curves to all of the plots. 
#Use 3 values for n, 10, 30, and 50. Display the histograms as well as your
#code below. 

#For n = 10
g <- rbinom(n=10, size = 100, prob = 0.5)
d <- density(g) # returns the density data 
plot(d) # plots the results
h <- hist(g)

#For n = 30
g <- rbinom(n=30, size = 100, prob = 0.5)
d <- density(g) # returns the density data 
plot(d) # plots the results
hist(g)

#For n = 50
g <- rbinom(n=50, size = 100, prob = 0.5)
d <- density(g) # returns the density data 
plot(d) # plots the results
hist(g)


#1b.)(3pts.) Now use the techniques described in class to improve graphs. 
# Explain each step you choose including why you are making the change. You
# might consider creating density plots, changing color, axes, labeling, legend, and others for example.

#For n = 10
g <- rbinom(n=10, size = 100, prob = 0.5)
d <- density(g)
plot(d, type = "l", cex.axis = 1.5, main = "Normal Curve", xlab = "Number of Observations", ylab = "Density")
# type: Adding type of the plot as line plot to show the curve clearly anyother type won't make it clear.
# ce.axis: 1.5 increas the font size of the axis poits, which increases the readability for the observer.
# main, xlab, ylab: these labels makes it easier for the observer to understand the parameters and context of the plot.

hist(g, density = 10, main = "Normal Curve", col = c("pink", "dark blue", "light green", "orange"), xlab = "Number of Observations", ylab = "Probability")
# density: shows the shaded bars to add more clarity to each block.
# main, xlab, ylab: these labels makes it easier for the observer to understand the parameters and context of the plot.
# col: adding colors makes the observer understand the density of the data in the plot. The denser block is demostrated by
#      darker shade and rest of the blocks wich are less denser are demostrated by light colors.

#For n = 30
g <- rbinom(n=30, size = 100, prob = 0.5)
d <- density(g) # returns the density data 
plot(d, type = "l", cex.axis = 1.5, main = "Normal Curve", xlab = "Number of Observations", ylab = "Density")
# type: Adding type of the plot as line plot to show the curve clearly anyother type won't make it clear.
# ce.axis: 1.5 increas the font size of the axis poits, which increases the readability for the observer.
# main, xlab, ylab: these labels makes it easier for the observer to understand the parameters and context of the plot.

hist(g, density = 10, main = "Normal Curve", col = c("pink", "dark blue", "light green", "orange"), xlab = "Number of Observations", ylab = "Probability")
# density: shows the shaded bars to add more clarity to each block.
# main, xlab, ylab: these labels makes it easier for the observer to understand the parameters and context of the plot.
# col: adding colors makes the observer understand the density of the data in the plot. The denser block is demostrated by
#      darker shade and rest of the blocks wich are less denser are demostrated by light colors.

#For n = 50
g <- rbinom(n=50, size = 100, prob = 0.5)
d <- density(g)
plot(d, type = "l", cex.axis = 1.5, main = "Normal Curve", xlab = "Number of Observations", ylab = "Density")
# type: Adding type of the plot as line plot to show the curve clearly anyother type won't make it clear.
# ce.axis: 1.5 increas the font size of the axis poits, which increases the readability for the observer.
# main, xlab, ylab: these labels makes it easier for the observer to understand the parameters and context of the plot.

hist(g, density = 10, main = "Normal Curve", col = c("pink", "dark blue", "light green", "orange"), xlab = "Number of Observations", ylab = "Probability")
# density: shows the shaded bars to add more clarity to each block.
# main, xlab, ylab: these labels makes it easier for the observer to understand the parameters and context of the plot.
# col: adding colors makes the observer understand the density of the data in the plot. The denser block is demostrated by
#      darker shade and rest of the blocks wich are less denser are demostrated by light colors.



rm(list=ls())

#Q2.) (2pts.)
#Why do you think the Data Life Cycle is crucial to understanding the opportunities
#and challenges of making the most of digital data? Give two examples.
# Data Life Cycle involves collection of data, Data cleaning, Analysis, Visualisation, Data and code sharing, data and ideas re-use.
# As the size of the data is increasing day by day its becoming more and more unstructred and to draw inferences out of it we need to
# follow all the processes mentioned above. We need to clean the data to represent it in tabular form or any derivable form. And then apply
# techniques on it to draw conclusions from it. Data Visualisation is done to deliver inferences of data to people who don't understand the 
# technicalities of the data and there are many tools and techniques involved for data visualisation processes too. The inferences drawn from
# a particular data may sometimes be applied to another set of data, this process is called data reuse.
# There are a lot of fields where data science and lifecycle processes are used for drawing conculsions and they are mentioned as below:
# (i). Prescriptions of doctors can be considered as data and we can convert it into tabular form extracting the main keywords(cleaning)
# these prescriptions can further be used to identify the similar symptoms in patient to diagnose the same disease. All this can be done
# by following Data Life Cycle processes.
# (ii). The similar kind of things can be done to read the text messages by people to make inferences about their reactions to particular
# expressions and on the basis of that we can make suggestions to user for replies. This can be done by following Data Life Cycle processes.


###Part 2###
#3.)  San Francisco Housing Data
#
# Load the data into R.
load(url("http://www.stanford.edu/~vcs/StatData/SFHousing.rda"))

# (2 pts.)
# What is the name and class of each object you have loaded into your workspace?
### Your code below

l<-ls()
# Name
l[1]
l[2]
# Class
class(l[1])
class(l[2])

### Your answer here


# What are the names of the vectors in housing?
### Your code below

ls(housing)

### Your answer here

# How many observations are in housing?
### Your code below

nrow(housing)

### Your answer here

# Explore the data using the summary function. 
# Describe in words two problems that you see with the data.
#### Write your response here

summary(housing)

# Q5. (2 pts.)
# We will work the houses in Albany, Berkeley, Piedmont, and Emeryville only.
# Subset the data frame so that we have only houses in these cities
# and keep only the variables city, zip, price, br, bsqft, and year
# Call this new data frame BerkArea. This data frame should have 4059 observations
# and 6 variables.

BerkArea <- housing[housing$city == 'Albany' | housing$city == 'Berkeley' | housing$city == 'Piedmont' | housing$city == 'Emeryville', c(2,3,5,6,8,9)]


# Q6. (2 pts.)
# We are interested in making plots of price and size of house, but before we do this
# we will further subset the data frame to remove the unusually large values.
# Use the quantile function to determine the 99th percentile of price and bsqft
# and eliminate all of those houses that are above either of these 99th percentiles
# Call this new data frame BerkArea, as well. It should have 3999 observations.

quantile(BerkArea$price, 0.99)
quantile(BerkArea$bsqft[!is.na(BerkArea$bsqft)], 0.99)
BerkArea <- BerkArea[(!BerkArea$bsqft > quantile(BerkArea$bsqft[!is.na(BerkArea$bsqft)], 0.99)) & (!BerkArea$price > quantile(BerkArea$price, 0.99)),]

# Q7 (2 pts.)
# Create a new vector that is called pricepsqft by dividing the sale price by the square footage
# Add this new variable to the data frame.

BerkArea$pricepsqft <- BerkArea$price/BerkArea$bsqft

#  Q8 (2 pts.)
# Create a vector called br5 that is the number of bedrooms in the house, except
# if this number is greater than 5, it is set to 5.  That is, if a house has 5 or more
# bedrooms then br5 will be 5. Otherwise it will be the number of bedrooms.

br5 <- BerkArea$br
br5[br5 > 5] <- 5

# Q9 (4 pts. 2 + 2 - see below)
# Use the rainbow function to create a vector of 5 colors, call this vector rCols.
# When you call this function, set the alpha argument to 0.25 (we will describe what this does later)
# Create a vector called brCols of 4059 colors where each element's
# color corresponds to the number of bedrooms in the br5.
# For example, if the element in br5 is 3 then the color will be the third color in rCols.

# (2 pts.)

rCols <- rainbow(n = 5, alpha = 0.25)
brCols <- rainbow(n = 4059)
brCols <- rCols[br5]

######
# We are now ready to make a plot.
# Try out the following code
plot(pricepsqft ~ bsqft, data = BerkArea,
     main = "Housing prices in the Berkeley Area",
     xlab = "Size of house (square ft)",
     ylab = "Price per square foot",
     col = brCols, pch = 19, cex = 0.5)
legend(legend = 1:5, fill = rCols, "topright")

# (2 pts.)
### What interesting features do you see that you didn't know before making this plot? 

# The plot shows a downfall. And it suggests an interesting point that in Berkeley as 
# the 'Size of house' increasing the 'Price per square foot' is decreasing which is not
# not a normal case.


# (2 pts.)
# Replicate the boxplots presented in class, with the boxplots sorted by median housing price (slide 45 of the lecture notes)

bymedian <- with(BerkArea, reorder(BerkArea$city, -BerkArea$price, median))
boxplot(BerkArea$price ~ droplevels(bymedian), data = BerkArea,
        xlab = "Cities", ylab = "Price of Houses",
        main = "Berkeley Area Housing Prices", varwidth = TRUE,
        col = "lightgray")

