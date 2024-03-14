# LECTURE 1: Introduction to R
# Monday, January 10, 2022

# ALEX LYFORD: 678-438-4375


# ----------------------------------- NOTES -----------------------------------
#  - Can make keyboard shortcuts under tools (command + enter is run)
#  - Can run something by just highlighting it, also runs where cursor is
#  - R works by calling "functions" - call function by name, give it an input,
#    then it gives us a desired output, call it by the name and parenthesis
#  - R does care about white space
#  - Stores variables by name = value
#  - R studio creates a list of values you stored (top right)
#  - In R, = has a different mathematical meaning, so to avoid confusion,
#    we are going to use the storing arrow name <- value to store 
#  - Type "? name_function" to get help from R studio directly 
#  - While naming, the dot doesn't do anything just part of the name
#  - NA is a missing value 
#  - R doesn't remove NA values by default so people know if there are NA in 
#    the data set 
#  - Remember to label in order or it will give an error!
#  - Only have to install packages once on your compiuter, but we must 
#    run the library each time we use it 



# --------------------------------- FUNCTIONS ---------------------------------

# THE "c" FUNCTION 
# This function concatenates things

c(1,4,8)                # This will make a vector of 1 4 8 together 

vector1 = c(1,4,8)      # Stores value in vector1

vector1                 # Prints the vector that we stored in vector1 

vector1 <- c(1,4,8)     # in R, we want to use <- to store to avoid confusion


# THE "mean" FUNCTION
# This function takes the average of a vector

mean(x = vector1)       # Argument x is equal to vector 1
mean(vector1)           # Don't need to label arguments as long as they are in order
mean1 <- mean(vector1)  # Can store value in same way above


# OPTIONAL ARGUMENTS IN FUNCTIONS 

student.sleep <- c(1, 4, 8, NA)

mean(x = student.sleep)   # this outputs NA! Can't add NA

mean(x = student.sleep, na.rm = TRUE)  # Optional argument to avoid NA numbers 



# ------------------------------ DATA + GRAPHS!  ------------------------------


# INSTALLING A PACKAGE

# install.packages("ggplot2")  --> comment it out so it doesn't run again


# LOAD A PACKAGE LIBRARY - must do for files that need a package

library("ggplot2")     # makes pretty graphs rather than base R which is ugly


# MAKING PLOTS IN ggplot2

diamonds               # pre-loaded data-set in ggplot2 

View(diamonds)         # view data set in spreadsheet like fashion
                       # you can also do this by command + click on data set

? diamonds             # teaches about diamonds data set 


# VISUALIZING DATA IN DIAMONDS - weight (carat) and price

? ggplot()             # learn about ggplot, technically doesn't need any arguments
                       # This is useful because we will be making graphs by
                       # "painting a canvas - keep adding layers to it

ggplot(data = diamonds)


# Two types of variables: quantitative/continuous and discrete/categorical 
#     - Quantitative: values are numbers, and the numbers have math significance
#     - Categorical:  limited number of possible values, typically groups/nominal



# STEPS FOR A ggplot - GENERAL 

ggplot(data = XXX) +                        # 1. Start with Canvas, then +
  geom_XXX(mapping = aes(x = xx, y = yy))  # 2. Add graph type (XXX is the type)
                                            # Must bind variables into vector using aes 



# STEPS FOR A ggplot - DIAMOND

ggplot(data = diamonds) +                         # Make canvas
  geom_point(mapping = aes(x = carat, y = price), # Make plot graph with x and y
             color = "blue",                      # Can color points
             size = 4) +                          # Can change size of points
  theme_dark()                                    # Spice it up a bit

# Can look at help documentation ?geom_point to see all the things you can change


