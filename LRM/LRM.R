#******************************************************************************************************
# Applied generalized linear model - FS 
# Viviana Amati 
# Social Network Labs
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 25 February 2020

# This script provides an introduction to R and a review of basic concepts of linear regression model
# For a detailed introduction to R please take a look at the files RIntro.pdf and RIntro.R
# For the material on linear regression models please refer to the lecture notes.
#******************************************************************************************************


# Introduction to R
# R is a language and environment for statistical computing and graphics. 
# It is a GNU project which is similar to the S language and environment.
# R provides a wide variety of statistical (linear and nonlinear modelling, 
# classical statistical tests, time-series analysis, classification, 
# clustering, ...) and graphical techniques, and it is highly extensible. 
# (source http://www.r-project.org/)

# RStudio is a convenient interface consisting of four windows:
# - editor window (upper left): collections and commands are edited and saved
# - console (lower left): where R computes and execute the commands
# - environment (upper right): shows which data and values R has in its memory
# - files/plots/packages/help window(lower right): you can open files, view plots  
#                                                  (also previous plots), install 
#                                                  and load packages or use the 
#                                                  help function.

#-----------------------------------------------------------------------------------------------------
# R as a calculator
#-----------------------------------------------------------------------------------------------------
# The standard arithmetic operators are +, -, *, and / for add, subtract, multiply and divide, 
# and ^ for exponentiation. Mathematical functions, such as sqrt, exp, and log are also defined  

5+8
15^2
exp(3)
log(1)
sqrt(4)

# The relational operators <=, <, ==, >, >= and != for less than or equal, less than, equal, greater than, 
# greater than or equal, and not equal can be used to create logical expressions that take 
# values TRUE and FALSE

5>3
a <- 4
b <- 6
a==b


#-----------------------------------------------------------------------------------------------------
# DATA STRUCTURES
#-----------------------------------------------------------------------------------------------------
# Everything in R is an object.  
# An object is a data structure having some characteristics (attributes)
# and methods which act on its attributes. 
# The most frequently used objects are vectors, matrices, array, data frame and lists
# To create an object we use 
# name <- value
# name: a valid name consists of letters, numbers, and the dot or undescore characters
#       R is case sensitive!!! 
# <- : assignment operator, read as "gets" R now accepts the equal sign as well

#---------------------------------------------------------
# VECTOR
# An ordered collection of numbers or characters
#---------------------------------------------------------
# The function c, short for concatenate is used to create 
#vectors from scalars or other vectors
vec0 <- c(2,3,4) 
vec0

# WARNING:
# c is the function that concatenates values. Do not use c
# as a name of an R object!!!

Vec0
# WARNING:
# R is case sensitive!!! Therefore, Vec0 is different from vec0

# Elements are addressed using subscripts in square brackets.
vec0[1] 
vec0[c(1,3)]


#---------------------------------------------------------
# MATRIX
# A generalization of a vector defined as
# an ordered collection of numbers
#---------------------------------------------------------
# The function matrix 
mat1 <- matrix(1:10, nrow=2, ncol=5, byrow=FALSE)
mat1

# Number of rows and columns 
dim(mat1) 

# The elements of a matrix may be addressed using the row and column subscripts 
# in square brackets, separated by a comma. 
mat1
mat1[1,2]     # single cell
mat1[1,]      # first raw
mat1[,1]      # first column


#---------------------------------------------------------
# DATA FRAME  
# It can have columns of different data type and can 
# be thought of the usual case-by-variable data matrix
#---------------------------------------------------------
# Creating data frames
peopleid <- 1:10
gender <- c(rep("Male",4),rep("Female",5),NA)
# data framecan also handle missing values that are usually 
# coded as NA which means not available
age <- sample(20:30,10)
dat1 <- data.frame(id = peopleid, gender = gender, age=age)

# Inspecting a data frame
head(dat1)     # shows the first six lines
dim(dat1)      # returns the number of rows and columns
names(dat1)    # shows the names of the columns
str(dat1)      # shows the structure

# The elements of a data.frame can be addressed as in matrices
# by using square brackets
dat1[1,2]      # single cell

# OBSERVATION: name of the columns can be used to address columns
dat1$peopleid
dat1[,"id"]
# How could you address age?

#---------------------------------------------------------
# LIST
# An object consisting of an ordered collection of objects
# Output of a model are usually saved in a list
#---------------------------------------------------------
list0 <- list(vec0,mat1,dat1)
str(list0)
list0[[1]]
list0[[3]][1:3,]


#-----------------------------------------------------------------------------------------------------
# HELP
#-----------------------------------------------------------------------------------------------------
# 1.  Internet: 
# when you do not know if there is already 
# a function doing what you would like to do

# 2. when you know the name of the function and you want to know more
# ? (or ??) name of the function
# to get a description of the function, its arguments and examples
?matrix


#-----------------------------------------------------------------------------------------------------
# WORKING DIRECTORY
#-----------------------------------------------------------------------------------------------------
# It is always convenient to set the working directory, i.e. the folder in which data are and 
# results will be saved. This is done using the function setwd and the path of the folder.
getwd()
setwd("C:\...")

# WARNING: You cannot use the backslash to specify the path. 
#          You must use the slash or double backslash!!!
# setwd("C:\\...")

# To see the name of the file in the directory
list.files() 


#-----------------------------------------------------------------------------------------------------
# R PACKAGES
#-----------------------------------------------------------------------------------------------------
# R Packages are collections of R functions, data, and compiled code in a 
# well-defined format. Packages (e.g. base) for the most standard statistical 
# analysis are automatically installed when R is installed. 

# To install a package we use the command install.packages("name").
# To use a package we need to load it using the command library(name)

# Packages that are useful when we write report or assignment solution

# Packages for exporting the results into latex code
# Useful to export many objects
install.packages("xtable")
library(xtable)

# Useful to export the results from several estimated models
install.packages("texreg")
library(texreg)

# Useful to make nice plots
install.packages("ggplot2")
library(ggplot2)

#-----------------------------------------------------------------------------------------------------
# READING DATA
#-----------------------------------------------------------------------------------------------------
# There are several functions that can be used to read files with different extension
# The most general function is read.table

? read.table
mental <- read.table("mental.csv", sep =",",header=TRUE)
# For tab spacing you should use  sep="\t"

# For csv files the two specific functions read.csv and read.csv2 can be used (what is the difference?)
mental <- read.csv("mental.csv")
head(mental)


#-----------------------------------------------------------------------------------------------------
# LINEAR REGRESSION MODEL
#-----------------------------------------------------------------------------------------------------
# The data set mental.csv is an excerpt from a study on mental health in Alachua County, Florida.
# Data are from Agresti and Finlay (2009). Statistical methods for the social sciences and have been
# slightly modified.

# The data set includes information on 42 individuals and the following variables: 
# - id: identifier
# - impair: mental impairment. The index of mental impairment incorporates various dimensions of 
#           psychiatric symptoms, including aspects of anxiety and depression. 
#           Higher scores indicate higher psychiatric impairment.
# - life: life events score. The life events score is a composite measure accounting for both the 
#         number and the severity of of major life events (e.g., death in the family, jail sentence, 
#         birth of a child, and moving within the same city) experienced by an individual within 
#         the past three years. Measured on a standard scale, it ranged from 0 to 100. 
#         The higher the score, the higher the number and/or greater severity of the life events.  
# - ses: social economic status. The socioeconomic status score is a composite index based on occupation, 
#        education and income. Measured on a standard scale, it ranged from 0 to 100. 
#        The higher the score, the higher the status. 

# Setting the directory and reading the file
setwd("...")
mental <- read.csv("mental.csv",header=T)

#---------------------------------
# EDA: exploratory data analysis
#---------------------------------
# Check the data were correctly imported, there are data-entry errors or missing values
# Get basic information on the variables (e.g., range, means, frequency distributions, etc.)
head(mental)
str(mental)
summary(mental)

# We notice that we have a missing value and a negative value for the life events score. 
# We discard the two cases from the analysis 
#??? The two cases were added to the original data set to simply illustrate the importance of EDA
mental[is.na(mental$impair),]
mental[mental$life < 0,]
mental <- mental[-c(41,42),]

# id are set as labels of the rows
rownames(mental) <- mental$id
mental <- mental[,-1]
head(mental)

# Scatterplot matrix for bivariate relationships
# panel.cor is a function to build the scatterplot matrix and report the Pearson's correlation 
# coefficient to investigate the linear relationship between pairs of variables but id
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt)
}
pairs(mental, lower.panel = panel.cor, pch = 18)

# Distribution of impair
hist(mental$impair, main="Histogram of impair")
plot(density(mental$impair), main="Density of impair")
mglu <- mean(mental$impair)
sdglu <- sd(mental$impair)
curve(dnorm(x, mean=mglu, sd=sdglu),col="red", lwd=2, add=TRUE,
      yaxt="n",main="Density of impair")

#---------------------------------
# Model estimation
#---------------------------------
mod0 <- lm(impair ~ life + ses, data = mental)
mod0
summary(mod0)

#---------------------------------
# Model diagnostics
#---------------------------------
par(mfrow=c(2,2))
plot(mod0)

#---------------------------------
# Model inference
#---------------------------------
summary(mod0)
anova(mod0)

# Could you compute the F-test and the R^2?

#---------------------------------
# Standardized coefficients
#---------------------------------
mod1 <- lm(scale(impair) ~ scale(life) + scale(ses), data = mental)
summary(mod1)
