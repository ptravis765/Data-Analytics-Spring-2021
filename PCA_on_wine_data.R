# There are 13 variables in the dataset such as Alcohol, Malic Acid, Ash, Alkalinity of Ash, Magnesium,...
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
# Header row is not avialable in the data, therefore, we need to add the variable names
head(wine_data)

# The first variable, which is the cultivar that is used to identify the Cv1, Cv2 and Cv3 
# Cv1 represent the cultivar1, Cv2 represent the cultivar2 and Cv3 represent the cultivar3,
nrow(wine_data) # there are 178 rows

# Adding the variable names
colnames(wine_data) <- c("Cvs", "Alcohol","Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data) # Now you can see the header names.


# Using the Heatmap() function, we can check the correlations,
# In the heatmap(), the "Dark Colors" represent the "Correlated"
# In the heatmap(), the "Light Colors" represent the "Not Correlated"
help("heatmap") # Read the heatmap() function Documentation in RStudio.
# Now we will use the heatmap() function to show the correlation among variables.
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 

# Our goal is to identify the 3 variables based on the chemical data on the wine dataset.
# In order to make it easy identify the 3 cultivars, we will declare 3 classes that represents 
# each cultivar (Cv1,Cv2,Cv3) by using the factor() function in R 
# Read the documentation in Rstudio for the factor() function
help(factor)

# declaring the cultivar_classes using the factor() function each cultivar Cv1,Cv2 and Cv3.
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes

# Now we will use PCA
# The default built in function in R for PCA is prcomp() function
# Read the documentation of prcomp() function in Rstudio
help(prcomp)


# We will normalize the wine data to a common scale using scale() function so that the PCA process will not 
# overweight variables that happen to have the larger values.
# Read the documentation of scale() function in RStudio.
help(scale)
# We will not normalize the Cvs variable (first colume) so we exclude the Cvs column with with -1 
wine_data_PCA <- prcomp(scale(wine_data[,-1]))

# We can use the summary() function on wine_data_PCA to see the cumulative proportion that each 
# principal component (PC) contributes,
summary(wine_data_PCA)

# We can see that PC1 gives the 36.2% cumulative contribution, which tells us that PC1 represents 36.2% variance of the data.


# Chapter4_Line_Graphs_R_Graphics
install.packages("gcookbook")
library(gcookbook)
library(ggplot2)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD
BOD1 <- BOD # make a copy of the dataset
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()
ggplot(BOD, aes(x=Time, y= demand)) +geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) +geom_line() + expand_limits(y=0)
# Adding points to a line graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
library(gcookbook) # For the data set
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
# same with log-y axis
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + scale_y_log10()  

