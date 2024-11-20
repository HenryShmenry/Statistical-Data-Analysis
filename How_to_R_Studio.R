setwd("C:/Users/henry/OneDrive/Documents/RStudio/H2R") # This sets the working directory
# The working directory is where R looks for any files you wish to input or read from.

############### LECTURE CONTENT ##############

####### Principal Component Analysis #######
prcomp(data, scale = T) # performs PCA on a given data set

# data: numeric data matrix (or data frame)
# scale: logical value T/F

# The Outputs are:
# sdev: standard deviations of the principle components
# center: variable means (before mean centering)
# rotation: the loadings matrix
# x: scores matrix

######## PCA Examples + Extras ######
# Some examples may not make sense as the fattyacids data set is seemingly incomplete.

fattyacids = read.csv("fattyacids.csv", header = T)

mypca = prcomp(fattyacids[,5:14]) # performing pca with selected data ranges

mypca$rotation # shows how much each variable contributes to the principle component

biplot(mypca, cex = 0.7) # creates a biplot

# cex: text / label size

mypca$x # The scores matrix

# If you want a more specific plot using different principal components:

plot(mypca$x[,1], mypca$x[,2], xlab = "PC1", ylab = "PC2", main = "Plot Name", cex = 3)

# xlab: The label on the x-axis
# main: The title of your plot

text(mypca$x[,1], mypca$x[,2]) 
# adds labels atop the graph.

plot(mypca$x[,1], mypca$x[,2], xlab = "PC1", ylab = "PC2", main = "Plot Name", cex = 3, col = as.factor(fattyacids$location))
# This adds colour to your plot based on the location column of the data set.

# Adding pch = 19 into your plot changes the symbol. 

summary(mypca)
# shows the variance accounted for by each PC

plot(mypca)
# graphs the variance accounted for by each PC | "Scree Plot"

# We can add percentage labels to the axes with:
plot(mypca$x[,1], mypca$x[,2], xlab = "PC1 (67.9%)", ylab = "PC2 (28.7%)", col = as.factor(fattyacids$location))

sbdata = fattyacids[19:54,] # only using specific data

dim(sbdata) # shows you the new dimension for the data

newpca = prcomp(sbdata[-35, 5:14]) # re-doing the pca, removing an outlier

plot(newpca$x[,1], newpca$x[,2], xlab = "PC1", ylab = "PC2", col = as.factor(fattyacids$location), pch = 19)
legend("bottomleft", inset = 0.01, legend = unique(sbdata$location), col = unique(as.factor(sbdata$location)), pch = 20)
# This would plot a new pca with a legend in the bottom right corner
# we can change the symbols to represent the fat_type using pch = as.numeric instead of as.factor.
# there are 25 symbols you can use with pch. pch = 11 is the star of david.
# We can add another legend for the extra symbols. 

legend("bottomright", inset = 0.01, legend = unique(sbdata$fat_type), col = "black", pch = unique(as.numeric(sbdata$fat_type)))

# we can make a new variable using the paste() function:
newgroup = as.factor(paste(fattyacids$location, fattyacids$fat_type))

# we can colour based on specific rows: 
points(mypca$x[10:18, 1], mypca$x[10:18,2], pch = 20, col = "red")

# And of course, we can label each observation:
text(mypca$x[,1]+0.5, mypca$x[,2], newergroup, cex = 0.5)

####### Function Summary: PCA ######
?prcomp
?plot
?read.csv
?biplot
?dim
?summary
?legend
?text
?points

######### Cluster Analysis #######

hclust(d, method) # performs hierarchical clustering

# method: agglomeration method used: "single", "complete", "average", "ward", "mcquitty", "median", "centroid".
# d: distance matrix.

dist(x, method) # produces a distance matrix

# method: distance measure: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
# x: data matrix
# If using "minkowski", then the power p also needs to be given as an argument

######## Cluster Analysis: Iris Example ######

d=dist(iris[,1:4], "minkowski", p=3) 
# Produces a distance matrix from the first 4 columns, the 5th column is the species and not numeric.
# Typing d will show the lower diagonal matrix.

# To see the full distance matrix use:
d=dist(iris[,1:4], diag = T, upper = T)

# If the distance measure is not specified, Euclidean will be used.

# Using:
hcl = hclust(d, "average") # performs hierarchical clustering

# we can plot the dendrogram using:
plot(hcl, cex = 0.5)
# cex adjusts the size of the text of each label.

# OR

plot(hcl, labels = iris[,5], cex = 0.5) # this uses the specific names from the 5th column.

rect.hclust(hcl, k) # finds the levels at which there are k clusters and draws a rectangle around them.

# for example, check:
rect.hclust(hcl, 3)

cutree(hcl, k) # assigns one of the k cluster numbers to each object
# for example, check:
cutree(hcl, 3)

c = cophenetic(hcl) # produces the matrix of cophenetic distances
cor(c, d) # gives the correlation of this with the distance matrix d
# this can be used to validate your thoughts for a clustering.

?heatmap # this provides another way to visualise the clustering

heatmap(as.matrix(d)) # for just the distance matrix or,
heatmap(as.matrix(d), Colv = NA, Rowv = NA) # without the clustering
# Levelplot might be a better option to explore. 

# For k-means we use:
kmeans(x, k)
# x: data matrix
# k: number of clusters

# As an example using the iris data:
kcl = kmeans(iris[,-5], 3)

plot(kcl$cluster)

pca = prcomp(iris[,-5])

plot(pca$x[,1], pca$x[,2], xlab = "PC1", ylab = "PC2", pch = as.numeric(iris[,-5]), col = kcl$cluster)

# But how do you decide how many clusters you should use in k-means?!
# There are a couple ways to go about it! 

#Performing k-means:
library("factoextra") # This is used to the upcoming plots:

fviz_nbclust(data, kmeans, method = "silhouette") # Performs the silhouette method

fviz_nbclust(data, kmeans, method = "gap_stat") # Performs the gap statistic method

# Both these should automatically suggest the optimal number of clusters for a given set of data!
# You are able to use other packages to present your data, 
# just be careful to thoroughly look into what your package is doing to your data.

######### Validation #########

# as an example with the iris data set:
test = rbind(iris[1:10,], iris[51:60,], iris[101:110,]) # Our testing data set
train = rbind(iris[11:50,], iris[61:100,], iris[111:150,]) # Our training data set

# LDA in R
library(MASS) # The package used to perform lda

lda() # performs linear discriminant analysis

# The inputs are:
# x: numeric data matrix for training
# grouping: vector of class labels for the training data

# The outputs are:
# prior: The prior probabilities of each group (based on the number of observations in each group)
# means: The group means
# scaling: the coefficients of the linear discriminants.

# Eg:
lda(train[,1:4], train[,5])

# Using the classifier
predict() # predicts the class 

# The arguments:
# model: a model produced by lda()
# data: a numeric data matrix

# The outputs:
# class: the predicted class of each observation
# posterior: the posterior probability for each class
# x: the scores for discriminant functions

pred$class # shows the classes assigned using
pred = predict(model, train[,1:4])

# this can be compared with the real class in a confusion matrix using:
table(predicted = pred$class, real = train[,5])

######### Naive Bayes (NB) #####################################################
install.packages("naivebayes") # Installing the package
library(naivebayes) # Loading the package

######### Comparing Classifiers in R ###########################################
model_nb = naive_bayes(train[,2:31], train[,1])
model_lda = lda(train[,2:31], train[,1])

# The predictions are given by
pred_nb = predict(model_nb, test[,2:31])
pred_lda = predict(model_lda, test[2:31])$class 
# you need the $class due to package variety
# We can get our confusion matrices using:
real = test[,1]
table(true = real, predicted =)
