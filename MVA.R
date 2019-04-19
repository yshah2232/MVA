#ggplot2 is used to plot the bar plot
#install.packages("ggplot2")
library("ggplot2")
#corrplot is used to plot the correlation matrix
#install.packages("corrplot")
library("corrplot")
#It is used to reshape a one-dimensional array into a two-dimensional array with one column and multiple arrays.
#install.packages("reshape")
library("reshape")

#Reading the dataset
wisc_bc_df <- read.csv("C://Users//Yshah//Downloads//Rutgers Sem 2//MVA//wisc_bc_data.csv")
head(wisc_bc_df)
cancer<-wisc_bc_df

#Displaying the dataset using head function
head(cancer)

#Displays structure of the dataset
str(cancer)

#Displays the names of the columns
names(cancer)

#Displays the summary of the dataset
summary(cancer)

#Remove the first column
bc_data <- cancer[,-c(0:1)]
#Remove the last column
bc_data <- bc_data[,-32]
#Tidy the data
bc_data$diagnosis <- as.factor(bc_data$diagnosis)

head(bc_data)

#check for missing variables
sapply(bc_data, function(x) sum(is.na(x)))
summary(bc_data)
#To display the frequency table
diagnosis.table <- table(cancer$diagnosis)
#Displays the table
#This shows how many patients are benign and malignant 
diagnosis.table
#Generate barplot
ggplot(data=cancer, aes(x=diagnosis)) + geom_bar(stat = "count") 
#Generate Pie chart represented in frequency 
diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
colors <- terrain.colors(2)
pie(diagnosis.prop.table,
    labels=pielabels,  
    clockwise=TRUE,
    col=colors,
    border="gainsboro",
    radius=0.8,
    cex=0.8, 
    main="frequency of cancer diagnosis")
legend(1, .4, legend=diagnosis.prop.df[,1], cex = 0.7, fill = colors)

#To Plot histograms of "mean" variables group by diagnosis
data_mean <- cancer[ ,c("diagnosis", "radius_mean", "texture_mean","perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean", "symmetry_mean" )]
#Plot histograms 
ggplot(data = melt(data_mean, id.var = "diagnosis"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales ='free_x')

#Generate a Scatter plot of two varaible ie. concavity against radius
data <- cancer[,c('concavity_worst','radius_worst')]
plot(x = cancer$concavity_worst,y = cancer$radius_worst,
     xlab = "concavity_worst",
     ylab = "radius_worst",
     main = "Concavity_worst vs radius_worst", 
     pch=15,
     col = c("red","blue")
     )
rug(cancer$concavity_worst, side = 1)
rug(cancer$radius_worst, side = 2)
#Generate Corelation Matrix of columns
corMatMy <- cor(cancer[,3:32])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)
#Generate Scatterplot Matrix
pairs(~radius_mean+perimeter_mean+area_mean+compactness_mean+concavity_mean,data = cancer,main = "Scatterplot Matrix",col=c("red","blue","green","yellow"))

#Multivariate analysis
#t-tEST
with(data=cancer,t.test(radius_mean[diagnosis=="B"],radius_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(texture_mean[diagnosis=="B"],texture_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(perimeter_mean[diagnosis=="B"],perimeter_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(area_mean[diagnosis=="B"],area_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(smoothness_mean[diagnosis=="B"],smoothness_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(compactness_mean[diagnosis=="B"],compactness_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(concavity_mean[diagnosis=="B"],concavity_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(points_mean[diagnosis=="B"],points_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(symmetry_mean[diagnosis=="B"],symmetry_mean[diagnosis=="M"],var.equal=TRUE))
with(data=cancer,t.test(dimension_mean[diagnosis=="B"],dimension_mean[diagnosis=="M"],var.equal=TRUE))

#Hotelling's T2 test
#install.packages("Hotelling")
library(Hotelling)
t2testcan <- hotelling.test(radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + points_mean + symmetry_mean + dimension_mean ~ diagnosis, data=cancer)
# Output of the function hotelling.test is given
cat("T2 statistic =",t2testcan$stat[[1]],"\n")
print(t2testcan)
#  T2 statistic is located in the first element of the list "stat"
#View(t2testcan)
#View(cancer)


#Levene's tests based on absolute differences around means using t-tests. Standarizing the data set with scale()
matstand <- scale(cancer[,3:10])
head(matstand)
matben <- matstand[cancer$diagnosis =="B",]
head(matben)
matmalign <- matstand[cancer$diagnosis == "M",]
vecmedianben <- apply(matben, 2, median)
# in the above 2 represents column. Hence, we are asking for column median
vecmedianben

vecmedianmalign <- apply(matmalign, 2, median)
matabsdevben <- abs(matben - matrix(rep(vecmedianben,nrow(matben)),nrow=nrow(matben), byrow=TRUE))

matabsdevmalign <- abs(matmalign - matrix(rep(vecmedianmalign,nrow(matmalign)),nrow=nrow(matmalign), byrow=TRUE))

head(matabsdevmalign)

matabsdev.all <- rbind(matabsdevben,matabsdevmalign)
matabsdev.all <- data.frame(cancer$diagnosis, matabsdev.all)

t.test(matabsdev.all$radius_mean[cancer$diagnosis == "B"],matabsdev.all$radius_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$texture_mean[cancer$diagnosis == "B"],matabsdev.all$texture_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$perimeter_mean[cancer$diagnosis == "B"],matabsdev.all$perimeter_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$area_mean[cancer$diagnosis == "B"],matabsdev.all$area_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$smoothness_mean[cancer$diagnosis == "B"],matabsdev.all$smoothness_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$compactness_mean[cancer$diagnosis == "B"],matabsdev.all$compactness_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$concavity_mean[cancer$diagnosis == "B"],matabsdev.all$concavity_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)
t.test(matabsdev.all$points_mean[cancer$diagnosis == "B"],matabsdev.all$points_mean[cancer$diagnosis == "M"], alternative="less",var.equal = TRUE)


head(matstand)
matstand.all <- data.frame(cancer$diagnosis, matstand)
head(matstand.all)
colnames(matstand.all) <- colnames(cancer[2:10])
t2testcan <- hotelling.test(radius_mean + texture_mean + perimeter_mean + area_mean + smoothness_mean + compactness_mean + concavity_mean + points_mean + symmetry_mean + dimension_mean ~ diagnosis, data=cancer)
cat("T2 statistic =",t2testcan$stat[[1]],"\n")
print(t2testcan)

# In the above we standardized using scale function
head(matabsdev.all)

#install.packages("car")
library(car)
#leveneTest() produces a two-sided test
# Leverne test is used to verify Homoscedasticity. It tests if the variance of two samples are # #equal. Levene's test is an inferential statistic used to assess the equality of variances for a #variable calculated for two or more groups.[1] Some common statistical procedures assume that #variances of the populations from which different samples are drawn are equal. Levene's test #assesses this assumption.
leveneTest(radius_mean ~ diagnosis, data=cancer)
leveneTest(texture_mean ~ diagnosis, data=cancer)
leveneTest(perimeter_mean ~ diagnosis, data=cancer)
leveneTest(area_mean ~ diagnosis, data=cancer)
leveneTest(smoothness_mean ~ diagnosis, data=cancer)
leveneTest(compactness_mean~ diagnosis, data=cancer)
leveneTest(concavity_mean~ diagnosis, data=cancer)
leveneTest(points_mean ~ diagnosis, data=cancer)
leveneTest(symmetry_mean ~ diagnosis, data=cancer)
leveneTest(dimension_mean ~ diagnosis, data=cancer)

#PCA
dim(cancer)
attach(cancer)
head(cancer)
#Get the Correlations between the measurements
cor(cancer[-2])
c <- (cor(cancer[-2]))
plot(c)
# Using prcomp to compute the principal components (eigenvalues and eigenvectors). With scale=TRUE, variable means are set to zero, and variances set to one
cancer_pca <- prcomp(cancer[,-2],scale=TRUE)
cancer_pca
plot(cancer_pca)
summary(cancer_pca)
#View(cancer_pca)
head(cancer_pca$x)
# sample scores stored in cancer_pca$x
# singular values (square roots of eigenvalues) stored in cancer_pca$sdev
# loadings (eigenvectors) are stored in cancer_pca$rotation
# variable means stored in cancer_pca$center
# variable standard deviations stored in sparrows_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2
(eigen_cancer <- cancer_pca$sdev^2) ## brackets for print
names(eigen_cancer) <- paste("PC",1:31,sep="")
eigen_cancer
sumlambdas <- sum(eigen_cancer)
sumlambdas
propvar <- eigen_cancer/sumlambdas
propvar
summary(eigen_cancer)
summary(cancer_pca)
cumvar_cancer <- cumsum(propvar)
cumvar_cancer
matlambdas <- rbind(eigen_cancer,propvar,cumvar_cancer)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(cancer_pca)
cancer_pca$rotation
print(cancer_pca)
# Sample scores stored in cancer_pca$x
head(cancer_pca$x)
# Identifying the scores by their diagnosis
diag_pca <- cbind(data.frame(diagnosis),cancer_pca$x)
head(diag_pca)
# Means of scores for all the PC's classified by diagnosis status
tabmeansPC <- aggregate(diag_pca[,2:31],by=list(diagnosis=cancer$diagnosis),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$diagnosis)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans
# Standard deviations of scores for all the PC's classified by diagnosis status
tabsdsPC <- aggregate(diag_pca[,2:31],by=list(cancer$diagnosis),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds
t.test(PC1~cancer$diagnosis,data=diag_pca)
t.test(PC2~cancer$diagnosis,data=diag_pca)
t.test(PC3~cancer$diagnosis,data=diag_pca)
t.test(PC4~cancer$diagnosis,data=diag_pca)
t.test(PC5~cancer$diagnosis,data=diag_pca)
t.test(PC6~cancer$diagnosis,data=diag_pca)
t.test(PC7~cancer$diagnosis,data=diag_pca)
t.test(PC8~cancer$diagnosis,data=diag_pca)
t.test(PC9~cancer$diagnosis,data=diag_pca)
t.test(PC10~cancer$diagnosis,data=diag_pca)
t.test(PC11~cancer$diagnosis,data=diag_pca)
t.test(PC12~cancer$diagnosis,data=diag_pca)
t.test(PC13~cancer$diagnosis,data=diag_pca)
t.test(PC14~cancer$diagnosis,data=diag_pca)
t.test(PC15~cancer$diagnosis,data=diag_pca)
t.test(PC16~cancer$diagnosis,data=diag_pca)
t.test(PC17~cancer$diagnosis,data=diag_pca)
t.test(PC18~cancer$diagnosis,data=diag_pca)
t.test(PC19~cancer$diagnosis,data=diag_pca)
t.test(PC20~cancer$diagnosis,data=diag_pca)
t.test(PC21~cancer$diagnosis,data=diag_pca)
t.test(PC22~cancer$diagnosis,data=diag_pca)
t.test(PC23~cancer$diagnosis,data=diag_pca)
t.test(PC24~cancer$diagnosis,data=diag_pca)
t.test(PC25~cancer$diagnosis,data=diag_pca)
t.test(PC26~cancer$diagnosis,data=diag_pca)
t.test(PC27~cancer$diagnosis,data=diag_pca)
t.test(PC28~cancer$diagnosis,data=diag_pca)
t.test(PC29~cancer$diagnosis,data=diag_pca)
t.test(PC30~cancer$diagnosis,data=diag_pca)
t.test(PC31~cancer$diagnosis,data=diag_pca)

# F ratio tests
var.test(PC1~cancer$diagnosis,data=diag_pca)
var.test(PC2~cancer$diagnosis,data=diag_pca)
var.test(PC3~cancer$diagnosis,data=diag_pca)
var.test(PC4~cancer$diagnosis,data=diag_pca)
var.test(PC5~cancer$diagnosis,data=diag_pca)
var.test(PC6~cancer$diagnosis,data=diag_pca)
var.test(PC7~cancer$diagnosis,data=diag_pca)
var.test(PC8~cancer$diagnosis,data=diag_pca)
var.test(PC9~cancer$diagnosis,data=diag_pca)
var.test(PC10~cancer$diagnosis,data=diag_pca)
var.test(PC11~cancer$diagnosis,data=diag_pca)
var.test(PC12~cancer$diagnosis,data=diag_pca)
var.test(PC13~cancer$diagnosis,data=diag_pca)
var.test(PC14~cancer$diagnosis,data=diag_pca)
var.test(PC15~cancer$diagnosis,data=diag_pca)
var.test(PC16~cancer$diagnosis,data=diag_pca)
var.test(PC17~cancer$diagnosis,data=diag_pca)
var.test(PC18~cancer$diagnosis,data=diag_pca)
var.test(PC19~cancer$diagnosis,data=diag_pca)
var.test(PC20~cancer$diagnosis,data=diag_pca)
var.test(PC21~cancer$diagnosis,data=diag_pca)
var.test(PC22~cancer$diagnosis,data=diag_pca)
var.test(PC23~cancer$diagnosis,data=diag_pca)
var.test(PC24~cancer$diagnosis,data=diag_pca)
var.test(PC25~cancer$diagnosis,data=diag_pca)
var.test(PC26~cancer$diagnosis,data=diag_pca)
var.test(PC27~cancer$diagnosis,data=diag_pca)
var.test(PC28~cancer$diagnosis,data=diag_pca)
var.test(PC29~cancer$diagnosis,data=diag_pca)
var.test(PC30~cancer$diagnosis,data=diag_pca)
var.test(PC31~cancer$diagnosis,data=diag_pca)
# Levene's tests (one-sided)
library(car)
(LTPC1 <- leveneTest(PC1~cancer$diagnosis,data=diag_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~cancer$diagnosis,data=diag_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~cancer$diagnosis,data=diag_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)
(LTPC4 <- leveneTest(PC4~cancer$diagnosis,data=diag_pca))
(p_PC4_1sided <- LTPC4[[3]][1]/2)
(LTPC5 <- leveneTest(PC5~cancer$diagnosis,data=diag_pca))
(p_PC5_1sided <- LTPC5[[3]][1]/2)
# Plotting the scores for the first and second components
plot(diag_pca$PC1, diag_pca$PC2,pch=ifelse(diag_pca$diagnosis == "S",1,16),xlab="PC1", ylab="PC2", main="569 entries against values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Benign","Malignant"), pch=c(1,16))
plot(eigen_cancer, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_cancer), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(cancer_pca))
#View(cancer_pca)
diag(cov(cancer_pca$x))
xlim <- range(cancer_pca$x[,1])
head(cancer_pca$x[,1])
head(cancer_pca$x)
plot(cancer_pca$x,xlim=xlim,ylim=xlim)

#Factor Analysis
#install.packages("psych", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(psych)
fit.pc <- principal(cancer[-2], nfactors=4, rotate="varimax")
fit.pc
round(fit.pc$values, 3)
fit.pc$loadings
# Loadings with more digits
for (i in c(1,3,2,4)) { print(fit.pc$loadings[[1,i]])}
# Communalities
fit.pc$communality
# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
head(fit.pc$scores)
# Play with FA utilities

fa.parallel(cancer[-2]) # See factor recommendation
fa.plot(fit.pc) # See Correlations within Factors
fa.diagram(fit.pc) # Visualize the relationship


#Multiple Regression
# Performing multiple regression on mtcars dataset
fit <- lm(radius_mean~texture_mean+perimeter_mean+area_mean+smoothness_mean+compactness_mean+concavity_mean+points_mean+symmetry_mean+dimension_mean, data=cancer)
#show the results
summary(fit)
#Summary has three sections. Section1: How well does the model fit the data (before Coefficients). Section2: Is the hypothesis supported? (until sifnif codes). Section3: How well does data fit the model (again).
# Useful Helper Functions
coefficients(fit)
library(GGally)
#install.packages("GGally", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(GGally)
confint(fit,level=0.95)
# Predicted Values
fitted(fit)
residuals(fit)
#Anova Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))
temp <- influence.measures(fit)
temp
#View(temp)
#diagnostic plots
# Assessing Outliers
outlierTest(fit)
qqPlot(fit, main="QQ Plot")
leveragePlots(fit) # leverage plots
# Influential Observations
# added variable plots
avPlots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
#influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
#Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)
#Multi-collinearity
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?
#Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
ceresPlots(fit)
#Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)
# Global test of model assumptions
library(gvlma)
#install.packages("gvlma", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1 <- fit
fit2 <- lm(mpg ~ disp + hp + wt, data = mtcars)
# compare models
anova(fit1, fit2)
step <- stepAIC(fit, direction="both")
step$anova # display results
summary(fit)
predict.lm(fit, data.frame(wt =3.2 ,drat=3.9,hp=130,disp=150) )

#LDA
library(MASS)
features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")
names(cancer) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))
head(cancer)
dim(cancer)
#The mean, standard error, and "worst" or largest (mean of the three largest values) of these features were computed for each image, resulting in 30 features. For instance, field 3 is Mean Radius, field 13 is Radius SE, field 23 is Worst Radius.
#we need to convert to matrix to facilitate distance measurement
cancer.data <- as.matrix(cancer[,c(3:32)])
row.names(cancer.data) <- cancer$id
dim(cancer.data)
dim(cancer)
cancer_raw <- cbind(cancer.data, as.numeric(cancer$diagnosis)-1)
dim(cancer_raw)
colnames(cancer_raw)[31] <- "diagnosis"
#What this does is it simply removes ID as a variable and defines our data as a matrix instead of a dataframe while still retaining the ID but in the column-names instead.
# Lets cut the data into two parts
smp_size_raw <- floor(0.75 * nrow(cancer_raw))
train_ind_raw <- sample(nrow(cancer_raw), size = smp_size_raw)
train_raw.df <- as.data.frame(cancer_raw[train_ind_raw, ])
test_raw.df <- as.data.frame(cancer_raw[-train_ind_raw, ])
# We now have a training and a test set. Training is 75% and test is 25%
cancer_raw.lda <- lda(formula = train_raw.df$diagnosis ~ ., data = train_raw.df)
cancer_raw.lda
summary(cancer_raw.lda)
print(cancer_raw.lda)
plot(cancer_raw.lda)
cancer_raw.lda.predict <- predict(cancer_raw.lda, newdata = test_raw.df)
cancer_raw.lda.predict$class
#View(cancer_raw.lda.predict)
#cancer_raw.lda.predict$x
# Get the posteriors as a dataframe.
cancer_raw.lda.predict.posteriors <- as.data.frame(cancer_raw.lda.predict$posterior)
#create ROC/AUC curve
#install.packages("ROCR")
library(ROCR)
pred <- prediction(cancer_raw.lda.predict.posteriors[,2], test_raw.df$diagnosis)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#kNN
library(class)
normalize <- function(x) {
  y <- (x - min(x))/(max(x) - min(x))
  y
}

wbcd_n_L <- lapply(cancer[, 3:32], normalize)
wbcd_n <- data.frame(wbcd_n_L)
wbcd_n[1:3, 1:4]

rownames(wbcd_n) <- cancer$id
BM_class <- cancer[, 2]
names(BM_class) <- wisc_bc_df$id
BM_class[1:3]

nrow(cancer)
rand_permute <- sample(x = 1:569, size = 569)
rand_permute[1:5]
# save(rand_permute, file='rand_permute.RData')

all_id_random <- cancer[rand_permute, "id"]
569/3

validate_id <- as.character(all_id_random[1:189])
training_id <- as.character(all_id_random[190:569])

wbcd_train <- wbcd_n[training_id, ]
wbcd_val <- wbcd_n[validate_id, ]
BM_class_train <- BM_class[training_id]
BM_class_val <- BM_class[validate_id]
table(BM_class_train)

table(BM_class_val)

sqrt(nrow(wbcd_train))

knn_predict <- knn(wbcd_train, wbcd_val, BM_class_train, k = 19)
knn_predict[1:3]

table(knn_predict, BM_class_val)

prop.table(table(knn_predict, BM_class_val))

knn_predict_3 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 3)
knn_predict_7 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 7)
knn_predict_11 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 11)
knn_predict_31 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 31)

table(knn_predict_3, BM_class_val)

table(knn_predict_7, BM_class_val)

table(knn_predict_11, BM_class_val)

table(knn_predict_31, BM_class_val)
