wisc_bc_df <- read.csv("C://Users//Yshah//Downloads//Rutgers Sem 2//MVA//wisc_bc_data.csv")
head(wisc_bc_df)
#Renmaing the dataset
cancer<-wisc_bc_df
library("ggplot2")
library("corrplot")
library("reshape")

#number of obsevations and data type
str(cancer)

#gives you the summary of the dataset
summary(cancer)

#Gives you frequency table
diagnosis.table <- table(cancer$diagnosis)
diagnosis.table
 

#Bar plot
ggplot(data=cancer, aes(x=diagnosis)) + geom_bar(stat = "count") 

#Pie chart represented in frequency 
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
#Plot histograms of "mean" variables group by diagnosis
data_mean <- cancer[ ,c("diagnosis", "radius_mean", "texture_mean","perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean", "symmetry_mean" )]

#Plot histograms 
ggplot(data = melt(data_mean, id.var = "diagnosis"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales ='free_x')

#Scatter plot of two varaible (concavity again radius)
a <-cancer[,c('concavity_worst','radius_worst')]
plot(x = cancer$concavity_worst,y = cancer$radius_worst,
     xlab = "concavity_worst",
     ylab = "radius_worst",
     main = "Concavity_worst vs radius_worst",
     pch=15,col=c("blue","yellow"))
rug(cancer$concavity_worst, side = 1)

rug(cancer$radius_worst, side = 2)


#Corelation Matrix of columns
corMatMy <- cor(cancer[,3:32])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)

#Scatterplot Matrix
pairs(~radius_mean+perimeter_mean+area_mean+compactness_mean+concavity_mean,data = cancer,main = "Scatterplot Matrix",col=c("blue","green","yellow","red"))


