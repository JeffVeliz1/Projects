# Jeff's code
install.packages("readxl")
install.packages("ggplot2")
install.packages("FactoMineR")
install.packages("factoextra")

library(readxl)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(regclass)

# median earnings~ game----------------------
esport <- read_xlsx("eSports Earnings.xlsx", sheet = "The Table")

esport
# Remove non-numeric characters from total_earnings column
esport$Total..Game. <- as.numeric(gsub("[^0-9.]", "", esport$Total..Game.))

# Subset data by game
games <- unique(esport$Highest.Paying.Game)

# Create empty vectors to store results
median_earnings <- c()

# Loop over games and calculate average and median earnings
for (Game in games) {
  subset_data <- subset(esport, Highest.Paying.Game == Game)
  median_earnings <- c(median_earnings, median(subset_data$Total..Game))
}

median_earnings <- median_earnings
# Create a data frame with the results
medianresults <- data.frame(Game = games, median_earnings = median_earnings)
# Create a bar plot of earnings ratio by game
ggplot(medianresults, aes(x = Game, y = median_earnings)) + 
  geom_bar(stat = "identity") +
  ylab("Median Earnings by Game") + 
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5))


#average~game---------------
esport <- read_xlsx("eSports Earnings.xlsx", sheet = "The Table")

esport
# Remove non-numeric characters from total_earnings column
esport$Total..Game. <- as.numeric(gsub("[^0-9.]", "", esport$Total..Game.))

# Subset data by game
games <- unique(esport$Highest.Paying.Game)

# Create empty vectors to store results
avg_earnings <- c()

# Loop over games and calculate average and median earnings
for (Game in games) {
  subset_data <- subset(esport, Highest.Paying.Game == Game)
  avg_earnings <- c(avg_earnings, mean(subset_data$Total..Game))
}

# Create a data frame with the results
meanresults <- data.frame(Game = games, avg_earnings = avg_earnings)

# Create a bar plot of earnings ratio by game
ggplot(meanresults, aes(x = Game, y = avg_earnings)) + 
  geom_bar(stat = "identity") +
  ylab("Average Earnings by Game") + 
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5)) + 
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix="", accuracy=0.01))


#Elvis's Code

#import the required libraries to execute the script 

library("readxl")
library("regclass")
library(ggplot2)

#Data Cleaning and Generation

ESports = read_xlsx("path/to/the/eSports Earnings.xlsx", sheet=2)
#2 refers to The Table Sheet

Loess_Smooth =read_xlsx("path/to/the/eSports Earnings.xlsx",sheet=3)
#3 refers to Age Group sheet 

dim(ESports);head(ESports)
colnames(ESports)
y = factor(ESports$Continent)
length(y)
z = ESports$`Total Earnings Overall`
length(z)
table(y)

#AF has only one value so running association resulted in an error best to omit this value

which(y=="AF") #Element 632 
y[632]

yadj = y[c(1:631,633:1000)] 
#Need to remove the AF level and continue code from there

yadj = droplevels(yadj)
zadj = log10(z[c(1:631,633:1000)])

table(yadj)

associate(zadj~yadj,data=ESports)

#Looks like we don't have an association due to the outliers when turning to Median Test

#===================================================================#
#LOESS Smoothing

Age = as.numeric(Loess_Smooth$`Raw Age`)
Earnings = Loess_Smooth$`Average Earnings`

which(Age == 1823) #Omit Row 51 which has a nonsense value

Age_adj = Age[-51]
Earnings_adj = Earnings[-51]

plot(Age_adj,Earnings_adj)

LSA = data.frame(Age_adj,Earnings_adj)
#Create a dummy data frame to allow us to work with the modified (x,y)

ggplot(LSA, mapping=aes(x=Age_adj, y=Earnings_adj)) +
  xlab("Age") + 
  ylab("Average Earnings") + 
  ggtitle("Age vs. Average Earning for Age") +
  geom_point(pch=1, size=1, col="black") +
  geom_smooth(method="loess", color="blue",fill="blue",alpha=0.2,span=0.75)

#Span is the fraction of x-values to run the local regression. It is noted that while a smaller span will produce a smoother curve, the complexity of the curve increases. Have to consider the trade off. 

localfit = loess(Earnings_adj ~ Age_adj,data=LSA,span=0.75)
summary(localfit)

y_pred <- predict(localfit,Age_adj,se=TRUE)

plot(Age_adj,localfit$residuals,main="Plot of Residuals")

plot(Age_adj,y_pred$fit,main="Age vs. Predicted y-values")


#Vaughn's Code  

library(readxl)

#===============================================================#
#Reading the respective sheets
esports <- read_xlsx("eSports Earnings.xlsx",sheet="The Table")

#===============================================================#
#Generating histogram for earnings

head(esports)
summary(esports)
hist(esports$'Total Earnings Overall')
earnings <- esports$'Total Earnings Overall'
earnings
overall <- as.numeric(earnings)
hist(overall)

#================================================================#
#Plotting Highest Earnings per Game vs. Total Earnings
plot(esports$`Total Earnings per Game`, esports$`Total Earnings Overall`, xlab='Total Earnings Per Game',
     ylab='Total Earnings Overall',
     abline(reg = lm(esports$`Total Earnings Overall` ~ esports$`Total Earnings per Game`), col = "red", lwd = 2))

#===============================================================#
#Table summary of Total Earnings 
summary(earnings)

#Terrence’s Code

# BoxPlot: Countries - Earnings Visualization
library(ggplot2)
top10_countries <- c("China", "Russian Federation", "United States", "Denmark", "Sweden", "Finland", "Germany", "Canada", "France", "Norway", "Korea, Republic of")

esports_top10 <- subset(esports, Player.Country %in% top10_countries)

ggplot(esports_top10, aes(x = Player.Country, y = Total.Overall)) +
  geom_boxplot(fill = "gray", color = "black") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = , x = "Country", y = "Total Earnings")


#Dave's code

#David Code
#PCA
library(ggplot2)

pca_scores <- as.data.frame(pca_result$x)
pca_scores$Gamer_Tag <- filtered_data$Gamer_Tag[rownames(pca_scores)]
scatter_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = Gamer_Tag)) +
  geom_point(alpha = 0.6, size = 3) +
  theme_minimal() +
  labs(title = "PCA Biplot",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Gamer Tag")

print(scatter_plot)

#Regression
library(ggplot2)

model <- lm(Estimated_Tournaments ~ Total_Earnings, data = raw_data)
plot <- ggplot(raw_data, aes(x = Total_Earnings, y = Estimated_Tournaments)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_minimal() +
  labs(title = "Linear Regression: Estimated Tournaments vs Total Earnings",
       x = "Total Earnings",
       y = "Estimated Tournaments")
print(plot)
summary(model)


