#### Table 3.2
chooseCRANmirror()
setwd("C:/DS535/projectwk3") 

house.df <- read.csv("data.csv")
str(house.df)  # structure of the data frame
head(house.df, 9)
tail(house.df)   # default, show last 5 records
View(house.df)

summary(house.df)
library(psych)
describe(house.df)

## scatter plot with axes names
install.packages("plot")
plot(house.df$price ~ house.df$sqft_living, xlab = "Price", ylab = "Square Feet Living Area")

# alternative plot with ggplot
install.packages("ggplot2")
library(ggplot2)
ggplot(house.df) + geom_point(aes(x = price, y = sqft_living), colour = "navy", alpha = 0.7)



install.packages("plotly")
library(plotly)

# histogram with smoothed density
install.packages("plotly")
library(plotly)
p <- ggplot(house.df, aes(x=price)) +
  geom_histogram(aes(y = ..density..), binwidth=density(house.df$price)$bw) +
  geom_density(fill="red", alpha = 0.2)

ggplotly(p)

# add linear regression fit
p <- ggplot(house.df, aes(x = price, y = sqft_living) ) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm, se=TRUE)    # Add linear regression line
  geom_smooth()            # Add a loess smoothed fit curve with confidence region
# loess (locally estimated scatterplot smoothing)
# lowess (locally weighted scatterplot smoothing)

ggplotly(p)

plot(house.df[,c(2,5,7,10)] )  

install.packages("GGally")
library(GGally)
# ggpairs(housing.df[,c(2,3,4,5)] )
ggpairs(housing.df[,c(2:9)] )     # variables in columns 2 to 9
#ggpairs(housing.df[,c(2:4, 6, 8:10)] ) 

## barchart of City vs. mean Price
# compute mean Price Each City = (0, 1)
data.for.plot <- aggregate(house.df$price, by = list(house.df$city), FUN = mean)
data.for.plot

names(data.for.plot) <- c("city", "Meanprice")
barplot(data.for.plot$Meanprice,  names.arg = data.for.plot$city, 
        xlab = "city", ylab = "Avg. Price")
# alternative plot with ggplot
ggplot(data.for.plot) + geom_bar(aes(x = city, y = Meanprice), stat = "identity")



## barchart of city Vs price
data.for.plot <- aggregate(house.df$price, by = list(house.df$city), FUN = mean)
data.for.plot
names(data.for.plot) <- c("city", "Meanprice")
barplot(data.for.plot$Meanprice * 100,  names.arg = data.for.plot$city, 
        xlab = "city", ylab = "% of Meanprice")



#### Figure 3.2

## histogram of yr_built
hist(house.df$yr_built, xlab = "Year Built")
# alternative plot with ggplot
library(ggplot2)
ggplot(house.df) + geom_histogram(aes(x = yr_built), binwidth = 5)

ggplot(house.df) + geom_boxplot( aes( y = price) ) 
boxplot(house.df$price ~ house.df$waterfront, xlab = "waterfront", ylab = "price")

# alternative plot with ggplot

ggplot(house.df) + geom_boxplot(aes(x = as.factor(waterfront), y = price , color =as.factor(waterfront) )) + xlab("price")


p <- ggplot(housing.df, aes(x=yr_built)) +
  geom_histogram(aes(y = ..density..), binwidth=density(housing.df$yr_built)$bw) +
  geom_density(fill="red", alpha = 0.2)


ggplotly(p)


# animation
df <- data.frame(
  x = c(1:10), 
  y = c(1:10), 
  f = c(1:10)   
)



p <- ggplot(df, aes(x, y)) +
  geom_point(aes(frame = f))

ggplotly(p)


# animation
# https://plot.ly/ggplot2/cumulative-animations/




# violin plot
# violin plot is similar to boxplot, with a kernel density plot on each side
ggplot(house.df) + geom_violin(aes(x = as.factor(waterfront), y = price)) + xlab("waterfront")


#### Figure 3.3

## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(housing.df$yr_built ~ housing.df$country, xlab = "country", ylab = "yr_built")
boxplot(housing.df$sqft_living ~ housing.df$country, xlab = "country", ylab = "sqft_living")
boxplot(housing.df$condition ~ housing.df$country, xlab = "country", ylab = "condition")
boxplot(housing.df$bedrooms ~ housing.df$country, xlab = "country", ylab = "bedrooms")



#### Figure 3.4
## simple heatmap of correlations (without values)
numeric_df <- na.omit(housing.df[sapply(housing.df, is.numeric)])
heatmap(cor(numeric_df), Rowv = NA, Colv = NA)

## heatmap with values
install.packages("reshape")
dev.off()
library(gplots)
heatmap.2(cor(numeric_df), #Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          cellnote = round(cor(numeric_df),2), 
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

install.packages("MASS")
library(MASS)
parcoord(numeric_df)
parcoord(numeric_df, col = rainbow(100), lty = 1, lwd = 2, var.label = TRUE)
