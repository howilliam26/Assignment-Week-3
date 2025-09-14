#1
attach(iris)
hist(Sepal.Width)

mean(Sepal.Width)
median(Sepal.Width)

quarts=quantile(Sepal.Width,c(.73))

pairs <- list(
  c("Sepal.Length", "Sepal.Width"),
  c("Sepal.Length", "Petal.Length"),
  c("Sepal.Length", "Petal.Width"),
  c("Sepal.Width", "Petal.Length"),
  c("Sepal.Width", "Petal.Width"),
  c("Petal.Length", "Petal.Width")
)

par(mfrow=c(3,2))

for (i in pairs) {
  x <- iris[[i[1]]]
  y <- iris[[i[2]]]
  
  plot(x,y,
       xlab = i[1],
       ylab = i[2],
       col = iris$Species,
       main = paste(i[1], "vs", i[2]))
}


par(mfrow=c(1,1))

#2

head(PlantGrowth)
attach(PlantGrowth)
hist(weight,breaks=seq(3.3,7,0.3))

boxplot(weight~group,col=2:4,
        main="Weights by group")

#find min value of weights in trt2
min_trt2 <- min(PlantGrowth$weight[PlantGrowth$group=="trt2"])
#data frame of just weights in group trt1
df_trt1 <- PlantGrowth$weight[PlantGrowth$group=="trt1"]
#find sum of trt1 weights that are below min of trt2
sum_below <- sum(df_trt1 < min_trt2)
#calcuate percentage
percent_below <- (sum_below/length(df_trt1))*100
percent_below

#dataframe wiht just plants that weigh more than 5.5
df_5.5 <- PlantGrowth$group[PlantGrowth$weight>5.5]
#Create frequency table
group_cat <- factor(df_5.5)
tcat <- table(group_cat)

barplot(tcat,
        main="Frequency of plants weighing over 5.5",
        xlab ="Groups",
        ylab = "Frequency",
        col = heat.colors(n=3,alpha=0.5,rev=FALSE)
        )
