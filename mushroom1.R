library(magrittr)
library(dplyr)
#read data
mush.data <- read.csv("mushrooms.csv")
View(mush.data)

#cleandata <- mush.data[complete.cases(mush.data),]

is.null(mush.data)

summary(mush.data) 

#making data more readible
#mush.data <- mush.data %>% map_dfr(function(.x) as.factor(.x))
#levels(mush.data$class) <- c("edible", "poisonous")
#didnt work


#viel.type,veil.color,ring.number, gill.attachment has no variation or little variation 
mush.data[,c("veil.type")]<-list(NULL)
mush.data[,c("veil.color")]<-list(NULL)
mush.data[,c("ring.number")]<-list(NULL)
mush.data[,c("gill.attachment")]<-list(NULL)
View(mush.data)




#visualize data to make some assumptions
library(ggplot2)
ggplot(mush.data, aes(x = cap.shape, y = cap.color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

ggplot(mush.data, aes(x = class, y = stalk.shape, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))


ggplot(mush.data, aes(x = class, y = odor, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

#finding correlation
char2num<-function(odor){
  groups = unique(odor)
  as.numeric(factor(odor, levels=groups))}
n=length(mush.data$class)
with(mush.data, cor(runif(n), char2num(mush.data$odor)))

char2num<-function(cap.color){
  groups = unique(cap.color)
  as.numeric(factor(cap.color, levels=groups))}

n=length(mush.data$class)
with(mush.data, cor(runif(n), char2num(mush.data$cap.color)))


char2num<-function(stalk.shape){
  groups = unique(stalk.shape)
  as.numeric(factor(stalk.shape, levels=groups))}
n=length(mush.data$class)
with(mush.data, cor(runif(n), char2num(mush.data$stalk.shape)))

#Model building

#model1: decision tree
library("rpart")
library("rpart.plot")
fit<-rpart(class ~ mush.data$cap.shape + mush.data$cap.surface, method="class", data=mush.data, control=rpart.control(minsplit=1),parms=list(split="information"))
rpart.plot(fit, type=4, extra=1)


#model2: naiveBayes 
library(e1071)
m <- naiveBayes(class ~ cap.shape + cap.surface + odor + cap.color + bruises + gill.spacing + gill.size + gill.color + stalk.shape + stalk.root + stalk.surface.above.ring + stalk.surface.below.ring + stalk.color.above.ring + stalk.color.below.ring + ring.type + spore.print.color + population + habitat, mush.data)

cap.shape <- c("b","c","x")
odor <- c("a","c","y")
cap.color <- c("n","c","g")
cap.surface <- c("s","y","y")
bruises <- c("t","t","f")

df <- data.frame(cap.shape, odor, cap.color, cap.surface, bruises)
predict(m,df)


