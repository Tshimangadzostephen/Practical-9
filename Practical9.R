# Name: Tshimangadzo
# Surname: Munzhelele
# Student number: u18142274


library(caret)
library(readr)
library(e1071)


# Data #
bank <- read_csv("bank_data.csv")
candy <- read_csv("candy-data.csv")

stars <- read_csv("stars.csv")
cluster <- read_csv("cluster.csv")

#Question 1
bank$y <- ifelse(bank$y == "yes", 1, 0)
bank$y <- factor(bank$y, levels = c(0,1))

set.seed(31)
split <- round(nrow(bank)*0.50)
train_ <- sample(1:nrow(bank),
                    split,
                    replace = FALSE)

trainData <- bank[train_,]
testData <- bank[-train_,]

logistic_mod <- glm(y ~ ., data = trainData, family = "binomial")
summary(logistic_mod)

pred <- predict(logistic_mod, newdata = testData, type = "response")

y_pred_num <- ifelse(pred > 0.6, 1, 0)
y_pred <- factor(y_pred_num)
y_act <- testData$y

confusionMatrix(data = y_pred, reference = y_act, positive = "1")

#Question 1a
q1_a <- 1925
q1_b <- 308
q1_c <- 715
q1_d <- 2341


#Question 1b
q1_accuracy <- 0.8066
q1_precision <- 0.8621
q1_sensitivity <- 0.7292
q1_error <- 1 - q1_accuracy




#Question 2
candy$rankpercent <- ifelse(candy$rankpercent > 50, "popular","unpopular")
candy$rankpercent <- ifelse(candy$rankpercent == "popular", 1, 0)
candy$rankpercent <- factor(candy$rankpercent,levels = c(0,1))

set.seed(21)
split2 <- round(nrow(candy)*0.65)


train_2 <- sample(1:nrow(candy),
                 split2,
                 replace = FALSE)
trainData2 <- candy[train_2,]
testData2 <- candy[-train_2,]

logistic_mod2 <- glm(rankpercent ~ ., data = trainData2, family = "binomial")
summary(logistic_mod2)

pred2 <- predict(logistic_mod2, newdata = testData2, type = "response")

y_pred_num2 <- ifelse(pred2 > 0.5, 1, 0)
y_pred2 <- factor(y_pred_num2)
y_act2 <- testData2$rankpercent

confusionMatrix(data = y_pred2, reference = y_act2, positive = "1")

#Question 2a
q2_a <- 6
q2_b <- 7
q2_c <- 7
q2_d <- 10


#Question 2b
q2_accuracy <- 0.5333
q2_precision <- 0.4615
q2_sensitivity <- 0.4615
q2_error <- 1 - q2_accuracy





#Question 3
WithinSumSq <- sapply(1:10,
                     function(k)
                     {kmeans(stars,
                             k,
                             nstart = 20,
                             iter.max = 20)$tot.withinss})
plot(1:10,
     WithinSumSq,
     type= "b",
     xlab = "Number of clusters(k)",
     ylab = "Within cluster sum of squares")

q3 <- 3

#Question 4
set.seed(72)
q4a <- kmeans(cluster, centers = 2, nstart = 20)
q4b <- kmeans(cluster, centers = 3, nstart = 20)
q4c <- kmeans(cluster, centers = 4, nstart = 20)
q4d <- kmeans(cluster, centers = 5, nstart = 20)
q4clust <- 3

