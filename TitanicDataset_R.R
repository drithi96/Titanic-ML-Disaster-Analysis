Titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
Titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

Titanic.train$TrainSet <- TRUE
Titanic.test$TrainSet <- FALSE

Titanic.test$Survived <- NA

titanic.new <- rbind(Titanic.train, Titanic.test)

titanic.new[titanic.new$Embarked=='', "Embarked"] <- 'S'

titanic.new[is.na(titanic.new$Age), "Age"] <- 28

titanic.new[is.na(titanic.new$Fare), "Fare"] <- 14.4542

# categorial casting
titanic.new$Pclass <- as.factor(titanic.new$Pclass)
titanic.new$Sex <- as.factor(titanic.new$Sex)
titanic.new$Embarked <- as.factor(titanic.new$Embarked)


# splitting dataset into train and test
Titanic.train<-titanic.new[titanic.new$TrainSet==TRUE,]
Titanic.test<-titanic.new[titanic.new$TrainSet==FALSE,]

# cast into factor
Titanic.train$Survived <- as.factor(Titanic.train$Survived)

# predict survived
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = Titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(Titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

Survived <- predict(titanic.model, newdata = Titanic.test)

PassengerId <- Titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file="final_file.csv", row.names = FALSE)




