load("mg_clean.rds")

clean <- mg[, -c(1:3)]

clean$property_type <- as.factor(clean$property_type)
clean$default <- as.factor(clean$default)

train <- clean[clean$Sample == "Estimation", -c(8)]
val <- clean[clean$Sample == "Validation", -c(8)]
test <- clean[clean$Sample == "Holdout", -c(8)]


seeds <- vector(mode = "list", length = 51)
for(i in 1:50) seeds[[i]] <- rep(i^2,16)
seeds[[51]] <- 208
control <- trainControl(method="repeatedcv", number=5, repeats=10, seeds = seeds)
nn.grid <- expand.grid(size = c(1, 2,3), decay = c(0.6, 1, 3))
nn.model <- train(default~., data=train, method="nnet",  maxiter = 500,
                  trControl=control, tuneGrid=nn.grid, preProcess = "scale")

nn.model

#pred <- predict(nn.model, newdata = val, type = "prob")
pred <- predict(nn.model, newdata = val)
pred.test <- predict(nn.model, newdata = test, type = "prob")[2]

confusionMatrix(pred, val$default)
result = cbind(mg_acc = mg[mg$Sample == "Holdout", 1], score = pred.test)
library(pROC)
probsTrain <- predict(nn.model, newdata = val, type = "prob")
rocCurve   <- roc(response = val$default,
                  predictor = probsTrain[, 2],
                  levels = rev(levels(val$default)))
plot(rocCurve, print.thres = "best", main = "ROC Curve")

### THRESHOLD IS 0.4

pred.thre <- ifelse(probsTrain > 0.4, 1, 0)[,2]

confusionMatrix(as.factor(pred.thre), val$default)

write.csv(result, "result.csv")
