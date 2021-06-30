pacman::p_load(pacman, rio, tidyverse, caret, skimr, magrittr, randomForest, caTools, e1071)
rm(list = ls())
theme_set(theme_bw())

# Preparing data

eurusd <- import(file = "clipboard")
colnames(eurusd)[5] <- "Change"
eurusd %<>% mutate(eurusd.Trend = ifelse(Change > 0, "Bull", "Bear"))
eurusd %<>% mutate(eurusd.TrendL1 = lag(eurusd.Trend, 1),
                   eurusd.TrendL2 = lag(eurusd.TrendL1, 1),
                   eurusd.TrendL3 = lag(eurusd.TrendL2, 1),
                   eurusd.TrendL4 = lag(eurusd.TrendL3, 1),
                   eurusd.TrendL5 = lag(eurusd.TrendL4, 1))
cols <- names(eurusd)[6:11]
eurusd[cols] <- lapply(eurusd[cols], factor)
eurusd <- na.omit(eurusd)

gbpusd <- import(file = "clipboard")
colnames(gbpusd)[5] <- "Change"
gbpusd %<>% mutate(gbpusd.Trend = ifelse(Change > 0, "Bull", "Bear"))
gbpusd %<>% mutate(gbpusd.TrendL1 = lag(gbpusd.Trend, 1),
                   gbpusd.TrendL2 = lag(gbpusd.TrendL1, 1),
                   gbpusd.TrendL3 = lag(gbpusd.TrendL2, 1),
                   gbpusd.TrendL4 = lag(gbpusd.TrendL3, 1),
                   gbpusd.TrendL5 = lag(gbpusd.TrendL4, 1))
cols <- names(gbpusd)[6:11]
gbpusd[cols] <- lapply(gbpusd[cols], factor)
gbpusd <- na.omit(gbpusd)

eurgbp <- import(file = "clipboard")
colnames(eurgbp)[5] <- "Change"
eurgbp %<>% mutate(eurgbp.Trend = ifelse(Change > 0, "Bull", "Bear"))
eurgbp %<>% mutate(eurgbp.TrendL1 = lag(eurgbp.Trend, 1),
                   eurgbp.TrendL2 = lag(eurgbp.TrendL1, 1),
                   eurgbp.TrendL3 = lag(eurgbp.TrendL2, 1),
                   eurgbp.TrendL4 = lag(eurgbp.TrendL3, 1),
                   eurgbp.TrendL5 = lag(eurgbp.TrendL4, 1))
cols <- names(eurgbp)[6:11]
eurgbp[cols] <- lapply(eurgbp[cols], factor)
eurgbp <- na.omit(eurgbp)

df <- as.data.frame(cbind(eurusd.Trend = eurusd$eurusd.Trend),
            eurusd.TrendL1 = eurusd$eurusd.TrendL1,
            eurusd.TrendL2 = eurusd$eurusd.TrendL2,
            eurusd.TrendL3 = eurusd$eurusd.TrendL3,
            eurusd.TrendL4 = eurusd$eurusd.TrendL4,
            eurusd.TrendL5 = eurusd$eurusd.TrendL5,
            gbpusd.TrendL1 = gbpusd$gbpusd.TrendL1,
            gbpusd.TrendL2 = gbpusd$gbpusd.TrendL2,
            gbpusd.TrendL3 = gbpusd$gbpusd.TrendL3,
            gbpusd.TrendL4 = gbpusd$gbpusd.TrendL4,
            gbpusd.TrendL5 = gbpusd$gbpusd.TrendL5,
            eurgbp.TrendL1 = eurgbp$eurgbp.TrendL1,
            eurgbp.TrendL2 = eurgbp$eurgbp.TrendL2,
            eurgbp.TrendL3 = eurgbp$eurgbp.TrendL3,
            eurgbp.TrendL4 = eurgbp$eurgbp.TrendL4,
            eurgbp.TrendL5 = eurgbp$eurgbp.TrendL5))

df[df == 1] <- "Bear"
df[df == 2] <- "Bull"

cols <- names(df)
df[cols] <- lapply(df[cols], factor)

str(df)


# Inspect data
skim(df)
glimpse(df)
sample_n(df, 3)


# Split the data into training and test set
set.seed(123)
training.samples <- createDataPartition(df$eurusd.Trend, p = 0.8, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]



# Logistic

model <- glm(eurusd.Trend ~ eurusd.TrendL1 + eurusd.TrendL3, train.data, family = binomial)
summary(model)

# Make Predictions
prob <- model %>% predict(test.data, type = "response")


predicted.classes <- ifelse(prob > 0.5, "Bull", "Bear")

# Model Accuracy
mean(predicted.classes == test.data$eurusd.Trend)

x <- predict(model, df[1:10,], type = "response")
y <- ifelse(x > 0.5, "Bull", "Bear")
y == df$eurusd.Trend[1:10]


rf <- randomForest(eurusd.Trend ~ 
                           eurusd.TrendL1+
                           eurusd.TrendL3,
                   ntree = 2000, 
                   mtry = 5,
                   data = train.data, 
                   proximity = T)
rf
trControl <- trainControl(method = "cv", number = 10, search = "grid")
trControl
