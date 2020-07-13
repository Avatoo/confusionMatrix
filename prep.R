# 9 normal layout shinythemes (adapted from fileupload4.R)

library(shiny)
library(readr)
library(shinythemes)
library(tibble)
library(dplyr)
library(tidyr)
library(caret)
library(ranger)

df_churn <- read_csv("WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  mutate(Churn = ifelse(Churn=='Yes', 1, 0)) %>% 
  drop_na(TotalCharges)

rownames(head(df_churn))

df_churn =  column_to_rownames(as.data.frame(df_churn), 'customerID')  
  
rownames(head(df_churn))

set.seed(3456)
trainIndex <- createDataPartition(df_churn$Churn, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train <- df_churn[ trainIndex,]
test  <- df_churn[-trainIndex,]

#mod <- glm(Churn ~ ., family = binomial(link='logit'), data = train)
mod <- ranger(Churn ~ ., data = train)

pred <- predict(mod, test, type = "response")

results = test %>% 
  select(actual = Churn) %>% 
  add_column(pred = pred$predictions)

write.csv(results, 'results.csv')

# turn probs into classes
p_class = ifelse(results$pred >.5, 1, 0)
table(p_class)

# confusion matrix
confusionMatrix(p_class, results$actual, positive = '1')

# hit rates i.e. sensitivity

