library(testthat)
library(Churn)
library(lubridate)
library(data.table)
library(dplyr)

dataCustomer <- fread(file = "D:\Studies\UZH\Business\R\Lectures\Day 5\Churn\data_customer.csv")
dataPersonal <- fread(file = "D:\Studies\UZH\Business\R\Lectures\Day 5\Churn\data_personal.csv")

jointData <- merge(dataCustomer, dataPersonal, by="CustomerId", all = TRUE)

churnProbability <- glm(Exited ~ CreditScore + Gender + Age + Tenure
                        + Balance + NumOfProducts + HasCrCard + IsActiveMember
                        + EstimatedSalary, family = "binomial",
                        data = jointData)
jointData$ChurnProbability <- predict(churnProbability, jointData,
                                      type = "response")

context("Greater than")

test_that("Probability check",{
  expect_gt(churnPrediction(jointData, 15653251),
            churnPrediction(jointData, 15662641))
})
