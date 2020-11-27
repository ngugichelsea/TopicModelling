#DETERMINING THE NUMBER OF TOPICS, K
#Cross- Validation- Divide the data into 5 folds, and each time us 4 folds of teh data to fit a model then use the keft one fold to evaluate the model fit
#Do these for a number of topics then select the optimal number of topics based on a certain measure of the model fit
#Use perplexity as the staistic to measure model fit
#Perplexity is the geometric mean of word likelihood
#Train the model using the 4 fols of data the use the left one fold of the data to calculate the perplexity
#set the LDA or CTM model to be the training model and not to estimate the beta parameters

#5-FOLD CROSS VALIDATION FOR FOR K RANGING FROM 2 TO 9 FOR LDA
#Directly create a categorical variable for the different folds of data
#Define a function that takes two parameters, K and fold (which fold of the data is used to calculate the perplexity for model fit)
#In the function, training.dtm gets the index of data for training the model and testing.dtm for the model test
#In the first LDA function, a topic model is estimated using the training data. In the 2nd LDA function, set the model to be training.model and estimate.beta=FALSE
#Then obtain the perplexity of the model using the function 'perplexity' and return as the output
#Run the analysis using the for loop and save all the perpelexity in the matrix res

k.topics <- 2:9
folding <- rep(1:100, each = round(nrow(newsDTM)*0.01))

runonce <- function(k, fold) {
  testing.dtm <- which(folding == fold)
  training.dtm <- which(folding != fold)
  
  training.model <- LDA(newsDTM[training.dtm, ], k = k)
  test.model <- LDA(newsDTM[testing.dtm, ], model = training.model, control = list(estimate.beta = FALSE))
  
  perplexity(test.model)
}

res <- NULL

for (k in 2:9) {
  for (fold in 1:100) {
    res <- rbind(res, c(k, fold, runonce(k, fold)))
  }
}

res

#To get the total perplexity with 2-9 topics
total.perp <- tapply(res[, 3], res[, 1], sum)
round(total.perp)
min.pep.value= min(round(total.perp))
round(min.pep.value)

plot(2:9, total.perp, type = "b", xlab = "Number of topics", ylab = "Perplexity")
