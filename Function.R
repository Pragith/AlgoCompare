

####    Sampling    ####

n = nrow(data)
m = floor(training_sample_size*n)


#######  Logistic Regression with LOGIT Link

fit.LR.logit = glm(bad ~ .
                        ,data = train.data
                        ,family = binomial(link=logit)
                        )

summary(fit.LR.logit)

pred.lr = predict(fit.LR.logit, newdata=test.data, type="response")

pred.lr2 = rep(NA, length(pred.lr))
pred.lr2[pred.lr > 0.5] = 1
pred.lr2[pred.lr <= 0.5] = 0
confusion.lr.logit = table(actual,pred.lr2)
confusion.lr.logit
####################################################################


##################### Support Vector Machines ######################

#library(e1071)

fit.svm = svm(as.factor(bad) ~ ., data=train.data)

pred.svm = predict(fit.svm, newdata=test.data)

confusion.svm = table(actual,pred.svm)
confusion.svm

####################################################################








########## Multiple Simulations ##########




##### Function for Simulator Execution #####
run.sim = function(sim.size){
  for (i in 1:sim.size){

    ######  Build Train data
    id = sample(1:n, m, replace=FALSE)
    train.data = data[id,]
    
    ######  Build Test data
    test.data = data[-id,]
    
    #############    Decision Tree     ###################
    
    #library(rpart)
    fit.dt = rpart(bad ~ ., data=train.data, method="class")
    
    pred.dt = predict(fit.dt, newdata=test.data, type="class")
    confusion.dt = table(actual, pred.dt)
    
    #############    END    ###################
    
    
    #############    Naive Bayes     ###################
    
    #library(e1071)
    fit.nb = naiveBayes(as.factor(bad) ~ ., data=train.data)
    
    pred.nb = predict(fit.nb, newdata=test.data)
    
    confusion.nb = table(actual,pred.nb)
    confusion.nb
    
    #############    END    ###################
    
    
    #############    Random Forest    ###################
    
    #library(randomForest)
    fit.rf = randomForest(as.factor(bad) ~ ., data=train.data)
    
    pred.rf = predict(fit.rf, newdata=test.data)
    
    confusion.rf = table(actual,pred.rf)
    confusion.rf
    
    #############    END    ###################
    
    p.nb = prec(confusion.nb)
    r.nb = recall(confusion.nb)
    f.nb = fscore(r.nb,p.nb)
    
    p.dt = prec(confusion.dt)
    r.dt = recall(confusion.dt)
    f.dt = fscore(r.dt,p.dt)
    
    p.rf = prec(confusion.rf)
    r.rf = recall(confusion.rf)
    f.rf = fscore(r.rf,p.rf)
    
    algo.comparison = data.frame( precs=c(p.nb,p.dt,p.rf,0)
                                  ,recs=c(r.nb,r.dt,r.rf,0)
                                  ,fsc=c(f.nb,f.dt,f.rf,0)
    )
    
    colnames(algo.comparison) = c("Precision","Recall","F-Score")
    rownames(algo.comparison) = c("Naive-Bayes","Decision Tree","Random Forest","Winner")
    
    for (j in 1:3){
      max.value = which.max(algo.comparison[,j])
      if (max.value == 1) winner.algo = "Naive-Bayes"
      if (max.value == 2) winner.algo = "Decision Tree"
      if (max.value == 3) winner.algo = "Random Forest"
      algo.comparison[4,j] = winner.algo
    }
    
    print(algo.comparison)              
    print ("\n")
  } # End of FOR loop
} # End of Function




