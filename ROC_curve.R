# Modified and expanded version of ROC() function from "Epi" package. 
# Use "ROCR" package to get true positive rate and false positive rate, using "hypothesis values" from models and labels. 
# Use default plot() function to draw ROC curve. 
# Calcalate the best best cutoff using a variaty of methods.

library(ROCR)
# method 
# "balanced" : select the best cutoff with respect to "balanced accuracy"
# "specificity" : select the best cutoff fixing specificity as a fixed value, default is 0.8
# "sensitivity" : select the best cutoff fixing sensitivity as a fixed value, default is 0.8 

AUC_plot <- function(hypothesis_prob, label, method = c("balanced", "specificity", "sensitivity"), value = 0.8){
  pred <- prediction(hypothesis_prob, label)
  perf <- performance(pred, "tpr", "fpr") 
  
  plot(perf@x.values[[1]], perf@y.values[[1]], main = "ROC curve", xlim = 0:1, xlab = "1-Specificity",
       ylim = 0:1, ylab = "Sensitivity", type = "n", bg="gray")
  
  rect(c(0.995, 0.995), c(0.005, 0.995), c(0.005, 0.005), c(0.995, 0.005), lty = 0, col = gray(0.9))
  grid = seq(0, 100, 10)
  if (is.numeric(grid))
    abline(h = grid/100, v = grid/100, col = gray(0.95))
  abline(0, 1, col = gray(0.4))
  box()
  lines(perf@x.values[[1]], perf@y.values[[1]], lwd = 2)
  
  auc <- performance(pred, "auc")@y.values[[1]]
  crn <- par()$usr
  text(0.95 * crn[2] + 0.05 * crn[1], 0, paste("Area under the curve:", formatC(auc, format = "f", digits = 3, width = 5)),
       adj = c(1, 0), cex = 0.7)
  
  if(method == "balanced"){
    bal_acc = ((1-perf@x.values[[1]])+perf@y.values[[1]])/2
    best_cut_idx = which.max(bal_acc)
    best_cutoff = perf@alpha.values[[1]][best_cut_idx]
    best_sensitivity = perf@y.values[[1]][best_cut_idx]
    best_specificity = (1-perf@x.values[[1]])[best_cut_idx]

  }
  if(method == "sensitivity"){
    best_cut_idx = which(perf@y.values[[1]]>=value)[1]
    best_cutoff = perf@alpha.values[[1]][best_cut_idx]
    best_specificity = (1-perf@x.values[[1]])[best_cut_idx]
    if(length(which(1-perf@x.values[[1]] == best_specificity))>=2){
      best_cut_idx = best_cut_idx + (length(which(1-perf@x.values[[1]] == best_specificity))-2)
    }
    best_sensitivity = perf@y.values[[1]][best_cut_idx]
  }
  if(method == "specificity"){
    best_cut_idx = max(which((1-perf@x.values[[1]])>=value))
    best_cutoff = perf@alpha.values[[1]][best_cut_idx]
    best_specificity = (1-perf@x.values[[1]])[best_cut_idx]
    if(length(which(1-perf@x.values[[1]] == best_specificity))>=2){
      best_cut_idx = best_cut_idx - (length(which(1-perf@x.values[[1]] == best_specificity))-1)
    }
    best_sensitivity = perf@y.values[[1]][best_cut_idx]


  }
  text(0.95 * crn[2] + 0.05 * crn[1], 0.15, paste("Best cutoff:", formatC(best_cutoff, format = "f", digits = 3, width = 5)),
       adj = c(1, 0), cex = 0.7)
  text(0.95 * crn[2] + 0.05 * crn[1], 0.1, paste("Sensitivity:", formatC(best_sensitivity, format = "f", digits = 3, width = 5)),
       adj = c(1, 0), cex = 0.7)
  text(0.95 * crn[2] + 0.05 * crn[1], 0.05, paste("Specificity:", formatC(best_specificity, format = "f", digits = 3, width = 5)),
       adj = c(1, 0), cex = 0.7)

}

# Special thank to Gyujin Huh, who "waited" 1 hr for me to finish this job. :)))

