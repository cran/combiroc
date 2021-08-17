
#'@title Classify data.frames using glm(link='binomial') models.
#'@description A function that applies the previously calculated models to an unclassified dataset and classifies the samples.
#'@details This function  can classify dataset loaded with load_unclassified_data() that MUST contain all the markers of the classified dataset used to train the models (the one loaded with load_data()).


#' @param unclassified_data a data.frame returned by load_unclassified_data().
#' @param Models a list of glm() objects returned by roc_reports().
#' @param Metrics a list of data.frame objects containing ROC metrics, returned by roc_reports().
#' @param Positive_class a numeric or a character that specifies the label of the samples that will be classified as positives
#' @param Negative_class a numeric or a character that specifies the label of the samples that will be classified as negatives
#' @return  a data.frame containing the predicted class of each sample, for each marker/combination in Models
#' @importFrom stats formula glm median predict quantile sd
#' @example R/examples/classify_example.R
#' @export
classify <- function(unclassified_data, Models, Metrics, Positive_class=1, Negative_class=0){

  classification <- list()
  pr_df <- data.frame(unclassified_data[,1])
  colnames(pr_df)[1] <- 'index'
  for (i in names(Models)){



    pred <- predict(Models[[i]], newdata = unclassified_data,
                    type = "response")
    cutoff <- Metrics[which(rownames(Metrics)==i), 4]

    pr_df[i] <- pred>cutoff
    pr_df[which(pr_df[i]=='TRUE'), i] <- Positive_class
    pr_df[which(pr_df[i]=='FALSE'), i] <- Negative_class
  }
  return(pr_df)}
