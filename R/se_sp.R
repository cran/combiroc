#' @title Compute sensitivity and specificity of each combination
#' @description A function to compute sensitivity and specificity of each combination for each class.
#' @details This function calculate SE and SP for each combination. The SE of a given combination (capability to find real positives/cases) corresponds to the SE of the case class, while its SP (capability to exclude real negatives/controls) corresponds to the SP of the control class.
#'@param data a data.frame returned by load_data().
#'@param combinations_table a data.frame containing how many samples of each class are "positive" for each combination (returned by combi()).
#'@example R/examples/se_sp_example.R
#'@return data.frame with SE, SP and number of composing markers for each combination.
#'@export

se_sp <- function(data, combinations_table){

  mks <- combinations_table
  names<- c()
  nclass <- unique(data$Class) # to retrieve the 2 classes

  SE_SP<-array(0,dim=c(dim(mks)[1],2*2)) # empty array to be filled

  for (i in  1:length(nclass)){
    SE_SP[,i]<- round(mks[,i+1]*100/length(colnames(t(data[data$Class==nclass[i],])))
                      ,digits=0) # SE of the given class
    names[i] <- paste0('SE_', nclass[i])
    SE_SP[,i+2]<- 100-SE_SP[,i] # SP of the given class
    names[i+2] <- paste0('SP_', nclass[i])
  }

  SE_SP <- data.frame(SE_SP) # from array to dataframe
  rownames(SE_SP)<-rownames(mks)
  colnames(SE_SP)<- names

  # to add the count of markers composing the combination
  n_markers<- rep(NA, dim(mks)[1])
  for (i in 1:dim(mks)[1]){
    n_markers[i] <- str_count(mks$Markers[i], pattern = "-")+1}
  # the number of markers is equal to number of '-' +1  => '-' must be avoided in marker name

  SE_SP$count <- data.frame(n_markers)[,1]
  colnames(SE_SP)[5] <- '#Markers'

  return(SE_SP)
}

