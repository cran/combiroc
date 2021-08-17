#' @title Rank combinations.
#' @description A function to rank combinations by a Youden index and select them if they have a min SE and/or SP.
#' @details This function is meant to help the user in finding the best combinations (in the first rows) and allows also (not mandatory) the SE/SP-dependent filtering of combinations.
#' @param data a data.frame returned by load_data().
#' @param combo_table a data.frame with SE, SP and number of composing markers for each combination (returned by se_sp()).
#' @param case_class a character that specifies which of the two classes of the dataset is the case class.
#' @param min_SE a numeric that specifies the min value of SE that a combination must have to be filtered-in.
#' @param min_SP a numeric that specifies the min value of SP that a combination must have to be filtered-in.
#' @example R/examples/ranked_combs_example.R
#' @return a named list containing:
#' - $table, a data.frame with ranked combination, reporting: SE, SP, number of markers composing the combination and the score.
#' - $bubble_chart, a dot plot showing the selected 'gold' combinations
#' @export

ranked_combs <- function(data, combo_table, case_class, min_SE=0, min_SP=0) {

  nclass <- unique(data$Class) # to retrieve the 2 classes
  Markers <- combo_table$`#Markers`
  # if case class is the first
  if (case_class == nclass[1]) {

    # computing Youden as 2*(SE 1st class * SP 2nd class) /  (SE 1st class + SP 2nd class)
    combo_table$Youden<-combo_table[,1] + combo_table[,4]
    combo_table$Youden<-(combo_table$Youden/100)-1
    ranked_SE_SP<-combo_table[order(-combo_table$Youden), ]
    ranked_SE_SP<- ranked_SE_SP[,c(1,4,5,6)]
    rss <- ranked_SE_SP[ranked_SE_SP[,1]>=min_SE & ranked_SE_SP[,2]>=min_SP,]
    colnames(combo_table)[1] <- 'SE'
    colnames(combo_table)[4] <- 'SP'

    SE <- combo_table$SE
    SP <- combo_table$SP

    combo_table$Combo <- (combo_table$SP>min_SP & combo_table$SE>min_SE)
    Combo <- combo_table$Combo

    combo_table[which(combo_table$Combo=='TRUE'), 7] <- 'gold'
    combo_table[which(combo_table$Combo=='FALSE'), 7] <- 'below_thr'

    rkc <- combo_table[which(combo_table$Combo=='gold'), 1:6]
    rkc <- rkc[order(-rkc$Youden),]

    bubble<-
      ggplot(combo_table, aes(x = SP, y = SE, size = Markers, color = Combo)) +
      geom_point(alpha=0.3) +
      scale_size(range = c(5, 15), name="# of markers") +
      scale_x_continuous(limits = c(0,100)) +
      scale_y_continuous(limits = c(0,100)) +
      labs(x ="specificity", y = "sensitivity") +
      guides(color = guide_legend(order=2),
             size = guide_legend(order=1)) +
      geom_vline(xintercept = min_SP, linetype="dotted", color = "grey", size=1) +
      geom_hline(yintercept = min_SE, linetype="dotted", color = "grey", size=1) +
      scale_color_manual(values=c("blue", "gold")) +
      theme_light()

    res <- list(rkc, bubble)
    names(res)<- c('table', 'bubble_chart')

    return(res)}



  # if case class is the second
  else if (case_class == nclass[2]) {

    # computing F1 as 2*(SE 2nd class * SP 1st class) /  (SE 2nd class + SP 1st class)
    combo_table$Youden<-combo_table[,2] + combo_table[,3]
    combo_table$Youden<-(combo_table$Youden/100)-1
    ranked_SE_SP<-combo_table[order(-combo_table$Youden), ]
    ranked_SE_SP<- ranked_SE_SP[,c(2,3,5,6)]
    colnames(combo_table)[2] <- 'SE'
    colnames(combo_table)[3] <- 'SP'

    SE <- combo_table$SE
    SP <- combo_table$SP

    combo_table$Combo <- (combo_table$SP>=min_SP & combo_table$SE>=min_SE)
    Combo <- combo_table$Combo

    combo_table[which(combo_table$Combo=='TRUE'), 7] <- 'gold'
    combo_table[which(combo_table$Combo=='FALSE'), 7] <- 'below_thr'

    rkc <- combo_table[which(combo_table$Combo=='gold'), 1:6]
    rkc <- rkc[order(-rkc$Youden),]
    
    bubble<-
      ggplot(combo_table, aes(x = SP, y = SE, size = Markers, color = Combo)) +
      geom_point(alpha=0.3) +
      scale_size(range = c(5, 15), name="# of markers") +
      scale_x_continuous(limits = c(0,100)) +
      scale_y_continuous(limits = c(0,100)) +
      labs(x ="specificity", y = "sensitivity") +
      guides(color = guide_legend(order=2),
             size = guide_legend(order=1)) +
      geom_vline(xintercept = min_SP, linetype="dotted", color = "grey", size=1) +
      geom_hline(yintercept = min_SE, linetype="dotted", color = "grey", size=1) +
      scale_color_manual(values=c("blue", "gold")) +
      theme_light()

    res <- list(rkc, bubble)
    names(res)<- c('table', 'bubble_chart')
    return(res)}

  else {
    stop('Please, specify the "case class" choosing from the 2 classes of your dataset')
  }}

