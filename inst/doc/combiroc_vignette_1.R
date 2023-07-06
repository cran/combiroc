## ----setup, include=FALSE-----------------------------------------------------
library(combiroc)

## -----------------------------------------------------------------------------
library(combiroc)

## ---- eval=FALSE--------------------------------------------------------------
#  data <- load_data("./data/demo_data.csv")

## -----------------------------------------------------------------------------
data <- demo_data
head(data)

## -----------------------------------------------------------------------------
data_long <- combiroc_long(data)
data_long

## -----------------------------------------------------------------------------
sms <- single_markers_statistics(data_long)

## -----------------------------------------------------------------------------
s_table <- sms[[1]]
s_table

## -----------------------------------------------------------------------------
plot_m1 <- sms[[2]]$Marker1
plot_m1

## -----------------------------------------------------------------------------
distr <- markers_distribution(data_long, case_class = 'A', 
                              y_lim = 0.0015, x_lim = 3000, 
                              signalthr_prediction = TRUE, 
                              min_SE = 40, min_SP = 80, 
                              boxplot_lim = 2000)

## ---- boxplot-----------------------------------------------------------------
distr$Boxplot

## ----eval=FALSE---------------------------------------------------------------
#  distr$ROC

## ----echo=FALSE, fig.align = "center"-----------------------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/roc_curve_vign1.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ---- coord1------------------------------------------------------------------
head(distr$Coord, n=10)

## ---- density-----------------------------------------------------------------
distr$Density_plot

## ---- summary-----------------------------------------------------------------
distr$Density_summary

## -----------------------------------------------------------------------------
tab <- combi(data, signalthr = 450, combithr = 1, case_class='A', max_length = 3)
head(tab, n=20)

## -----------------------------------------------------------------------------
rmks <- ranked_combs(tab, min_SE = 40, min_SP = 80)
rmks$table

## -----------------------------------------------------------------------------
rmks$bubble_chart

## -----------------------------------------------------------------------------
reports <-roc_reports(data, markers_table = tab, 
                      case_class = 'A',
                      single_markers =c('Marker1'), 
                      selected_combinations = c(11,15))

## -----------------------------------------------------------------------------
reports$Plot
reports$Metrics
reports$Models

## -----------------------------------------------------------------------------
head(predict(reports$Models$`Combination 11`, type='link')) # link = f(x)

## -----------------------------------------------------------------------------
head(predict(reports$Models$`Combination 11`, type='response')) # response = p(x)

## ---- eval=FALSE--------------------------------------------------------------
#  unc_data <- load_data(data = './data/demo_unclassified_data.csv', sep = ',', labelled_data = F)

## -----------------------------------------------------------------------------
head(demo_unclassified_data)

## -----------------------------------------------------------------------------
unc_data <- demo_unclassified_data
cl_data <- combi_score(unc_data, 
                       Models =  reports$Models, 
                       Metrics = reports$Metrics, 
                       Positive_class = "abnormal", 
                       Negative_class = "normal",
                       classify = TRUE)

## -----------------------------------------------------------------------------
cl_data

## -----------------------------------------------------------------------------
cl_data$index
cl_data$`Combination 11`

## -----------------------------------------------------------------------------
unc_data <- demo_unclassified_data
cs_data <- combi_score(unc_data, 
                       Models =  reports$Models, 
                       Metrics = reports$Metrics, 
                       Positive_class = "abnormal", 
                       Negative_class = "normal",
                       classify = FALSE)
cs_data

## -----------------------------------------------------------------------------
show_markers(selected_combinations =c(11,15), markers_table = tab)

## -----------------------------------------------------------------------------
combs_list <- combs_with(markers=c('Marker1', 'Marker3'), markers_table = tab)
combs_list

## -----------------------------------------------------------------------------
sessionInfo()

