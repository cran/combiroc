## ---- include = FALSE---------------------------------------------------------
#library(httr)
#knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
knitr::opts_chunk$set(echo = TRUE)

## ----setup, include=FALSE-----------------------------------------------------
# library(combiroc)
library(devtools)
load_all()

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

## ---- ROC---------------------------------------------------------------------
distr$ROC

## ---- coord1------------------------------------------------------------------
head(distr$Coord, n=10)

## ---- density-----------------------------------------------------------------
distr$Density_plot

## ---- summary-----------------------------------------------------------------
distr$Density_summary

## -----------------------------------------------------------------------------
tab <- combi(data, signalthr = 450, combithr = 1)
head(tab, n=20)

## -----------------------------------------------------------------------------
mks <- se_sp(data, tab)
mks

## -----------------------------------------------------------------------------
rmks <- ranked_combs(data, mks, 
                     case_class = 'A', 
                     min_SE = 40, min_SP = 80)
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
#  unc_data <- load_unclassified_data(data = './data/demo_unclassified_data.csv', sep = ',')

## -----------------------------------------------------------------------------
head(demo_unclassified_data)

## -----------------------------------------------------------------------------
unc_data <- demo_unclassified_data
cl_data <- classify(unc_data, 
                    Models =  reports$Models, 
                    Metrics = reports$Metrics, 
                    Positive_class = "abnormal", 
                    Negative_class = "normal")

## -----------------------------------------------------------------------------
cl_data

## -----------------------------------------------------------------------------
cl_data$index
cl_data$`Combination 11`

## -----------------------------------------------------------------------------
show_markers(selected_combinations =c(11,15), markers_table = tab)

## -----------------------------------------------------------------------------
combs_list <- combs_with(markers=c('Marker1', 'Marker3'), markers_table = tab)
combs_list

## ---- message=FALSE-----------------------------------------------------------
library(dplyr) # needed for the filter() function and %>% operator
library(ggplot2)

myMarker <- "Marker2"

data_long %>%
  filter(Markers == myMarker) %>%
  ggplot(aes(x= Patient.ID, y=Values)) +
  geom_point(aes(color=Class)) +
  labs(title=myMarker, x ="Samples") +
  scale_x_discrete(labels = NULL, breaks = NULL)

## ---- message=FALSE-----------------------------------------------------------
library(dplyr)
library(moments) # needed for skewness() function

data_long %>%
  group_by(Markers, Class) %>%
  summarize(Mean = mean(Values),
            Min = min(Values),
            Max = max(Values),
            Sd = sd(Values),
            CV = sd(Values)/mean(Values),
            First_Quart. = quantile(Values)[2],
            Median = median(Values),
            Third_Quart. = quantile(Values)[4],
            Skewness = skewness(Values))

## ---- message=FALSE-----------------------------------------------------------
library(dplyr) # needed for the filter() function and %>% operator
library(ggplot2)

mks %>% 
  ggplot(aes(x = `SP_B`, y = `SE_A`, size = `#Markers` )) +
  geom_point(alpha=0.5) +
  scale_size(range = c(5, 15), name="# of markers") +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x ="specificity", y = "sensitivity")

## ---- message=FALSE-----------------------------------------------------------
library(dplyr) # needed for the mutate() function and %>% operator
library(ggplot2)

min_SE <- 70 # choosen SE cutoff (as in the web-app slider)
min_SP <- 85 # choosen SP cutoff (as in the web-app slider)

library(dplyr)

mks %>%
  mutate(Combo = ifelse((`SP_B`>min_SP & `SE_A`>min_SE),"gold","below_thr")) %>% 
  ggplot(aes(x = `SP_B`, y = `SE_A`, size = `#Markers`, color = Combo)) +
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

