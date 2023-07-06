## ---- include = FALSE---------------------------------------------------------
library(httr)
library(knitr)
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "..")

## ----warning=FALSE,message=FALSE----------------------------------------------
# main libraries:
library(combiroc)
library(dplyr)
library(tidyr)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  library(devtools) # used to install devel-level packages
#  library(Seurat) # used to process scRNAseq data
#  library(SeuratData) # used to install and load scRNAseq datasets
#  library(SeuratDisk) # used to read h5seurat files

## ----eval=FALSE---------------------------------------------------------------
#  # read the downloaded h5seurat dataset file (using SeuratDisk functions)
#  atlas <- LoadH5Seurat("pbmc_multimodal.h5seurat")
#  # activate the level-1 annotations
#  Idents(atlas) <- atlas@meta.data$celltype.l1
#  # overview annotated cell clusters with UMAP
#  DimPlot(atlas, label = T, repel = T)

## ----echo=FALSE, fig.align = "center"-----------------------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/atlas_dimplot.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  # Performing differential expression analysis
#  nk.de.markers <- FindMarkers(atlas, ident.1 = "NK", ident.2 = NULL)
#  nk.de.markers <- nk.de.markers[order(-nk.de.markers$avg_log2FC), ]
#  nk_genes <- rownames(nk.de.markers)[1:30]

## ----eval= FALSE, fig.height = 10, fig.width = 8, fig.align = "center"--------
#  FeaturePlot(atlas, nk_genes[1:4])

## ----echo=FALSE, fig.align = "center"-----------------------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/FeaturePlot1.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## -----------------------------------------------------------------------------
nk.de.markers <- read.csv('inst/precomp/nk_degs')
nk_genes <- rownames(nk.de.markers)[1:30]
nk_genes

## ----eval=FALSE---------------------------------------------------------------
#  train <- seurat_to_combiroc(SeuratObject = atlas,
#                              gene_list = nk_genes,
#                              assay = 'SCT',
#                              labelled_data = T,
#                              case_class = 'NK',
#                              case_label = 'NK',
#                              control_label =  'Other')

## ----eval=FALSE---------------------------------------------------------------
#  train <- readRDS(file = "inst/precomp/train.rds")
#  head(train)

## ----echo=FALSE---------------------------------------------------------------
train_sub <- readRDS(file = "inst/precomp/train_sub.rds")
head(train_sub)

## ----eval=FALSE---------------------------------------------------------------
#  train_long <- combiroc_long(train)

## ----echo=FALSE---------------------------------------------------------------
n_markers <- seq(1, 5)
n_combs <- as.numeric(lapply(n_markers, choose, n=30))
tot_combs <-cumsum(n_combs) 
n_df <- cbind(as.data.frame(n_markers), as.data.frame(n_combs), as.data.frame(tot_combs))
n_df

## ----echo=FALSE, fig.height=2.5, fig.width=4, fig.align = 'center'------------
n_markers <- seq(1, 30)
n_combs <- as.numeric(lapply(n_markers, choose, n=30))
tot_combs <-cumsum(n_combs) 
n_df <- cbind(as.data.frame(n_markers), as.data.frame(n_combs), as.data.frame(tot_combs))
ggplot(n_df, aes(n_markers)) +
  geom_line(aes(y = n_combs, color = "n_combs")) +
  geom_point(aes(y = n_combs, color = "n_combs")) +
  geom_line(aes(y = tot_combs, color = "tot_combs")) +
  geom_point(aes(y = tot_combs, color = "tot_combs")) +
  theme(
    legend.position = c(.95, .60),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(0, 4, 4, 4),
    legend.title = element_blank()
    )

## ----eval=FALSE---------------------------------------------------------------
#  distr <- markers_distribution(train_long,
#                                signalthr_prediction = TRUE,
#                                case_class = "NK")
#  distr$Density_plot

## ----echo=FALSE, fig.width = 6------------------------------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/distr_dens_plot.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  autoThreshold <- distr$Coord[distr$Coord$Youden==max(distr$Coord$Youden), 'threshold'][1]
#  
#  # the exact value of optimal threshold in this case is 0.8958797, as can be seen with:
#  distr$Coord[distr$Coord$Youden==max(distr$Coord$Youden),]

## ----eval=FALSE---------------------------------------------------------------
#  train_tab <- combi(train, signalthr = autoThreshold, combithr = 2, max_length = 5, case_class = 'NK')

## ----eval=FALSE---------------------------------------------------------------
#  train_tab <- read.table('inst/precomp/train_tab.rds')
#  tail(train_tab.rds)

## ----echo=FALSE---------------------------------------------------------------
train_tab_sub <- readRDS('inst/precomp/train_tab_sub.rds')
train_tab_sub

## ----eval=FALSE---------------------------------------------------------------
#  rmks <- ranked_combs(combo_table = train_tab,
#                       min_SE = 95,
#                       min_SP = 95)
#  head(rmks$table)

## ----echo=FALSE---------------------------------------------------------------
rmks_table <- readRDS('inst/precomp/rmks_table_sub.rds')
rmks_table

## ----eval=FALSE---------------------------------------------------------------
#  reports <-roc_reports(train,
#                        markers_table = train_tab,
#                        selected_combinations =  c(169807,172173,163878, 137550),
#                        case_class = 'NK',
#                        single_markers= nk_genes[1:4])

## ----eval=FALSE---------------------------------------------------------------
#  reports$Plot

## ----echo=FALSE, fig.width = 6, fig.align = "center"--------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/roc_curve.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  rocplot <- (reports$Plot) + coord_cartesian(xlim = c(0, 0.2))
#  rocplot
#  reports$Metrics

## ----echo=FALSE, fig.width = 6, fig.align = "center"--------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/roc_curve_zoom.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----echo=FALSE, fig.align = "center"-----------------------------------------
reports_metrics <- readRDS('inst/precomp/reports_metrics.rds')
reports_metrics

## ----eval=FALSE---------------------------------------------------------------
#  show_markers(selected_combinations =c(169807,172713), markers_table = train_tab)

## ----eval=FALSE---------------------------------------------------------------
#  # retrieve cbmc data from SeuratData package
#  if (!require("SeuratData", quietly = TRUE))
#      devtools::install_github('satijalab/seurat-data')
#  library(SeuratData)
#  InstallData("cbmc.SeuratData")
#  data(cbmc)
#  
#  # process data with standard Seurat protocol
#  library(Seurat)
#  cbmc <- NormalizeData(cbmc, verbose = F) %>%
#    FindVariableFeatures(verbose = F) %>%
#    ScaleData(verbose = F) %>%
#    RunPCA(verbose = F)
#  cbmc <- FindNeighbors(cbmc, dims = 1:30, verbose = F)
#  cbmc <- FindClusters(cbmc, resolution = 0.8, verbose = FALSE)
#  cbmc <- RunUMAP(cbmc, dims = 1:30, verbose = F)
#  
#  # add cell annotations from CITE seq to identities
#  Idents(cbmc) <- cbmc@meta.data$protein_annotations
#  cbmc[['ID']]<- Idents(cbmc)
#  cbmc

## ----eval=FALSE---------------------------------------------------------------
#  # visualize UMAP clusters
#  DimPlot(cbmc, repel = T, label = T)

## ----echo=FALSE, fig.width = 6, fig.align = "center"--------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/cbmc_dimplot.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ---- eval=FALSE--------------------------------------------------------------
#  test_cbmc <- seurat_to_combiroc(SeuratObject = cbmc,
#                                  gene_list = nk_genes,
#                                  assay = 'RNA')
#  head(test_cbmc)

## ----eval=FALSE, fig.height=8, fig.align = "center"---------------------------
#  VlnPlot(cbmc, features = nk_genes[1:4], group.by = 'ID', pt.size=0, ncol = 2)

## ----echo=FALSE, fig.align = "center"-----------------------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/cbmc_violinplots_genes.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  # adding combi score for combination 172173
#  cs_cbmc <- combi_score(test_cbmc, Models = reports$Models, Metrics = reports$Metrics, Positive_class = 'NK', Negative_class = 'Other')
#  cbmc[['c172173_combi_score']]<- cs_cbmc$`Combination 172173`

## ----eval=FALSE---------------------------------------------------------------
#  p1 <- FeaturePlot(cbmc, features = "c172173_combi_score")
#  p2 <- VlnPlot(cbmc, features = "c172173_combi_score", group.by = "ID", pt.size = 0)
#  p1 | p2

## ----echo=FALSE, fig.width=10, fig.align = "center"---------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/cbmc_dimviolin_c172173.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  # retrieve pbmc3k dataset from SeuratData package
#  # library(SeuratData)
#  InstallData("pbmc3k.SeuratData")
#  data("pbmc3k.final")
#  pbmc3k.final <- pbmc3k.final
#  
#  # add cell annotations to identities
#  pbmc3k.final[['ID']] <- Idents(pbmc3k.final)
#  pbmc3k.final

## ----eval=FALSE---------------------------------------------------------------
#  DimPlot(pbmc3k.final, reduction = "umap", label = T, repel = T)

## ----echo=FALSE, fig.align = "center"-----------------------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/pbmc3k_dimplot.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ---- eval=FALSE--------------------------------------------------------------
#  test_pbmc3k <- seurat_to_combiroc(SeuratObject = pbmc3k.final, gene_list = nk_genes, assay = 'RNA')
#  head(test_pbmc3k)

## ----eval=FALSE---------------------------------------------------------------
#  VlnPlot(pbmc3k.final, features = nk_genes[1:4], group.by = 'ID', pt.size=0, ncol = 2)

## ----echo=FALSE, fig.height=8, fig.align="center"-----------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/pbmc3k_violinplots_genes.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  sub <- reports$Models
#  sub$`Combination 169807` <- NULL
#  sub$`Combination 172173` <- NULL
#  cs_pbmc3k <- combi_score(test_pbmc3k, Models = sub,
#      Metrics = reports$Metrics, Positive_class = "NK", Negative_class = "Other")
#  pbmc3k.final[["c137550_combi_score"]] <- cs_pbmc3k$`Combination 137550`  # to add combi score of combination 137550

## ----eval=FALSE---------------------------------------------------------------
#  p1 <- FeaturePlot(pbmc3k.final, features = "c137550_combi_score")
#  p2 <- VlnPlot(pbmc3k.final, features = "c137550_combi_score", group.by = "ID", pt.size = 0)
#  p1 | p2

## ----echo=FALSE, fig.width=10, fig.align="center"-----------------------------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/pbmc3k_dimviolin_c137550.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## -----------------------------------------------------------------------------
source("inst/external_code/signature_score.R")

## ----eval=FALSE---------------------------------------------------------------
#  # computing whole signature gene-signature-score for CBMC
#  NK_sig_cbmc<-signature_score(SeuratObj = cbmc, geneset = nk_genes)
#  cbmc$NK_signature_score <- NK_sig_cbmc$scaled_combined_score
#  
#  # computing whole signature gene-signature-score for PBMC-3K
#  NK_sig_pbmc3k<-signature_score(SeuratObj = pbmc3k.final, geneset = nk_genes)
#  pbmc3k.final$NK_signature_score <- NK_sig_pbmc3k$scaled_combined_score

## ----eval=FALSE---------------------------------------------------------------
#  # computing gene-signature-score of combination 172173 for CBMC
#  comb_cbmc<-signature_score(SeuratObj = cbmc, geneset = c('GZMB','IL2RB','KLRF1','SPON2','TRDC'))
#  cbmc[['c172173_signature_score']] <- comb_cbmc$scaled_combined_score
#  
#  # computing gene-signature-score of combination 137550 for PBMC-3K
#  comb_pbmc3k<-signature_score(SeuratObj = pbmc3k.final, geneset = c('CLIC3', 'FCGR3A', 'IL2RB','KLRD1','KLRF1'))
#  pbmc3k.final[['c137550_signature_score']] <- comb_pbmc3k$scaled_combined_score

## ----eval=FALSE---------------------------------------------------------------
#  v1 <- VlnPlot(cbmc, features='NK_signature_score', group.by = 'ID', pt.size=0)
#  v2 <- VlnPlot(cbmc, features='c172173_signature_score', group.by = 'ID', pt.size=0)
#  v1|v2

## ----echo=FALSE, fig.height = 5, fig.width = 10, fig.align = "center"---------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/cbmc_NK_c172173_sigscore.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  v1 <- VlnPlot(pbmc3k.final, features='NK_signature_score', group.by = 'ID', pt.size=0)
#  v2 <- VlnPlot(pbmc3k.final, features = "c137550_signature_score", group.by = "ID", pt.size = 0)
#  v1|v2

## ----echo=FALSE, fig.height = 5, fig.width = 10, fig.align = "center"---------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/pbmc3k_NK_c137550_sigscore.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  v1 <- VlnPlot(cbmc, features='NK_signature_score', group.by = 'ID', pt.size=0)
#  v2 <- VlnPlot(cbmc, features='c172173_combi_score', group.by = 'ID', pt.size=0)
#  v1|v2

## ----echo=FALSE, fig.height = 5, fig.width = 10, fig.align = "center"---------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/cbmc_NK_c172173_combiscore.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  v1 <- VlnPlot(pbmc3k.final, features='NK_signature_score', group.by = 'ID', pt.size=0)
#  v2 <- VlnPlot(pbmc3k.final, features = "c137550_combi_score", group.by = "ID", pt.size = 0)
#  v1|v2

## ----echo=FALSE, fig.height = 5, fig.width = 10, fig.align = "center"---------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/pbmc3k_NK_c137550_combiscore.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  combs_length <- c(30, 5)
#  random_combs <- c(0, 0)
#  set.seed(1492)
#  for (i in combs_length) {
#        random_list <- sample(rownames(cbmc), i)
#        dfc <- signature_score(cbmc, random_list)
#        cbmc[[paste0("random_", as.character(i))]] <- dfc$scaled_combined_score
#        dfp <- signature_score(pbmc3k.final, random_list)
#        pbmc3k.final[[paste0("random_", as.character(i))]] <- dfp$scaled_combined_score
#  }

## ----eval=FALSE---------------------------------------------------------------
#  VlnPlot(cbmc, features = c('random_30', 'random_5'), group.by = 'ID', pt.size=0)

## ----echo=FALSE, fig.height = 5, fig.width = 8, fig.align = "center"----------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/cbmc_violin_randomcontrol.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

## ----eval=FALSE---------------------------------------------------------------
#  VlnPlot(pbmc3k.final, features = c('random_30', 'random_5'), group.by = 'ID', pt.size=0)

## ----echo=FALSE, fig.height = 5, fig.width = 8, fig.align = "center"----------
mywd <- getwd()
knitr::include_graphics(paste0(mywd, "/vignettes/pbmc3k_violin_randomcontrol.png"), dpi = 2, rel_path = getOption("knitr.graphics.rel_path", FALSE))

