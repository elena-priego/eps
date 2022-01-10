#' Mice genotypes list
#'
#' Named list with the genotypes of the mice I am working with
#'
#' @docType data
#'
#' @usage data(micecode)
#'
#' @format Named chr
#'
#' @keywords datasets
#'
#' @examples
#' data(micecode)
#' get_genotype("Animalario-VHL2101.csv", micecode)
#'
"micecode"

micecode <- c("Ndufs4 KO CD11c-CRE \\(wt/wt\\)  Ndufs4 \\(lx/lx\\)" = "Ndufs4-WT",
              "Ndufs4 KO CD11c-CRE \\(tg\\+/wt\\)  Ndufs4 \\(lx/lx\\)" = "Ndufs4-CD11c-KO",
              "Ndufs4 KO Ndufs4 \\(D/D\\)" = "Ndufs4-KO",
              "Ndufs4 KO CD11c-CRE \\(wt/wt\\)  Ndufs4 \\(D/D\\)" = "Ndufs4-KO",
              "Ndufs4 KO CD11c-CRE \\(tg\\+/wt\\)  Ndufs4 \\(D/D\\)" = "Ndufs4-KO",
              "Ndufs4 KO CD11c-CRE \\(wt/wt\\)  Ndufs4 \\(D/lx\\)" = "Ndufs4-het-WT",
              "Ndufs4 KO CD11c-CRE \\(tg\\+/wt\\)  Ndufs4 \\(D/lx\\)" = "Ndufs4-het-KO",
              "CD11cCre VHLflox limpio CD11c-CRE \\(-/-\\)" = "VHL-WT",
              "CD11cCre VHLflox limpio CD11c-CRE \\(\\+/\\+\\)" = "VHL-KO",
              "CD11cCre x VHL\\&HIF1-fl/fl Cre \\(wt/wt\\)" = "VHL-HIF1a-WT",
              "CD11cCre x VHL\\&HIF1-fl/fl Cre \\(tg\\+/wt\\)" = "VHL-HIF1a-KO",
              "CD11c-cre Vhl\\&Hif2a-fl/fl  Cre \\(wt/wt\\)" = "VHL-HIF2a-WT",
              "CD11c-cre Vhl\\&Hif2a-fl/fl  Cre \\(tg\\+/wt\\)" = "VHL-HIF2a-KO",
              "CD11c-Cre Vhl\\&Hif1\\&Hif2-fl/fl  Cre \\(wt/wt\\)" = "VHL-HIF1a-HIF2a-WT",
              "CD11c-Cre Vhl\\&Hif1\\&Hif2-fl/fl  Cre \\(tg\\+/wt\\)" = "VHL-HIF1a-HIF2a-KO",
              "CD11cCre x VHL\\&HIF1-fl/fl cre \\(wt/wt\\)" = "VHL-HIF1a-WT",
              "CD11cCre x VHL\\&HIF1-fl/fl cre \\(tg\\+/wt\\)" = "VHL-HIF1a-KO",
              "CD11c-cre Vhl\\&Hif2a-fl/fl  cre \\(wt/wt\\)" = "VHL-HIF2a-WT",
              "CD11c-cre Vhl\\&Hif2a-fl/fl  cre \\(tg\\+/wt\\)" = "VHL-HIF2a-KO",
              "CD11c-Cre Vhl\\&Hif1\\&Hif2-fl/fl  cre \\(wt/wt\\)" = "VHL-HIF1a-HIF2a-WT",
              "CD11c-Cre Vhl\\&Hif1\\&Hif2-fl/fl  cre \\(tg\\+/wt\\)" = "VHL-HIF1a-HIF2a-KO")

BBV_levels <- c("Ndufs4-WT", "Ndufs4-KO")
CCT_levels <- c("VHL-WT", "VHL-KO")
BVR_levels <- c("VHL-HIF1a-WT", "VHL-HIF1a-KO")
DCW_levels <- c("VHL-HIF2a-WT", "VHL-HIF2a-KO")
DCX_levels <- c("VHL-HIF1a-HIF2a-WT", "VHL-HIF1a-HIF2a-KO")
