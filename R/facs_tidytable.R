####facs_tidytable####

#' Prepare the data in a tidy format from the data obtained in Flowjo
#' 
#' 
#' Generation of a tidytable from the .xls generated from Flowjo.  
#' It's important to have the tubes correctly labelled: specimen should have a 
#' descriptive name without using "_" and each tube should be named using ONLY 
#' the full name of the mice.
#'
#' @param file .xls generated from Flowjo with cell percentages and fluorescent
#'  intensities
#' @param gate_pattern named list with the replacements desired for the gates. 
#' gate_pattern = c("Lymphocytes/Single Cells/Single Cells/CD452/" = "", 
#' "Freq. of Parent" = "Freq.", "Freq. of Grandparent" = "Freq.",
#' "Geometric Mean" = "GMFI", "Median" = "MdFI", "\\)" = "")  
#' @param gates named list with the desired replacement for the remaining gates
#' after the removal of the gate_pattern
#' @param marker_pattern 
#'
#' @return
#' @export
#'
#' @examples 
#' facs_tidytable("table.xls", gate_pattern = 
#' c("Lymphocytes/Single Cells/Single Cells/CD452/" = "", 
#' "Freq. of Parent" = "Freq.", "Freq. of Grandparent" = "Freq.",
#' "Geometric Mean" = "GMFI", "Median" = "MdFI", "\\)" = "", 
#' "Ly6G, Ly6C subset/AMs" = "AMs", "Ly6G, Ly6C subset/EOs" = "EOs",
#' "Monocytes" = "Mos", "Neutrophils" = "NOs")
#'
#'  
facs_tidytable <-
  function(file,
           gate_pattern,
           gates,
           marker_pattern) {
    x <- read_excel(file)
    x <- sapply(x[], function(y)
      as.character(y))
    x <- as_tibble(x)
    x <- x[!(x[, 1] == "Mean" | x[, 1] == "SD"), ]
    freq <- grep("(.*) Freq. (.*)", names(x))
    x[freq] <- lapply(x[freq], function(y)
      sub("%", "", y))
    names(x) <- str_replace_all(names(x), gate_pattern)
    x <- x %>%  pivot_longer(cols = -"...1",
                             names_to = "statistic",
                             values_to = "value") %>%
      separate("...1",
               into = c("organ", "genotype", "mice"),
               sep = "_") %>%
      separate("statistic",
               into = c("cell", "stat2"),
               sep = "\\|") %>%
      separate("stat2", into = c("stat", "marker"), sep = "\\(") %>%
      mutate(cell = str_replace_all(cell, gates),
             marker = str_replace_all(marker, marker_pattern)) %>%
      mutate(marker = replace_na(marker, "freq")) %>%
      mutate_all(trimws) %>%
      mutate(
        genotype = factor(genotype, levels = c("WT", "KO"), ordered = TRUE),
        value = as.numeric(value),
        organ = as.factor(organ),
        mice = as.factor(mice),
        cell = as.factor(cell),
        stat = as.factor(stat),
        marker = as.factor(marker)
      )
    return(x)
  }

