save_data <- function(table, time.i = "0h", experiment.i = "22.00"){
  table <- table %>%
    mutate(treatment = organ,
           time = time.i) %>%
    select(-organ) %>%
    experiment = experiment.i
}
)

Time, mice, genotype, treatment, marker, stat, value, experiment, cell
