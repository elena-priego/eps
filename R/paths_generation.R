paths_generation <- function(experiment_name){
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
  path_data <- here::here("data", experiment_name)
  path_doc <- here::here("doc")
  path_raw <- here::here("raw", experiment_name)
  path_result <- here::here("result", experiment_name)
  path_src <- here::here("src")
}
