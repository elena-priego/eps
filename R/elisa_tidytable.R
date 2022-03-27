#' Make tidytable from ELISA
#' Function to calculate the concentrations of ELISA experiments giving two files as input, one coming from the spectrometer with the raw OD data and other with a template of the plate used. In the template is expected to be at least 1 point of blank_0 that will be subtracted to all values and two or more values from a standard curve labelled as standard_0, standard_10 ... For samples well the format is name_concentration as proportion of samples (for example a sample diluted 1/2 would be 0.5)
#'
#'
#' @param data_file txt file with the OD data.
#' @param template_file csv file with the template of the experiment. Format will be sample-type_concentration with standard and blank as required keyword
#' @param path_file path where data_file and template_file are located. Usually path_raw
#' @param sep_data separator character from data_file. Frequently tab
#' @param sep_template separator character from template_file. Frequently sep
#' @param model model to fit the analysis: either linear ("lm") or 4-parameter log-logistic ("log-log")
#' @param max_value to restrict the standard curve to a maximal value
#' @param plot_curve print the fitted plot
#'
#' @return
#' @export
#'
#' @examples
#' elisa_tidytable(
#'   data_file = "elisa-IFNg.txt",
#'   template_file = "IFNg-plate.csv",
#'   path_file = path_raw,
#'   model = "lm",
#'   max_value = 50,
#'   plot_curve = FALSE
#' )
#'
elisa_tidytable <- function(data_file = "elisa.txt",
                            template_file = "plate.csv",
                            path_file = path_raw,
                            sep_data = "\t",
                            sep_template = ",",
                            model = "log-log",
                            max_value = Inf,
                            plot_curve = TRUE) {
  path_data <- here::here(path_file, data_file)
  path_template <- here::here(path_file, template_file)

  table_data <-
    read.delim(file = path_data,
               sep = sep_data,
               header = FALSE)
  template <-
    read.delim(file = path_template,
               sep = sep_template,
               header = FALSE)
  table <-
    data.frame(
      sample = unlist(template),
      data = unlist(table_data),
      row.names = NULL
    ) %>%
    drop_na() %>%
    filter(sample != "") %>%
    separate(sample, c("sample", "concentration"), "_") %>%
    mutate(concentration = as.numeric(concentration))

  blank <- mean(table[table$sample == "blank", "data"])
  table_std <- filter(table, table$sample == "standard") %>%
    mutate(data = data - blank) %>%
    mutate(logcon = log10(concentration))

  table_data <-
    filter(table, !table$sample %in% c("standard", "blank")) %>%
    mutate(data = data - blank)
  if (model == "log-log") {
    fit <- drc::drm(
      formula = data ~ logcon,
      data = table_std,
      subset = (concentration < max_value),
      fct = drc::LL.4()
    )
    if (plot_curve == TRUE) {
      plot(
        table_std$logcon,
        table_std$data,
        main = "log standard curve",
        xlab = "x=log(conc)",
        ylab = "y=OD"
      )
      x <-
        seq(min(table_std$logcon), max(table_std$logcon), length = 100)
      y <- fit$coefficients[2] +
        (fit$coefficients[3] - fit$coefficients[2]) /
        (1 + (x / fit$coefficients[4]) ^ fit$coefficients[1])
      lines(x, y, lty = "dotted", col = "red")
    }

    table_data$fitted <-
      fit$coefficients[4] *
      (((-1 * fit$coefficients[3] + table_data$data) /
          (fit$coefficients[2] - table_data$data)
      ) ^
        (1 / fit$coefficients[1]))
    table_data <- table_data %>%
      mutate_all( ~ replace(., is.nan(.), NA)) %>%
      mutate(value = (10 ^ fitted) / concentration) %>%
      select(-fitted)
  }
  else if (model == "lm") {
    fit <- lm(
      formula = data ~ concentration,
      data = table_std,
      subset = (concentration < max_value)
    )
    if (plot_curve) {
      fit_plot <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
        geom_point() +
        stat_smooth(method = "lm", col = "red") +
        labs(
          caption = paste(
            "Adj R2 = ",
            signif(summary(fit)$adj.r.squared, 4),
            "Intercept =",
            signif(fit$coef[[1]], 5),
            " Slope =",
            signif(fit$coef[[2]], 5),
            " P =",
            signif(summary(fit)$coef[2, 4], 4)
          )
        )
      print(fit_plot)
    }
    table_data$fitted <-
      (table_data$data - fit$coef[[1]]) /
      fit$coef[[2]]

    table_data <- table_data %>%
      mutate_all( ~ replace(., is.nan(.), NA)) %>%
      mutate(value = fitted / concentration) %>%
      select(-fitted)

    table_data[table_data$value < 0, "value"] <- NA
  }


  return(table_data)
}

