make_sl_table <- function(sl_lrnr_fit,
                          file_out,
                          loss_fun = sl3::loss_squared_error,
                          format = c("latex", "markdown")) {
  ###############################################################
  # sl_lrnr_fit : A trained sl3 Lrnr_sl object.
  # file_out    : The name of the file to which to write.
  # loss_fun    : A loss function used to evaluate the CV-risk.
  #               Default is sl3::loss_squared_error
  # format      : A file type indicating the format of the table.
  #               Default is LaTeX.
  ###############################################################

  # catch formatting
  format <- match.arg(format)
  format_ext <- dplyr::case_when(format == "latex" ~ ".tex",
                                 format == "markdown" ~ ".md")

  # create CV-risk output
  sl_lrnr_risks <- sl_lrnr_fit$cv_risk(loss_fun)

  # open file for writing table
  sink(here::here("tables", paste0(file_out, format_ext)))
  sl_lrnr_risks[, c(1, 2, 6, 3, 7)] %>%
    knitr::kable(format = format,
                 booktabs = TRUE,
                 label = "sl_coefs_risks",
                 col.names = c("Learner", "Weight", "Min. Fold Risk",
                               "Mean CV-Risk", "Max. Fold Risk"),
                 caption = "Estimated CV-risk of learning algorithms") %>%
    kableExtra::kable_styling() %>%
    print()
  sink()
}
