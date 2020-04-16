make_sl_table <- function(sl_lrnr_fit,
                          file_out,
                          loss_fun = sl3::loss_squared_error,
                          format_out = c("latex", "markdown")) {
  #####################################################################
  # sl_lrnr_fit  :   A trained Super Learner (sl3::Lrnr_sl) object.
  # file_out     :   The name of the file to which to write.
  # loss_fun     :   A loss function used in evaluating the CV-risk.
  #                  Default is sl3::loss_squared_error.
  # format_out   :   A file type indicating the format of the table.
  #                  Default is LaTeX.
  #####################################################################

  # catch formatting
  format_out <- match.arg(format_out)
  format_ext <- dplyr::case_when(format_out == "latex" ~ ".tex",
                                 format_out == "markdown" ~ ".md")

  # create CV-risk output
  sl_lrnr_risks <- sl_lrnr_fit$cv_risk(loss_fun)

  # check metalearner fits and collapse coefficients across regions
  coefs <- sapply(sl_lrnr_fit$fit_object$cv_meta_fit$fit_object, stats::coef)
  coefs_collapsed <- as.numeric(apply(coefs, 1, mean))
  sl_lrnr_risks[, coefficients := c(coefs_collapsed, sum(coefs_collapsed))]

  # open file for writing table
  sink(here::here("tables", paste0(file_out, format_ext)))
  sl_lrnr_risks[, c(1, 2, 6, 3, 7)] %>%
    knitr::kable(format = format_out,
                 booktabs = TRUE,
                 label = "sl_coefs_risks",
                 col.names = c("Learner", "Weight", "Min. Fold Risk",
                               "CV-Mean Risk", "Max. Fold Risk"),
                 caption = "Estimated CV-risk of learning algorithms") %>%
    kableExtra::kable_styling() %>%
    print()
  sink()
}


