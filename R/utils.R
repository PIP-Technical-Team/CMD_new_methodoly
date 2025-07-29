# country estimates by country -------
#' Calculate welfare distribution for a country-year
#'
#' This function computes the welfare distribution for a given country and
#' reporting year using the coefficients data.table and quantile vector.
#'
#' @param country_code Character. Country code matching the 'code' column in CF.
#' @param reporting_year Integer or character. Year matching the 'year' column in CF.
#' @param CF data.table. Coefficient table containing country-year rows and required columns (see details).
#' @param qs Numeric vector. Vector of quantiles (logit transformed).
#'
#' @details
#' CF must contain columns: code, year, t1_comp1, t1_qf, t2_comp1, t2_qf, tier1_sme, tier2_sme.
#' The function filters CF for the given country_code and reporting_year, then calculates welfare
#' using either Tier 1 or Tier 2 formula depending on data availability.
#'
#' @return Numeric vector of welfare values, or NULL if no valid row found.
#' @export
#'
#' @examples
#' get_cmd_welfare("ETH", 2021, CF, qs)
get_cmd_welfare <- function(country_code, reporting_year, CF, qs) {

  cf <- CF[code == country_code & year == reporting_year]
  stopifnot(nrow(cf) == 1)
  welfare <- if (nrow(cf) == 1) {
    if (is.na(cf$t1_comp1)) {
      # Tier 2
      lny <- cf$t2_comp1 + cf$t2_qf * qs
      exp(lny) * cf$tier2_sme
    } else {
      # Tier 1
      lny <- cf$t1_comp1 + cf$t1_qf * qs
      exp(lny) * cf$tier1_sme
    }
  } else {
    NULL
  }
  welfare
}



# Get ALL country estimates -------
#' Calculate welfare distributions for all country-years in a data.table
#'
#' This function loops over all rows in md, executing get_cmd_welfare() for each
#' country-year, and returns a named list of results.
#'
#' @param md data.table. Must contain columns: country_code, year, reporting_pop, id.
#' Each row is a country-year to process.
#' @param CF data.table. Coefficient table as required by get_cmd_welfare().
#' @param qs Numeric vector. Vector of quantiles (logit transformed).
#'
#' @details
#' For each row in md, the function calls get_cmd_welfare() and constructs a
#' data.table with welfare, weight, and area if successful, or NULL otherwise.
#' The output list is named by md$id.
#'
#' @return Named list of data.tables (or NULLs) for each country-year in md.
#' @export
#'
#' @examples
#' list_cmd_welfare(md, CF, qs)
list_cmd_welfare <- function(md, CF, qs) {
  l_cmd <- vector("list", length = nrow(md))
  for (i in seq_len(nrow(md))) {
    country_code   <- md$country_code[i]
    reporting_year <- md$year[i]
    weight         <- md$reporting_pop[i]/length(qs)

    welfare <- get_cmd_welfare(country_code, reporting_year, CF, qs)

    if (is.null(welfare)) {
      l_cmd[[i]] <- NULL
    } else {
      l_cmd[[i]] <- data.table(welfare = welfare,
                               weight  = weight,
                               area    = "national")
    }
  }
  names(l_cmd) <- md[, id]
  l_cmd
}
