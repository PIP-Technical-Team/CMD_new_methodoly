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
get_cmd_welfare <- function(country_code, reporting_year, CF, qs, py = 2021) {

  cf <- CF[code == country_code & year == reporting_year]
  stopifnot(nrow(cf) == 1)
  welfare <- if (nrow(cf) == 1) {
    if (is.na(cf$t1_comp1)) {
      lny <- cf$t2_comp1 + cf$t2_qf * qs
      exp(lny)
    } else {
      lny <- cf$t1_comp1 + cf$t1_qf * qs
      exp(lny)
    }
  } else {
    NULL
  }

  # bottom censoring
  if (py == 2021) {
    bc <- 0.28

  } else if (py == 2017) {
    bc <- .25
  } else if (py == 2011) {
    bc <- .22
  } else {
    bc <- 0
  }

  # Bottom censoring
  welfare[welfare <= bc] <- bc

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
list_cmd_welfare <- function(md, CF, qs, py) {

  l_cmd <- vector("list", length = nrow(md))

  for (i in seq_len(nrow(md))) {
    country_code   <- md$country_code[i]
    reporting_year <- md$year[i]
    weight         <- md$reporting_pop[i]/length(qs)

    welfare <- get_cmd_welfare(country_code,
                               reporting_year,
                               CF,
                               qs,
                               py = py)

    # Add attributes
    dt <- data.table(welfare         = welfare,
                     weight          = weight,
                     reporting_level = "national",
                     country_code    = country_code,
                     reporting_year  = reporting_year)

    dt <- add_cmd_attributes(dt,
                             country_code,
                             reporting_year)


    # Add to list
    if (is.null(welfare)) {
      l_cmd[[i]] <- NULL
    } else {
      l_cmd[[i]] <- dt
    }
  }
  names(l_cmd) <- md[, id]
  l_cmd
}


#' Add important attributes to CMD data frame
#'
#' @param dt data frame: individual distribution from one country-year
#' @param country_code character: character vector of length 1 giving country's iso3c code
#' @param reporting_year numeric: vector of length 1 giving reporting year
#'
#' @return data frame: same as input but with added attributes
#' @export
add_cmd_attributes <- function(dt, country_code, reporting_year) {

  # reporting year
  attr(dt,
       "reporting_year") <- reporting_year

  # country_code
  attr(dt,
       "country_code") <- country_code

  # Reporting level
  attr(dt,
       "reporting_level_rows") <-
    list(reporting_level = "national",
         rows            = nrow(dt))

  # dist stats
  attr(dt,
       "dist_stats") <- get_dist_stats(dt)

  # lineup approach
  attr(dt,
       "lineup_approach") <- "missing_data"

  dt

}








#' Write all CMD distributions as qs files, including attributes
#'
#' @param l_cmd list of data frames of CMD distributions, output from [list_cmd_welfare]
#' @param path location of where to write these files
#'
#' @return logical: invisible
#' @export
write_cmd_dist <- function(l_cmd, path) {

    # all country years
    country_years <- names(l_cmd)

    lapply(cli::cli_progress_along(country_years,
                                   total = length(country_years)),

           FUN = \(i) {

             x  <- l_cmd[[i]]
             cn <- country_years[i]

             qs::qsave(x    = x,
                       file = fs::path(path,
                                       paste0(cn,
                                              ".qs")))
           })
    invisible(TRUE)
  }



#' Load coefficients from github
#'
#' @return list: two data frames with coeffs
#' @export
load_coeff <- function() {

  gh_user   <- "https://raw.githubusercontent.com"
  org_data  <- paste(gh_user,
                     "PIP-Technical-Team",
                     "aux_missing_countries",
                     "qs_file",
                     "04-outputdata/cmd_coeff.qs",
                     sep = "/")

  temp_file <- tempfile(fileext = fs::path_ext("cmd_coeff.qs"))
  req <- httr::GET(org_data,
                   # write result to disk
                   httr::write_disk(path = temp_file))

  qs::qread(temp_file)

}



#' Get qs inputs by giving the number of quantiles
#'
#' @param n: number of quantiles
#'
#' @return
#' @export
calc_quantiles <- function(n = 1000) {
  quantiles <- seq(1, n, 1)/n - 5/(n*10)
  qs        <- log(quantiles/(1-quantiles))
  qs
}









