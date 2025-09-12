#' Estimate and save country by country
#'
#' First load pop from aux, then go through each country and
#' estimate CMD, scale pops, and save
#'
#' The reason this is used, is that when number of quantiles is larges
#' (e.g. 100k) then the list becomes too large to keep fully in memory
#'
#' @param md missing data countries
#' @param CF coefficients
#' @param qs quantiles
#' @param py ppp year
#' @param dir directory for lineups
#'
#' @return
#' @export
estimate_and_write_full_cmd <- function(md,
                                        CF,
                                        qs,
                                        py,
                                        dir,
                                        env_acc = NULL) {

  aux_dir <- fs::path(dir,
                      "_aux")
  print(aux_dir)

  lineup_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path(release,
             "lineup_data")
  save_dir <- lineup_dir |>
    fs::path("CMD")

  # Load pop data
  #--------------------------
  pop     <- get_pop_to_scale(aux_dir = aux_dir)

  # Loop country estimate & save
  #--------------------------
  countries <- md$country_code |>
    funique()
  for (x in seq_along(countries)) {
    print(x)
    cn <- countries[x]
    print(cn)
    l_cmd <- list_cmd_welfare(md |>
                                fsubset(country_code == cn),
                              CF,
                              qs,
                              py = py,
                              env_acc = env_acc)

    l_cmd <- scale_weights(l_cmd = l_cmd,
                           pop   = pop)

    l_cmd <- select_and_order(l_cmd)

    # write
    write_cmd_dist(l_cmd,
                   path = save_dir)


  }

  return(invisible(TRUE))

}

#' Select and order columns in a list of data.tables
#'
#' Selects `welfare`, `weight`, and `reporting_level` columns from each data.table
#' in the list and orders rows by `welfare`.
#'
#' @param l_cmd A list of `data.table` objects.
#' @return A list of modified `data.table`s with selected and ordered columns.
#' @export
select_and_order <- function(l_cmd) {
  lapply(l_cmd, function(dt) {
    dt <- dt[, .(welfare, weight, reporting_level)]

    dt <- get_csum_dist(dt)

    dt
  })
}


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
#' @param env_acc environment: where dist stats will be accumulated before
#' appending them and saving them
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
list_cmd_welfare <- function(md, CF, qs, py, env_acc = NULL) {

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
                     country_code    = country_code,
                     reporting_year  = reporting_year,
                     reporting_level = "national")

    dt <- add_cmd_attributes(dt,
                             country_code,
                             reporting_year,
                             env_acc = env_acc)


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
add_cmd_attributes <- function(dt, country_code, reporting_year, env_acc = NULL) {

  # reporting year
  attr(dt,
       "reporting_year") <- reporting_year

  # country_code
  attr(dt,
       "country_code") <- country_code

  # dist stats
  dist <- get_dist_stats(dt)
  attr(dt,
       "dist_stats") <- dist

  # Save dist_stats to env_acc if provided
  if (!is.null(env_acc)) {
    key <- paste(country_code, reporting_year, sep = "_")
    rlang::env_poke(env   = env_acc,
                    nm    = key,
                    value = dist$dt_dist)
  }

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

             fst::write_fst(x    = x,
                            path = fs::path(path,
                                            paste0(cn,
                                                   ".fst")))
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





get_csum_dist <- function(qq) {

  first_rows <- data.table(reporting_level = funique(qq$reporting_level)) |>
    fmutate(welfare = 0,
            weight  = 0)
  qq <- rowbind(qq,
                first_rows) |>
    setorder(reporting_level, welfare)

  qq[, `:=`(
    cw     = weight,
    cwy    = weight*welfare,
    cwy2   = weight*welfare*welfare,
    cwylog = log(pmax(welfare, 1e-10))*weight  # for the watts
  )]


  # fix bug
  g <- GRP(qq, ~ reporting_level,
           sort = FALSE # I still don't understand this argument. I think it could
           # be FALSE because the data is already sorted, but I am not sure.
  )

  csum <-  add_vars(get_vars(qq,
                             c("reporting_level",
                               "welfare",
                               "weight")),
                    get_vars(qq,
                             c("cw",
                               "cwy",
                               "cwy2",
                               "cwylog")) |>
                      fcumsum(g)) # Here you do fcumsum by reporting_level

  csum[, index := as.integer(rowid(reporting_level) - 1)]

  csum

}







