#' Load pop aux data and prep to check vs sum of weights
#'
#' @param aux_dir directory of aux data for [pop.fst]
#'
#' @return
#' @export
get_pop_to_scale <- function(aux_dir) {

  # load aux pop file
  pop <-
    fst::read_fst(path = fs::path(aux_dir,
                                  "pop.fst")) |>
    pivot(ids = c("country_code",
                  "data_level")) |>
    frename(reporting_year = variable,
            reporting_pop  = value) |>
    qDT() |>
    fmutate(reporting_year = as.numeric(as.character(reporting_year)))

  pop

}


#' Scale weights of CMD dist to reflect population
#'
#' Check the sum of weights for each country-year
#' to ensure it is the same as the populations in aux data.
#' If not, scale weight vector to reflect correct pop
#'
#' @param l_cmd list of CMD distributions
#' @param pop aux data [pop.fst]
#'
#' @return
#' @export
scale_weights <- function(l_cmd,
                          pop) {

  # STEP 1: Extract weight sums from l_cmd
  #--------------------------------------------
  # Convert list of data.tables to one data.table with source info
  dt_weights <- rbindlist(l_cmd,
                          idcol = "id")

  # Sum weights by country_code and reporting_year
  weights_summary <- dt_weights[,
                                .(weight_sum = sum(weight)),
                                by = .(country_code,
                                       reporting_year,
                                       reporting_level)]

  # STEP 2: Filter relevant population info
  #--------------------------------------------
  # We only want population data that matches the level (national, rural, urban)
  pop_filtered <- pop[,
                      .(country_code,
                        reporting_year,
                        data_level,
                        reporting_pop)]

  # Join weights with population
  setnames(pop_filtered,
           "data_level",
           "reporting_level")  # for join
  wt_check <- joyn::joyn(weights_summary,
                         pop_filtered,
                         by         = c("country_code",
                                        "reporting_year",
                                        "reporting_level"),
                         match_type = "1:1",
                         keep       = "left")

  # Check ratio between weight sum and pop
  wt_check[, scaling_factor := reporting_pop / weight_sum]

  # View which country-years need rescaling
  wt_check[abs(scaling_factor - 1) > 1e-6]


  # STEP 3
  #--------------------------------------------
  # Create a lookup table for scaling factors
  scaling_lookup <- wt_check[,
                             .(country_code,
                               reporting_year,
                               reporting_level,
                               scaling_factor)]

  # Convert to character keys for easy matching
  scaling_lookup[,
                 key := paste(country_code,
                              reporting_year,
                              sep = "_")]
  which_scale <- which(names(l_cmd) %in% scaling_lookup[!round(scaling_factor, 5) == 1,]$key)
  which_scale <- names(l_cmd)[which_scale]

  # Add attributes
  l_cmd <-
    lapply(l_cmd,
           function(x) {
             cntry <- x$country_code |>
               funique()
             yr    <- x$reporting_year |>
               funique()
             attr(x, which = "key") <- paste(cntry, yr, sep = "_")

             x
           })

  # Apply scaling
  l_cmd_scaled <-
    lapply(l_cmd,
           function(x){

             if (attr(x, "key") %in% which_scale) {

               x <-
                 joyn::joyn(x  = x,
                            y  = scaling_lookup,
                            by = c("country_code",
                                   "reporting_year",
                                   "reporting_level"),
                            match_type = "m:1",
                            reportvar  = FALSE,
                            keep       = "left",
                            verbose    = FALSE) |>
                 fmutate(weight = weight*scaling_factor)

               gv(x,
                  c("scaling_factor",
                    "key")) <- NULL

             }


             x

           })


  l_cmd_scaled

}



