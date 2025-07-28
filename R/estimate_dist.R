library(fastverse)


# Prep coefficients data ---------

## main parameters ----------
release <- "20250401_2021_01_02_PROD"
aux_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
  fs::path(release, "_aux")



## clean coeff data -----------
fs::path("data/cmd_coeff.Rda") |>
  load()

CF <- as.data.table(data)

CF[, `:=`(
  # Tier 1 component 1
  t1_comp1 = tier1_int +
    (tier1_gdp * log(gdp)) +
    (tier1_u5m * log(u5m)) +
    (tier1_rps * rps) +
    (tier1_leb * leb) +
    (tier1_inc * inc) +
    (tier1_incgdp * inc * log(gdp)),

  # Tier 1 quantile factor
  t1_qf = tier1_pct + (tier1_eca*eca+tier1_lac*lac+tier1_ssa*ssa),

  # Tier 2 component 1
  t2_comp1 = tier2_int +
    (tier2_u5m * log(u5m)) +
    (tier2_rps * rps) +
    (tier2_leb *leb) +
    (tier2_lmc * lmc + tier2_umc * umc) +
    (tier2_hic * hic),

  # Tier 2 quantile factor
  t2_qf = tier2_pct + (tier2_eca*eca+tier2_lac*lac+tier2_ssa*ssa)
  )
  ]

# Quantiles
n <- 1000
quantiles = seq(1,n,1)/n - 0.0005
qs <- log(quantiles/(1-quantiles))


## MD countries

md <- fs::path(aux_dir, "missing_data.qs") |>
  qs::qread() |>
  ftransform(id = paste(country_code, year, sep = "_"))

# country estimates -------

get_cmd_welfare <- function(country_code, reporting_year, CF, qs) {

  cf <- CF[code == country_code & year == reporting_year]
  stopifnot(nrow(cf) == 1)
  welfare <- if (nrow(cf) == 1) {
    if (is.na(cf$t1_comp1)) {
      lny <- cf$t2_comp1 + cf$t2_qf * qs
      exp(lny) * cf$tier2_sme
    } else {
      lny <- cf$t1_comp1 + cf$t1_qf * qs
      exp(lny) * cf$tier1_sme
    }
  } else {
    NULL
  }
  welfare
}


# country estimates -------
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

# Remove NULL elements from l_cmd

# Split l_cmd into two lists: non-NULL and NULL elements
l_cmd_ok   <- l_cmd[!sapply(l_cmd, is.null)]
l_cmd_fail <- l_cmd[sapply(l_cmd, is.null)]
names(l_cmd_fail)


