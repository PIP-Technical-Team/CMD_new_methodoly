library(fastverse)


# Prep coefficients data ---------
fs::path("data/cmd_coeff.Rda") |>
  load()

CF <- as.data.table(data)

CF[, `:=`(
  # Tier 1 component 1
  t1_comp1 = tier1_int + tier1_gdp * log(gdp) + tier1_u5m *
    log(u5m) + tier1_rps * rps + tier1_leb * leb + tier1_inc * inc + tier1_incgdp *
    inc * log(gdp),

  # Tier 1 quantile factor
  t1_qf = tier1_pct + tier1_eca*eca+tier1_lac*lac+tier1_ssa*ssa,

  # Tier 2 component 1
  t2_comp1 = tier2_int + tier2_u5m * log(u5m) + tier2_rps * rps + tier2_leb *
    leb + tier2_lmc * lmc + tier2_umc * umc + tier2_hic * hic,

  # Tier 2 quantile factor
  t2_qf = tier2_pct + tier2_eca*eca+tier2_lac*lac+tier2_ssa*ssa
)]


# Quantiles
n <- 1000
quantiles = seq(1,n,1)/n - 0.0005
qs <- log(quantiles/(1-quantiles))


# country estimates -------
country_code = "ETH"
reporting_year = 2021

cf <- CF[code == country_code & year == reporting_year]

stopifnot(nrow(cf) == 1)

welfare <-
  if(is.na(cf$t1_comp1)) {
    # Tier 2
    lny <- cf$t2_comp1 + cf$t2_qf*qs
    exp(lny)*cf$tier2_sme
  } else {
    # Tier 1
    lny <- cf$t1_comp1 + cf$t1_qf*qs
    exp(lny)*cf$tier1_sme
  }



