shannon_nplot <- function(x, ba, n.plot=rep(1, length(x))) {
  p_df <- data.frame(x, ba, n.plot) %>%
    group_by(x) %>%
    summarise(sum_ba = sum(ba * n.plot)) %>%
    mutate(p_n = sum_ba / sum(sum_ba))
  shannon <- - sum ( p_df$p_n * log(p_df$p_n) )
  return(shannon)
}

qmean_nplot <- function(d, n.plot=rep(1, length(d))) {
  # quadratic mean, accounting for n.plot
  q_mu2 <- mean_nplot(d^2, n.plot)
  q_mu <- sqrt(q_mu2)
  return(q_mu)
}

SDI <- function(stems_per_ha, q_mean_mm) {
  # stand density index: SDI = N * (Dq/10)^k
  # N = number of trees per acre, Dq = q.mean DBH in inches, k = 1.605
  # Equation taken from https://www.srs.fs.usda.gov/pubs/gtr/gtr_srs175/gtr_srs175_577.pdf
  N <- stems_per_ha * 2.47105 # convert from ha to acre
  Dq <- q_mean_mm / 25.4 # mm to inch
  k <- 1.605
  sdi_out <- N * (Dq/10)^k
  return(sdi_out)
}


indices <- tree %>%
  filter(census.n == 2 & tree.status == 0) %>%
  group_by(tmt.census.id) %>%
  summarise(shannon = shannon_nplot(x = species.cor, ba = ba),
            q_mean_mm = qmean_nplot(d),
            stems_ha = sum(n.ha)) %>%
  mutate(SDI = SDI(stems_ha, q_mean_mm))
