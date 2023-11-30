# Consider using ready made functions e.g. mase::horvitzThompson()


mean_nplot <- function(d, n.plot) {
  mu <- sum(d*n.plot)/sum(n.plot)
  return(mu)
}

qmean_nplot <- function(d, n.plot) {
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

stdev_nplot <- function(x, n.plot) {
  diff_to_mean <- (x-mean(x))*n.plot
  n <- sum(n.plot)
  sigma_sq <- sum( (diff_to_mean)^2 ) /(n-1)
  sigma <- sqrt(sigma_sq)
  return(sigma)
}

shannon_nplot <- function(x, n.plot) {
  p_df <- data.frame(x, n.plot) %>%
    group_by(x) %>%
    summarise(sum_n = sum(n.plot)) %>%
    mutate(p_n = sum_n / sum(sum_n))
  shannon <- - sum ( p_df$p_n * log(p_df$p_n) )
  return(shannon)
}

shannon_p <- function(p) {
  shannon <- - sum ( p * log(p) )
  return(shannon)
}