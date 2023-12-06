abatch_tukey <- function(x) {
  probesets <- p(x)
  res <- ptukey(x, .by = probesets)
}

ptukey <- function(x, .by) {
  groups <- vctrs::vec_group_loc(.by)$loc
  purrr::map_dbl(groups, \(.x) {tukey_biweight(x[.x])})
  
  #purrr::map(groups, \(.x) {tukey_biweight(x, .x)})
  
}

pfunc <- function(x, .by) {
  groups <- vctrs::vec_group_loc(.by)$loc
  purrr::map_dbl
}

#
# NB: These are matrixStats implementations. Near as I can tell, about
# as optimized as I can make them (for R). To compare against C/C++
# based implementations. Tukey may not be the best choice for comparisons,
# but this is what I'm doing (currently).
matrixStats_tukeybw_vector <- function(x, c = 5, epsilon = 1e-04) {
  dim. <- c(length(x), 1)
  m <- matrixStats::colMedians(x, dim.=dim.)
  # NB: The constant scaling factor is not used
  s <- matrixStats::colMads(x, dim.=dim., constant=1)
  u <- (x - m) / (c * s + epsilon)
  w <- (1-u^2)^2
  w[abs(u) > 1] <- 0
  
  # With a single vector, it's quicker to just do sum.
  t.bi <- sum(w*x)/ sum(w)

  t.bi
}

matrixStats_tukeybw_matrix <- function(x, c = 5, epsilon = 1e-04) {
  dim. <- dim(x)
  
  # Medians and MADs first
  m <- matrixStats::colMedians(x, dim.=dim.)
  # NB: The constant scaling factor is not used
  s <- matrixStats::colMads(x, dim.=dim., constant=1)

  # x - median / c*s+epsilon. These are t_tx_OP_y() calls since they
  # are applied rowwise.
  u <- matrixStats::t_tx_OP_y(
    matrixStats::t_tx_OP_y(x, m, OP = "-"),
    (c * s + epsilon), 
    OP="/"
  )

  w <- (1-u^2)^2
  w[abs(u) > 1] <- 0

  t.bi <- matrixStats::colSums2(w * x, dim.=dim.)/matrixStats::colSums2(w, dim.=dim.)

  t.bi
}
matrixStats_tukeybw <- function(x, c = 5, epsilon = 1e-04) {
  # To use a vector, the dimensions are set explicitly. So set them
  # regardless.
  if ( is.vector(x) ) {
    return(matrixStats_tukeybw_vector(x,c,epsilon));
  }
  return(matrixStats_tukeybw_matrix(x,c,epsilon));
  
}

# z <- sapply(1:20, \(.x) {rnorm(n=10000)})
# 