
benchmark_mean <- function(d) {
  microbenchmark::microbenchmark(
    sapply(d$probes, \(.x) {colMeans(d$data[.x,])}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,mean)}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,libaffyperftest::mean)}, simplify = FALSE),
    sapply(d$probes, \(.x) {libaffyperftest::col_means(d$data[.x,])}, simplify = FALSE),
    sapply(d$probes, \(.x) {matrixStats::colMeans2(d$data[.x,])}, simplify = FALSE),
    purrr::map(d$probes, \(.x) {colMeans(d$data[.x,])})
  )
}


benchmark_median <- function(d) {
  microbenchmark::microbenchmark(
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,median)}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,libaffyperftest::median)}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,naive_median1)}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,naive_median2)}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,naive_median3)}, simplify = FALSE),
    sapply(d$probes, \(.x) {libaffyperftest::col_medians(d$data[.x,])}, simplify = FALSE),
    sapply(d$probes, \(.x) {matrixStats::colMedians(d$data[.x,])}, simplify = FALSE),
    purrr::map(d$probes, \(.x) {matrixStats::colMedians(d$data[.x,])})
  )
}

benchmark_tukey <- function(d) {
  microbenchmark::microbenchmark(
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,affy::tukey.biweight)}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,libaffyperftest::tukey_biweight)}, simplify = FALSE),
    sapply(d$probes, \(.x) {apply(d$data[.x,],2,matrixStats_tukeybw_vector)}, simplify = FALSE),
    sapply(d$probes, \(.x) {affy::tukeybiweight(d$data[.x,])}, simplify = FALSE),
    sapply(d$probes, \(.x) {matrixStats_tukeybw_matrix(d$data[.x,])}, simplify = FALSE),
    sapply(d$probes, \(.x) {libaffyperftest::col_tukey_biweights(d$data[.x,])}, simplify=FALSE),
    purrr::map(d$probes, \(.x) {matrixStats_tukeybw_matrix(d$data[.x,])}),
    times = 10
  )
}
