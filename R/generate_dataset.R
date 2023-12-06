generate_dataset <- function(n=100, p = 1000000, np = 50000) {
  d <- matrix(data = 1000 * rexp(n*p), ncol=n, nrow=p)
  probes <- sapply(1:np,\(.x) {as.integer(runif(n=15,min=1,max=p))},simplify=FALSE)
  probes <- setNames(
    probes,
    nm = sprintf("%05d_at", as.integer(runif(n=np, min=10000,max =50000)))
  )
  list(data = d, probes = probes)
}