## Stream-alignment fingerprint. After N draws the MT state is a pure function
## of the NUMBER of uniforms consumed, so comparing .Random.seed across OSes
## detects a desync while ignoring ULP differences in the values themselves.
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
fp <- function() {
  s <- as.numeric(.Random.seed[-1])
  sprintf("mti=%d|%015.0f", .Random.seed[2], sum((s %% 65536) * seq_along(s)) %% 2^40)
}
N <- as.numeric(Sys.getenv("N", "1e7"))
chunk <- N/10
draw <- list(
  rbeta_p  = function(n) rbeta(n, 4.0, 76.0),
  rbeta_q  = function(n) rbeta(n, 99.9, 11.1),
  rbeta_u  = function(n) rbeta(n, 99.0, 5841.0),
  rpois    = function(n) rpois(n, 100),
  rgeom    = function(n) rgeom(n, 0.5),
  rmultinom= function(n) rmultinom(n, 1, rep(1/4, 4)),
  rnorm    = function(n) rnorm(n, 9.5, 3)
)
cat("OS:", Sys.info()[["sysname"]], "|", R.version.string, "| N =", format(N, scientific=TRUE), "\n")
for (nm in names(draw)) {
  set.seed(1)
  for (i in 1:10) invisible(draw[[nm]](chunk))
  cat(sprintf("%-10s %s\n", nm, fp()))
}
