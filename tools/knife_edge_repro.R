## Cross-OS reproducer for the `while (any(s < 30))` loop in claim_size().
## Crafts a Mersenne-Twister state whose first rnorm draw lands 2.5e-15 from
## the rejection threshold, so macOS rejects it and Linux/Windows accept it.
## The loop then consumes a different number of uniforms and the whole
## downstream stream desyncs.
suppressWarnings(library(SynthETIC))

M32 <- 2^32
shl <- function(x, n) (x * 2^n) %% M32
shr <- function(x, n) floor(x / 2^n)
bit2 <- function(a, b, f) {
  f2 <- function(x, y) as.numeric(f(as.integer(x), as.integer(y)))
  f2(floor(a/65536), floor(b/65536))*65536 + f2(a %% 65536, b %% 65536)
}
uxor <- function(a, b) bit2(a, b, bitwXor)
uand <- function(a, b) bit2(a, b, bitwAnd)
untemper <- function(y) {
  y <- uxor(y, shr(y, 18))
  y <- uxor(y, uand(shl(y, 15), 0xefc60000))
  t <- y; for (i in 1:5) t <- uxor(y, uand(shl(t, 7), 0x9d2c5680)); y <- t
  t <- y; for (i in 1:3) t <- uxor(y, shr(t, 11)); t
}
as_seed_int <- function(u) as.integer(ifelse(u >= 2^31, u - M32, u))

BIG <- 134217728; m <- 813546; k <- 1868573657
knife_state <- function() {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(1)
  s <- .Random.seed; s[2] <- 1L
  s[4] <- as_seed_int(untemper(m * 32)); s[5] <- as_seed_int(untemper(k))
  s
}

cat("OS:", Sys.info()[["sysname"]], "|", R.version.string,
    "| SynthETIC", as.character(packageVersion("SynthETIC")), "\n")

assign(".Random.seed", knife_state(), envir = .GlobalEnv)
x <- rnorm(1, mean = 9.5, sd = 3)
cat(sprintf("first deviate^5 = %a  -> %s\n", x^5, ifelse(x^5 < 30, "REJECT", "accept")))

set.seed(99)
freq <- claim_frequency(I = 4, simfun = stats::rpois, lambda = 3)
assign(".Random.seed", knife_state(), envir = .GlobalEnv)
cs <- unlist(claim_size(freq))
cat(sprintf("claim_sizes[1:3] = %s\n", paste(sprintf("%a", cs[1:3]), collapse = " ")))
cat(sprintf("sum = %a\nuniforms consumed = %d\n", sum(cs), .Random.seed[2]))
