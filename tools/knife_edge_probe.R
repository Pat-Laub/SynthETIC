## Knife-edge probe: does qnorm differ across OSes at the draws that straddle
## the s < 30 rejection threshold in claim_size()'s default sampler?
BIG <- 134217728; M32 <- 2^32
xstar <- 30^(1/5); m <- floor(pnorm((xstar - 9.5)/3) * BIG)

kc <- round((pnorm((xstar - 9.5)/3)*BIG - m) * M32)
ks <- (kc - 400):(kc + 400)
u  <- (m + ks*2^-32)/BIG
s  <- (9.5 + 3*qnorm(u))^5
sel <- which(abs(s - 30)/30 < 1e-13)

cat("OS:", Sys.info()[["sysname"]], "|", R.version.string, "\n")
cat("candidates:", length(sel), "\n")
cat("VERDICTS:", paste(as.integer(s[sel] < 30), collapse = ""), "\n")
cat("DIGEST:", paste(sprintf("%a", s[sel]), collapse = " "), "\n")
cat("CLOSEST:", sprintf("%a", s[sel][which.min(abs(s[sel] - 30))]), "\n")
