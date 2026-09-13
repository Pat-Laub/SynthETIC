## Knife-edge probe: evaluate the SAME reachable rnorm draws on every OS and
## report each one's verdict in claim_size()'s `while (any(s < 30))` test.
## m and the k window are hard-coded so all platforms probe identical inputs.
BIG <- 134217728
m   <- 813546
kc  <- 1868573651
ks  <- (kc - 400):(kc + 400)

u <- (m + ks * 2^-32) / BIG
s <- (9.5 + 3 * qnorm(u))^5

cat("OS:", Sys.info()[["sysname"]], "|", R.version.string, "\n")
cat("VERDICTS:", paste(as.integer(s < 30), collapse = ""), "\n")
cat("NREJECT:", sum(s < 30), "of", length(s), "\n")
cat("BOUNDARY_K:", ks[which.min(abs(s - 30))], "\n")
cat("BOUNDARY_S:", sprintf("%a", s[which.min(abs(s - 30))]), "\n")
i <- seq(which.min(abs(s - 30)) - 2, which.min(abs(s - 30)) + 2)
for (j in i) cat(sprintf("  k=%d s=%a %s\n", ks[j], s[j], ifelse(s[j] < 30, "REJECT", "accept")))
