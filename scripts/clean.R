paths <- c(
  ".cache",
  ".pkgdown-assets",
  ".r-library",
  "windcut.Rcheck",
  "teststhat"
)

existing <- paths[file.exists(paths)]
if (length(existing) > 0) {
  unlink(existing, recursive = TRUE, force = TRUE)
}



