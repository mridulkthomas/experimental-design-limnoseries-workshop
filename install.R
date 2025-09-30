# Minimal package installer for learners
pkgs <- c(
  "ggplot2",
  "dplyr",
  "ellipse",
  "rootSolve"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
message("All set. Loaded packages: ", paste(pkgs, collapse = ", "))
