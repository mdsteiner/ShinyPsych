# ShinyPsych - an R package to program and run experiments

To install the package run the following code:

```
if (!require(devtools)) install.packages("devtools")

devtools::install_github("mdsteiner/ShinyPsych", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

Note that the build and build_opts arguments are necessary for the vignettes to be built and available on your computer (previously that was controlled with the argument build_vignettes = TRUE).
