# ShinyPsych - an R package to program and run experiments

ShinyPsych is an R package designed to help implement experiments in shiny apps.

To install `ShinyPsych` run the following code:

```
# Install devtools
if (!require(devtools)) install.packages("devtools")

# Install ShinyPsych from GitHub
devtools::install_github("mdsteiner/ShinyPsych", 
                         build = TRUE, 
                         build_opts = c("--no-resave-data", "--no-manual"),
                         build_vignettes = TRUE)

# Open the main vignette
ShinyPsych_Guide()       
```

Note that the build and build_opts arguments are necessary for the vignettes to be built and available on your computer (previously that was controlled with the argument build_vignettes = TRUE).
