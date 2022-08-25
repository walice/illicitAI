# jupyter-rstudio-binder
# Alice Lepissier
# alice.lepissier@gmail.com


## ## ## ## ## ## ## ## ## ## ##
# REQUIREMENTS              ####
## ## ## ## ## ## ## ## ## ## ##

# Install regular packages
install.packages(c("wesanderson",
                   "sysfonts",
                   "showtext",
                   "GGally",
                   "mapproj",
                   "gridExtra"), 
                 repos = "http://cran.us.r-project.org",
                 type = "source")

# Install arrow package
# from https://cran.r-project.org/web/packages/arrow/vignettes/install.html
options(
  HTTPUserAgent =
    sprintf(
      "R/%s R (%s)",
      getRversion(),
      paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
    )
)

install.packages("arrow", 
                 repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest")
