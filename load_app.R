# Install devtools if not already installed
install.packages("devtools")

# Load devtools
library(devtools)


devtools::install_github("https://github.com/JosephCliffordPerry/Fly_survival_application.git", ref = "Package")

library(flySurvivalApp)
Load_fly_app()
