##Computer1
installed <- as.data.frame(installed.packages())
write.csv(installed, './inst/installed_previously.csv')

##Computer2
installedPreviously <- read.csv('installed_previously.csv')
baseR <- as.data.frame(installed.packages())
toInstall <- setdiff(installedPreviously, baseR)
install.packages(toInstall)
