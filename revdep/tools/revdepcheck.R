pkg <- revdepcheck:::pkg_check(".")
revdepcheck:::db_setup(pkg)
db <- revdepcheck:::db(pkg)
DBI::dbListTables(db)


revdepcheck::revdep_todo()
to_run <- c("EpiNow2","OncoBayes2","Sleuth3","bain","breathteststan","clustermq","insight","parameters","personalized")
revdepcheck::revdep_add(packages = to_run)
revdepcheck::revdep_add(packages = "intkrige")
revdepcheck::revdep_rm(packages = "bain")
revdepcheck::revdep_todo()

options(repos = c(CRAN = "https://packagemanager.rstudio.com/cran/__linux__/focal/latest"))

revdepcheck::revdep_check(timeout = as.difftime(5, units = "hours"), num_workers = 1)

# mlr: 2hours
# clustermq 10min
# Sleuth3: 31min
# insight:
# EpiNow2:
# parameters:
# personalized:
# intkrige:
