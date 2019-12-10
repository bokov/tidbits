# do everything we can to install the latest BINARY version dammit
if(!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")};
message('** Starting custom install of dependencies **');
options(install.packages.check.source='no');
options(install.packages.compile.from.source='never');
deps <- remotes::dev_package_deps(dependencies = NA);
utils::install.packages('data.table',type='binary');
remotes::install_deps(dependencies = TRUE,type='binary',upgrade='never');
if (!all(deps$package %in% installed.packages())) {
  message("missing: ", paste(setdiff(deps$package, installed.packages())
                             , collapse=", "))};
q(status = 1, save = "no")
