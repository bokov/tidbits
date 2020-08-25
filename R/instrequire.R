#' try to install and load each of a list of packages
#'
#' This function takes a list of package names, loads them if they are
#' available, otherwise attempts to install each one and then again
#' attempts to load it. Will produce an error if any packages in \code{pkgs}
#' are not possible to install and load.
#'
#' The goal is to provide a robust and automation-friendly alternative to
#' putting a bunch of \code{\link[base]{require}} or \code{\link[base]{library}}
#' statements at the start of each script and have them keep erroring until each
#' dependency is manually installed.
#'
#' @param pkgs         character vector of package names that are needed, e.g.
#'                     by a script that uses this command.
#' @param quietly      passed to \code{\link[base]{require}}
#' @param dependencies whether to also install dependencies, passed to
#'                     \code{\link[utils]{install.packages}}
#' @param type         passed to \code{link[utils]{install.packages}}. By
#'                     default it is \code{'binary'} for all MacOS and Windows
#'                     and \code{'source'} otherwise
#' @param checksource  temporarily sets
#'                     \code{options('install.packages.check.source')}
#'                     to this value
#' @param compile      temporarily sets
#'                     \code{options('install.packages.compile.from.source')}
#'                     to this value
#' @param repos        passed to \code{\link[utils]{install.packages}} to
#'                     specify which repositories to use (optional).
#' @param ...          passed to \code{\link[utils]{install.packages}} to
#'                     specify any additional options needed (e.g. local urls
#'                     and such).
#'
#' @examples instrequire(c('readr','readxl','tibble'))
#' @export
instrequire <- function(pkgs # nodeps
                        ,quietly=TRUE
                        # the dependencies argument is ignored and is
                        # only here so that it doesn't end up in '...'
                        ,dependencies=TRUE
                        ,type=if(get_os() %in% c('osx','windows')){
                          'binary'} else 'source'
                        ,checksource='no'
                        ,compile='never'
                        ,repos=getOption('repos','https://cran.rstudio.com/')
                        ,...){
  orig.checksource <- options('install.packages.check.source');
  orig.compile <- options('install.packages.compile.from.source');
  on.exit({
    options(install.packages.check.source=orig.checksource);
    options(install.packages.compile.from.source=orig.compile);
  });
  options(install.packages.check.source=checksource);
  options(install.packages.compile.from.source=compile);
  suppressWarnings(pkgs_installed <- sapply(pkgs,require,character.only=TRUE
                                            ,quietly=quietly));
  if(length(pkgs_needed <- names(pkgs_installed[!pkgs_installed]))>0){
    utils::install.packages(pkgs_needed,repos=repos,dependencies = TRUE
                            ,type=type,...);
    pkgs_final <- sapply(pkgs_needed,require,character.only=TRUE
                         ,quietly=quietly);
    if(!all(pkgs_final)){
      stop(c('the following required packages could not be installed:\n'
             ,paste0(names(pkgs_final[!pkgs_final]),collapse = ', ')));
    }
  };
}

