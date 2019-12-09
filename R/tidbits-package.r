#' tidbits.
#'
#' Various functions to streamline tasks that are repetitive and/or require
#' funky code tricks. A byproduct of UTHealth's TSCI 5050 Introduction to Data
#' Science course.
#'
#' @name tidbits
#' @docType package
#' @importFrom methods is
NULL

# variables that will otherwise be mistaken for global:
# load_deps()
utils::globalVariables('tload');
# tblinfo()
utils::globalVariables(c('c_factor','c_numeric','c_ordinal','c_tf','c_tm'
                         ,'c_uninformative','c_safe','nn'
                         ,'frc_missing','isnum','n_missing','n_nonmissing'
                         ,'uniquevals','frc_mostcommon'));
c()

