% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{v}
\alias{v}
\title{Returns a list of column names from the data dictionary for which the column
named in the first argument is true. The first arg can be either a string or
a name. The second must be a data.frame}
\usage{
v(
  var,
  dat,
  retcol = getOption("tb.retcol", "column"),
  dictionary = get("dct0"),
  asname = F
)
}
\arguments{
\item{var}{Either a string or a name, of a column in `dictionary`}

\item{dat}{An optional data.frame, to constrain which rows of the
'dictionary' object get used}

\item{retcol}{Which column to return-- by default the same as used for 'matchcol'}

\item{dictionary}{A 'data.frame' that is used as a data dictionary. It must at
minimum contain a column of column-names for the dataset for
which it is a data dictionary ('matchcol') and one or more
columns each representing a _group_ of columns in the dataset,
such that a TRUE or T value means the column whose name is
the value of 'matchcol' is the name of a column in the data
that belongs to the group defined by the grouping column.
These grouping columns are what the argument 'var' is
supposed to refer to. We will use the convention that grouping
column names begin with 'c_' but this convention is not
(currently) enforced programmatically.}
}
\description{
Returns a list of column names from the data dictionary for which the column
named in the first argument is true. The first arg can be either a string or
a name. The second must be a data.frame
}
\examples{

dct0 <- tblinfo(mtcars);

v();

# Numeric variables in mtcars that behave like discrete variables
v(c_ordinal);
# Numeric variables in mtcars
v(c_numeric);
# Variables in mtcars that only have two values, so could be encoded as
# boolean
v(c_tf);

# Non-default data dictionary
dct1 <- tblinfo(state.x77)
v(c_ordinal,dict=dct1)
v(c_factor,dict=dct1)
v(c_tf,dict=dct1)
v(c_numeric,dict=dct1)

}
