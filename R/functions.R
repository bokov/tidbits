# small utils ------------------------------------------------------------------

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' try to install and load each of a list of packages
#'
#' This function takes a list of package names, loads them if they are
#' available, otherwise attempts to install each one and then again
#' attempts to load it. Will produce an error if any packages in \code{pkgs}
#' are not possible to install and load.
#'
#' @param pkgs         character vector of package names that are needed, e.g.
#'                     by a script that uses this command.
#' @param quietly      passed to \code{\link[base]{require}}
#' @param dependencies whether to also install dependencies, passed to
#'                     \code{\link[utils]{install.packages}}
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
                        # the dependencies argument is ignored and is only here
                        # so that it doesn't end up in the '...'
                        ,dependencies=TRUE
                        ,repos=getOption('repos','https://cran.rstudio.com/')
                        ,...){
  pkgs_installed <- sapply(pkgs,require,character.only=TRUE);
  if(length(pkgs_needed <- names(pkgs_installed[!pkgs_installed]))>0){
    utils::install.packages(pkgs_needed,repos=repos,dependencies = TRUE,...);
    pkgs_final <- sapply(pkgs_needed,require,character.only=TRUE
                         ,quietly=quietly);
    if(!all(pkgs_final)){
      stop(c('the following required packages could not be installed:\n'
             ,paste0(names(pkgs_final[!pkgs_final]),collapse = ', ')));
    }
  };
}

clean_slate <- function(command="",removepatt='^\\.RData$|*.R\\.rdata$' # deps:git_subupd
                        ,all=TRUE,cleanglobal=TRUE
                        ,updatemodules=!file.exists('.developer')
                        ,envir=parent.frame()){
  if(!interactive()) warning('This function is intended to run in an '
                            ,'interactive session to restart that\n  '
                            ,'session on a clean slate. If you are calling it '
                            ,'non-interactively  (from a\n  script or '
                            ,'function), don\'t expect any code that you put '
                            ,'after it to work!');
  # remove cached files
  file.remove(list.files(pattern=removepatt,all.files=TRUE,recursive=TRUE,full.names = TRUE));
  # Update the git submodules
  if(updatemodules) git_subupd();
  # clear out calling environment
  rm(list=ls(all.names=all,envir = envir),envir = envir);
  # also global environment if specified
  if(cleanglobal) rm(list=ls(all.names=all,envir=.GlobalEnv),envir = .GlobalEnv);
  # if rstudioapi available, use it to restart the session
  if(requireNamespace('rstudioapi') && rstudioapi::isAvailable()){
    rstudioapi::restartSession(command)};
}

#' Append or replace attributes of any object in a pipeline-frindly way.
#'
#' @param xx        Object whose attributes to modify and then return the object
#' @param rep.attrs Named list of attributes to create or replace
#' @param app.attrs Named list of attributes to create or append to
#'
#' @return The object `xx` with modified attributes
#' @export
#'
#' @examples
#' # Change an object's attribute
#' with_attrs(iris,list(class='list'))
#'
#' # Create a new attribute for an object
#' foo <- with_attrs(LETTERS,list(comment='Hello world'))
#' comment(foo)
#' foo <- with_attrs(foo,rep='One more comment.')
#' comment(foo)
#'
with_attrs<-function(xx,rep.attrs=list(),app.attrs=list()){ # nodeps
  attrs <- attributes(xx); if(is.null(attrs)) attrs<-list();
  for(ii in names(rep.attrs)) attrs[[ii]] <- rep.attrs[[ii]];
  for(ii in names(app.attrs)) attrs[[ii]] <- c(attrs[[ii]],app.attrs[[ii]]);
  attributes(xx) <- attrs;
  xx;
}

#' takes input and returns it with a comment attribute
#'
#' @param xx       Any R object
#' @param comment  Comment to add to the object.
#' @param append   If \code{TRUE} (default) appends comment to those that
#'                 are already attached to the object, if any.
#' @param transf   Function that takes one character vector argument and returns
#'                 a character vector. By default it is
#'                 \code{\link[stringr]{str_squish}} which removes extra whitespace.
#'                 To bypass entirely set this argument to \code{NULL}.
#'
#' @return The same object passed as the first argument but with the added
#'         comment.
#'
#' @examples
#' data(iris)
#' comment(iris)
#' foo <- cm(iris,comment=' What  pretty ')
#' comment(foo)
#'
#' foo <- cm(foo,comment='flowers!',transf=toupper)
#' comment(foo)
#'
#' # This replaces the earlier comments due to append being FALSE now.
#' foo <- cm(foo,'Hello world.',append=FALSE)
#' comment(foo)
#'
#' @export
with_cm <- function(xx,comment=NULL,append=TRUE,transf=stringr::str_squish){
  if(!is.null(transf)) comment <- transf(comment);
  if(append) with_attrs(xx,app.attrs=list(comment=comment)) else {
    with_attrs(xx,rep.attrs=list(comment=comment));
  }
}

#' @rdname with_cm
#' @export
cm <- with_cm


getCall.list <- getCall.data.frame <- getCall.gg <- function(x,...) {attr(x,'call')};

# why not update calls?
update.call <- function(object,...){
  dots <- list(...);
  for(ii in names(dots)) object[[ii]] <- dots[[ii]];
  object;
}

#' Stack a vector to form a matrix with repeating rows, with optional
#' column names and transformation
#'
#' @param  vv    A vector which will become the row of a matrix
#' @param  nr    Number of (identical) rows this matrix will contain
#' @param  trans An optional function that can take a matrix as its
#'              sole argument. Useful functions include `as.data.frame()`
#'              `as_tibble()` and `as.table()`
#' @return A matrix, unless the function specified in the `trans` argument
#'         causes the output to be something else.
#' @export
#'
#' @examples
#' vec2m(1:10,5);
#' vec2m(1:10,5,tr=data.frame);
#' vec2m(setNames(1:12,month.name),3);
vec2m <- function(vv,nr=1,trans=identity) {
  return(trans(matrix(as.matrix(c(vv)),nrow=nr,ncol=length(vv),byrow=T
                      ,dimnames=list(NULL,names(vv)))));
}

#' returns call with ALL arguments specified, both the defaults and those
#' explicitly provided by the user
#'
#' Intended for programmers to include inside functions they write in order for
#' their functions to be able to examine arguments further up the calling stack
#' that would otherwise not be in scope.
#'
#' @param syspar An positive integer, which indicates which function in the
#'               calling stack is the one whose arguments should be read
#' @param env    The environment from which to retrieve the \code{...}, if any.
#'               Passed to \code{\link[base]{match.call}}
#' @param expand.dots  Passed to \code{\link[base]{match.call}}. \code{TRUE} by
#'                     default, set to \code{FALSE} to not include \code{...}
fullargs <- function(syspar=sys.parent(),env=parent.frame(syspar)
                     ,expand.dots=TRUE){
  fn <- sys.function(syspar);
  frm <- formals(fn);
  cll <- match.call(fn,sys.call(syspar),expand.dots = expand.dots,envir = env);
  defaults <- setdiff(names(frm),c(names(cll),'...'));
  for(ii in defaults) cll[[ii]] <- frm[[ii]];
  return(cll);
}

#' Take a set of objects coercible to matrices and perform sprintf on them while
#' preserving their dimensions (obtained from the first argument of ...)

# figure out how the current OS represents the top of its file system
systemRootDir <- function(){
  dir <- dirname(normalizePath('.'));
  newdir <- dirname(dir);
  while(dir!=newdir){dir<-newdir; newdir <- dirname(newdir)}
  return(newdir);
}


# extract the error message of the argument
getTryMsg <- function(xx,ifNotErr=xx){ # revdeps: t_autoread
  if(methods::is(xx,'try-error')) return(attr(xx,'condition')$message);
  return(ifNotErr);}

# to be used inside a function to get a list of unevaluated calls
# from all the ... args
getParentDots <- function(xx,call=sys.call(-1),fun=sys.function(-1)){ # revdeps: colinfo,tblinfo
  out <- list();
  for(ii in setdiff(names(call),c(names(formals(fun)),''))){
    out[[ii]] <- call[[ii]]};
  out;
}

# Credit: http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function(){ # nodeps
  sysinf <- Sys.info();
  if (!is.null(sysinf)){
    os <- sysinf['sysname'];
    if (os == 'Darwin') os <- "osx";
  } else { ## mystery machine
    os <- .Platform$OS.type;
    if (grepl("^darwin", R.version$os)) os <- "osx";
    if (grepl("linux-gnu", R.version$os)) os <- "linux";
  }
  tolower(os);
};

#' Simply take all the arguments and turn them into a comma-delimited string
#' insertion into dynamically generated R code or whatever else needs it.
#'
#' @param ... Any arguments, named or un-named.
#'
#' @return    Character string
#' @export
#'
#' @examples deparseargs(foo=42,bar="baz","Hello world")
#'
deparseargs <- function(...){
  gsub('^list\\(|\\)','',deparse(list(...),control=c('keepInteger','keepNA')))}

systemwrapper <- function(cmd='',...,VERBOSE=getOption('sysverbose',T)
                          ,CHECKFILES=c('files')){ # nodeps
  args <- list(...); sysargs <- list();
  # separate out the args intended for system
  for(ii in intersect(names(args),names(formals(system)))){
    sysargs[[ii]] <- args[[ii]]; args[[ii]] <- NULL;};
  # check to make sure all arguments listed in checkfiles contain only files
  # that exist
  for(ii in intersect(CHECKFILES,names(args))){
    if(!all(.exist <- file.exists(args[[ii]]))){
      stop('The following files cannot be found:\n'
           ,paste(args[[ii]][!.exist],collapse=', '))}};
  for(xx in args) cmd <- paste(cmd,paste(xx,collapse=' '));
  if(VERBOSE) message('Executing the following command:\n',cmd);
  return(do.call(system,c(command=cmd,sysargs)));
}
# git ----
#' Simple git wrappers
#'
#' @param ... Passed to the \code{git} shell command via \code{system}.
#'
#' @template shellreturn
#' @family git wrappers
#' @name git
NULL

#'
#' git checkout
#'
#' @param which The branch you wish to check out from the current repo.
#' @inheritParams git
#'
# @family git wrappers
git_checkout <- function(which=getOption('git.workingbranch','master'),...){
  systemwrapper('git checkout',which,...)};

#' @rdname git_checkout
gco <- git_checkout;

git_commit <- function(files='-a',comment
                       ,autopush=getOption('git.autopush',T),...){
  .changed<-git_status(VERBOSE=F,intern=T);
  filenames <- if(!missing(files)){
    paste0(paste(files,collapse=','),': ')} else 'multi: ';
  comment <- paste0('"',filenames,comment,'"');
  systemwrapper('git commit',files,'-m',comment,...);
  if(autopush) git_push();}

gci <- git_commit;

#' List the files in the repo having a particular status
#'
#' Quoting from git documentation (\code{git help diff}):
#' \emph{Select only files that are Added (A), Copied (C), Deleted (D),
#' Modified (M), Renamed (R), have their type (i.e. regular file, symlink,
#' submodule, ...) changed (T), are Unmerged (U), are Unknown (X), or have had
#' their pairing Broken (B). Any combination of the filter characters (including
#' none) can be used. When * (All-or-none) is added to the combination, all
#' paths are selected if there is any file that matches other criteria in the
#' comparison; if there is no file that matches other criteria, nothing is
#' selected.}
#'
#' @param xx String containing one or more of A,C,D,M,R,T,U,X,B, or *
#'
#' @family git wrappers
git_diff_filter <- function(xx) {
  system(paste('git diff --name-only --diff-filter',xx),intern=TRUE)};

#' Nicely formatted and concise status of current git repo.
#'
#' @param print        If \code{TRUE} (default) prints formatted results to
#'                     console.
#' @param diff_filters Which statuses to display (all of them, by default)
#' @inheritParams git
#'
#' @family git wrappers
git_status <- function(print=TRUE
                       ,diff_filters=list(Added='A',Copied='C',Deleted='D'
                                          ,Modified='M',Renamed='R'
                                          ,ChangedType='T',Unmerged='U'
                                          ,Unknown='X',Broken='B')
                       ,...){
  branch <- system('git rev-parse --abbrev-ref HEAD',intern=T);
  tracking <- system('git rev-parse --abbrev-ref --symbolic-full-name @{u}'
                     ,intern=T);
  commits <- if(length(tracking)==0) character(0) else {
    system(paste('git log',paste0(tracking,'..',branch),'--oneline')
           ,intern=T)};
  diffs <- lapply(diff_filters,git_diff_filter);
  if(print){
    message('Branch: ',branch);
    if(length(commits)>0) {
      message('Ahead of ',tracking,' by ',length(commits),' commit'
              ,if(length(commits)>1) 's.' else '.')} else {
                if(!any(sapply(diffs,length)>0)){
                  message('All local changes have already been pushed')}};
    # TODO: check for un-pulled upstream changes
    for(ii in names(diffs)) if(length(diffs[[ii]])>0){
      message(ii,':'); cat(paste(' ',diffs[[ii]]),sep='\n');}
    }
  invisible(list(branch=branch,tracking=tracking,commits=commits
                 ,diffs=diffs));
}

#' @rdname git_status
gst <- git_status;

#' \code{git_lsfiles}: List only the files currently being tracked by git
#'
#' @rdname git
git_lsfiles <- function(...) {systemwrapper('git ls-files',...)};

#' \code{git_other},\code{git_}: Whatever other git functions that aren't
#'                               explicitly implemented yet. Just put any
#'                               combination of git arguments as arguments to
#'                               this function, leaving out \code{git} itself.
#'
#' @rdname git
git_other <- function(...){systemwrapper('git',...)};
#' @rdname git
git_ <- git_other;

#' Make the specified file start getting tracked by the current git repository.
#'
#' @param files Character vector of file names.
#' @inheritParams git
#'
#' @family git wrappers
git_add <- function(files,...){
  systemwrapper('git add',files=files,...)};
#' @rdname git_add
gadd <- git_add;

#' Rename a file tracked by git, so git knows you did not delete it.
#'
#' @param from Current name/path of a file tracked by git.
#' @param to   New name/path of a file tracked by git.
#' @inheritParams git
#'
#' @family git wrappers
git_rename <- function(from,to,...){systemwrapper('git rename',from,to,...)};

#' \code{git_move}: Move a file tracked by git, so git knows you did not delete it.
#'
#' @inheritParams git_rename
#'
#' @rdname git
git_move <- function(from,to,...) {systemwrapper('git mv',from,to,...)};

#' \code{git_push}: Push committed changes to the origin (for example, but not
#' necessarily, github.com)
#'
#' @rdname git
git_push <- function(...) {systemwrapper('git push',...)};
gp <- git_push;

#' Create a new branch \emph{and} check it out immediately. Optionally also
#' push.
#'
#' @param branch     Name of the branch to create and immediately check out.
#' @param pushorigin If \code{TRUE} will also push the new branch to the
#'                   git repository with the \code{--set-upstream} option.
#' @inheritParams git
#'
#' @family git wrappers
git_newbranch <- function(branch,pushorigin=FALSE,...){
  systemwrapper('git checkout -b',branch,...);
  if(pushorigin) systemwrapper('git push origin --set-upstream',branch);
}
#' @rdname git_newbranch
gbr <- git_newbranch;

# TODO: detect conflicts in advance and ask what to do
git_merge <- function(which,fastfwd=getOption('git.fastfwd',F)
                      ,verbose=getOption('git.verbose',T),...){
  cmd <- paste('git merge',if(!fastfwd) '--no-ff' else '',...);
  if(verbose) message('Executing the following command:\n',cmd);
  system(cmd);}
gmr <- git_merge;

#' Delete and re-download git submodules if any exist.
#'
#' @param stopfile The name of a file which, if exists, will cause this function
#'                 to exit without doing anything. Will silently return errors
#'                 from shell but will not throw an error.
#' @template shellreturn
#' @export
#'
#' @examples
#'
#' \dontrun{ git_subupd() }
#'
#' @family git wrappers
git_subupd <- function(stopfile='.developer'){if(!file.exists(stopfile)){
  unlink(systemwrapper("git submodule --quiet foreach 'echo $path'"
                       ,intern=TRUE,VERBOSE=FALSE)
         ,recursive = TRUE,force = TRUE);
  systemwrapper('git submodule update --init --recursive --remote')} else {
    message('Developer mode-- ignoring.'); return(0);
  }};

#' Automatically configure your global .gitconfig with your name and email
#' (if not already configured) so that git will allow you to commit changes.
#' If run from a script this function will not do anything other than set the
#' upstream repository if applicable.
#'
#' @param upstream    The upstream repository to add. Can also be specified via
#'                    \code{options('git.upstream')} which will become this
#'                    argument's default value. If there is already an upstream
#'                    repository configured, it will be left as-is.
#' @inheritParams git
#'
#' @return NULL
#'
#' @examples git_autoconf()
#' @export
#' @family git wrappers
git_autoconf <- function(upstream=getOption('git.upstream'),...){
  # should only be run in an interactive context
  if(!'upstream' %in% system('git remote',intern=TRUE) && !is.null(upstream)){
    systemwrapper('git remote add upstream',upstream);
  }
  # Set username and email
  if(interactive() &&
     length(.username <- system('git config user.name',intern=T))==0){
    message("Please type in your name as you want it to appear in git logs:");
    .username <- paste0('"',readline(),'"');
    systemwrapper('git config --global user.name',.username)};
  if(interactive() &&
     length(.useremail <- system('git config user.email',intern=T))==0){
    message("Please type in your email as you want it to appear in git logs:");
    .useremail <- paste0('"',readline(),'"');
    systemwrapper('git config --global user.email',.useremail)};
}



#' Add a pattern to a .gitignore file
#'
#' @param patterns A character vector of patterns to ignore. Required.
#'                 Always appended. If you need to un-ignore something
#'                 you will have to edit .gitignore manually.
#' @param ignorepath Path to .gitignore (you can have multiple ones)
#'                   current directory by default.
#' @param preamble What to put in the line/s before a set of ignore
#'                 patterns. Empty line by default, set to NULL if you
#'                 want to not skip a line.
#'
#' @return NULL
#' @export
#'
#' @examples git_ignore(c('*.csv','*.tsv'))
git_ignore <- function(patterns,ignorepath='.',preamble='') {
  write(c(preamble,patterns),file.path(ignorepath,'.gitignore')
        ,append=TRUE)};

#' Switch between ssh authentication and ssl authentication for a git repo.
#'
#' A use-case for this is some environments that by default initialize projects
#' as ssl/https (e.g. RStudio Cloud) but some users may prefer ssh
#' authentication. This easily converts between the two settings without having
#' to remember the whole git command. Will silently return errors from shell but
#' will not throw an error.
#'
#' @param tossh If `TRUE`, will attempt to convert the remote.origin.url from
#'              https to ssh. Default: `TRUE`
#' @param sshstr A string to use as the prefix for ssh connection. Optional,
#'               defaults to the values used by github.com
#' @param sslstr A string to use as the prefix for the ssl connection. Optional,
#'               defaults to the values used by github.com.
#'
#' @return Invisibly returns `0` or an error code.
#'
#' @export
#' @examples
#' \dontrun{
#' # Convert from https://github.com/... to git@github.com:...
#' git_ssh()
#'
#' # Convert from git@github.com:... to https://github.com/...
#' git_ssh(FALSE)
#'
#' }
git_ssh <- function(tossh=TRUE,sshstr='git@github.com:'
                    ,sslstr='https://github.com/'){
  currentorigin <- systemwrapper('git config remote.origin.url',intern=TRUE);
  message('Current origin: ',currentorigin);
  matchrepl <- if(tossh) c(sslstr,sshstr) else c(sshstr,sslstr);
  matchrepl[1]<-paste0('^',matchrepl[1]);
  neworigin <- gsub(matchrepl[1],matchrepl[2],currentorigin);
  message('Setting origin to: ',neworigin);
  systemwrapper('git config remote.origin.url',neworigin);
  systemwrapper('git remote -v');
}

#' Returns \code{TRUE} if the git command is available through \code{system} and
#' \code{FALSE} otherwise.
#'
#' @param xx A git command to use for testing.
#'
#' @return logical
#' @export
#' @examples \dontrun{git_exists()}
#'
git_exists <- function(xx='--version'){
  oo <- git_(xx,ignore.stdout=TRUE,ignore.stderr=TRUE,VERBOSE=FALSE);
  oo==0;
}

# TODO: git nagger

# renaming and remapping  ----


#' takes a character vector and perform multiple search-replace
#' operations on it.
#' @param xx A \code{vector} of type \code{character} (required)
#' @param searchrep A \code{matrix} with two columns of type \code{character} (required). The left column is the pattern and the right, the replacement.
#' @param method One of 'partial','full', or 'exact'. Controls whether to replace only the matching regexp, replace the entire value that contains a matching regexp, or replace the entire value if it's an exact match.
#'
#' @examples submulti(letters,cbind(c('a','Q','v'),c('11','22','33')))
#' @export
submulti <- function(xx,searchrep
                     ,method=c('partial','full','exact'
                               ,'starts','ends','startsends')){
  # if no method is specified by the user, this makes it take the first value
  # if a method is only partially written out, this completes it, and if the
  # method doesn't match any of the valid possibilities this gives an informativ
  # error message
  method<-match.arg(method);
  # if passed a single vector of length 2 make it a matrix
  if(is.null(dim(searchrep))&&length(searchrep)==2) searchrep<-rbind(searchrep);
  # rr is a sequence of integers spanning the rows of the searchrep matrix
  rr <- 1:nrow(searchrep);
  # oo will hold the result that this function will return
  oo <- xx;
  switch(method
         ,partial = {for(ii in rr)
           oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo)}
         ,full =    {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo)]<-searchrep[ii,2]}
         ,exact = {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo,fixed=T)]<-searchrep[ii,2]}
           #oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo,fixed = T)}
         ,starts = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1]),searchrep[ii,2],oo)}
         ,ends = {for(ii in rr)
           oo <- gsub(paste0(searchrep[ii,1],'$'),searchrep[ii,2],oo)}
         ,startsends = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1],'$'),searchrep[ii,2],oo)}
  );
  oo;
}

#' Take a data.frame or character vector and a vector of grep targets and return
#' the values that match (for data.frame, column names that match). If no
#' patterns given just returns the names
#' @param xx A \code{data.frame} or character vector (required)
#' @param patterns A character vector of regexp targets to be OR-ed
grepor <- function(xx,patterns='.') {
  if(is.list(xx)) xx <-names(xx);
  grep(paste0(patterns,collapse='|'),xx,value=TRUE);
}



# table utilities ----

#' Extends trailR package with integrated universal (almost) file reader
#'
#' @param file Any of the common delimited file formats
#' @param ... Arguments passed to `tread()`
#'
#' @return Whatever object the underlying read function passed to `tread()`
#'         will output, usually a class that inherits from `data.frame`
#'
#' export
# t_autoread <- function(file,...){ #deps: getTryMsg
#   # make sure prerequisite function exists
#   if(requireNamespace('trailR')){
#     do.call(tread,c(list(file,readfun=autoread),list(...)))} else {
#       stop("The 't_autoread()' function only works if the trailR package is installed")
#     }};


#' Autoguessing function for reading most common data formats
#'
#' Supported so far are: xls, xlsx, csv and most other delimited text formats,
#' SPSS, Stata, and SAS.
#'
#' @param file       The name of a file you want to read into R
#' @param na         Vector of strings that should get translated to `NA` upon
#'                   import. Optional, defaults to a reasonable set of values.
#' @param fixnames   A function that normalizes column names after importing the
#'                   data. If you want to leave them untouched, set this equal
#'                   to `identity()`. Optional, defaults to making them lower
#'                   case, R-legal, and unique.
#' @param file_args  This is to easily pass project-level or script-level
#'                   defaults in the form of an `alist()` to whichever lucky
#'                   function ends up winning the contest to read your file.
#'                   Only names that match the formal arguments of your function
#'                   will be used, the rest will be silently ignored. This way,
#'                   you can pass some `read_xlsx` specific arguments without
#'                   worrying that something else will intercept them and error
#'                   out.
#' @param ...        Additional named arguments passed to this function will
#'                   be added to those in `file_args` overriding any that have
#'                   matching names.
#'
#' @return A `tibble`
#' @importFrom readxl read_xls read_xlsx excel_sheets
autoread <- function(file,na=c('','.','(null)','NULL','NA')
                     # change this to identity to do nothing to names
                     ,fixnames=function(xx) {
                       stats::setNames(xx,tolower(make.names(names(xx),unique = TRUE)))}
                     ,file_args=list(),...){
  if(!RCurl::url.exists(file)){
    if(!file.exists(file)) stop(sprintf('File "%s" not found.'),file);
    if(dir.exists(file)) stop(sprintf('"%s" is not a file, it\'s a directory.')
                              ,file)};
  args <- list(...);
  # allow file_args to be overridden by ... args, while preserving
  # order of ...
  for(ii in intersect(names(args),names(file_args))) file_args[[ii]] <- NULL;
  xlformat <- readxl::format_from_signature(file);
  args <- c(file_args,args);
  reader <- if(!is.na(xlformat)) paste0('read_',xlformat) else 'auto';
  if(reader == 'auto' && nrow(enc<-readr::guess_encoding(file))>0){
    # if it's a zip file, this unzips it and replaces the original file arg
    # with the temporary unzipped version
    unzfile <- suppressWarnings(utils::unzip(file,exdir = tempfile("autoread")));
    if(length(unzfile)>1){ if(!'sheet' %in% names(args)){
      warning(
        "\nMultiple files found in ",file,":\n"
        ,paste(list.files(dirname(unzfile),recursive = T,all.files = T)
               ,collapse=',')
        ,"\nReading in the first file. If you want a different one"
        ,"\nplease specify a 'sheet' argument");
      unzfile <- unzfile[1]} else unzfile <- unzfile[args$sheet]};
    if(length(unzfile==1)){
      message('Reading unzipped file',basename(unzfile));
      file <- unzfile;}
    message('Trying to read as a text file with fread()')
    # try to read as a delimited file via fread
    txargs <- args[intersect(names(args)
                             ,names(formals(data.table::fread)))];
    txargs$na.strings <- na;
    out <- try(tibble::as_tibble(do.call(data.table::fread
                                         ,c(list(input=file),txargs)))
               ,silent = T);
    if(!methods::is(out,'try-error')) return(fixnames(out));
    message('fread() failed! Falling back on read_delim');
    txargs <- args[intersect(names(args),names(formals(readr::read_delim)))];
    txargs$na <- na;
    txargs$delim <- '\t';
    suppressMessages(out <- try({
      problems<-problems(oo<-do.call(readr::read_delim,c(list(file=file)
                                                         ,txargs)));
      oo},silent=T));
    if(!methods::is(out,'try-error') && ncol(out)>1) return(fixnames(out)) else out_tab <- out;
    txargs$delim <- ',';
    suppressMessages(out <- try({
      problems<-problems(oo<-do.call(readr::read_delim,c(list(file=file)
                                                         ,txargs)));
      oo},silent=T));
    if(!methods::is(out,'try-error')) return(fixnames(out));
    cat('\nGuessed encoding:\n');print(enc);
    stop(attr(out,'condition')$message);
  }
  # try various binary formats
  if(reader %in% c('read_xls','read_xlsx')){
    # check for Excel formats
    message('checking sheets in workbook');
    sheets <- readxl::excel_sheets(file);
    if(length(sheets)>1 && !'sheet' %in% names(args)){
      warning(
        "\nMultiple sheets found:\n",paste(sheets,collapse=', ')
        ,"\nReading in the first sheet. If you want a different one"
        ,"\nplease specify a 'sheet' argument")};
    xlargs <- args[intersect(names(args)
                             ,names(formals(eval(as.name(reader)))))];
    xlargs$na <- na;
    message('About to read Excel file');
    out <- do.call(reader,c(list(path=file),xlargs));
    message('Fixing column names on Excel file');
    out <- fixnames(out);
    return(out)};

  # SPSS, SAS, and Stata
  # one of these has some error message that bubbles through despite silent=T
  # so we sink before the for loop, unsink if one of the readers succeeds...
  sink(tempfile());
  for(ff in c(haven::read_sav,haven::read_por,haven::read_dta,haven::read_sas
              ,haven::read_xpt)){
      {
        if(!methods::is(try(out <- ff(file),silent=T),'try-error')){
          sink();
          return(fixnames(out))}}
  }
  # and unsink at the end if none of them succeed
  sink();

  message('\nUnknown file type?\n');
  stop(attr(out,'condition')$message);
  }

#' Sumarize a table column
#'
#' The following summary statistics are calculated on \code{col}:
#' \tabular{rr}{
#' \code{class} \tab a colon-delimited string with all the classes that
#'              \code{col} belongs to \cr
#' \code{uniquevals} \tab number of unique non-missing values \cr
#' \code{isnum} \tab whether or not \code{col} is numeric (logical)\cr
#' \code{frc_int} \tab the fraction of non-missing values in \code{col} that are
#'                     integers\cr
#' \code{n_nonmissing} \tab the number of non-missing values in \code{col}\cr
#' \code{n_missing} \tab the number of missing values in \code{col}\cr
#' \code{frc_missing} \tab the fraction of missing values in \code{col}\cr
#' \code{n_nonrepeat} \tab the number of values that occur only once in
#'                    \code{col}\cr
#' \code{frc_nonrepeat} \tab the fraction of values in \code{col} that do not
#'                      repeat\cr
#' \code{top3} \tab the top three most commonly occurring values in \code{col}
#'                  as a semicolon-delimited string
#' }
#'
#' @param col          A vector of any type.
#' @param custom_stats An \code{alist} of unevaluated calls or similar objects
#'                     that will be evaluated in the scope described in the
#'                     details section.
#' @param ...          Additional named expressions that will also be evaluated
#'                     in the scope described below but after the
#'                     \code{custom_stats} expressions have been evaluated, so
#'                     they can use any objects they have created.
#'
#' @return A list with the objects described in details, in addition to objects
#'        having the same names as those in \code{custom_stats} and \code{...}
#'        and the values those of the corresponding expressions.
#'
colinfo <- function(col,custom_stats=alist(),...){
  nn <- length(col);
  nona <- stats::na.omit(col);
  isna <- is.na(col);
  coltab <- table(nona);
  out <- list(class=paste0(class(col),collapse=':')
              ,uniquevals=length(coltab)
              ,isnum=is.numeric(col)
              ,frc_int=if(is.numeric(nona)) mean(nona%%1==0) else 0
              ,n_nonmissing=nn-sum(isna)
              ,n_missing=sum(isna)
              ,frc_missing=mean(isna)
              ,n_nonrepeat=sum(coltab==1)
              ,frc_nonrepeat=sum(coltab==1)/length(nona)
              ,top3=paste0('"',names(sort(coltab,decreasing = T)[1:3]),'"'
                              ,collapse='; ')
              ,frc_mostcommon=max(coltab)/nn
              ,md5=digest::digest(col)
  );
  for(ii in names(custom_stats)){
    out[[ii]] <- eval(custom_stats[[ii]],envir = out)};
  dots <- getParentDots();
  for(ii in names(dots)) out[[ii]] <- eval(dots[[ii]],envir=out);
  out;
  }

#' Create an automated data dictionary
#'
#' The unevaluated expressions in \code{custom_stats} and \code{info_cols} use
#' variables that exist during execution and are documented in
#' \code{\link{colinfo}}
#'
#' @param dat          An object that inherits from \code{data.frame}
#' @param custom_stats An \code{alist} of statistics to calculate on each
#'                     column of \code{dat} in addition to the defaults
#'                     in \code{info_cols} (below). Optional.
#' @param info_cols    Another \code{alist}, this one has default values but
#'                     can be overridden on an all-or-none basis.
#' @param ...          Eats any extra arguments, to keep them from
#'                     causing trouble.
#'
#' @return A data-frame having one row for each column in \code{dat}
#' @export
#'
#' @examples
#'
#' tblinfo(datasets::iris)
#'
#' @seealso v
#' @seealso colinfo
tblinfo <- function(dat,custom_stats=alist()
                    # some handy column groupers
                    ,info_cols=alist(
                       c_empty=frc_missing==1
                      ,c_uninformative=n_nonmissing<2
                      ,c_ordinal=uniquevals<10&isnum
                      ,c_tm=uniquevals==1&n_missing>0
                      ,c_tf=uniquevals==2
                      ,c_numeric=isnum&!c_ordinal
                      ,c_factor=uniquevals<10&!isnum
                      ,c_safe=frc_missing<.2 & frc_mostcommon < .7 &
                        !c_uninformative
                      ,c_safetf=c_safe & c_tf
                      ,c_safenumeric=c_safe & c_numeric
                      ,c_complex=!(c_ordinal|c_tm|c_tf|c_numeric|c_factor)
                    ),...){
  out <- dplyr::bind_rows(sapply(dat,colinfo,custom_stats=custom_stats,simplify=F)
                   ,.id='column');
  for(ii in names(info_cols)) out[[ii]] <- eval(info_cols[[ii]],envir=out);
  dots <- getParentDots();
  for(ii in names(dots)) out[[ii]] <- eval(dots[[ii]],envir=out);
  class(out)<-c('dtdict',class(out));
  return(out);
}


#' Returns a list of column names from the data dictionary for which the column
#' named in the first argument is true. The first arg can be either a string or
#' a name. The second must be a data.frame
#'
#' @param var        Either a string or a name, of a column in `dictionary`
#' @param dat        An optional data.frame, to constrain which rows of the
#'                   'dictionary' object get used
#' @param retcol     Which column to return-- by default the same as used for
#'                   'matchcol'
#' @param dictionary A 'data.frame' that is used as a data dictionary. It must at
#'                   minimum contain a column of column-names for the dataset for
#'                   which it is a data dictionary ('matchcol') and one or more
#'                   columns each representing a _group_ of columns in the dataset,
#'                   such that a TRUE or T value means the column whose name is
#'                   the value of 'matchcol' is the name of a column in the data
#'                   that belongs to the group defined by the grouping column.
#'                   These grouping columns are what the argument 'var' is
#'                   supposed to refer to. We will use the convention that grouping
#'                   column names begin with 'c_' but this convention is not
#'                   (currently) enforced programmatically.
#' @param asname     If set to `TRUE` will output a list of objects of type
#'                   `name` rather than a vector of character strings.
#'
#' @return           Either a character vector (default) or a list of `name`
#'                   objects.
#'
#' @examples
#'
#' dct0 <- tblinfo(mtcars);
#'
#' # see what columns exist in the current data dictionary
#' v();
#'
#' # Numeric variables in mtcars that behave like discrete variables
#' v(c_ordinal);
#' # Numeric variables in mtcars
#' v(c_numeric);
#' # Variables in mtcars that only have two values, so could be encoded as
#' # boolean
#' v(c_tf);
#'
#' # Return as names rather than characters
#' v(c_ordinal,asname=TRUE)
#'
#' # Constrain to only columns that exist in a given dataset.
#' mtsmall <- mtcars[,1:5]
#' v(c_numeric,dat=mtsmall)
#'
#' # Supposing you had another version of the variable names, e.g. for display
#' # purposes. You would access it via the 'retcol' argument.
#' dct0$printname <- toupper(dct0$column)
#' v()
#' v(c_numeric,retcol='printname')
#'
#' # You can also update the data dictionary with new column groupings.
#' dct0$c_ordinal2 <- with(dct0,uniquevals <6)
#' v()
#' v(c_ordinal2)
#'
#' # Non-default data dictionary
#' dct1 <- tblinfo(state.x77)
#' v(c_ordinal,dict=dct1)
#' v(c_factor,dict=dct1)
#' v(c_tf,dict=dct1)
#' v(c_numeric,dict=dct1)
#'
#' @export
#' @seealso tblinfo
v <- function(var,dat
              ,retcol=getOption('tb.retcol','column')
              ,dictionary=try(get('dct0'),silent = TRUE)
              ,asname=FALSE) {
  # check built-in dictionary
  if(!missing(dat) && (missing(dictionary) || is(dictionary,'try-error'))){
    dictionary <- attr(dat,'tblinfo');
    if(is.null(dictionary)) dictionary <- tblinfo(dat);
  }
  # convenience function: if forgot what column names are available, call with
  # no arguments and they will be listed
  if(missing(var)) return(names(dictionary));
  # support both standard or non-standard evaluation
  var<-as.character(substitute(var));
  # TODO: Think about what to do when nothing matches... not necessarily an error
  #       condition, might just be something to warn about and move on.
  out <- unique(as.vector(stats::na.omit(unlist(dictionary[dictionary[[var]]
                                                           ,retcol]))));
  if(!is(try(cnames<-colnames(dat),silent = T),'try-error')&&length(cnames)>0) {
    out <- out[out%in%cnames];}
  if(asname) out <- lapply(out,as.name);
  #return(unname(out));
  return(out);
}


# string hacking ----

# Project Utilities ----
personalizeTemplate <- function(file,title='TITLE',author='AUTHOR'
                                ,deps=c('dictionary.R'),packages=c()
                                ,date=Sys.Date(),template='TEMPLATE.R'
                                ,path_to_global
                                ,paths=c('.','..','scripts')
                                ,notebook=F){
  # TODO: interactively prompt for non-default title and author if interactive
  # TODO: ask if want to be prompted for the other arguments if interactive
  if(length(deps)>0){
    .files <- sapply(deps,function(ii) !is.null(find_path(ii,paths)));
    if(!all(.files)) stop(
"Of the files you specified in the 'deps' argument the following are missing:\n"
      ,paste0(deps[!.files],collapse=', '))};
  # make sure the template exists
  whichtemplate <- find_path(template,paths);
  if(is.null(whichtemplate)) stop(sprintf('Cannot find file %s',template));
  # make sure global.R exists
  if(missing(path_to_global)) path_to_global<-find_relpath('global.R'
                                                           ,c('.','..','../..')
                                                           ,recursive = T
                                                           ,normalize = F);
  if(length(path_to_global)==0) stop("Cannot find file 'global.R'");
  out <- sprintf(readLines(whichtemplate)
                 ,title # The title that will appear in the header
                 ,author # Author, ditto
                 ,format(date,'%d/%m/%Y') # Date, ditto
                 # packages (optional)
                 ,paste('c(',paste0("'",packages,"'",collapse=','),')')
                 ,file # ...so the file knows it's own name!
                 # dependencies on previously run files
                 ,paste('c(',paste0("'",deps,"'",collapse=','),')')
                 # location of global.R
                 ,path_to_global[1]
  );
  write(out,file);
  if(notebook) knitr::spin(file,knit=F);
}

find_path <- function(file,paths=c('.','..')){
  # get the basename of the file
  filebase <- basename(file);
  # generate a search-paths for this file, starting with the path component
  # of 'file'
  filedirs <- if(filebase!=file) dirname(file) else c();
  filedirs <- normalizePath(unique(c(filedirs,paths)));
  # return the first full path in which the file is found to exist
  for(ii in file.path(filedirs,filebase)) if(file.exists(ii)) return(ii);
  return(c());
}

#' Find a file in some adjacent path.
#'
#' @param file      Character string for one file or file path.
#' @param paths     Paths where to look for this file, optional character
#'                  vector.
#' @param pathexcl  Character vector of regular expressions for excluding
#'                  paths that would otherwise match.
#' @param recursive Whether to look in subdirectories for this file, logical.
#' @param lastonly  Logical. If more than one paths are found, and this is set
#'                  to \code{TRUE} (default) then only return the first path.
#' @param normalize Apply \code{normalizePath()} to results before returning.
#'
#' @return Character vector with path/s to file.
#' @export
#'
#' @examples \dontrun{find_filepath('.Rprofile')}
#'
find_filepath <- function(file,paths=c('..','../..','.')
                          ,pathexcl=c('backup')
                          ,recursive=FALSE,lastonly=TRUE,normalize=TRUE){
  filebase <- basename(file);
  paths<-c(if(filebase!=file && file.exists(dirname(file))){
    dirname(file)} else c(),paths);
  for(ii in paths){
    .paths <- unique(file.path(c(ii,list.dirs(ii,full.names = T,recursive=recursive))
                        ,filebase));
    if(length(pathexcl)>0) .paths <- setdiff(.paths,grepor(.paths,pathexcl));
    if(any(.found<-file.exists(.paths))){
      out <- .paths[.found];
      if(normalize) out <- normalizePath(out);
      out <- unique(out);
      if(lastonly) out <- utils::tail(out,1);
      return(out)};
  }
  # if returns empty vector means none found
  return(c());
}

find_relpath <- find_filepath;

#' Load or render script dependencies if not already cached.
#'
#' @param deps        Character vector of script names.
#' @param scriptdir   Where to look for each script.
#' @param cachedir    Where to save the \code{SCRIPTNAME.R.rdata} output from
#'                    each script and where to first look for cached results if
#'                    they exist.
#' @param fallbackdir Where to look for each script that is not found in
#'                    \code{scriptdir}.
#' @param envir       Environment in which to evaluate scripts (recommend
#'                    leaving unaltered).
#' @param loadfn      Function for loading \code{SCRIPTNAME.R.rdata} cached
#'                    results
#' @param rendfn      String with name of function to run on each of the files.
#'                    If it's not from the \code{base} library it should be
#'                    fully qualified (i.e. like the default value). The two
#'                    anticipated values for this argument are
#'                    \code{'rmarkdown::render'} for generating reports together
#'                    with cached output and \code{'source'} for generating just
#'                    the output. Other functions might also do useful things,
#'                    but no guarantees.
#' @param ...         Arguments to pass to the function specified in
#'                    \code{rendfn}
#'
#' @return Character vector of objects created and saved by the scripts that
#'         have been loaded into the working environment.
load_deps <- function(deps,scriptdir=getwd(),cachedir=scriptdir
                      ,fallbackdir='scripts',envir=parent.frame()
                      ,loadfn=if(exists('tload')) tload else load
                      ,rendfn='rmarkdown::render'
                      ,...){
  if(length(deps)==0||identical(deps,'')){message('No dependencies.');return();}
  # what objects got loaded by this function
  loadedobj=c();
  for(ii in deps){
    # if a cached .rdata file for this dependency cannot be found...
    if(is.null(iicached<-find_path(paste0(ii,'.rdata')
                                   ,c(cachedir,scriptdir,fallbackdir)))){
      # run that script and create one
      if(!is.null(iiscript<-find_path(ii,c(scriptdir,fallbackdir)))){
        # TODO: modify all files to write their cached results to a user
        # specified path if one is provided
        message(sprintf('Trying to initialize cache using script %s'
                        ,iiscript));
        .junk <- system(sprintf('R --no-restore -e ".workdir<-\'%s\'; source(\'%s\',chdir=TRUE)"'
                                ,normalizePath(cachedir)
                                ,normalizePath(iiscript)),intern = TRUE);
        # again try to find a valid path to it
        iicached <- find_path(paste0(ii,'.rdata')
                              ,c(cachedir,scriptdir,fallbackdir));
        } else{
          # if cannot find script, error
          stop(sprintf('The script %s was not found',ii));
        }};
    # if there is still no cached .rdata found, error
    if(is.null(iicached)){
      stop(sprintf('The cached file for %s could not be found',iiscript));
      # otherwise, the cached .rdata now exists one way or another, load it
    } else {
      loadedobj <- union(loadedobj,loadfn(normalizePath(iicached),envir=envir));
      message(sprintf('Loaded data for %s from %s',ii,iicached));
      };
  }
  return(loadedobj);
}


#' Search for all sample datasets in all currently installed packages.
#'
#' A goal of this function is to be able to quickly filter through currently
#' available datasets and find ones that meet your needs so you're not using
#' the same old `mtcars` and `iris` for everything. Warning, though: this
#' takes a long time to gather information about every single dataset registered
#' to every single library currently installed.
#'
#' @return A `data.frame` with columns `Package`: name of the package that
#'         provides that dataset, `LibPath`: path where that package is
#'         currently installed, `Item`: the name of the dataset, `Title`: a
#'         brief description of the dataset, `Class`: the class of the object
#'         listed in `Item` (if multiple classes, they are delimited by
#'         semicolons),`IsDataFrame`: whether or not the object listed in
#'         `Item` inherits from `data.frame`,`NumberNonNumeric`: number of
#'         columns that are not `numeric` (`character`, `factor`, `POSIXct`,
#'         etc.),`Rows`: number of rows in the `Item` if applicable,`Cols`:
#'         number of columns in the dataset in the `Item` if applicable.
#'
#' @param verbose If set to `TRUE` (default) prints the library and dataset
#'                name to the console as it reads each one.
#' @export
#'
#' @examples \dontrun{ allTheData() }
allTheData <- function(verbose=TRUE){
  # get all datasets provided by all loaded packages
  dt = as.data.frame(utils::data(package = .packages(all.available = TRUE))$results
                     ,stringsAsFactors=FALSE);
  # df = data.frame?, nnn = number not numeric, nr/nc = nrows, ncols
  dt[,c('Class','IsDataFrame','NumberNonNumeric','Rows','Cols')] <- NA;
  for(ii in unique(dt$Package)){
    for(jj in dt[dt$Package==ii,'Item']) {
      path <- paste0(ii,'::',jj);
      rows <- dt$Item==jj & dt$Package == ii;
      oo <- try(eval(parse(text=path)),silent=T);
      if(verbose) message(path);
      if(!is(oo,'try-error')){
        dt[rows,'Class'] <- paste0(class(oo),collapse=';');
        if(dt[rows,'IsDataFrame'] <- is(oo,'data.frame')){
          dt[rows,'NumberNotNumeric'] <- ncol(oo) -
            sum(sapply(oo,is.numeric))};
        dt[rows,c('Rows','Cols')]<-c(c(nrow(oo),NA)[1]
                                     ,c(ncol(oo),NA)[1]);}
    }};
  return(dt);
};

#' Read a file and split it according to lines containing the specified regular
#' expression
#'
#' @param file       Name of text file to read in.
#' @param sectionrxp Regular expression to identify section breaks in that file
#' @param targetrxp  Optional target expression so that only sections containing
#'                   that pattern are returned. Otherwise all sections returned.
#' @param subfrom    Regexp indicating what to replace in order to extract the
#'                   name of each section from its first line (if that line
#'                   starts with a comment character).
#' @param subto      Value with which to replace \code{subfrom}
#' @param namefn     Optional function to extract names from sections. Should
#'                   be able to accept three arguments, with the first one being
#'                   a character vector representing a section of the file. The
#'                   idea is for it assign a name to the section based on its
#'                   contents.
#' @param ...        Passed to \code{readLines}
#'
#' @return  List of character vectors, which may be named, but the names are not
#'          guaranteed legal or unique.
#'
#' @export
#'
#' @examples
#'
#' # Note that section names are not necessarily legal variable names and not
#' # necessarily unique
#' cat('foo','bar','baz','#### section 1',123,456,789,'#### section 2'
#'     ,'#### section 3','bob','boo','#### section 3','bla','bip'
#'     ,'#### section 4','The End',sep='\n',file='file_w_sections.txt')
#' filesections('file_w_sections.txt')
#'
#' # keep only sections 2 and 3
#' filesections('file_w_sections.txt',targetrxp='^#{4} section [23]')
#'
#' # keep only sections where at least one line has the string 'oo'
#' filesections('file_w_sections.txt',targetrxp='oo')
#'
#' # keep all sections EXCEPT 'section 2'
#' filesections('file_w_sections.txt',targetrxp='^#{4} section [^2]')
#'
filesections <- function(file,sectionrxp='^#{4,}',targetrxp
                         ,subfrom='^#*\\s*|\\s*#*$',subto=''
                         ,namefn=function(xx,subfrom,subto){
                           if(grepl("^#'",xx[1])||!grepl('^#',xx[1])) '' else {
                             gsub(subfrom,subto,xx[1])}}
                         ,...){
  oo <- readLines(file,...);
  sbreaks <- grep(sectionrxp,oo);
  if(! 1 %in% sbreaks) sbreaks <- c(1,sbreaks);
  oo <- Map(function(ii,jj) oo[ii:jj],sbreaks
            ,cumsum(diff(c(sbreaks,length(oo)+1))));
  names(oo) <- sapply(oo,namefn,subfrom=subfrom,subto=subto);
  if(!missing(targetrxp)){
    return(oo[sapply(oo,function(xx) any(grepl(targetrxp,xx)))])} else {
      return(oo)};
}

#' Extract key-value pairs from structured data
#'
#' Simply reads each line, finds the
#'
#' @param lines       Character vector (e.g. read from a file) where each
#'                    element is a line that could contain a key-value pair
#' @param key         Character vector of key names
#' @param joiner      Regexp for what to search for between each key and its
#'                    value
#' @param commentrxp  Regexp for stripping out comments
#'
#' @return Named list, where the names are in \code{key}
#' @export
#'
#' @examples
#' getkeyval(c('foo=1','bar=2','foo=DUPE','baz= 3 # comment',"bat = hello world"
#'             ,"# bat = old value")
#'           ,key=c('baz','foo','bat'))
#'
getkeyval <- function(lines,key,joiner='\\s*=\\s*',commentrxp='#'){
  lines <- gsub(paste0(commentrxp,'.*$'),'',lines);
  sapply(unique(key),function(xx){
    gsub(paste0('.*',xx,joiner,'(.*)'),'\\1'
         ,lines[grepl(paste0(xx,joiner),lines)])},simplify=FALSE)};


#' The recursive file move that R is missing for some reason?
#'
#' @param from      Directory from which to move all files.
#' @param to        Directory where to move them.
#' @param backupdir Name of directory where to back up any existing files in
#'                  \code{to} that would otherwise be overwritten with files
#'                  from \code{from}.
#' @param mvpatt    Character vector of regular expression patterns for files
#'                  to move to the backup directory even if they do not collide
#'                  with the new files.
#' @param cleanup   Whether to delete the \code{from} directory.
#'
#' @return NULL
#' @export
#'
#' @examples
#' # source directory tree
#' dir.create(file.path('foo','boo'),recursive=TRUE);
#' file.create(file.path('foo',letters[1:5]));
#' file.create(file.path('foo','boo','boing.txt'));
#' dir.create(file.path('bar','boo'),recursive=TRUE);
#' file.create(file.path('bar',letters[3:6]));
#' file.create(file.path('bar','boo','boing.txt'));
#'
#' mergedirs('foo','bar');
#'
#' dir.exists('foo') # FALSE
#' dir.exists('bar') # TRUE
#' list.files('bar',recursive=TRUE);
mergedirs <- function(from,to='.'
                      ,backupdir=paste0('backup.'
                                        ,format(Sys.time(),'%Y%m%d%H%M%S'))
                      ,mvpatt=c(),cleanup=getOption('cleanup',TRUE)){
  sourcefiles <- list.files(from,all.files = TRUE,no.. = TRUE
                            ,recursive = TRUE);
  currentfiles <- list.files(to,all.files=TRUE,no..=TRUE,recursive = TRUE);
  tobackup <- intersect(sourcefiles,currentfiles);
  if(length(mvpatt)>0){
    tobackup <- union(tobackup,grepor(currentfiles,mvpatt))};
  # recursively backup files with colliding names
  if(length(tobackup)>0 ){
    dir.create(file.path(to,backupdir));
    for(ii in setdiff(dirname(tobackup),'.')){
      dir.create(file.path(to,backupdir,ii),recursive = TRUE)};
    message('The following files will be moved to ',tobackup
            ,' in order to avoid being overwritten:\n'
            ,paste0(tobackup,collapse=', '));
    for(ii in tobackup) file.rename(file.path(to,ii)
                                    ,file.path(to,backupdir,ii));
  }
  # create the TO directories if necessary
  for(ii in setdiff(dirname(sourcefiles),'.')){
    if(!dir.exists(file.path(to,ii))) dir.create(file.path(to,ii))};
  # move file from the FROM locations
  file.rename(file.path(from,sourcefiles),file.path(to,sourcefiles));
  if(cleanup) unlink(from,recursive = TRUE,force = TRUE);
  return(NULL);
}


c()
