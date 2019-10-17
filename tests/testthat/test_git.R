context("git wrappers");

test_that("git",{
  expect_is(origstate <-gst(VERBOSE=FALSE,print=FALSE),'list');
  expect_gt(length(origstate$branch),0);
  expect_silent(git_newbranch('foo',VERBOSE=FALSE));
  expect_equal(git_status(print=FALSE,VERBOSE=FALSE)$branch,'foo');
  write('hello world','testfile.txt');
  expect_message(git_add('testfile.txt'),'git add testfile.txt'
                 ,'testfile.txt now being tracked on branch foo');
  #
  expect_silent(git_commit('testfile.txt','testing file commit'
                            ,autopush = FALSE,VERBOSE=FALSE));
  expect_true('testfile.txt' %in% git_lsfiles('test*.txt',intern=TRUE
                                              ,VERBOSE=FALSE)
              ,'testfile.txt showing up in a list of files git is tracking');
  expect_silent(git_move('testfile.txt','testfile00.txt',VERBOSE=FALSE));
  expect_false('testfile.txt' %in% git_lsfiles('test*.txt',intern=TRUE
                                               ,VERBOSE=FALSE));
  expect_false(file.exists('testfile.txt'));
  expect_true('testfile00.txt' %in% git_lsfiles('test*.txt',intern=TRUE
                                                ,VERBOSE=FALSE));
  expect_true(file.exists('testfile00.txt'));
  expect_silent(git_other('rm -f testfile00.txt',VERBOSE=FALSE));
  expect_false(file.exists('testfile00.txt'));
  expect_match(git_checkout(origstate$branch,'2>&1',VERBOSE=FALSE,intern=TRUE)
                 ,'Switched to branch',all=FALSE);
  expect_match(git_other('branch -D foo 2>&1',VERBOSE=FALSE,intern=TRUE)
               ,'Deleted branch foo',all=FALSE);
})
