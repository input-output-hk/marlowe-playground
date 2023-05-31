#!/usr/bin/env bash
# Note: The reporter won't generate on failure, because it's in a post-cucumber hook,
# and technically because of the way that node works, it won't call post-hooks,
#  if the underlying script it's running, in our case, cucumber, fails. So we use
#  this file to get around this problem.


#cucumber tag
tag=$1

yarn run cucumber --profile "$tag"  || yarn run postcucumber
