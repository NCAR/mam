#!/bin/bash

# exit on error
set -e
# turn on command echoing
set -v
# make sure that the current directory is the one where this script is
cd ${0%/*}

exec_str="../../../test_mock_radiation"

if ! $exec_str; then
  echo FAIL
  exit 1
else
  echo PASS
  exit 0
fi
