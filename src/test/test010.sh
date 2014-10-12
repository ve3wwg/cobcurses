#!/bin/sh

TEST_CASE_1=CASE001
TEST_CASE_2=/usr/local/cobcurses/data/somwhere

export TEST_CASE_1
export TEST_CASE_2

exec ./dotest 010

# End
