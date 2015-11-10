#! /usr/bin/env bash

set -x
set -e

testdir=$(mktemp -d -t pijul-test.XXXXXXXXXX)
test ! -z $testdir

cd $testdir
mkdir a
cd a
$pijul init
echo saluton > file_to_record
$pijul add file_to_record
$pijul record -a --author "Paulina Chiziane" --name "patch1"
echo revert_me > file_to_record
$pijul revert -a
ls
cat file_to_record
grep saluton file_to_record
grep -v revert_me file_to_record

rm -rf $testdir
