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
echo malbonaĵo ĉi tie > file_to_remove
$pijul add file_to_remove
$pijul record -a --author "Roger Martin du Gard" --name "patch2"
$pijul add file_to_record
$pijul record -a --author "Roger Martin du Gard" --name "patch1"
$pijul remove file_to_remove
$pijul record -a --author "Roger Martin du Gard" --name "patch2"
test -f file_to_remove

cd ..
$pijul get a b
cd b
grep saluton file_to_record
test ! -f file_to_remove

rm -rf $testdir
