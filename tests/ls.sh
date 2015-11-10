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
echo malbonaĵo ĉi tie > file_to_leave_alone
$pijul add file_to_record
$pijul record -a --author "Roger Martin du Gard" --name "patch1"
$pijul ls | grep file_to_record
$pijul ls | grep -v file_to_leave_alone

