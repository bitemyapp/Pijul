#! /usr/bin/env bash

set -x
set -e

testdir=$(mktemp -d -t pijul-test.XXXXXXXXXX)
test ! -z $testdir

function finish {
    if [ ! -z $testdir ]
    then rm -rf "$testdir"
    fi
}
trap finish EXIT

cd $testdir
mkdir a
cd a
$pijul init
echo saluton > file_to_record
echo malbonaĵo ĉi tie > file_to_leave_alone
$pijul add file_to_record
$pijul record -a --author "Emma Goldman" --name "patch1"
$pijul add file_to_leave_alone
$pijul record patch2 -a --author "赤染衛門" --name "patch2"
echo "yny" | $pijul rollback --author "Ὑπατία" --name "-patch2"
grep malbonaĵo file_to_leave_alone
cd ..
$pijul get a b
cd b
ls -a
grep saluton file_to_record
test ! -f file_to_leave_alone

rm -rf $testdir
