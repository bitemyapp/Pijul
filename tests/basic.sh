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

cat .pijul/patches/*

$pijul debug

if test ! -z $pdf_out
then dot -Tpdf -o $pdf_out/basic.pdf debug
fi

cd ..
$pijul get a b
cd b
cat file_to_record
grep saluton file_to_record

rm -rf $testdir
