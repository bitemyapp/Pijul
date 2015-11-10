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
echo malbonaĵo >> file_to_record
echo ĉi tie >> file_to_record
$pijul add file_to_record
$pijul record -a --author "حافظ شیرازی" --name "patch1"
cd ..
$pijul get a b

cd a
echo saluton > file_to_record
echo grouptan >> file_to_record
echo ĉi tie >> file_to_record
$pijul record -a --author "فردوسی" --name "patch2"

cd ../b
yes | $pijul pull ../a

#echo ynyy | $pijul rollback --author "فردوسی" --name "rollback"

if test ! -z $pdf_out
then
    $pijul debug
    dot -Tpdf -o $pdf_out/test.pdf debug
fi

rm -rf $testdir
