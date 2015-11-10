#! /usr/bin/env bash

set -x
set -e

testdir=$(mktemp -d -t pijul-test.XXXXXXXXXX)
test ! -z $testdir

cd $testdir
mkdir a
cd a
$pijul init
echo saluton > file
echo malbão >> file
echo probolon >> file

$pijul add file
$pijul record -a --author "Jean-Paul" --name "L'existentialisme"


cd ..
$pijul get a b
cd b
echo saluton > file
echo probolon >> file
$pijul record -a --author "Albert" --name "J'en ai fait quand j'étais jeune"
echo ynyyy | $pijul rollback --author "Albert" --name "J'en ai fait quand j'étais jeune"

cd ../a
echo saluton > file
echo probolon >> file
$pijul record -a --author "Jean-Paul" --name "Bof"

$pijul pull -a ../b

if test ! -z $pdf_out
then
    $pijul debug
    dot -Tpdf -o $pdf_out/test.pdf debug
fi

cd ..
$pijul get a c
cd c

cat file

rm -rf $testdir
