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
echo malbonaĵo ĉi tie >> file
$pijul add file
$pijul record -a --author "Jean-Paul" --name "L'existentialisme"


cd ..
$pijul get a b
cd b
echo saluton > file
echo malbão >> file
echo probolon >> file
echo malbonaĵo ĉi tie >> file
$pijul record -a --author "Albert" --name "J'en ai fait quand j'étais jeune"

cd ../a
echo saluton > file
echo probolon >> file
echo raatözato >> file
echo raatöz!ato >> file
echo malbonaĵo ĉi tie >> file
$pijul record -a --author "Jean-Paul" --name "Bof"
$pijul pull -a ../b

if test ! -z $pdf_out
then
    $pijul debug
    dot -Tpdf -o $pdf_out/basic.pdf debug
fi

cd ..
$pijul get a c
cd c

# echo "saluton
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>
# malbão
# probolon
# ==========================
# probolon
# raatözato
# raatöz!ato
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<
# malbonaĵo ĉi tie" > test

# Problem here: the order in which conflicts are output is not deterministic, so this diff might differ or not. Adding time stamps or sequence numbers is not sufficiently one-way/highly entropic to justify the extra bytes in node keys.

#diff -a test file

rm -rf $testdir
