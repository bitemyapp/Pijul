#! /usr/bin/env bash

set -x
set -e

testdir=$(mktemp -d -t pijul-test.XXXXXXXXXX)
test ! -z $testdir

cd $testdir
mkdir a
cd a
$pijul init
echo blabla > a
echo blibli >> a
echo blublu >> a
$pijul add a
$pijul record -a --author "marcel dupont" --name "+a"
echo blibli > a
echo blublu >> a
$pijul record -a --author "marcel dupont" --name "-blibli"

$pijul debug

if test ! -z $pdf_out
then dot -Tpdf -o $pdf_out/basic2_0.pdf debug
fi

echo blublu > a
$pijul record -a --author "marcel dupont" --name "+blibli"

cd ..
$pijul get a b
cd b

$pijul debug

if test ! -z $pdf_out
then dot -Tpdf -o $pdf_out/basic2_1.pdf debug
fi

cat a

rm -Rf $testdir
