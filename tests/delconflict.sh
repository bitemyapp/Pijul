#! /usr/bin/env bash

set -x
set -e

testdir=$(mktemp -d -t pijul-test.XXXXXXXXXX)
test ! -z $testdir

cd $testdir
mkdir a
cd a
$pijul init
echo a > a
echo b >> a
echo c >> a
echo d >> a
$pijul add a
$pijul record -a --author "René" --name "oui"

cd ..
$pijul get a b
cd b
echo a > a
echo d >> a
$pijul record -a --author "Marcelle" --name "non"


cd ../a
echo a > a
echo b >> a

echo x >> a

echo c >> a
echo d >> a
$pijul add a
$pijul record -a --author "René" --name "si"


cd ../b
yes | $pijul pull ../a

$pijul debug
if test ! -z $pdf_out
then dot -Tpdf -o $pdf_out/delconflict.pdf debug
fi

cat a

grep x a
grep ========================== a


rm -Rf $testdir
