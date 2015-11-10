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
$pijul record -a --author "Frantz Fanon" --name "patch1"
$pijul move file_to_record file_after_move
test -f file_after_move
test ! -f file_to_record
cd ..
$pijul get a b
cd b
grep saluton file_to_record
test ! -f file_after_move
cd ../a

$pijul record -a --author "Michel Foucault" --name "such a moving patch"

$pijul push -a ../b
cd ../b

$pijul debug
if test ! -z $pdf_out
then dot -Tpdf -o $pdf_out/test.pdf debug
fi


grep saluton file_after_move
test ! -f file_to_record
rm -rf $testdir
