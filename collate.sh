#!/bin/bash

set -e

BEANFLD="beanCore"
BEANZIP="bean.zip"

BEANFILES="bean.ml
           bean_ast.ml \
           bean_ast.mli \
           bean_lex.mll \
           bean_parse.mly \
           bean_pprint.ml \
           bean_pprint.mli \
           Makefile \
           Makefile.depend \
           README.txt"


if [ -e $BEANFLD ]; then
  rm -rf $BEANFLD
fi

if [ -f $BEANZIP ]; then
  rm -f $BEANZIP
fi

mkdir $BEANFLD

for file in $BEANFILES; do
  cp $file $BEANFLD
done

zip -r -o $BEANZIP $BEANFLD

rm -rf $BEANFLD
