#!/bin/bash

set -e

BEAN_ARCHIVE="parser.tar.gz"

BEANFLD="beanCore"

BEANFILES="bean.ml \
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

mkdir $BEANFLD

for file in $BEANFILES; do
  cp $file $BEANFLD
done

cd $BEANFLD
tar cvzf $BEAN_ARCHIVE --owner=root --group=student $BEANFILES
cd ..
mv $BEANFLD/$BEAN_ARCHIVE .
rm -rf $BEANFLD
