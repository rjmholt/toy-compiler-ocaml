#!/bin/bash

PLI="COMP90045"
STAGE="1b"

BEANZIP="bean.zip"
BEANFLD="beanCore"
FEEDBACK="feedback.txt"

if [ ! -e $PLI ]; then
  mkdir $PLI
elif [ ! -d $PLI]; then
  mv $PLI $PLI.backup
  mkdir $PLI
fi

unzip $BEANZIP
mv $BEANFLD $PLI
rm -f $BEANZIP

submit $PLI $STAGE $PLI/$BEANFLD/*

sleep 20

if [ -e $FEEDBACK ]; then
  mv $FEEDBACK $FEEDBACK.backup
fi

verify $PLI $STAGE > $FEEDBACK
