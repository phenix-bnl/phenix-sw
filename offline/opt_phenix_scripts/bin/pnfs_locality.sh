#!/bin/bash
file=`basename $1`
dir=`dirname $1`
cat $dir/".(get)($file)(locality)"
