#!/bin/csh -f 

source phenix.csh

# Post-DST Analysis
root -b << EOF >& sample.log
.x sample.C;
.q
EOF

