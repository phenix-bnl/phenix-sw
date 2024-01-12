#!/bin/csh
source ~/.login
cd /phenix/data31/phnxreco/embedding_run4/production
root -b<<EOF
.x DiceSimDST.C
EOF

