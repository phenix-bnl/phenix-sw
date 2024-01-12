#!/bin/csh
source ~/.login
cd /direct/phenix+data31/phnxreco/embedding_run4/production
root -b<<EOF
char *mcdst= "/phenix/data11/embedding/merged_simDST/SIMDST1_proton_vtx0.root";
char *ntname = "proton.root";
.x embed.C(0);
EOF
