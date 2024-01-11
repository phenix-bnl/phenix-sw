#!/opt/phenix/bin/python

import os

pid = "pions_n0"
filtMacro = "filter/filter.C"
recalMacro = "recal/runRecal.C"
d82 = "/phenix/hp/data82/phnxhp01/andrew/emb/eval/"
inChain = "{0}{1}.root".format(d82, pid)
inChainSm = "{0}{1}_com.root".format(d82, pid)

# Reduce chained EmbedMcRecoTrack ntuple from 350+ variables to a few for speed
command = "root -b -q \'{0}( \"{1}\" )\'".format(filtMacro,inChain)
print(command)
os.system(command)

# Recalibrate PC3 match variables in filtered ntuple
command = "root -b -q \'{0}( \"{1}\" )\'".format(recalMacro,inChainSm)
print(command)
os.system(command)
