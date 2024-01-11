void loadCosmicCommit(int runnumber = 70076){
gSystem->Load("libPgCal.so");
gSystem->SetIncludePath(" -I$OFFLINE_MAIN/include ");
gROOT->LoadMacro("commitCbCCosmicProd.C+");
commitCbCCosmicProd("/phenix/workarea/rjnewby/fcal/run3/funmacros/wrk/fcal-calib-69946-CRC2-2.root",67282,80312);
 return;
}
