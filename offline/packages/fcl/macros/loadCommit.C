void loadCommit(int runnumber = 70076){
gSystem->Load("libPgCal.so");
gSystem->SetIncludePath(" -I$OFFLINE_MAIN/include ");
gROOT->LoadMacro("commitCbCZdcXtalkProd.C+");
commitCbCZdcXtalkProd("/phenix/workarea/rjnewby/fcal/run3/funmacros/wrk/CbCXtalk_FclSummary_70076.root",67282,76284);
 return;
}
