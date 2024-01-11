void run_align(char *dstin = "udst_elc.root",char *evalout,int offset,int nentry){
  gSystem->Load("libRICH_Alignment.so");
  RICH_Alignment *ali = new RICH_Alignment(dstin);
  ali->UsePAD(1);
  ali->VerifByPhi(1);
  //  ali->SetVerbosity(2);
  ali->SetDSTLvl(2);
  ali->ImportHotPMTList("rich_hotPMTlist.txt");
  ali->SetEval(evalout);
  ali->SetEventOffset(offset);
  ali->SetMaxent(nentry);
  ali->ProcessTracks();
}
