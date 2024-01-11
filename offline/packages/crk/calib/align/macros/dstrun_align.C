void dstrun_align(char *dstin = "DST.root",int udst=2){
  gSystem->Load("libRICH_Alignment.so");
  RICH_Alignment *ali = new RICH_Alignment(dstin);
  ali->UsePAD(1);
  ali->VerifByPhi(1);
  ali->SetVerbosity(2);
  ali->SetDSTLvl(udst); 
  ali->ImportHotPMTList("rich_hotPMTlist.txt");
  //    ali->SetMaxent(5);
  ali->SetEval();
  ali->Process();
}
