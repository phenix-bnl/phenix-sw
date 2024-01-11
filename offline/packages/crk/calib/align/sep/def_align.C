void def_align(){
  gSystem->Load("libRICH_Alignment.so");
  RICH_Alignment *ali = new RICH_Alignment();
  ali->UsePAD(1);
  ali->VerifByPhi(1);
  //  ali->SetVerbosity(2);
  ali->SetDSTLvl(2);
  ali->ImportOutput("tracks.root");
  ali->SetEval("evals.root");
  for(int i=-1;i<16;i++){
    ali->ProcessRings(i);
  }
  ali->Calc();
  ali->Verify();
  ali->Write("aligmentf.txt");
}
