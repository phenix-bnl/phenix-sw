void dstrun_align_RUN5(char *dstin = "RICH_Alignment_output.root"){
  gSystem->Load("./libRICH_Alignment.so");

  gStyle->SetPalette(1);
  gStyle->SetCanvasColor(0);
  gStyle->SetPadColor(0);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetPadBorderMode(0);
  gStyle->SetFrameBorderMode(0);
  gStyle->SetTitleFillColor(0);
  gStyle->SetStatColor(0);
  gStyle->SetTitleBorderSize(1);
  gStyle->SetStatBorderSize(1);

  RICH_Alignment *ali = new RICH_Alignment(dstin);
  ali->Init(dstin);
  ali->SetEval();
  ali->SetVerbosity(2);
  //   ali->VerifByPhi(1);
  //   ali->SetMaxent(5);

  // 1. mirror alignment
  ali->Process();

  // 2. just check without alignment using parameters
  /*
  ali->Init_alignment_parameters("alignment.dat");
  ali->Verify();
  */

  ali->Write();
}
