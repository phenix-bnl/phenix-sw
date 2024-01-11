void dstrun_align_RUN9(char *dstin = "RICHAlignment_track_output.root",
		       char *fout = "Evalout_Run8Alignment.root")
{
  gSystem->Load("../install/lib/libRICH_Alignment.so");

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
  ali->SetEval(fout);
  ali->SetVerbosity(3);
  //   ali->VerifByPhi(1);
  //   ali->SetMaxent(5);

  // 1. mirror alignment
  // 
  // Initial alignment parameters is used the previous Run's data
  // After some iterations, output files is produced.
  //
  ali->Init_alignment_parameters_0th("alignment_Run8.dat");
  ali->Process();

  // 2. just check without alignment using parameters
  //    ^^^^^^^^^^
  //
  //  ali->Init_alignment_parameters("alignment_Run8.dat");
  //  ali->Verify();
  

  ali->Write();
}
