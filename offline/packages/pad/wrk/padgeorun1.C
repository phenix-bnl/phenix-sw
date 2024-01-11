//************************************************************
// Analysis macro written by the Pyrite GUI for PHOOL         
//************************************************************

void padgeorun1() {

  // Executing initialization and parameter macros
  gROOT->Macro("padgeoini1.C");
  gROOT->Macro("padgeopar1.C");

  mainIter.cd();
  PHNodeReset reset;
  gROOT->cd();

  // Build the PC geometries
  mPadDetGeo->BuildAllGeo();

  // Print the results
  mPadDetGeo->PrintParams();
  mPadDetGeo->PrintGeo(0,0);
  mPadDetGeo->PrintGeo(0,1);
  mPadDetGeo->PrintGeo(1,0);
  mPadDetGeo->PrintGeo(1,1);
  mPadDetGeo->PrintGeo(2,0);
  mPadDetGeo->PrintGeo(2,1);

  mPadDetGeo->PutIntoFile();

}

