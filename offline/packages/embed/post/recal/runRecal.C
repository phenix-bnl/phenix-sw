void runRecal( char* inFileName = "/phenix/hp/data82/phnxhp01/andrew/emb/eval/pions_n0_com.root",  
	       char* pid = "pions_n0")
{
  gROOT->ProcessLine(".L /phenix/u/workarea/adare/offline/packages/embed/post/recal/Recal.C+");

  const int ncentbins = 3;
  double centbin[ncentbins+1] = {0, 20, 60, 93.1};
  const int nptbins = 8;
  double ptbin[nptbins+1] = {0.4, 0.8, 1.2, 1.6, 2, 3, 5, 7, 10};
//   const int nptbins = 7;
//   double ptbin[nptbins+1] = {0.0, 0.5, 1, 2, 3, 5, 7, 10};

  TFile* infile = new TFile(inFileName, "read");
  cout << "Running on " << infile->GetName() << endl;

  TNtuple* nt = (TNtuple*)infile->Get("EmbedMcRecoTrack");
  Recal r(nt, Form("recal_%s.root", pid),  // Pass in tree and outfile name
	  ncentbins, centbin, nptbins, ptbin);
  
  cout << "bookHistos() " << endl;
  r.bookHistos();

  cout << "matchRecal() " << endl;
  r.matchRecal();
  
  cout << "Loop() " << endl;
  r.Loop();
  
  return;
}
