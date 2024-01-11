void filter(char* inFile = "/phenix/hp/data82/phnxhp01/andrew/emb/eval/pions_n0.root")
{
  string outFile(inFile);
  int indx = outFile.find(".root");
  outFile.replace(indx, 9, "_com.root"); // 9 chars in replacement string

  gROOT->ProcessLine(".L /phenix/u/workarea/adare/offline/packages/embed/post/filter/EmbedMcRecoTrack.C");

  TFile* infile = new TFile(Form("%s", inFile), "read");
  TNtuple* nt = (TNtuple*)infile->Get("EmbedMcRecoTrack");
  EmbedMcRecoTrack t(nt);
  t.Loop(outFile.c_str());
  
  return;
}
