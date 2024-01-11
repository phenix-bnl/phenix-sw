#include <strstream>

TH1F *h[5120];
int nhist = 5120;

merge_12399_12468(const char *ofname="merge12399_12468.root") {
  void add_h1 (char*);
  TFile *outfile = TFile::Open (ofname, "recreate", "no_field", 1);

  int i;
  for(i=0;i<nhist;i++) {
    strstream sname;
    sname << "sADC" << i;
    h[i] = new TH1F (sname.str(),sname.str(),256,-100.,1024.);
    //    cout << sname.str() << "is created" <<endl;
  }

  add_h1("HISTOS/run12399_online_monitor_histos.root");
  add_h1("HISTOS/run12404_online_monitor_histos.root");
  add_h1("HISTOS/run12409_online_monitor_histos.root");
  add_h1("HISTOS/run12411_online_monitor_histos.root");
  add_h1("HISTOS/run12414_online_monitor_histos.root");
  add_h1("HISTOS/run12420_online_monitor_histos.root");
  add_h1("HISTOS/run12424_online_monitor_histos.root");
  add_h1("HISTOS/run12426_online_monitor_histos.root");
  add_h1("HISTOS/run12428_online_monitor_histos.root");
  add_h1("HISTOS/run12431_online_monitor_histos.root");
  add_h1("HISTOS/run12464_online_monitor_histos.root");
  add_h1("HISTOS/run12466_online_monitor_histos.root");
  add_h1("HISTOS/run12468_online_monitor_histos.root");
  
  outfile->Write();
  outfile->Close();
  delete outfile;
  
}

void add_h1(char *fname) {
  tf = new TFile(fname);
  int i;
  for(i=0;i<nhist;i++) {
    strstream hname;
    hname << "ADC" << i;
    h[i]->Add((TH1F*)tf->Get(hname.str()));
  }
  cout << "file "<<fname<<" is added" << endl;
  delete tf;
}
