#include <strstream>

TH1F *h[5120];
int nhist = 5120;

merge_11303_12127(const char *ofname="merge11303_12127.root") {
  void add_h1 (char*);
  TFile *outfile = TFile::Open (ofname, "recreate", "no_field", 1);

  int i;
  for(i=0;i<nhist;i++) {
    strstream sname;
    sname << "sADC" << i;
    h[i] = new TH1F (sname.str(),sname.str(),256,-100.,1024.);
    //    cout << sname.str() << "is created" <<endl;
  }

  add_h1("HISTOS/run11303_online_monitor_histos.root");
  add_h1("HISTOS/run11305_online_monitor_histos.root");
  add_h1("HISTOS/run11309_online_monitor_histos.root");
  add_h1("HISTOS/run11313_online_monitor_histos.root");
  add_h1("HISTOS/run11318_online_monitor_histos.root");
  add_h1("HISTOS/run11797_online_monitor_histos.root");
  add_h1("HISTOS/run11801_online_monitor_histos.root");
  add_h1("HISTOS/run11805_online_monitor_histos.root");
  add_h1("HISTOS/run11807_online_monitor_histos.root");
  add_h1("HISTOS/run11814_online_monitor_histos.root");
  add_h1("HISTOS/run11891_online_monitor_histos.root");
  add_h1("HISTOS/run11893_online_monitor_histos.root");
  add_h1("HISTOS/run11908_online_monitor_histos.root");
  add_h1("HISTOS/run11928_online_monitor_histos.root");
  add_h1("HISTOS/run11951_online_monitor_histos.root");
  add_h1("HISTOS/run11955_online_monitor_histos.root");
  add_h1("HISTOS/run11965_online_monitor_histos.root");
  add_h1("HISTOS/run11967_online_monitor_histos.root");
  add_h1("HISTOS/run11970_online_monitor_histos.root");
  add_h1("HISTOS/run12010_online_monitor_histos.root");
  add_h1("HISTOS/run12026_online_monitor_histos.root");
  add_h1("HISTOS/run12087_online_monitor_histos.root");
  add_h1("HISTOS/run12123_online_monitor_histos.root");
  add_h1("HISTOS/run12127_online_monitor_histos.root");

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
