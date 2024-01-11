#include <strstream>

TH1F *h[5120];
int nhist = 5120;

merge_12134_12397(const char *ofname="merge12134_12397.root") {
  void add_h1 (char*);
  TFile *outfile = TFile::Open (ofname, "recreate", "no_field", 1);

  int i;
  for(i=0;i<nhist;i++) {
    strstream sname;
    sname << "sADC" << i;
    h[i] = new TH1F (sname.str(),sname.str(),256,-100.,1024.);
    //    cout << sname.str() << "is created" <<endl;
  }

  add_h1("HISTOS/run12134_online_monitor_histos.root");
  add_h1("HISTOS/run12139_online_monitor_histos.root");
  add_h1("HISTOS/run12143_online_monitor_histos.root");
  add_h1("HISTOS/run12146_online_monitor_histos.root");
  add_h1("HISTOS/run12149_online_monitor_histos.root");
  add_h1("HISTOS/run12153_online_monitor_histos.root");
  add_h1("HISTOS/run12156_online_monitor_histos.root");
  add_h1("HISTOS/run12161_online_monitor_histos.root");
  add_h1("HISTOS/run12167_online_monitor_histos.root");
  add_h1("HISTOS/run12169_online_monitor_histos.root");
  add_h1("HISTOS/run12275_online_monitor_histos.root");
  add_h1("HISTOS/run12277_online_monitor_histos.root");
  add_h1("HISTOS/run12280_online_monitor_histos.root");
  add_h1("HISTOS/run12299_online_monitor_histos.root");
  add_h1("HISTOS/run12303_online_monitor_histos.root");
  add_h1("HISTOS/run12306_online_monitor_histos.root");
  add_h1("HISTOS/run12307_online_monitor_histos.root");
  add_h1("HISTOS/run12322_online_monitor_histos.root");
  add_h1("HISTOS/run12323_online_monitor_histos.root");
  add_h1("HISTOS/run12324_online_monitor_histos.root");
  add_h1("HISTOS/run12325_online_monitor_histos.root");
  add_h1("HISTOS/run12335_online_monitor_histos.root");
  add_h1("HISTOS/run12350_online_monitor_histos.root");
  add_h1("HISTOS/run12397_online_monitor_histos.root");

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
