#include <stdio.h>
#include <stream.h>
#include "SumupTH.hh"
#include "EmcCalibratorTOFObj.hh"

ClassImp(EmcCalibratorTOFObj)

//------------------------------------------------------------
EmcCalibratorTOFObj::EmcCalibratorTOFObj(): EmcCalibratorTOF(){
  _initialized = false;
  return;
}
//------------------------------------------------------------
EmcCalibratorTOFObj::EmcCalibratorTOFObj(const char* name, const char* title) : EmcCalibratorTOF(name,title){
  _initialized = false;
  return;
};
//------------------------------------------------------------
EmcCalibratorTOFObj::EmcCalibratorTOFObj(const char* name, const char* title,int nch,
					 int xnbins,float xlow,float xup,int ynbins,float ylow,float yup)
  : EmcCalibratorTOF(name,title,nch,ynbins,ylow,yup){
  Initialize(nch,xnbins,xlow,xup,ynbins,ylow,yup);
  return;
};
//------------------------------------------------------------
EmcCalibratorTOFObj::~EmcCalibratorTOFObj(){
  if( _initialized ){
    delete _th2xch;
    delete _th2xch_afttwr;
  }
  return;
};
//------------------------------------------------------------
bool EmcCalibratorTOFObj::Initialize(int nch,int xnbins,float xlow,float xup,int ynbins,float ylow,float yup) {
  cout<<" "<<GetName()<<"::EmcCalibratorTOFObj::Initialize()... "<<endl;
  char hname[128],htitle[128];
  //
  sprintf(hname,"%s_th2xch",GetName());
  sprintf(htitle,"%s th2xch",GetTitle());
  _th2xch = new TH2Xch(hname,htitle,nch,xnbins,xlow,xup,ynbins,ylow,yup);
  //
  sprintf(hname,"%s_th2xch_afttwr",GetName());
  sprintf(htitle,"%s th2xch after twr-by-twr",GetTitle());
  _th2xch_afttwr = new TH2Xch(hname,htitle,nch,xnbins,xlow,xup,ynbins,ylow,yup);

  _initialized = true;
  return true;
};
//------------------------------------------------------------
int EmcCalibratorTOFObj::AnalyzeCalibObj(CalibObj* calibobj,char* opt){
  if(! _initialized ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() is not initialized ."<<endl;
    return 0;
  }
  char* opt_d;
  opt_d = strstr(opt,"d");  //DEBUG option
  char* opt_0;
  opt_0 = strstr(opt,"0");  // Using current run-by-run T0
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Using QA info
  char* opt_t;
  opt_t = strstr(opt,"t"); // Using p(re)shift before the run-by-run...
  float run_shift,run_shift_err;
  bool found = false;
  int found_bin = -1;

  TH2Xch* th2xch = dynamic_cast<TH2Xch*>(calibobj);
  if( th2xch == NULL ){
    cout<<" Error: "<<GetName()<<"::AnalyzeCalibObj() get no TH2Xch* object. "<<endl;
    return 0;
  }
  cout<<" "<<GetName()<<"::AnalyzeCalibObj(): Processing. (run,seq) = ("
      <<_cur_run<<","<<_cur_seq<<") : "<<th2xch->GetName()<<endl;
  //============================================================
  if( opt_0 ){
    found = _th0xrun_shift.FindNear(_cur_run,run_shift,run_shift_err,100);
    cout<<" "<<GetName()<<"::AnalyzeCalibObj():: option 0 means using current setup... ";
    if( found ) cout<<"  run_shift = "<<run_shift<<" +- "<<run_shift_err<<endl;
    else cout<<" Warning:: can't find nearest run # "<<endl;
  //============================================================
  } else { // ............................... !opt_0
    cout<<" "<<GetName()<<"::AnalyzeCalibObj() doesn't support without \"0\" option "<<endl;
    return 0;
  }
  //============================================================
  // Sumup th2xch into sumth2....
  TGraph* gshift = new TGraph(_nch);
  int n = _nch;
  // ---------------- optiont_t
  if( opt_t ){
    while( n-- ){
      int bin = _h_twr_pshift->FindBin(n);
      float t0_shift = _h_twr_shift->GetBinContent(bin);
      gshift->SetPoint(n, n, run_shift + t0_shift );
    }
  } else {
    while( n-- )
      gshift->SetPoint(n,n,run_shift);
  }
  // ---------------- optiont_qa
  if( opt_qa ){  // && GetNQAtwr() == _nch ){
    n = _nch;
    int stat = 0;
    while( n-- )
      if( GetQAtwr(n) > 0 ){
	gshift->SetPoint(n,n,-10000);
	stat++;
      }
    if( opt_d )cout<<" "<<GetName()<<"::AnalyzeCalibObj() QA selection applied for "<<stat<<" / "<<_nch<<endl;
  };
  //---- Main part ----------------------------------------------------------------------
  cout<<" "<<GetName()<<"::AnalyzeCalibObj() calling TH2Xch::Sumup()... "<<endl;
  //  if( opt_t || opt_qa )
  _th2xch->SumupY(th2xch,gshift,opt);
  //  else
  //_th2xch->SumupY(th2xch,run_shift,opt);
  //---- Main part ----------------------------------------------------------------------
  delete gshift;
  if( opt_d ){
    int n = ( gshift->GetN() < 3 ? gshift->GetN() : 3 );
    double* y = gshift->GetY();
    while( n-- ) cout<<" "<<GetName()<<"::AnalyzeCalibObj() ghisft->y["<<n<<"] = "<<y[n]<<endl;
    TCanvas* c1 = new TCanvas("c1","test");
    c1->Divide(2);
    c1->cd(1);
    th2xch->_h_all->Draw("colz");
    c1->cd(2);
    _th2xch->_h_all->Draw("colz");
    c1->cd();
    c1->Update();
    getchar();
    delete c1;
  }

  if( opt_d ){ // FIX.ME....
    cout<<" "<<GetName()<<" AnalyzeCalibObj():: run_shift = "<<run_shift<<" +- "<<run_shift_err<<endl;
    cout<<"  --------------- graph --------------- "<<endl;
    _th0xrun_shift._gra.Print();
    cout<<"  ------------------------------------- "<<endl;
  }
  //
  return(1);
}
//------------------------------------------------------------
int EmcCalibratorTOFObj::Reset(char* opt){
  EmcCalibratorTOF::Reset(opt);
  _th2xch->Reset(opt);
  _th2xch_afttwr->Reset(opt);
  return 1;
};
//------------------------------------------------------------
//
//

