#include <ZdcCalTiming.h>

#include <string>
#include <fstream>
#include <TF1.h>

using namespace std;

ZdcCalTiming::ZdcCalTiming()
{
  InitHistograms();

  zdccalib = 0;
}

ZdcCalTiming::ZdcCalTiming(ZdcCalib *z)
{
  InitHistograms();

  zdccalib = z;
}

ZdcCalTiming::~ZdcCalTiming()
{
  delete zvtxbvtxcheck;
  delete zt0bt0check;
  return;
}

void ZdcCalTiming::InitHistograms()
{
  zvtxbvtxcheck = new TH1F("zvtxbvtxcheck","ZDC-BBC vertex",400,-20.,20.);
  zt0bt0check = new TH1F("zt0bt0check","ZDC-BBC T0",3000,-10.,20.);

  zvtx_offset = 0.;
  t0_offset = 0.;
}

int ZdcCalTiming::Fill(const float d_zdc_bbc_zvtx, const float d_zdc_bbc_t0)
{
  zvtxbvtxcheck->Fill( d_zdc_bbc_zvtx );
  zt0bt0check->Fill( d_zdc_bbc_t0 );

  return 1;
}

int ZdcCalTiming::CalculateConstants()
{
  // for p+p there is a second gaussian from the low energy background
  //TF1 gaus2("gaus2","gaus+gaus(3)",-20.,20.);
  //gaus2.SetParameters(100,0,2,10,0,8);
  TF1 gaus2("gaus2","gaus",-20.,20.);
  gaus2.SetParameters(100,0,2);
  gaus2.SetLineColor(2);
  gaus2.SetLineWidth(1);

  zvtxbvtxcheck->Fit(&gaus2);
  cout << "vertex offsets (mean1 rms1 mean2 rms2 red_chi2)" << endl;
  cout << gaus2.GetParameter(1) << "\t" << gaus2.GetParameter(2) << "\t"
       << gaus2.GetParameter(4) << "\t" << gaus2.GetParameter(5)
       << "\t" << gaus2.GetChisquare()/gaus2.GetNDF() << endl;
  zvtx_offset = gaus2.GetParameter(1);

  //gaus2.SetParameters(100,0,0.1,10,0,1);
  gaus2.SetParameters(100,0,0.1);
  zt0bt0check->Fit(&gaus2);
  cout << "t0 offsets (mean1 rms1 mean2 rms2 red_chi2)" << endl;
  cout << gaus2.GetParameter(1) << "\t" << gaus2.GetParameter(2) << "\t"
       << gaus2.GetParameter(4) << "\t" << gaus2.GetParameter(5)
       << "\t" << gaus2.GetChisquare()/gaus2.GetNDF() << endl;
  t0_offset = gaus2.GetParameter(1);

  return 1;
}

void ZdcCalTiming::SetCalibration(ZdcCalib *z)
{
  zdccalib = z;
}

int ZdcCalTiming::SaveToFile(const char *fname)
{
  // output files
  string name = "ZdcCalib.zvtx"; 
  name += fname;
  ofstream zvtxfile( name.c_str() );
  zvtxfile << zvtx_offset << "  0.0 0" << endl;
  zvtxfile.close();

  name = "ZdcCalib.tzero"; 
  name += fname;
  ofstream tzerofile( name.c_str() );
  tzerofile << t0_offset << "  0.0 0" << endl;

  return 1;
}
