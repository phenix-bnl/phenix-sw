#include <ZdcCalSlew.h>

#include <packet.h>
#include <ZdcCalib.hh>

#include <TFile.h>
#include <TGraphErrors.h>
#include <TH1.h>
#include <TH2.h>
#include <TProfile.h>
#include <TSpline.h>

#include <cmath>
#include <fstream>

static const int maxchannels = 40;
static const float adcmin = -0.5;	// min adc 
static const float adcmax = 1000.5;	// max adc

ZdcCalSlew::ZdcCalSlew()
{
  InitHistograms();

  zdccalib = 0;
}

ZdcCalSlew::ZdcCalSlew(ZdcCalib *z)
{
  InitHistograms();

  zdccalib = z;
}

void ZdcCalSlew::InitHistograms()
{
  TString name;

  for (int itdc=0; itdc<2; itdc++)
    {
      for (int ich=0; ich<8; ich++)
        {
          name = "zslewprofch"; name += ich;
          name += "tdc"; name += itdc;
          TProfile *tempprof = new TProfile(name,name,4096,-0.5,4095.5);
          tempprof->SetLineColor(2);
          slewprof.push_back( tempprof );

          name = "zslewch"; name += ich;
          name += "tdc"; name += itdc;
          TH2F *temphist = new TH2F(name,name,560,300.,4100.,250,-10.,15.);
          slewhist.push_back( temphist );
        }
    }

  for (int itdc=0; itdc<2; itdc++)
    {
      for (int ich=0; ich<8; ich++)
        {
          name = "ztdiffch"; name += ich; name += "tdc"; name += itdc;
          TH1F *temphist = new TH1F(name,name,2000,-10.,10.);
          ztdiff.push_back( temphist );
        }
    }
}

int ZdcCalSlew::FillFromFile(const char *zdctreefname)
{
  TFile *infile = new TFile(zdctreefname,"READ");
  FillFromFile( infile );
  return 1;
}
int ZdcCalSlew::FillFromFile(TFile *infile)
{
  TString name;
  for (int itdc=0; itdc<2; itdc++)
    {
      for (int ich=0; ich<8; ich++)
        {
          name = "zslewprofch"; name += ich;
          name += "tdc"; name += itdc;
          TProfile *tempprof = (TProfile*)infile->Get( name );
          if ( slewprof[ich+itdc*8] && tempprof )
            {
              delete slewprof[ich+itdc*8];
              slewprof[ich+itdc*8] = (TProfile*)tempprof->Clone();
            }
        }
    }

  return 1;
}

int ZdcCalSlew::FillFromPacket(Packet *p, float bbcntime, float bbcstime)
{
  if (!p) return 0;

  for (int ich=0; ich<8; ich++)
    {
      double bbctime = -9999.;
      if ( ich<4 ) bbctime = bbcstime;
      else         bbctime = bbcntime;
 
      int q = p->iValue(ich);
      int t0 = p->iValue(ich,"T1");
      int t1 = p->iValue(ich,"T2");

      if ( fabs(bbctime) < 1000. )
        {
          if ( t0<3800 )
            {
              double tdc02ns = zdccalib->getTdcGain0()->getCalibPar(ich)->getPar1();
              slewprof[ich]->Fill(q,t0*tdc02ns-bbctime);
              slewhist[ich]->Fill(q,t0*tdc02ns-bbctime);
            }
          if ( t1<3800 )
            {
              double tdc02ns = zdccalib->getTdcGain1()->getCalibPar(ich)->getPar1();
              slewprof[ich+8]->Fill(q,t0*tdc02ns-bbctime);
              slewhist[ich+8]->Fill(q,t0*tdc02ns-bbctime);
            }
        }
    }
  return 1;
}

int ZdcCalSlew::CalculateConstants()
{
  for (int itdc=0; itdc<1; itdc++)
    {
      for (int ich=0; ich<1; ich++)
        {
          int ipmt = ich+itdc*8;
          slewprof[ipmt]->Rebin(8);
          const int nbins = 4096/8;
          float x[nbins];
          float y[nbins];
          float yerr[nbins];
          for (int ibin=0; ibin<nbins; ibin++)
            {
              x[ibin] = ibin;
              y[ibin] = slewprof[ipmt]->GetBinContent(ibin);
              yerr[ibin] = slewprof[ipmt]->GetBinError(ibin);
            }
          TGraphErrors *gr = new TGraphErrors(nbins,x,y,0,yerr);
          gr->Draw("ALP");

          TSpline3 *s = new TSpline3("grs",gr);
          s->SetLineColor(kRed);
          s->Draw("same");
        }
    }

  return 1;
}

void ZdcCalSlew::SetCalibration(ZdcCalib *z)
{
  zdccalib = z;
}

int ZdcCalSlew::SaveToFile(const char *fname)
{
  // output files
  std::ofstream adcfile( fname );

  return 1;
}
