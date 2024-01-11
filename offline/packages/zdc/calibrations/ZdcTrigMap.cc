#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <unistd.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TString.h>
#include <TROOT.h>
#include <TTree.h>
#include <TBranch.h>
#include <TProfile.h>

#include <fileEventiterator.h>
#include <Event.h>
#include <EventTypes.h>
#include <packet.h>
#include <ZdcRaw.h>
#include <ZdcRawv1.h>
#include <ZdcOut.h>
#include <ZdcOutv1.h>
/*
#include <BbcOut.h>
#include <BbcOutv1.h>
*/
#include <BbcEvent.hh>
#include <BbcCalib.hh>
#include <ZdcEvent.hh>
#include <ZdcCalib.hh>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include "ZdcTrigMap.h"

static const double TMAX = 33.;		// maximum time value
static const double DVTXMAX = 99.;	// maximum difference in vtx
static const double C = 29.9792458;	// speed of light in cm/ns

static const int southmax = 1;		// we use only 1 channel for south time
static const int northmax = 5;		// we use only 1 channel for north time

static const int zdc_packetid = 13001;         // ZDC Packet Id

using namespace std;

ZdcTrigMap::ZdcTrigMap()
{
  // output root file
  const int compression = 3;
  savefile = new TFile("zdctrigmap.root","RECREATE","ZDC trigger map",compression);

  // for finding the mean time zero
  TString name;
  for (int ich=0; ich<8; ich++)
    {
      name = "zch"; name += ich; name += "tdc1";
      ztdc1[ich] = new TH1F(name,name,4096,0.,TMAX);
      name = "zch"; name += ich; name += "tdc1"; name += "minusbbc";
      ztdc1minusbbc[ich] = new TH1F(name,name,4096,-TMAX/2.,TMAX/2);
    }

  // for evaluation of zdc vertex (pass 1)
  for (int itrig=0; itrig<ZdcTrigMap::MAX_TRIGGERS; itrig++)
    {
      name = "zdclvl1bbcvtx";
      name += "_trig"; name += itrig;
      zdclvl1bbcvtx[itrig] = new TH1F(name,name,DVTXMAX*4,-DVTXMAX,DVTXMAX);
      name = "zdclvl1zdcvtx";
      name += "_trig"; name += itrig;
      zdclvl1zdcvtx[itrig] = new TH1F(name,name,DVTXMAX*4,-DVTXMAX,DVTXMAX);
      name = "hzdcL1vtx";
      name += "_trig"; name += itrig;
      hzdcL1vtx[itrig] = new TH1F(name,name,DVTXMAX*4,-DVTXMAX,DVTXMAX);
      name = "hzdcvtx";
      name += "_trig"; name += itrig;
      hzdcvtx[itrig] = new TH1F(name,name,DVTXMAX*4,-DVTXMAX,DVTXMAX);
      name = "hbbcvtx";
      name += "_trig"; name += itrig;
      hbbcvtx[itrig] = new TH1F(name,name,DVTXMAX*4,-DVTXMAX,DVTXMAX);
    }

  bbccalib = new BbcCalib();
  bbcevent = new BbcEvent();
  zdccalib = new ZdcCalib();
  zdcevent = new ZdcEvent();

  double overflow1cut[8] = { 0. };
  double tdc1ns[8] = { 0. };
  double tdc1t0[8] = { 0. };
  for (int ich=0; ich<8; ich++)
    {
      overflow1cut[ich] = 4095.;
      tdc1ns[ich] = 0.007;
      tdc1t0[ich] = 0.;
    }

  // peakfit is used for finding the mean t0 of a channel
  int ngaus = 1;
  if ( ngaus==1 )
    {
      peakfit = new TF1("peakfit","gaus",0.,TMAX);
      peakfit->SetParameters(100,13.,4.);
    }
  else if ( ngaus==3 )
    {
      peakfit = new TF1("gaus3","gaus+gaus(3)+gaus(6)",0.,TMAX);;
      peakfit->SetParameters(100,13.,2.,10,17.,2.,10,9.,2.);
    }
  peakfit->SetLineColor(2);

  dvtxmean = 0.;
  trigtimestep = 0.260;
  processed_event = 0;
  trigmask = 0xffffffff;
}

ZdcTrigMap::~ZdcTrigMap()
{
  savefile->Write();
  savefile->Close();
}

void ZdcTrigMap::InitCalibrations(const Event *evt)
{
  /*
  PHTimeStamp BeginRunTimeStamp(0);
  BeginRunTimeStamp.setTics(evt->getTime());
  BeginRunTimeStamp.print();
  cout << endl;
  bbccalib->restore( BeginRunTimeStamp, 1004 );
  */
  bbccalib->restore( "/home/phnxbb/Y4/calib/BbcCalib" );
  bbcevent->setCalibDataAll( bbccalib );

  TString calibdir = getenv("HOME");
  calibdir += "/run04commissioning/final_calibrations/ZdcCalib";
  zdccalib->restore( (char *)calibdir.Data() );
  zdcevent->setCalibDataAll(zdccalib);

  for (int ich=0; ich<8; ich++)
    {
      float oflow1 = zdccalib->getOverflow1()->getCalibPar(ich)->getPeakChannel();
      float oflow1err = zdccalib->getOverflow1()->getCalibPar(ich)->getDeviation();
      overflow1cut[ich] = oflow1 - oflow1err*10.0;
      if (oflow1>4094.) overflow1cut[ich] = 4080.;
      underflow1cut[ich] = 4;
      tdc1ns[ich] = zdccalib->getTdcGain1()->getCalibPar(ich)->getPar1();
      cout << "oflowcut " << ich << " " << overflow1cut[ich]
	   << " mean " << oflow1 << " rms " << oflow1err << endl;
    }
}

int ZdcTrigMap::ProcessEvent(Event *evt)
{
  if ( processed_event==0) InitCalibrations(evt);

  bbcevent->Clear();
  bbcevent->setRawData(evt);
  bbcevent->calculate();

  Packet *p = evt->getPacket(zdc_packetid);

  if (!p) return 0;

  for (int ich=0; ich<8; ich++)
    {
      int tdc1 = p->iValue(ich,"T2");
      if ( tdc1 > overflow1cut[ich] || tdc1<underflow1cut[ich] ) continue;
		  
      // get time in a channel
      double time = p->iValue(ich,"T2")*tdc1ns[ich];
      ztdc1[ich]->Fill( time );

      // now get zdctime - bbctime
      Bbc::ArmType arm = Bbc::South;
      if (ich<4) arm = Bbc::South;
      else       arm = Bbc::North;
      double bbctime = bbcevent->getArmHitTime(arm);
      ztdc1minusbbc[ich]->Fill( time - bbctime );
    }

  processed_event++;
  return 1;
}

int ZdcTrigMap::FindTdcMean()
{
  for (int ich=0; ich<8; ich++)
    {
      ztdc1[ich]->Fit(peakfit,"RLOQ");
      double mean = peakfit->GetParameter(1);
      double rms = peakfit->GetParameter(2);
      peakfit->SetRange(mean-2.5*rms,mean+2.5*rms);
      ztdc1[ich]->Fit(peakfit,"RLOQ");
      tdc1t0[ich] = mean;
      cout << ich << " " << mean << " " << rms << endl;
    }

  return 1;
}

int ZdcTrigMap::SaveLUT(const char *fname)
{
  ofstream zdclutfile( fname );

  for (int ich=0; ich<8; ich++)
    for (int itdc=0; itdc<2048; itdc++)
      zdclutfile << zdclut[ich][itdc] << endl;

  zdclutfile.close();

  return 1;
}

int ZdcTrigMap::ReadLUT(const char *fname)
{
  ifstream zdclutfile( fname );

  for (int ich=0; ich<8; ich++)
    {
      for (int itdc=0; itdc<2048; itdc++)
	{
          zdclutfile >> zdclut[ich][itdc];
	}
      cout << endl;
    }

  for (int ich=0; ich<8; ich++)
    {
      cout << "ich: ";
      int itdc = 0;
      while (zdclut[ich][itdc]==0) itdc++;

      cout << zdclut[ich][itdc]*trigtimestep << endl;;
    }

  zdclutfile.close();

  return 1;
}

int ZdcTrigMap::UpdateLUT()
{
  // now that we have the time range in ns, we can map the tdc
  // values to the lut (0 values means disabled)
  //
  trigtimestep = (maxtime-mintime)/127.0;
  double vtxcorrection = dvtxmean/C;

  cout << "time per level1 bin is " << trigtimestep << endl;
  for (int ich=0; ich<8; ich++)
    {
      if ( ich<4 ) vtxcorrection = -1.0*dvtxmean/C;
      else         vtxcorrection = dvtxmean/C;

      for (int itdc=0; itdc<2048; itdc++)
        {
          double time = -1.*tdc1t0[ich] + vtxcorrection - mintime + (itdc+0.5)*2*tdc1ns[ich];
          if ( (itdc*2)>overflow1cut[ich] )
            {
              zdclut[ich][itdc] = 0;
            }
          else if ( (itdc*2)<underflow1cut[ich] || time<0. )
            {
              zdclut[ich][itdc] = 0;
            }
          else
            {
              zdclut[ich][itdc] = int(time/trigtimestep);
            }
        }
    }
 
  return 1;
}

int ZdcTrigMap::CalculateMinMaxTimes()
{
  // now find minimum and maximum time value from all channels
  maxtime = -999.;
  mintime = 999.;

  double vtxcorrection = -1.0*dvtxmean/C;

  cout << "min/max times:" << endl;
  for (int ich=0; ich<8; ich++)
    {
      cout << "ich" << ": ";

      if (ich<4) vtxcorrection = -1.0*dvtxmean/C;
      else       vtxcorrection = dvtxmean/C;

      double lowtime = -1.*tdc1t0[ich] + vtxcorrection;	// channel min time
      cout << lowtime << "  ";

      double hightime = overflow1cut[ich]*tdc1ns[ich] - tdc1t0[ich] + vtxcorrection;
      cout << hightime << endl;

      if (ich!=0 && ich!=4) continue;
      if ( lowtime < mintime ) mintime = lowtime;
      if ( hightime > maxtime ) maxtime = hightime;
    }

  cout << "min/max   " << mintime << "  " << maxtime << endl;

  return 1;
}

int ZdcTrigMap::ProcessVertexComparisonEvent( Event *evt )
{
  if ( processed_event == 0 ) InitCalibrations(evt);

  bbcevent->Clear();
  bbcevent->setRawData(evt);
  bbcevent->calculate();

  zdcevent->Clear();
  zdcevent->setRawData(evt);
  zdcevent->calculate();

  double bbcvtx = bbcevent->getZVertex();
  double zdcvtx = zdcevent->getZvertex();

  Packet * p = evt->getPacket(zdc_packetid);
  if (p)
    {
      double ssum = 0.;
      double s_hits = 0.;
      for (int ich=0; ich<southmax; ich++)
        {
          int tdc1 = p->iValue(ich,"T2");
          if ( tdc1>overflow1cut[ich] || tdc1<underflow1cut[ich] ) continue;

          double lvl1time = zdclut[ich][tdc1/2]*trigtimestep;
          ssum += lvl1time;
          s_hits += 1.0;
        }

      double nsum = 0.;
      double n_hits = 0.;
      for (int ich=4; ich<northmax; ich++)
        {
          int tdc1 = p->iValue(ich,"T2");
          if ( tdc1 > overflow1cut[ich] || tdc1<underflow1cut[ich] ) continue;

          double lvl1time = zdclut[ich][tdc1/2]*trigtimestep;
          nsum += lvl1time;
          n_hits += 1.0;
        }
      if ( (s_hits>0.) && (n_hits>0.) )
        {
          double savgtime = ssum/s_hits;
          double navgtime = nsum/n_hits;
          double zdcl1vtx = (savgtime - navgtime)*C/2.0;

          if (bbcvtx>-999.0) 
	    {
	      for (int itrig=0; itrig<ZdcTrigMap::MAX_TRIGGERS; itrig++)
	        {
		  unsigned int trigbit = (0x1 << itrig);
		  if ( (trigmask&trigbit) != 0 )
		    {
                      zdclvl1bbcvtx[itrig]->Fill( zdcl1vtx - bbcvtx );
                      zdclvl1zdcvtx[itrig]->Fill( zdcl1vtx - zdcvtx );
                      hzdcL1vtx[itrig]->Fill( zdcl1vtx );
                      hzdcvtx[itrig]->Fill( zdcvtx );
                      hbbcvtx[itrig]->Fill( bbcvtx );
		    }
	        }
	    }
        }
    }

  processed_event++;
  return 1;
}

void ZdcTrigMap::CalculateVertexOffset()
{
  // now get vertex offset
  static TF1 *gaussian = new TF1("gaussian","gaus",-DVTXMAX,DVTXMAX);
  gaussian->SetParameters(100.,0.,30.);
  gaussian->SetLineColor(2);

  zdclvl1bbcvtx[ZdcTrigMap::ZDCLL1wide]->Fit(gaussian,"RLOQ");
  dvtxmean = gaussian->GetParameter(1);
  double dvtxerr = gaussian->GetParError(1);
  double dvtxrms = gaussian->GetParameter(2);
  cout << "dvtx: " << dvtxmean << " +- " << dvtxerr
       << " rms " << dvtxrms << endl;
}

