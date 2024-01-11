#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <unistd.h>

#include <fileEventiterator.h>
#include <Event.h>
#include <packet.h>
#include "zutility.h"

#include <TFile.h>
#include <TH1.h>
#include <TF1.h>
#include <TGraphErrors.h>
#include <TString.h>
#include <TROOT.h>

TROOT groot("tdcgain","tdc gain calibration");

using namespace std;

// tp delay makes 256 ticks over 4 clocks
const double tpdelaytons = -4.0*106.5/256.0;

void usage()
{
  cout << "usage: tdcgain -t<packetid> logfname" << endl;
}

int main(int argc, char **argv)
{
  int packetId = 13001;		// default is zdc
  double TDCMAX = 4075.;	// minimum value of tdc overflow

  int c;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "p:m:h")) != -1)
    {
      switch(c)
	{
	case 'p':               // packet id
	  packetId = atoi(optarg);
	  break;
	case 'm':               // packet id
	  TDCMAX = atof(optarg);
	  break;
	case 'h':               // help
	  usage();
	  exit(0);
	  break;
	default:
	  usage();
	  exit(-1);
	  break;
	}
    }

  char *prdflistfname = argv[optind];
  cout << "processing " << prdflistfname << endl;
  ifstream prdflist(prdflistfname);

  // need to figure out how many channels automatically
  const unsigned int maxch = 8*5;	// number of channels
  unsigned int numch = 16;

  // Book Histograms
  vector<TH1F*> adc(maxch);
  vector<TH1F*> tdc0(maxch);
  vector<TH1F*> tdc1(maxch);

  const int maxsteps = 256;
  Double_t tdc0mean[maxch][maxsteps];
  Double_t tdc0rms[maxch][maxsteps];
  Double_t tdc1mean[maxch][maxsteps];
  Double_t tdc1rms[maxch][maxsteps];
  Double_t tpdelay[maxsteps];

  TString name;
  for (unsigned int ich=0; ich<maxch; ich++)
    {
      name = "adc"; name += ich;
      adc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      name = "tdc0ch"; name += ich;
      tdc0[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      name = "tdc1ch"; name += ich;
      tdc1[ich] = new TH1F(name,name,4096,-0.5,4095.5);
    }

  int steps = 0;

  int tpdelayval;
  int tpdelayorigin = 0;
  TString prdf_file;

  time_t mtime;

  while ( prdflist >> tpdelayval >> prdf_file )
    {
      if ( prdf_file.Contains("/buffer/") )
	{
	  prdf_file.Remove(0,strlen("/buffer/"));
	  prdf_file.Prepend("/common/buffer2/");
	}
      mtime = get_mtime(prdf_file);
      cout << prdf_file << "  ";
      if ( steps==0 ) tpdelayorigin = tpdelayval;

      int status = 0;   
      Eventiterator *it =  new fileEventiterator(prdf_file, status);
      if (status)
        {
           cout << "Couldn't open input file " << prdf_file << endl;
           exit(1);
        }

      for (unsigned int ich=0; ich<maxch; ich++)
 	{
          adc[ich]->Reset();
          tdc0[ich]->Reset();
          tdc1[ich]->Reset();
        }

      unsigned int eventnum = 0;

      Event *evt = 0;
      while ( (evt = it->getNextEvent()) != 0 )
        {
          Packet *p = evt->getPacket(packetId);
          if (p)
            {
	      if (packetId==13001) numch = 40;

              // fill the ntuple (just one board for now)
              for (unsigned int ch=0; ch<numch; ch++)
 	        {
                  int q  = p->iValue(ch);
                  int t0 = p->iValue(ch, "T1");
                  int t1 = p->iValue(ch, "T2");

                  adc[ch]->Fill(q);
                  tdc0[ch]->Fill(t0);
                  tdc1[ch]->Fill(t1);
	        }
              delete p;   // we are done with p and delete it again.
            }
          eventnum++;
          delete evt;
        }

      for (unsigned int ich=0; ich<numch; ich++)
        {
          tdc0mean[ich][steps] = tdc0[ich]->GetMean();
          tdc0rms[ich][steps] = tdc0[ich]->GetRMS();
          tdc1mean[ich][steps] = tdc1[ich]->GetMean();
          tdc1rms[ich][steps] = tdc1[ich]->GetRMS();
        }

      tpdelay[steps] = (tpdelayval - tpdelayorigin)*tpdelaytons;
      steps++;

      cout << " processed " << eventnum << " events" << endl;
    }

  const int compression = 3;
  TFile *savefile = new TFile("tdcgain.root","RECREATE","pedestal run",compression);
  TGraphErrors *tdc0gr[maxch];
  TGraphErrors *tdc1gr[maxch];

  TF1 *line = new TF1("line","[0]*x + [1]",tpdelaytons*20,5.);
  line->SetLineWidth(.1);

  TString cdate = ctime(&mtime);
  cdate.ReplaceAll(" ","_");
  cdate.ReplaceAll("\n","");
  ofstream tdc0file("ZdcCalib.tdc0."+cdate);
  ofstream tdc1file("ZdcCalib.tdc1."+cdate);

  cout << "ch   " << "tdc2ns " << " intercept " << " chisq/ndf"
       << "  ch   " << "tdc2ns " << " intercept " << " chisq/ndf" << endl;
  for (unsigned int ich=0; ich<numch; ich++)
    {
      name = "tdc0ch"; name += ich;
      tdc0gr[ich] = new TGraphErrors(steps,tpdelay,tdc0mean[ich],0,tdc0rms[ich]);
      tdc0gr[ich]->SetName( name );
      tdc0gr[ich]->SetMarkerColor(2);
      tdc0gr[ich]->SetLineColor(2);

      // now get range for fit
      Double_t min = tpdelay[steps-1]-1.0;
      Double_t max = tpdelay[0]+1.0;
      for (int istep=0; tdc0mean[ich][istep]>TDCMAX&&istep<steps; istep++)
        {
	  max = tpdelay[istep]-1.0;
	}
      for (int istep=steps-1; tdc0mean[ich][istep]<25.&&istep>0; istep--)
        {
   	  min = tpdelay[istep]+1.0;
	}

      line->SetRange(min,max);
      line->SetParameters(.007, 4000.);
      tdc0gr[ich]->Fit("line","RQ");
      cout << setw(3) << ich
	   << setw(10) << setprecision(4) << 1./line->GetParameter(0)
	   << setw(10) << setprecision(6) << line->GetParameter(1)
	   << setw(6) << setprecision(3) << line->GetChisquare()/line->GetNDF();
      if(1./line->GetParameter(0) > 0.01 || 1./line->GetParameter(0) < 0.00001)
	{
	  cout<<"Exiting because of high tdc2ns constant:"<<ich<<" "<<1./line->GetParameter(0)<<endl;
	  return 1;
	}
      tdc0file << "0.0 " << setprecision(4) << 1./line->GetParameter(0)
	       << " 0.0 0.0 0.0 1.0 0" << endl;

      tdc0gr[ich]->Write();

      name = "tdc1ch"; name += ich;
      tdc1gr[ich] = new TGraphErrors(steps,tpdelay,tdc1mean[ich],0,tdc1rms[ich]);
      tdc1gr[ich]->SetName( name );
      tdc1gr[ich]->SetMarkerColor(4);
      tdc1gr[ich]->SetLineColor(4);

      min = tpdelay[steps-1]-1.0;
      max = tpdelay[0]+1.0;
      for (int istep=0; tdc1mean[ich][istep]>TDCMAX&&istep<steps; istep++)
        {
          max = tpdelay[istep]-1.0;
        }
      for (int istep=steps-1; tdc1mean[ich][istep]<25.&&istep>0; istep--)
        {
          min = tpdelay[istep]+1.0;
        }

      line->SetRange(min,max);
      line->SetParameters(.007, 4000.);
      tdc1gr[ich]->Fit("line","RQ");
      cout << setw(3) << ich
	   << setw(10) << setprecision(4) << 1./line->GetParameter(0)
           << setw(10) << setprecision(6) << line->GetParameter(1)
           << setw(6) << setprecision(3) << line->GetChisquare()/line->GetNDF() << endl;

      if(1./line->GetParameter(0) > 0.01 || 1./line->GetParameter(0) < 0.00001)
	{
	  cout<<"Exiting because of high tdc2ns constant:"<<ich<<" "<<1./line->GetParameter(0)<<endl;
	  return 1;
	}
  
      tdc1file << "0.0 " << setprecision(4) << 1./line->GetParameter(0)
	       << " 0.0 0.0 0.0 1.0 0" << endl;
      tdc1gr[ich]->Write();
    }

  tdc0file.close();
  tdc1file.close();

  savefile->Write();
  savefile->Close();

  return 0;
}
