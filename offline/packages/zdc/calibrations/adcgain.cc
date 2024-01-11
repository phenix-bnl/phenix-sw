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

TROOT groot("adcgain","adc gain calibration");

using namespace std;

void usage()
{
  cout << "usage: adcgain -t<packetid> logfname" << endl;
}


int main(int argc, char **argv)
{
  int packetId = 13001;         // default is zdc
  TString calibname = "ZdcCalib";
  double mindac = 500.;
  double maxdac = 1700.;

  int c;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "p:h")) != -1)
    {
      switch(c)
	{
	case 'p':               // packet id
	  packetId = atoi(optarg);
	  if (packetId==15001)
	    {
	      calibname = "NtcCalib";
	      mindac = 1000;
	      maxdac = 4096;
	    }
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

  const int numch = 8*5;	// number of channels

  // Book Histograms
  vector<TH1F*> adc(numch);
  vector<TH1F*> tdc0(numch);
  vector<TH1F*> tdc1(numch);

  const int maxsteps = 4096;
  float meanadc[numch][maxsteps];
  float meanadcerr[numch][maxsteps];
  float dac[maxsteps];

  TString name;
  const int compression = 3;
  for (int ich=0; ich<numch; ich++)
    {
      name = "adc"; name += ich;
      adc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      name = "tdc0ch"; name += ich;
      tdc0[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      name = "tdc1ch"; name += ich;
      tdc1[ich] = new TH1F(name,name,4096,-0.5,4095.5);
    }

  int steps = 0;

  int dacval;
  TString prdf_file;

  time_t mtime;

  while ( prdflist >> dacval >> prdf_file )
    {
      if (prdf_file.BeginsWith("/buffer/"))
	{
	  prdf_file.Remove(0,strlen("/buffer/"));
	  prdf_file.Prepend("/common/buffer2/");
	}
      mtime = get_mtime(prdf_file);
      cout << prdf_file << "  ";

      int status = 0;   
      Eventiterator *it =  new fileEventiterator(prdf_file, status);
      if (status)
        {
           cout << "Couldn't open input file " << prdf_file << endl;
           exit(1);
        }

      for (int ich=0; ich<numch; ich++)
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
              // fill the ntuple (just one board for now)
              for (int ch=0; ch<numch; ch++)
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

      for (int ich=0; ich<numch; ich++)
        {
          meanadc[ich][steps] = adc[ich]->GetMean();
          meanadcerr[ich][steps] = adc[ich]->GetRMS();
        }

      dac[steps] = dacval;
      steps++;

      cout << " processed " << eventnum << " events" << endl;
    }

  TFile *savefile = new TFile("adcgain.root","RECREATE","pedestal run",compression);
  TGraphErrors *dacgr[numch];

  TString cdate = ctime(&mtime);
  cdate.ReplaceAll(" ","_");
  cdate.ReplaceAll("\n","");
  ofstream pedfile(calibname+".pedestal."+cdate);

  TF1 *line = new TF1("line","[0]*x + [1]",mindac,maxdac);
  line->SetLineWidth(.2);

  cout << "ch  adcgain  pedestal  pedrms chisq" << endl;
  for (int ich=0; ich<numch; ich++)
    {
      name = "dacch"; name += ich;
      dacgr[ich] = new TGraphErrors(steps,dac,meanadc[ich],0,meanadcerr[ich]);
      dacgr[ich]->SetName( name );
      dacgr[ich]->SetLineColor(4);
      dacgr[ich]->SetMarkerColor(4);
      dacgr[ich]->SetMarkerStyle(21);
      dacgr[ich]->SetMarkerSize(.2);

      if ( ich<8 || (ich%4==0) )
        {
   	  mindac = 250.;
	  maxdac = 3000.;
	}
      else
        {
	  mindac = 250.;
	  maxdac = 2250.;
	}

      if ( packetId == 15001 )
	{
	  mindac = 1000;
	  maxdac = 4096;
	}

      line->SetRange(mindac,maxdac);
      line->SetParameters(.5, 500.);
      dacgr[ich]->Fit("line","RQ");
      cout << ich << "  "
	   << setprecision(4) << line->GetParameter(0) << "  "
	   << setprecision(4) << line->GetParameter(1) << "  "
	   << setprecision(2) << line->GetParError(1) << "  "
	   << setprecision(4) << line->GetChisquare()/line->GetNDF() << endl;

      pedfile << setw(6) << setprecision(4) << line->GetParameter(1)
	      << setw(6) << setprecision(3) << line->GetParError(1)
	      << " 0" << endl;

      dacgr[ich]->Write();
    }
  pedfile.close();

  savefile->Write();
  savefile->Close();

  return 0;
}
