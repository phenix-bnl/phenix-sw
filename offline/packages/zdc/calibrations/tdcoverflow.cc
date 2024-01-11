#include <iostream>
#include <stdlib.h>
#include <fstream>
#include <vector>
#include <sys/stat.h>
#include <unistd.h>

#include <fileEventiterator.h>
#include <Event.h>
#include <packet.h>
#include "zutility.h"

#include <TFile.h>
#include <TH1.h>
#include <TF1.h>
#include <TString.h>
#include <TGraphErrors.h>
#include <TApplication.h>

#include <TROOT.h>
TROOT groot("tdcoverflow","overflow calibration");

using namespace std;

int main(int argc, char **argv)
{
  const int numch = 8*5;	// number of channels

  bool quietflag = false;	// whether to display histograms
  unsigned int nevents = 0;

  int c = -1;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "n:")) != -1)
    {
      switch(c)
        {
          case 'n':
	    nevents = atoi(optarg);
	    cout << "Processing " << nevents << endl;
	    break;
	  default:
	    break;
	}
    }

  TApplication *theApp = 0;
  if ( !quietflag )
    {
      //theApp = new TApplication("App",0,0);
    }



  // Book Histograms
  vector<TH1F*> adc(numch);
  vector<TH1F*> tdc0(numch);
  vector<TH1F*> tdc1(numch);
  vector<double> channel(numch);

  TString name;
  const int compression = 3;
  TFile *savefile = new TFile("tdcoverflow.root","RECREATE","pedestal run",compression);
  for (int ich=0; ich<numch; ich++)
    {
      name = "adc"; name += ich;
      adc[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      adc[ich]->SetLineColor(2);
      name = "tdc0ch"; name += ich;
      tdc0[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      tdc0[ich]->SetLineColor(4);
      name = "tdc1ch"; name += ich;
      tdc1[ich] = new TH1F(name,name,4096,-0.5,4095.5);
      tdc1[ich]->SetLineColor(6);

      channel[ich] = ich;
    }

  time_t mtime;		// modify time of file
  const int packetId = 13001;
  unsigned int eventnum = 0;

  for (int ifile=1; ifile<argc; ifile++)
    {
      char *prdf_file = argv[optind++];
      cout << "processing " << prdf_file << endl;
      mtime = get_mtime(prdf_file);

      int status = 0;   
      Eventiterator *it =  new fileEventiterator(prdf_file, status);
      if (status)
        {
          cout << "Couldn't open input file " << prdf_file << endl;
          exit(1);
        }


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

	   if ( nevents!=0 && eventnum>nevents ) break;
         }

      if ( nevents!=0 && eventnum>nevents ) break;
    }

  // do fits to a gaussian
  vector<double> meanadc(numch);
  vector<double> meanadcerr(numch);
  vector<double> oflow0(numch);
  vector<double> oflow1(numch);
  vector<double> oflow0err(numch);
  vector<double> oflow1err(numch);

  TString cdate = ctime(&mtime);
  cdate.ReplaceAll(" ","_");
  cdate.ReplaceAll("\n","");
  cout << cdate << endl;
  ofstream oflow0file( "ZdcCalib.overflow0." + cdate );
  ofstream oflow1file( "ZdcCalib.overflow1." + cdate );

  for (int ich=0; ich<numch; ich++)
    {
      meanadc[ich] = adc[ich]->GetMean();
      meanadcerr[ich] = adc[ich]->GetRMS();

      oflow0[ich] = tdc0[ich]->GetMean();
      oflow0err[ich] = tdc0[ich]->GetRMS();

      oflow1[ich] = tdc1[ich]->GetMean();
      oflow1err[ich] = tdc1[ich]->GetRMS();

      cout << ich << "  "
	   << meanadc[ich] << "  " << meanadcerr[ich] << "  "
	   << oflow0[ich] << "  " << oflow0err[ich] << "  "
	   << oflow1[ich] << "  " << oflow1err[ich] << endl;

      // output in zdc offline calib format
      // here we fix the 4095 overflow on the tdc
      if ( oflow0[ich]>4093. ) oflow0err[ich] = 4.0;
      oflow0file << oflow0[ich] << "\t" << oflow0err[ich] << "\t0" << endl;
      if ( oflow1[ich]>4093. ) oflow1err[ich] = 4.0;
      oflow1file << oflow1[ich] << "\t" << oflow1err[ich] << "\t0" << endl;
    }

  oflow0file.close();
  oflow1file.close();

  TGraphErrors adcgr(numch,&channel[0],&meanadc[0],0,&meanadcerr[0]);
  adcgr.SetName("adcmean");
  adcgr.SetLineColor(5);
  adcgr.SetMarkerColor(5);
  adcgr.SetMarkerStyle(20);
  adcgr.Write();
  TGraphErrors tdc0gr(numch,&channel[0],&oflow0[0],0,&oflow0err[0]);
  tdc0gr.SetName("tdc0mean");
  tdc0gr.SetTitle("ZDC Overflow, TDC0 = red, TDC1 = blue");
  tdc0gr.SetLineColor(2);
  tdc0gr.SetMarkerColor(2);
  tdc0gr.SetMarkerStyle(25);
  tdc0gr.Write();
  TGraphErrors tdc1gr(numch,&channel[0],&oflow1[0],0,&oflow1err[0]);
  tdc1gr.SetName("tdc1mean");
  tdc1gr.SetLineColor(4);
  tdc1gr.SetMarkerColor(4);
  tdc1gr.SetMarkerStyle(22);
  tdc1gr.Write();

  savefile->Write();
  savefile->Close();

  //if ( !quietflag ) theApp->Run();

  cout << "processed " << eventnum << " events" << endl;

  return 0;
}

