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

using namespace std;

// local definition of trigger bits
static const unsigned int BBCLL1 = 0x1;
static const unsigned int ZDCNS = 0x2;
static const unsigned int NTCNSwide = 0x4;
static const unsigned int BBCLL1_noVtx = 0x8;
static const unsigned int ZDCS = 0x10;
static const unsigned int ZDCNorS = 0x20;
static const unsigned int CLOCK = 0x40;

static const unsigned int PPGpedestal = 0x10000000;
static const unsigned int PPGtestpulse = 0x20000000;
static const unsigned int PPGlaser = 0x40000000;

static const double TMAX = 33.;		// maximum time value
static const double DVTXMAX = 99.;	// maximum difference in vtx
static const double C = 29.9792458;	// speed of light in cm/ns

static const int southmax = 1;		// we use only 1 channel for south time
static const int northmax = 5;		// we use only 1 channel for north time

static const int zdc_packetid = 13001;         // ZDC Packet Id

// program driver code below
void usage()
{
  cout << "makezdctrigmap -h -p <pass> -f <lutfile> -t <tdc2ns> -n <nevents> prdf_files" << endl;
}

int main(int argc, char **argv)
{
  unsigned int nevents = 0;
  int ngaus = 1;
  int passflag = 0;
  TString lutfname = "zdclut";

  ZdcTrigMap zdctrigmap;

  int c;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "p:t:f:g:n:h")) != -1)
    {
      switch(c)
        {
        case 'n':               // number events to process
          nevents = atoi(optarg);
          break;
        case 'p':               // pass to run
          passflag = atoi(optarg);
          break;
        case 'f':               // lookup table to read in
          lutfname = optarg;
          break;
        case 't':               // conversion time (used in pass 2)
          zdctrigmap.SetConversionTime( atof(optarg) );
          break;
        case 'g':               // number of gaussians to fit
          ngaus = atoi(optarg);	// not used yet
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

  unsigned int scaledtrig = 0;
  unsigned int rawtrig = 0;
  unsigned int prdf_eventnum = 0;

  // loop over all files and process them for mean time offsets
  // this can be considered pass 0
  unsigned int eventnum = 0;

  int fstart = optind;
  for (int ifile=fstart; (passflag!=2)&&(ifile<argc); ifile++)
    {
      if ( (nevents!=0) && (eventnum==nevents) ) break;

      char *prdftempname = argv[ifile];
      cout << "processing " << prdftempname << endl;

      int status = 0;
      Eventiterator *it =  new fileEventiterator(prdftempname, status);
      if (status)
	{
	  cout << "Couldn't open input file " << prdftempname << endl;
	  exit(1);
	}
 
      Event *evt = 0;
      while ( ((evt = it->getNextEvent())!=0) && (nevents==0||eventnum<nevents) )
	{
	  if ( evt->getEvtType() != DATAEVENT ) continue;

          prdf_eventnum = evt->getEvtSequence();
          if ( (eventnum%1000)==0 ) cout << "event " << eventnum << endl;

	  static const int gl1_packetid = 14001;
	  Packet *gl1pkt = evt->getPacket( gl1_packetid );
	  if ( !gl1pkt )
	    {
	      cout << "GL1 Packet missing, skipping event " << endl;
	      continue;
	    }
	  else	// skip PPG events
	    {
	      rawtrig = gl1pkt->iValue(0,"RAWTRIG");
	      scaledtrig = gl1pkt->iValue(0,"SCALEDTRIG");
	      if ( rawtrig>=0x10000000 ) continue;
            }

          // require BBCLL1&(ZDCN|ZDCS) trigger
          //if ( (scaledtrig&0x200)==0 ) continue;

	  zdctrigmap.ProcessEvent( evt );

	  eventnum++;
	  delete evt;
	}

      delete it;
    }

  cout << " processed " << eventnum << " events for zdc trigger map" << endl;

  if ( passflag!=2 )
    {
      zdctrigmap.FindTdcMean();

      // find the common origin for each tdc
      zdctrigmap.CalculateMinMaxTimes();
      zdctrigmap.UpdateLUT();
    }

  if ( passflag==2 )
    {
      zdctrigmap.ReadLUT( lutfname );
    }

  // after getting the mean t0 for each channel, we loop
  // again on the data to correct for any vertex offset
  eventnum = 0;
  for (int ifile=fstart; ifile<argc; ifile++)
    {
      if ( (nevents!=0) && (eventnum==nevents) ) break;

      char *prdftempname = argv[ifile];
      cout << "processing " << prdftempname << endl;
      
      int status = 0;
      Eventiterator *it =  new fileEventiterator(prdftempname, status);
      if (status)
        {
          cout << "Couldn't open input file " << prdftempname << endl;
          exit(1);
        }
      
      Event *evt = 0;
      while ( ((evt = it->getNextEvent())!=0) && (nevents==0||eventnum<nevents) )
        {
          if ( evt->getEvtType() != DATAEVENT ) continue;

	  static const int gl1_packetid = 14001;
	  Packet *gl1pkt = evt->getPacket( gl1_packetid );
	  if ( !gl1pkt )
	    {
	      cout << "GL1 Packet missing, skipping event " << endl;
	      continue;
	    }
	  else	// skip PPG events
	    {
	      rawtrig = gl1pkt->iValue(0,"RAWTRIG");
	      scaledtrig = gl1pkt->iValue(0,"SCALEDTRIG");
	      if ( rawtrig>=0x10000000 ) continue;
            }

	  unsigned int trigmask = ZdcTrigMap::ANYmask;
          if ( (scaledtrig&0x4)==0 ) trigmask |= ZdcTrigMap::BBCLL1mask; // BBCLL1>=1 trigger
          if ( (scaledtrig&0x8)==0 ) trigmask |= ZdcTrigMap::BBCLL1widemask; // BBCLL1>=1 trigger
          if ( (scaledtrig&0x10)==0 ) trigmask |= ZdcTrigMap::ZDCNSmask; // ZDCNS trigger
          if ( (scaledtrig&0x20)==0 ) trigmask |= ZdcTrigMap::ZDCLL1widemask; // ZDCNS trigger

	  zdctrigmap.SetTrigMask( trigmask );
	  zdctrigmap.ProcessVertexComparisonEvent( evt );

	  eventnum++;
	  delete evt;
	}
      
      delete it;
    }

  zdctrigmap.CalculateVertexOffset();

  if (passflag!=2)
    {
      zdctrigmap.CalculateMinMaxTimes();
      zdctrigmap.UpdateLUT();
      zdctrigmap.SaveLUT();
    }
 
  return 0;
}

