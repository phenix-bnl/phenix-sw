#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <cmath>
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
#include <TString.h>
#include <TNtuple.h>

#include <fileEventiterator.h>
#include <Event.h>
#include <EventTypes.h>
#include <packet.h>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include <MpcMap.h>
#include <MpcCalib.h>

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

//static const double C = 29.9792458;	// speed of light in cm/ns

static const int NFEM = 4;
static const int NAMU = 64;
static const int MAX_PACKETIDS = 12;
static const int mpc_packetid[] =
  {
    21011, 21012, 21021, 21022, 21031, 21032, 21041, 21042, 21001, 21002, 21003, 21004
  }; //MPC packet Ids... Order is important

// program driver code below
void usage()
{
  cout << "mpcled -h -n <nevents> -t <trigtype> -v -g prdf_files" << endl;
  cout << "If -t is not specified, all events are selected" << endl << endl;
  cout << "Trigger Selection (-t):" << endl;
  cout << " 1   LED (BLUE)" << endl;
  cout << " 2   LED (RED)" << endl;
  cout << " 3   PEDESTAL" << endl;
  cout << endl;
  cout << " -g  means do a gaussian fit" << endl;
}


//static float tdcped[NFEM*144][NAMU] = {{0}};
//static float tdcped_sigma[NFEM*144][NAMU] = {{0}};
static float lopostped[NFEM*144][NAMU] = {{0}};
static float lopreped[NFEM*144][NAMU] = {{0}};

int main(int argc, char **argv)
{
  unsigned int nevents = 0;
  int verbose = 0;
  int do_gaussian_fit = 0;
  unsigned int trigger = 0;
  int trignum = 0;

  TString savefname;

  int c;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "n:hvt:f:")) != -1)
    {
      switch(c)
        {
        case 'n':               // number events to process
          nevents = atoi(optarg);
          break;
        case 'f':               // filename
          savefname = optarg;
          break;
        case 'v':               // verbose
          verbose = 1;
          break;
        case 'g':               // verbose
          do_gaussian_fit = 1;
          break;
        case 't':               // trigger type
          trignum = atoi(optarg);
          if ( trignum == 1 )	// LED
            {
              trigger |= 0x40000000U;
            }
          if ( trignum == 2 )	// LED
            {
              trigger |= 0x20000000U;
            }
          if ( trignum == 3 )	// PEDESTAL
            {
              trigger |= 0x10000000U;
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

  MpcMap *mpcmap = MpcMap::instance();
  //MpcCalib *mpccalib = MpcCalib::instance();

  //unsigned int scaledtrig = 0;
  unsigned int rawtrig = 0;
  unsigned int prdf_eventnum = 0;

  // skip for now, until we can get the pedestals for the north arm
  //read_pedestals();

  if ( savefname.Length()==0 )
    {
      savefname = argv[1];
      if ( savefname.EndsWith(".prdf") )
        {
          // strip 
          Ssiz_t index = savefname.Index("rc-",0,TString::kExact);
          savefname.Remove(0,index+3);
          //cout << savefname << endl;
          savefname.Remove(7,1000);
          //cout << savefname << endl;
          savefname.Prepend("mpcled_");
          //savefname.Prepend("mpcped_");
          savefname.Append(".root");
          cout << "Writing to " << savefname << endl;
        }
      else if ( savefname.EndsWith(".PRDFF") )
        {
          // strip 
          Ssiz_t index = savefname.Index("-",0,TString::kExact);
          savefname.Remove(0,index+1);
          //cout << savefname << endl;
          savefname.Remove(15,1000);
          //cout << savefname << endl;
          savefname.Prepend("mpcled_");
          //savefname.Prepend("mpcped_");
          savefname.Append(".root");
          cout << "Writing to " << savefname << endl;
        }
      else 
        {
          savefname = "mpcled.root";	// default output name
        }
    }

  TFile *savefile = new TFile(savefname,"RECREATE");
  savefile->SetCompressionLevel(3);

  TString name;

  // Instantiate Histograms
  TH1F *hamutdc = new TH1F("hamutdc","hamutdc",64,0,64);
  TH1F *hamupre = new TH1F("hamupre","hamupre",64,0,64);
  TH1F *hamupost = new TH1F("hamupost","hamupost",64,0,64);

  TH1F *htdc[NFEM*144] = {0};
  TH1F *hlo[NFEM*144] = {0};

  for (int ifem=0; ifem<NFEM; ifem++)
    {
      for (int ich=0; ich<144; ich++)
        {
          int ifee576ch = ifem*144 + ich;

          name = "htdc_"; name += ifee576ch;
          htdc[ifee576ch] = new TH1F(name,name,4096,-0.5,4095.5);
          name = "hlo_"; name += ifee576ch;
          hlo[ifee576ch] = new TH1F(name,name,4100,-100,4000);
        }
    }

  TNtuple *mpcled = new TNtuple("mpc","MPC LED Events","run:evt:ch:amutdc:amupre:amupost:tdc:lopre:lopost:hipre:hipost");
  TNtuple *mpcledsummary = new TNtuple("mpc_summary","MPC LED Summary","run:ch:lomean:lomeanerr:lorms:lormserr:lochi2:londf");

  // loop over all files and process them for mean time offsets
  // this can be considered pass 0
  unsigned int eventnum = 0;
  int run_number = -1;

  int fstart = optind;
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
          eventnum++;

	  if ( evt->getEvtType() != DATAEVENT )
            {
              delete evt;
              continue;
            }

          // get run number from the event header
          if ( run_number < 0 )
            {
              run_number = evt->getRunNumber();
            }

          prdf_eventnum = evt->getEvtSequence();
          if ( (eventnum%1000)==0 ) cout << "event " << eventnum << endl;

	  static const int gl1_packetid = 14001;
	  Packet *gl1pkt = 0;
          if ( trigger != 0 )
            {
              gl1pkt = evt->getPacket( gl1_packetid );
    	      if ( !gl1pkt )
    	        {
    	          cout << "GL1 Packet missing, skipping event " << prdf_eventnum << endl;
                  delete evt;
    	          continue;
    	        }
    	      else	// select specified trigger
    	        {
    	          rawtrig = gl1pkt->iValue(0,"RAWTRIG");
    	          //scaledtrig = gl1pkt->iValue(0,"SCALEDTRIG");

                  delete gl1pkt; // done with gl1, delete it

    	          if ( (rawtrig&trigger) == 0 )
                    {
                      continue;
                    }
                }
            }

          // require BBCLL1&(ZDCN|ZDCS) trigger
          //if ( (scaledtrig&0x200)==0 ) continue;

          for (int ifem=0; ifem<MAX_PACKETIDS; ifem++)
            {
              Packet *p = evt->getPacket( mpc_packetid[ifem] );
              if (!p) continue;

              int maxch = 72;
              int ifem_offset = 0;
              //int realfem = ifem/2;
              if ( mpc_packetid[ifem]<21010 )
                {
                  ifem_offset = 8;
                  maxch = 144;
                  //realfem = ifem - ifem_offset;
                }


              for (int ifemch=0; ifemch<maxch; ifemch++)
                {
                  int ifee576ch = (ifem-ifem_offset)*maxch + ifemch;

                  int amutdc   = p->iValue(0,"AMU");
                  int amupre   = p->iValue(1,"AMU");
                  int amupost   = p->iValue(2,"AMU");

                  hamutdc->Fill( amutdc );
                  hamupre->Fill( amupre );
                  hamupost->Fill( amupost );

                  int tdc    = p->iValue(ifemch, 0);
                  int lopre  = p->iValue(ifemch, 4);
                  int lopost = p->iValue(ifemch, 2);
                  int hipre  = p->iValue(ifemch, 3);
                  int hipost = p->iValue(ifemch, 1);
                  float lo = lopost - lopre - lopostped[ifee576ch][amupre] - lopreped[ifee576ch][amupre];

                  if ( tdc==0 && lopre==0 && lopost==0 && hipre==0 && hipost==0 )
                    {
                      continue;	// this is zero suppressed
                    }

                  if ( verbose )
                    {
                      cout << ifee576ch << "\t" << ifemch << "\t" << tdc << "\t" << lopre << "\t" << lopost
                           << "\t" << hipre << "\t" << hipost << endl;
                    }

                  //if ( tdc < tdcped[ifee576ch][amutdc]-5*tdcped_sigma[ifee576ch][amutdc] )
                  if ( lo>30. )
                    {
                      hlo[ifee576ch]->Fill( lo );
                      htdc[ifee576ch]->Fill( tdc );
                    }

                  mpcled->Fill(run_number,prdf_eventnum,ifee576ch,amutdc,amupre,amupost,tdc,lopre,lopost,hipre,hipost);

                }

              delete p;
            }

	  delete evt;
	}

      delete it;
    }

  // open mpc calibration file
  if ( run_number>0 )
    {
      TString ledfname = ""; ledfname += run_number; ledfname += ".led";
      ofstream ledfile( ledfname );

      TString ledrefname = ""; ledrefname += run_number; ledrefname += "_led.ref";
      ofstream ledrefile( ledrefname );
    
      TString tdcfname = ""; tdcfname += run_number; tdcfname += ".tdc";
      ofstream tdcfile( tdcfname );
    
      //TF1 *gaussian = new TF1("gaussian","[0]*expo(-0.5*((x-[1])/[2])^2)",0,4000);
      TF1 *gaussian = new TF1("gaussian","gaus",0,4000);
    
      for (int ifem=0; ifem<NFEM; ifem++)
       {
         for (int ich=0; ich<144; ich++)
          {
            int ifee576ch = ifem*144 + ich;
    
            if ( hlo[ifee576ch]->GetEntries() == 0 ) continue;
    
            float lomean = hlo[ifee576ch]->GetMean();
            float lorms  = hlo[ifee576ch]->GetRMS();
            float nentries = hlo[ifee576ch]->GetEntries();
    
            float lochi2 = -1.;
            float londf = nentries;
            float lomeanerr = lomean/sqrt(londf);
            float lormserr = -1.;
    
            if ( do_gaussian_fit )
              {
                gaussian->SetParameters(nentries/sqrt(2*M_PI*lorms),lomean,lorms);
                hlo[ifee576ch]->Fit("gaussian","NQL");
        
                if ( gaussian->GetParameter(1) > -2 )
                  {
                    lomean = gaussian->GetParameter(1);
                    lorms = gaussian->GetParameter(2);
                    lochi2 = gaussian->GetChisquare();
                    londf = gaussian->GetNDF();
                    lomeanerr = gaussian->GetParError(1);
                    lormserr = gaussian->GetParError(2);
                  }
                else
                  {
                    londf = nentries;
                    lomeanerr = lomean/sqrt(londf);
                    //lormserr = -1.;
                  }
              }
    
            mpcledsummary->Fill(run_number,ifee576ch,lomean,lomeanerr,lorms,lormserr,lochi2,londf);
    
            ledfile << ifee576ch << "\t" << lomean << "\t" << lomeanerr << "\t"
                    << lorms << "\t" << lormserr << "\t" << lochi2 << "\t" << londf << endl;
    
            //ledrefile << ifee576ch/288 << "\t"
            ledrefile << ifee576ch << "\t"
                      << mpcmap->getGridX(ifee576ch) << "\t" 
                      << mpcmap->getGridY(ifee576ch) << "\t"
                      << setprecision(5) << lomean << endl;

            // now process tdc info
            float tdcmean = htdc[ifee576ch]->GetMean();
            float tdcrms  = htdc[ifee576ch]->GetRMS();
            nentries = htdc[ifee576ch]->GetEntries();
    
            float tdcchi2 = -1.;
            float tdcndf = nentries;
            float tdcmeanerr = tdcmean/sqrt(tdcndf);
            float tdcrmserr = -1.;
    
            gaussian->SetParameters(nentries/sqrt(2*M_PI*tdcrms),tdcmean,tdcrms);
            htdc[ifee576ch]->Fit("gaussian","NQL");
    
            if ( do_gaussian_fit )
              {
                if ( gaussian->GetParameter(1) > -2 )
                  {
                    tdcmean = gaussian->GetParameter(1);
                    tdcrms = gaussian->GetParameter(2);
                    tdcchi2 = gaussian->GetChisquare();
                    tdcndf = gaussian->GetNDF();
                    tdcmeanerr = gaussian->GetParError(1);
                    tdcrmserr = gaussian->GetParError(2);
                  }
              }
    
            tdcfile << ifee576ch << "\t" << tdcmean << "\t" << tdcmeanerr << "\t"
                    << tdcrms << "\t" << tdcrmserr << "\t" << tdcchi2 << "\t" << tdcndf << endl;
    
            // now process tdc info
            if ( verbose )
              {
                cout << "tdc\t" << ifee576ch << "\t" << htdc[ifee576ch]->GetMean()
                     << "\t" << htdc[ifee576ch]->GetRMS() << endl;
                cout << "hlo\t" << ifee576ch << "\t" << hlo[ifee576ch]->GetMean()
                     << "\t" << hlo[ifee576ch]->GetRMS() << endl;
              }
          }
       }
    
     ledfile.close();
     ledrefile.close();
     tdcfile.close();

   }

  savefile->Write();
  savefile->Close();

  cout << " processed " << eventnum << " events for mpc led" << endl;

  sleep(10);

  return 0;
}
