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
#include <TString.h>
#include <TNtuple.h>

#include <fileEventiterator.h>
#include <Event.h>
#include <EventTypes.h>
#include <packet.h>
#include <packet_emc_dcm32.h>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include "MpcPed.h"

using namespace std;

// local definition of trigger bits
static const unsigned int PPGpedestal = 0x10000000;
static const unsigned int PPGtestpulse = 0x20000000;
static const unsigned int PPGlaser = 0x40000000;

static int mpc_packetid[] = { 21001, 21002, 21003, 21004 };  // MPC Packet Ids

// program usage
void usage()
{
  cout << "mpcped -h -n <nevents> -t <trigtype> -v prdf_files" << endl;
  cout << "If -t is not specified, all events are selected" << endl << endl;
  cout << "Trigger Selection (-t):" << endl;
  cout << " 1   LED" << endl;
  cout << " 2   PEDESTAL" << endl;
}

TNtuple *MakeSummaryNtuple(const char *ntup_name, const char *ntup_title)
{
  cout << "creating " << ntup_name << " ntuple" << endl;
  TNtuple *temp_ntup = new TNtuple(ntup_name,ntup_title,"nevt:ch:amu:mean:rms");
  return temp_ntup;
}

// program driver code below
int main(int argc, char **argv)
{
  unsigned int nevents = 0;
  //int verbose = 0;
  //int max_amu = 64;
  unsigned int trigger = 0;
  int trignum = 0;
  TString savefname;

  int c;
  extern int optind;
  extern char *optarg;
  while ((c = getopt(argc, argv, "n:bhvt:f:")) != -1)
    {
      switch(c)
        {
        case 'b':               // test bench
          mpc_packetid[0] = 21099;
          mpc_packetid[1] = 0;
          break;
        case 'n':               // number events to process
          nevents = atoi(optarg);
          break;
        case 'a':               // no amu dependency
          //max_amu = 1;
          break;
       case 'f':               // filename
          savefname = optarg;
          break;
        case 'v':               // verbose
          //verbose = 1;
          break;
        case 't':               // trigger type
          trignum = atoi(optarg);
          if ( trignum == 1 )	// NOISE
            {
              trigger |= 0x80000000U;
            }
          if ( trignum == 2 )	// LED
            {
              trigger |= 0x40000000U;
            }
          if ( trignum == 4 )	// PEDESTAL
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

  // Parse File Name
  if ( savefname.Length()==0 )
    {
      savefname = argv[optind];
      if ( savefname.EndsWith(".prdf") )
        {
          // strip
          Ssiz_t index = savefname.Index("rc-",3,TString::kExact);
          savefname.Remove(0,index+4);
          //savefname.Remove(0,index+3);
          cout << savefname << endl;
          savefname.Remove(7,1000);
          cout << savefname << endl;
          savefname.Prepend("mpcped_");
          //savefname.Append(".root");
          cout << "Saving to " << savefname << endl;
        }
      else if ( savefname.EndsWith(".PRDFF") )
        {
          // strip
          Ssiz_t index = savefname.Index("-",1,TString::kExact);
          savefname.Remove(0,index+1);
          //savefname.Remove(0,index+3);
          cout << savefname << endl;
          index = savefname.Index("-",1,TString::kExact);
          savefname.Remove(index,1000);
          cout << savefname << endl;
          savefname.Prepend("mpcped_");
          //savefname.Append(".root");
          cout << "Saving to " << savefname << endl;
        }
      else
        {
          savefname = "mpcped";    // default output name
        }
    }

  // Now Process summary TNtuples
  TString sum_savefname = savefname; sum_savefname += ".root";
  cout << "Saving summary TNtuples to " << sum_savefname << endl;
  TFile *summaryfile = new TFile(sum_savefname,"RECREATE");
  TNtuple *fTdcPed = MakeSummaryNtuple("tdc","MPC TDC Pedestals");
  TNtuple *fLoPrePed = MakeSummaryNtuple("lopre","MPC LOPRE Pedestals");
  TNtuple *fLoPostPed = MakeSummaryNtuple("lopost","MPC LOPOST Pedestals");
  TNtuple *fHiPrePed = MakeSummaryNtuple("hipre","MPC HIPRE Pedestals");
  TNtuple *fHiPostPed = MakeSummaryNtuple("hipost","MPC HIPOST Pedestals");
  TNtuple *fLoPed = MakeSummaryNtuple("lo","MPC LO Pedestals");
  TNtuple *fHiPed = MakeSummaryNtuple("hi","MPC HI Pedestals");

  TString name;
  //TFile *savefile = new TFile(savefname,"RECREATE");
  //savefile->SetCompressionLevel(3);

  MpcPed *mpcped[4] = {0};
  for (int ifem = 0; ifem<4; ifem++)
    {
      cout << "Processing FEM " << ifem << endl;
      TString fem_savefname = savefname; fem_savefname += "_";
      fem_savefname += ifem; fem_savefname += ".root";
      cout << "Saving to " << fem_savefname << endl;
      mpcped[ifem] = new MpcPed(fem_savefname.Data(),ifem);
      int fstart = optind;
      for (int ifile=fstart; ifile<argc; ifile++)
        { 
          char *prdftempname = argv[ifile];
          cout << "processing " << prdftempname << endl;
    
          mpcped[ifem]->ProcessPRDF(prdftempname,nevents);
        }

      mpcped[ifem]->FillSummaryNtuples(fTdcPed,fLoPrePed,fLoPostPed,fHiPrePed,fHiPostPed,fLoPed,fHiPed);

      mpcped[ifem]->WriteAndClose();
      delete mpcped[ifem];
      cout << "Processed FEM " << ifem << endl;
    }

  summaryfile->Write();
  summaryfile->Close();
  //cout << " processed " << eventnum << " events for mpc pedestal" << endl;

  return 0;
}

