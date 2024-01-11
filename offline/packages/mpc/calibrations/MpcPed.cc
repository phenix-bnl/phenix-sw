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

static const int MPC_PacketIds[] = {
      21001, 21002, 21003, 21004
};  // MPC full packet Ids

static const int MPC_SplitPacketIds[][2] =
{
    { 21011, 21012}, {21021, 21022}, {21031, 21032}, {21041, 21042}
}; //MPC split packet Ids... Order is important

static int fEventNum = 0;

MpcPed::MpcPed(const char *outfname, const int fem)
{
  //fOutFile = new TFile(outfname,"UPDATE");
  fOutFile = new TFile(outfname,"RECREATE");
  fOutFile->SetCompressionLevel(3);

  if ( fem>=21001 && fem<=21004 )
    {
      fNpackets = 1;
      fPacketId[0] = fem;
      fFEM = fem - 21001;
    }
  else if ( fem>=0 && fem<=3 )
    {
      fFEM = fem;
      fNpackets = 3;
      fPacketId[0] = MPC_PacketIds[fem];
      fPacketId[1] = MPC_SplitPacketIds[fem][0];
      fPacketId[2] = MPC_SplitPacketIds[fem][1];
    }
  else
    {
      cout << "MpcPed: Must Specify 21001-21004, or 0-3" << endl;
      exit(2);
    }

  InitHistograms();

  fTrigger = 0;
  fVerbose = 0;
  fEventNum = 0;
}

MpcPed::~MpcPed()
{
}

void MpcPed::FillSummaryNtuples(TNtuple *ntdc, TNtuple *nlopreped, TNtuple *nlopostped, TNtuple *nhipreped,
                                TNtuple *nhipostped, TNtuple *nloped, TNtuple *nhiped)
{
  // First Fill the summary TNtuples
  // store mean and rms of each channel and amu in ntuple
  for (int ich=0; ich<NUM_CH; ich++)
    {
      int ifee576ch = NUM_CH*fFEM + ich;
      for (int iamu=0; iamu<NUM_AMU; iamu++)
       {
         ntdc->Fill(htdc[ich][iamu]->GetEntries(),ifee576ch,iamu,
                      htdc[ich][iamu]->GetMean(),htdc[ich][iamu]->GetRMS());
         nlopreped->Fill(hlopre[ich][iamu]->GetEntries(),ifee576ch,iamu,
                      hlopre[ich][iamu]->GetMean(),hlopre[ich][iamu]->GetRMS());
         nlopostped->Fill(hlopost[ich][iamu]->GetEntries(),ifee576ch,iamu,
                      hlopost[ich][iamu]->GetMean(),hlopost[ich][iamu]->GetRMS());
         nhipreped->Fill(hhipre[ich][iamu]->GetEntries(),ifee576ch,iamu,
                      hhipre[ich][iamu]->GetMean(),hhipre[ich][iamu]->GetRMS());
         nhipostped->Fill(hhipost[ich][iamu]->GetEntries(),ifee576ch,iamu,
                      hhipost[ich][iamu]->GetMean(),hhipost[ich][iamu]->GetRMS());
         nloped->Fill(hlo[ich][iamu]->GetEntries(),ifee576ch,iamu,
                      hlo[ich][iamu]->GetMean(),hlo[ich][iamu]->GetRMS());
         nhiped->Fill(hhi[ich][iamu]->GetEntries(),ifee576ch,iamu,
                      hhi[ich][iamu]->GetMean(),hhi[ich][iamu]->GetRMS());
       }
    }
}

void MpcPed::WriteAndClose()
{
  TString run_number = fOutFile->GetName();
  run_number.ReplaceAll("mpcped_","");
  while ( run_number.BeginsWith("0") )
    {
      run_number.Remove(0,1);
    }
  run_number.Remove(7,100);
  TString name;
  name = "MpcCal.overflow"; name += "."; name += run_number;
  ofstream overflowfile;
  if ( fFEM==0 ) overflowfile.open( name.Data() );
  else           overflowfile.open( name.Data(), ios::app );
  name = "MpcCal.lopostped"; name += "."; name += run_number;
  ofstream lopostfile;
  if ( fFEM==0 ) lopostfile.open( name.Data() );
  else           lopostfile.open( name.Data(), ios::app );
  name = "MpcCal.lopreped"; name += "."; name += run_number;
  ofstream loprefile;
  if ( fFEM==0 ) loprefile.open( name.Data() );
  else           loprefile.open( name.Data(), ios::app );
  name = "MpcCal.hipostped"; name += "."; name += run_number;
  ofstream hipostfile;
  if ( fFEM==0 ) hipostfile.open( name.Data() );
  else           hipostfile.open( name.Data(), ios::app );
  name = "MpcCal.hipreped"; name += "."; name += run_number;
  ofstream hiprefile;
  if ( fFEM==0 ) hiprefile.open( name.Data() );
  else           hiprefile.open( name.Data(), ios::app );

  overflowfile << setprecision(6);
  lopostfile << setprecision(6);
  loprefile << setprecision(6);
  hipostfile << setprecision(6);
  hiprefile << setprecision(6);

  for (int ich=0; ich<NUM_CH; ich++)
    {
      int fee576 = fFEM*NUM_CH + ich;
      for (int iamu=0; iamu<NUM_AMU; iamu++)
        {
         // check for overflow in histogram
          CheckUnderOverflow(htdc[ich][iamu]);
          CheckUnderOverflow(hlopre[ich][iamu]);
          CheckUnderOverflow(hlopost[ich][iamu]);
          CheckUnderOverflow(hhipre[ich][iamu]);
          CheckUnderOverflow(hhipost[ich][iamu]);
          CheckUnderOverflow(hlo[ich][iamu]);
          CheckUnderOverflow(hhi[ich][iamu]);

          if ( fVerbose )
            {
              cout << "tdc\t" << ich << "\t" << iamu << "\t" << htdc[ich][iamu]->GetMean()
                   << "\t" << htdc[ich][iamu]->GetRMS() << endl;
              cout << "lopre\t" << ich << "\t" << iamu << "\t" << hlopre[ich][iamu]->GetMean()
                   << "\t" << hlopre[ich][iamu]->GetRMS() << endl;
              cout << "lopost\t" << ich << "\t" << iamu << "\t" << hlopost[ich][iamu]->GetMean()
                   << "\t" << hlopost[ich][iamu]->GetRMS() << endl;
              cout << "hipre\t" << ich << "\t" << iamu << "\t" << hhipre[ich][iamu]->GetMean()
                   << "\t" << hhipre[ich][iamu]->GetRMS() << endl;
              cout << "hipost\t" << ich << "\t" << iamu << "\t" << hhipost[ich][iamu]->GetMean()
                   << "\t" << hhipost[ich][iamu]->GetRMS() << endl;
              cout << "hlo\t" << ich << "\t" << iamu << "\t" << hlo[ich][iamu]->GetMean()
                   << "\t" << hlo[ich][iamu]->GetRMS() << endl;
              cout << "hhi\t" << ich << "\t" << iamu << "\t" << hhi[ich][iamu]->GetMean()
                   << "\t" << hhi[ich][iamu]->GetRMS() << endl;
            }

          overflowfile << fee576 << "\t" << setw(4) << iamu << setw(12) << htdc[ich][iamu]->GetMean() << setw(12) << htdc[ich][iamu]->GetRMS() << setw(12) << 0 << endl;
          lopostfile << fee576 << "\t" << setw(4) << iamu << setw(12) << hlopost[ich][iamu]->GetMean() << setw(12) << hlopost[ich][iamu]->GetRMS() << setw(12) << 0 << endl;
          loprefile << fee576 << "\t" << setw(4) << iamu << setw(12) << hlopre[ich][iamu]->GetMean() << setw(12) << hlopre[ich][iamu]->GetRMS() << setw(12) << 0 << endl;
          hipostfile << fee576 << "\t" << setw(4) << iamu << setw(12) << hhipost[ich][iamu]->GetMean() << setw(12) << hhipost[ich][iamu]->GetRMS() << setw(12) << 0 << endl;
          hiprefile << fee576 << "\t" << setw(4) << iamu << setw(12) << hhipre[ich][iamu]->GetMean() << setw(12) << hhipre[ich][iamu]->GetRMS() << setw(12) << 0 << endl;

        }
    }

  // Write Out Calibration Files
  overflowfile.close();
  lopostfile.close();
  loprefile.close();
  hipostfile.close();
  hiprefile.close();

  // Write Out Histograms
  if ( fOutFile )
    {
      fOutFile->Write();
      fOutFile->Close();
    }

}

void MpcPed::InitHistograms()
{
  // this should be changed to 1/2 * nevents/64
  TH1::SetDefaultBufferSize(500);
  //TH1::AddDirectory(kFalse);

/*
      int NBINS = 100;
      int RANGE_MIN = 0;
      int RANGE_MAX = -1;
*/
  int NBINS = 4096;
  float RANGE_MIN = -0.5;
  float RANGE_MAX = 4095.5;

  TString name;
  for (int ich=0; ich<NUM_CH; ich++)
    {
      int ifee576ch = ich + fFEM*144;

      name = "htdc_"; name += ifee576ch;
      htdc_all[ich] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
      name = "hlopre_"; name += ifee576ch;
      hlopre_all[ich] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
      name = "hlopost_"; name += ifee576ch;
      hlopost_all[ich] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
      name = "hhipre_"; name += ifee576ch;
      hhipre_all[ich] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
      name = "hhipost_"; name += ifee576ch;
      hhipost_all[ich] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
      name = "hlo_"; name += ifee576ch;
      hlo_all[ich] = new TH1S(name,name,2000,-100,100);
      name = "hhi_"; name += ifee576ch;
      hhi_all[ich] = new TH1S(name,name,2000,-100,100);

      for (int iamu=0; iamu<NUM_AMU; iamu++)
        {
          name = "htdc_"; name += ifee576ch; name += "_"; name += iamu;
          htdc[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "hlopre_"; name += ifee576ch; name += "_"; name += iamu;
          hlopre[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX); 
          name = "hlopost_"; name += ifee576ch; name += "_"; name += iamu;
          hlopost[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "hhipre_"; name += ifee576ch; name += "_"; name += iamu;
          hhipre[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX); 
          name = "hhipost_"; name += ifee576ch; name += "_"; name += iamu;
          hhipost[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "hlo_"; name += ifee576ch; name += "_"; name += iamu;
          hlo[ich][iamu] = new TH1S(name,name,2000,-100,100);
          name = "hhi_"; name += ifee576ch; name += "_"; name += iamu;
          hhi[ich][iamu] = new TH1S(name,name,2000,-100,100);
        }
    }

/*
  // Reference Channels
  for (int ich=0; ich<NUM_REFCH; ich++)
    {
      int irefch = fFEM*NUM_REFCH + ich;

      for (int iamu=0; iamu<NUM_AMU; iamu++)
        {
          name = "refhtdc_"; name += irefch; name += "_"; name += iamu;
          refhtdc[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "refhlopre_"; name += irefch; name += "_"; name += iamu;
          refhlopre[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "refhlopost_"; name += irefch; name += "_"; name += iamu;
          refhlopost[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "refhhipre_"; name += irefch; name += "_"; name += iamu;
          refhhipre[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "refhhipost_"; name += irefch; name += "_"; name += iamu;
          refhhipost[ich][iamu] = new TH1S(name,name,NBINS,RANGE_MIN,RANGE_MAX);
          name = "refhlo_"; name += irefch; name += "_"; name += iamu;
          refhlo[ich][iamu] = new TH1S(name,name,2000,-100,100);
          name = "refhhi_"; name += irefch; name += "_"; name += iamu;
          refhhi[ich][iamu] = new TH1S(name,name,2000,-100,100);
        }
    }
*/

  name = "hamutdc_"; name += fFEM;
  hamutdc = new TH1S(name,name,64,-0.5,63.5);
  name = "hamupre_"; name += fFEM;
  hamupre = new TH1S(name,name,64,-0.5,63.5);
  name = "hamupost_"; name += fFEM;
  hamupost = new TH1S(name,name,64,-0.5,63.5);

  // Now get summary TNtuples, or create them if they don't exist
/*
  fTdcPed = GetSummaryNtuple("tdc","MPC TDC Pedestals");
  fLoPrePed = GetSummaryNtuple("lopre","MPC LOPRE Pedestals");
  fLoPostPed = GetSummaryNtuple("lopost","MPC LOPOST Pedestals");
  fHiPrePed = GetSummaryNtuple("hipre","MPC HIPRE Pedestals");
  fHiPostPed = GetSummaryNtuple("hipost","MPC HIPOST Pedestals");
  fLoPed = GetSummaryNtuple("lo","MPC LO Pedestals");
  fHiPed = GetSummaryNtuple("hi","MPC HI Pedestals");
*/

}

TNtuple *MpcPed::GetSummaryNtuple(const char *ntup_name, const char *ntup_title)
{
  TNtuple *temp_ntup = (TNtuple*)fOutFile->Get(ntup_name);
  if (!temp_ntup)
    {
      cout << "creating " << ntup_name << " ntuple" << endl;
      temp_ntup = new TNtuple(ntup_name,ntup_title,"nevt:ch:amu:mean:rms");
    }
  return temp_ntup;
}

void MpcPed::ProcessPRDF(const char *prdfname, const int nevents)
{
  int status = 0;
  Eventiterator *it =  new fileEventiterator(prdfname, status);
  if (status)
    {
      cout << "Couldn't open input file " << prdfname << endl;
      exit(1);
    }

  //unsigned int scaledtrig = 0;
  unsigned int rawtrig = 0;

  Event *evt = 0;
  while ( ((evt = it->getNextEvent())!=0) && (nevents==0||fEventNum<nevents) )
    {
      fEventNum++;
  
      if ( evt->getEvtType() != DATAEVENT )
        {
          delete evt;
          continue;
        }
  
      unsigned int prdf_eventnum = evt->getEvtSequence();
      if ( (fEventNum%1000)==0 ) cout << "event " << fEventNum << endl;
  
      static const int gl1_packetid = 14001;
      Packet *gl1pkt = 0;
      if ( fTrigger != 0 )
        {
          gl1pkt = evt->getPacket( gl1_packetid );
          if ( !gl1pkt )
            {
              cout << "GL1 Packet missing, skipping event " << prdf_eventnum << endl;
              delete evt;
              continue;
            }
          else      // select specified trigger
            {
              rawtrig = gl1pkt->iValue(0,"RAWTRIG");
              //scaledtrig = gl1pkt->iValue(0,"SCALEDTRIG");
  
              delete gl1pkt; // done with gl1, delete it
  
              if ( (rawtrig&fTrigger) == 0 )
                {
                  continue;
                }
            }
        }
  
      for (int ipkt=0; ipkt<fNpackets; ipkt++)
        {
          Packet *p = evt->getPacket( fPacketId[ipkt] );
          if (!p) continue;
      
          int amutdc   = p->iValue(0,"AMU");
          int amupre   = p->iValue(1,"AMU");
          int amupost  = p->iValue(2,"AMU");
      
          hamutdc->Fill( amutdc );
          hamupre->Fill( amupre );
          hamupost->Fill( amupost );

          int num_ch = 144;
          int ch_offset = 0;

          if ( ipkt>0 )
            {
              num_ch = 72;
              if ( ipkt==2 ) ch_offset = 72;
            }

          for (int ifemch=0; ifemch<num_ch; ifemch++)
            {
              int ich = fFEM*144 + ch_offset + ifemch;
      
              int tdc    = p->iValue(ifemch, 0);
              int hipost = p->iValue(ifemch, 1);
              int lopost = p->iValue(ifemch, 2);
              int hipre  = p->iValue(ifemch, 3);
              int lopre  = p->iValue(ifemch, 4);
              int lo     = lopost - lopre;
              int hi     = hipost - hipre;
      
              if ( tdc==0xfff && lopost==0xfff && lopre==0xfff )
                {
                  continue;
                }
      
              if ( fVerbose )
                {
                  cout << ich << "\t" << tdc << "\t" << lopre << "\t" << lopost
                       << "\t" << hipre << "\t" << hipost << endl;
                }
      
      
              htdc_all[ifemch+ch_offset]->Fill( tdc );
              hlopre_all[ifemch+ch_offset]->Fill( lopre );
              hlopost_all[ifemch+ch_offset]->Fill( lopost );
              hhipre_all[ifemch+ch_offset]->Fill( hipre );
              hhipost_all[ifemch+ch_offset]->Fill( hipost );
              hlo_all[ifemch+ch_offset]->Fill( lo );
              hhi_all[ifemch+ch_offset]->Fill( hi );
    
              htdc[ifemch+ch_offset][amutdc]->Fill( tdc );
              hlopre[ifemch+ch_offset][amupre]->Fill( lopre );
              hlopost[ifemch+ch_offset][amupost]->Fill( lopost );
              hhipre[ifemch+ch_offset][amupre]->Fill( hipre );
              hhipost[ifemch+ch_offset][amupost]->Fill( hipost );
              hlo[ifemch+ch_offset][amupost]->Fill( lo );
              hhi[ifemch+ch_offset][amupost]->Fill( hi );
      
            }	// loop over channels in one packet
      
          // Fill Reference Channels
          //struct emcChannelLongList ecl[144];
/*
          int rawdata[144*5+1000];
          int nw = 0;
          int nchannels = p->fillIntArray( (int*) rawdata, 144*5+1000, &nw, "DATA" );
          //cout << "nw = " << nw << endl;
      
          for (int igroup=0; igroup<12; igroup++)
            {
              // 9 words of header in the front
              unsigned int iword = 9 + 16*5*igroup + 12*5;
      
              for (int irefch=0; irefch<4; irefch++)
                {
                  int tdc    = 4095-(rawdata[iword+5*irefch]&0xfff);
                  int hipost = 4095-(rawdata[iword+5*irefch+1]&0xfff);
                  int lopost = 4095-(rawdata[iword+5*irefch+2]&0xfff);
                  int hipre  = 4095-(rawdata[iword+5*irefch+3]&0xfff);
                  int lopre  = 4095-(rawdata[iword+5*irefch+4]&0xfff);
      
                  unsigned int ich = ifem*48 + igroup*4 + irefch;
      
                  if ( fVerbose )
                    {
                      cout << "ref\t" << ich << "\t" << tdc << "\t" << lopre << "\t" << lopost
                           << "\t" << hipre << "\t" << hipost << endl;
                    }
      
                  refhtdc[ich][amutdc]->Fill( tdc );
                  refhlopre[ich][amupre]->Fill( lopre );
                  refhlopost[ich][amupost]->Fill( lopost );
                  refhhipre[ich][amupre]->Fill( hipre );
                  refhhipost[ich][amupost]->Fill( hipost );
                  refhlo[ich][amupost]->Fill( lopre - lopost );
                  refhhi[ich][amupost]->Fill( hipre - hipost );
                }
            }
*/
      
          delete p;		// delete packet
        }

      delete evt;	// delete event
    }	// loop over events

  delete it;
}

// check for excessive over or underflow
void MpcPed::CheckUnderOverflow(TH1 *th1, Float_t max_percentage) 
{
  int nbinsx = th1->GetNbinsX();
  Stat_t integral = th1->Integral();
  Float_t underflow = th1->GetBinContent(0)/integral;
  Float_t overflow  = th1->GetBinContent(nbinsx+1)/integral;
  if ( underflow>max_percentage || overflow>max_percentage )
    {
      cout << th1->GetName() << " has over or underflow "
           << underflow << "\t" << overflow << endl;
    }
}

