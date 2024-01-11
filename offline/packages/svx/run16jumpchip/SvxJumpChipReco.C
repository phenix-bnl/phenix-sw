#include <SvxJumpChipReco.h>
#include <RunHeader.h>
#include <recoConsts.h>
#include <getClass.h>
#include <Fun4AllReturnCodes.h>
#include <TrigLvl1.h>

#include <PHIODataNode.h>
#include <PHObject.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTimeStamp.h>
#include <RunToTime.hh>

#include <Event.h>
#include <packet_vtxp_fpga.h>
#include <PHGlobal.h>

#include <TString.h>
#include <TGraph.h>
#include <TFile.h>
#include <TTree.h>

#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;

SvxJumpChipReco::SvxJumpChipReco(const std::string &name): SubsysReco(name),
  _verbose(0)
{
  /*
  for (int i=0; i<BUFFER_SIZE; i++)
  {
    _eventnumBuf[i] = 0;
  }
  */

  fCorrdata = new CorrDataV1();

  return;
}

SvxJumpChipReco::~SvxJumpChipReco()
{
  savefile->cd();
  for (int ipkt=0; ipkt<NPACKETS; ipkt++)
  {
    for (int ichip=0; ichip<NCHIPS; ichip++)
    {
      _bbcq_vs_chiphits[ipkt][ichip]->Write();
      _corr_vs_event[ipkt][ichip]->Write();
    }
  }

  savefile->Write();
  savefile->Close();

  /*
  for (int ipkt=0; ipkt<NPACKETS; ipkt++)
  {
    for (int ichip=0; ichip<NCHIPS; ichip++)
    {
      delete _bbcq_vs_chiphits[ipkt][ichip];
      delete _corr_vs_event[ipkt][ichip];
    }
  }
  */
}

int SvxJumpChipReco::Init(PHCompositeNode *topNode)
{
  return 0;
}

int SvxJumpChipReco::EndRun(const int runnumber)
{
  return 0;
}


int SvxJumpChipReco::InitRun(PHCompositeNode *topNode)
{
  // Set the energy scale based on run
  int run_number = -999999;
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if ( runheader!=0 )
    {
      run_number = runheader->get_RunNumber();
    }

  if ( run_number == -999999 )
    {
      // Didn't get good run number from run header,
      // try again using recoConst
      recoConsts *rc = recoConsts::instance();
      run_number = rc->get_IntFlag("RUNNUMBER");
    } 

  cout << "SvxJumpChipReco RUNNUMBER " << run_number << endl;

  TString name = "vtxpjump.root";
  savefile = new TFile(name,"RECREATE");
  ttree = new TTree("t","JumpChip Study");
  ttree->Branch("evt", &fEvent);
  ttree->Branch("strig", &fStrig);
  ttree->Branch("vtxpsum", &fVTXPsum);
  ttree->Branch("vtxssum", &fVTXSsum);
  ttree->Branch("fvtxsum", &fFVTXsum);
  ttree->Branch("cross", &fCross);
  ttree->Branch("clk", &fClock);
  ttree->Branch("corr", &fCorrdata);

  TString gname;
  for (int ipkt=0; ipkt<NPACKETS; ipkt++)
  {
    for (int ichip=0; ichip<NCHIPS; ichip++)
    {
      _bbcq_vs_chiphits[ipkt][ichip] = new TGraph();
      gname = "bbcq_vs_chiphits_p"; gname += ipkt; gname += "c"; gname += ichip;
      _bbcq_vs_chiphits[ipkt][ichip]->SetName(gname);

      _corr_vs_event[ipkt][ichip] = new TGraph();
      gname = "corr_vs_event_p"; gname += ipkt; gname += "c"; gname += ichip;
      _corr_vs_event[ipkt][ichip]->SetName(gname);
    }
  }

  return 0;
}

// Count the number of active bits in v
UInt_t SvxJumpChipReco::HammingWeight(UInt_t v)
{
  v = v - ((v >> 1) & 0x55555555);                    // reuse input as temporary
  v = (v & 0x33333333) + ((v >> 2) & 0x33333333);     // temp
  return (((v + (v >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;  // count
}

int SvxJumpChipReco::process_event(PHCompositeNode *topNode)
{
  TrigLvl1 *trigl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if ( trigl1 != 0 )
  {
    _strig = trigl1->get_lvl1_trigscaled();
    _cross = trigl1->get_lvl1_clock_cross();
    _clock = trigl1->get_lvl1_beam_clk(1);
    _clock = (_clock<<32);
    _clock += trigl1->get_lvl1_beam_clk(0);
    //cout << "scaled trig " << hex << _strig << dec << endl;
    //cout << "clock " << _clock << "\t" << _cross << "\t" << trigl1->get_lvl1_beam_clk(1) << "\t" << trigl1->get_lvl1_beam_clk(0) << endl;
  }

  const int packetid[NPACKETS] = {
    24016, 24017, 24018
  };

  // Packet data
  int evtnr[NPACKETS] = {0};
  int detid[NPACKETS] = {0};
  int evtnr03[NPACKETS] = {0};

  PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  if ( global != 0 )
  {
    _bbc_qsum = global->getBbcChargeS() + global->getBbcChargeN();
  }
  
  Event *event = findNode::getClass<Event>(topNode,"PRDF");


  if ( event==0 )
  {
    cout << PHWHERE << "Unable to get PRDF, is Node missing?" << endl;
    return False;
  }

  _EventNumber = event->getEvtSequence();
  if ( _EventNumber < 0 ) return EVENT_OK;

  if ( _EventNumber%1000 == 0 )
  {
    cout << "Evt " << _EventNumber << endl;
  }

  static int index = 0;
  static int nhits[100][NPACKETS][NCHIPS] = {{{0}}};  // number of hits in a chip

  // Scan through events and get chip data, including num hits in a chip
  for (int ipkt=0; ipkt<NPACKETS; ipkt++)
  {
    Packet *p = event->getPacket( packetid[ipkt] );
    if ( p == 0 ) continue;

    evtnr[ipkt] = p->iValue(0,"EVTNR");
    detid[ipkt] = p->iValue(0,"DETID");
    evtnr03[ipkt] = p->iValue(0,"EVTNR03");
    //cout << _EventNumber << "\t" << evtnr[ipkt] << "\t" << detid[ipkt] << "\t" << evtnr03[ipkt] << endl;
 
    // Get number of hits in a chip
    for (int ichip=0; ichip<NCHIPS; ichip++)
    {
      nhits[index][ipkt][ichip] = 0;
      for (int irow=0; irow<256; irow++)
      {
        uint32_t val = p->iValue(ichip,irow);
        nhits[index][ipkt][ichip] += HammingWeight( val );
      }
    }

    if ( _verbose > 0 )
    {
      cout << _EventNumber << "\t" << _bbc_qsum;
      for (int ichip=0; ichip<NCHIPS; ichip++)
      {
        cout << "\t" << nhits[index][ipkt][ichip];
      }
      cout << endl;
    }

    delete p;
  }

  CorrDataV1 corrdata;
  corrdata.set_bbcq(_bbc_qsum);

  // Compute Correlation with BBC
  for (int ipkt=0; ipkt<NPACKETS; ipkt++)
  {
    for (int ichip=0; ichip<NCHIPS; ichip++)
    {
      _bbcq_vs_chiphits[ipkt][ichip]->SetPoint(index,nhits[index][ipkt][ichip],_bbc_qsum);

      Double_t corr = _bbcq_vs_chiphits[ipkt][ichip]->GetCorrelationFactor();
      int n = _corr_vs_event[ipkt][ichip]->GetN();
      _corr_vs_event[ipkt][ichip]->SetPoint(n,_EventNumber,corr);

      corrdata.set_nhits(ipkt*NCHIPS+ichip,nhits[index][ipkt][ichip]);
    }
  }

  index++;
  if ( index>=100 ) index = 0;

  //== Fill Run Buffer
  // First check if buffer is empty at the event location
  /*
  if ( _eventnumBuf[_EventNumber%BUFFER_SIZE] != 0 )
  {
    cout << "ERROR, buf for " << _EventNumber << " is full, "
      << "_eventnumBuf[" << _EventNumber%BUFFER_SIZE << "] = "
      << _eventnumBuf[_EventNumber%BUFFER_SIZE]
      << endl;
  }
  */

  // Get Summed size of VTXS, VTXP, FVTX packets
  GetSiliconSums(topNode);

  // Write out data
  fEvent = _EventNumber;
  fStrig = _strig;
  fCross = _cross;
  fClock = _clock;
  *fCorrdata = corrdata;
  ttree->Fill();

  return 0;
}

void SvxJumpChipReco::GetSiliconSums(PHCompositeNode *topNode)
{
  Event *event = findNode::getClass<Event>(topNode,"PRDF");

  fVTXPsum = 0;
  fVTXSsum = 0;
  fFVTXsum = 0;

  // Get VTXP summed size
  for (int ipkt=24001; ipkt<=24060; ipkt++)
  {
    Packet *p = event->getPacket( ipkt );
    if ( p == 0 ) continue;

    fVTXPsum += p->getLength();
    delete p;
  }

  // Get VTXS summed size
  for (int ipkt=24101; ipkt<=24140; ipkt++)
  {
    Packet *p = event->getPacket( ipkt );
    if ( p == 0 ) continue;

    fVTXSsum += p->getLength();
    delete p;
  }

  // Get FVTX summed size
  // FVTX Packet : 25001 - 25024
  // FVTX Packet : 25101 - 25124
  for (int ipkt=25001; ipkt<=25124; ipkt++) 
  {
    if ( 25024 < ipkt && ipkt < 25101 ) continue;
    Packet *p = event->getPacket( ipkt );
    if ( p == 0 ) continue;

    fFVTXsum += p->getLength();
    delete p;
  }

  return;
}
