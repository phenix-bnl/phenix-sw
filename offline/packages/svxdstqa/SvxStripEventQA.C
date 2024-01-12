
#include "SvxStripEventQA.h"
#include "SvxQAInfov1.h"
#include <svxAddress.hh>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <Event.h>
#include <RunHeader.h>
#include <TH2.h>
#include <TH3.h>
#include <algorithm>

using namespace findNode;
using namespace std;

SvxStripEventQA::SvxStripEventQA(const string &name): SubsysReco(name)
{
  numsc = 10; // default: difine as stuck cell if the cell ID is same for 10 events
  qaflg = 0; 
  m_cellidhist=0;
  m_cellidadc=0;
  m_address  = new svxAddress();
  m_address->Initialize();
}

SvxStripEventQA::~SvxStripEventQA()
{
  delete m_address;
  return;
}

int SvxStripEventQA::Init(PHCompositeNode *topNode)
{

  if(qaflg>0) CreateQAHisto();
  
  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) {
    evcel[imodule].clear();
  }
  
  return EVENT_OK;
}

int SvxStripEventQA::InitRun(PHCompositeNode *topNode)
{

  int i = CreateNodeTree(topNode);
  if(verbosity>0) cout << "SvxStripEventQA::Init() CreateNodeTree() returned " << i << endl;
  if(i!=EVENT_OK) { return ABORTEVENT; }

  return EVENT_OK;
}

int SvxStripEventQA::process_event(PHCompositeNode *topNode)
{
  static int evtno = 0;

  if(verbosity>0) {
    if((evtno%100)==0){
      cout<<"event processed : "<<evtno<<endl;
    }
  }

  // cout << "SvxStripEventQA::process_event() Getting Event ..." <<endl;
  Event *evt_stripixel = findNode::getClass<Event>(topNode, "PRDF");

  if (!evt_stripixel) {
    cout << PHWHERE << "SvxStripEventQA ERROR: Event is empty" << endl;
    return ABORTEVENT;
  }
  
  int evtnum = evt_stripixel->getEvtSequence();

  ///////////////////////////////////

  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) {

    //    int packetID = 24101 + imodule;
    int packetID = m_address->getStripPacketID(imodule); // packetID is 24101-24140

    Packet *packet_stripixel = evt_stripixel->getPacket(packetID);

    if ( !packet_stripixel ) {
      //     cout <<"No SVX STRIPIXEL packet : "<<packetID<<" , skipping..." << endl;
      continue;
    }

    // Need to be updated as soon as Martin implement "cell" info into the PRDF decoder
    int cellid = packet_stripixel->iValue(0, 0, "CELLID");

    //    if(imodule==0) cout<<"Evt:CellID =  "<<evtnum<<" "<<cellid[0]<<endl;
    if(qaflg>0) hSvxSCellvsEvt[imodule]->Fill(evtnum,cellid);
    evcel[imodule].push_back(pair<int,int>(evtnum, cellid));
    ///////////////////////////////////

    delete packet_stripixel;
  }

  //////////////////////////////////
  // fill cellID hist by iValue
  if(qaflg>0) fillCellIDHistoByDecoder(evt_stripixel);
  if(qaflg>0) fillAdcChip(evt_stripixel);

  evtno++;

  return EVENT_OK;
}

int SvxStripEventQA::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int SvxStripEventQA::EndRun(const int runno)
{ 

  Fun4AllServer *se = Fun4AllServer::instance();

  d_qainfo = findNode::getClass<SvxQAInfo>(se->topNode(), "SvxQAInfo");
  if (!d_qainfo) {
    cerr << PHWHERE << "SvxStripEventQA ERROR: eventqa not in the Node Tree" << endl;
    return ABORTEVENT;
  }

  int stuckevt[SVXNMODULESTRIP];
  int precell, scellcnt, fststuckevt;

  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) {

//    int packetID = m_address->getStripPacketID(imodule);

    stuckevt[imodule] = -9999;
    fststuckevt = -9999;
    precell = -9999;
    scellcnt = 0;

    sort(evcel[imodule].begin(),evcel[imodule].end(),sort_at_first);

    vector<Pair>::iterator pair_end = evcel[imodule].end();
    for(vector<Pair>::iterator i = evcel[imodule].begin(); i != pair_end; ++i ) {
      int evtnum = (*i).first;
      int cellnum = (*i).second;
      //      printf( "imod:Evt:Cell = %d, %d, %d \n", imodule, evtnum, cellnum );

      if( scellcnt==0 ) fststuckevt = evtnum;

      if( (cellnum==precell) && cellnum>0 && cellnum<65 ) {
	scellcnt++;
      } else {
	scellcnt = 0;
      }

      if(scellcnt>numsc) {
	stuckevt[imodule] = fststuckevt;
	break;
      }
      precell = cellnum;
    }

  }

  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) {
    int packetID = m_address->getStripPacketID(imodule);
    if(verbosity>0) cout<<"SvxStripEventQA::Set stucked cell : packet = "<<packetID<<" : evt = "<<stuckevt[imodule]<<endl;
    d_qainfo->set_CellStuckEvent(imodule,stuckevt[imodule]);
  }

  return EVENT_OK;
}

int SvxStripEventQA::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  // Look for the RUN node
  PHCompositeNode *runNode;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if (!runNode) { cerr << PHWHERE << "RUN node missing, abort event." << endl; return ABORTEVENT; }

  // Find/Create EventInfo node
  PHIODataNode<PHObject>* SvxQAInfoNode = NULL;
  SvxQAInfoNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxQAInfo");
  if (!SvxQAInfoNode)
    {
      d_qainfo = new SvxQAInfov1();
      SvxQAInfoNode = new PHIODataNode<PHObject>(d_qainfo, "SvxQAInfo", "PHObject");
      runNode->addNode(SvxQAInfoNode);
    }

  return EVENT_OK;
}

int SvxStripEventQA::CreateQAHisto()
{
  Fun4AllServer *se = Fun4AllServer::instance();
  char buff1[255], buff2[255];
  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++)
    {
      int packetID = m_address->getStripPacketID(imodule);
      sprintf(buff1,"hSvxSCellvsEvt_%02d",imodule);
      sprintf(buff2,"CellID vs event number : packet %d",packetID);
      hSvxSCellvsEvt[imodule] = new TH2F(buff1,buff2,1000,0,10000000,65,0,65);
      se->registerHisto(buff1,hSvxSCellvsEvt[imodule]);

      //////////////
      sprintf(buff1,"h1_cellidcount_%d", imodule);
      sprintf(buff2,"CellID count : module %d", imodule);
      h1_cellidcount[imodule] = new TH1F(buff1, buff2, 65, 0, 65);
      se->registerHisto(buff1, h1_cellidcount[imodule]);

    }


  if(m_cellidhist>0){
   for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++)
     {
       //////////////
       sprintf(buff1,"h3_cellidchipevt_%d", imodule);
       sprintf(buff2,"CellID vs chip vs event number : module %d", imodule);
       h3_cellidchipevt[imodule] = new TH3F(buff1,buff2, 500,0,50000000, 72,0,72, 64,0,256);
       se->registerHisto(buff1,h3_cellidchipevt[imodule]);

       //////////////
       sprintf(buff1,"h3_celliddiffchipevt_%d", imodule);
       sprintf(buff2,"CellID Diff vs chip vs event number : module %d", imodule);
       h3_celliddiffchipevt[imodule] = new TH3F(buff1,buff2, 500,0,50000000, 72,0,72, 65,-65,65);
       se->registerHisto(buff1,h3_celliddiffchipevt[imodule]);

       //////////////
       sprintf(buff1,"h2_cellidavgevt_%d", imodule);
       sprintf(buff2,"CellID avg vs event number : module %d", imodule);
       h2_cellidavgevt[imodule] = new TH2F(buff1,buff2, 500,0, 50000000, 66,0,66);
       se->registerHisto(buff1,h2_cellidavgevt[imodule]);

       //////////////
       sprintf(buff1,"h2_cellidndiffevt_%d", imodule);
       sprintf(buff2,"CellID ndiff vs event number : module %d", imodule);
       h2_cellidndiffevt[imodule] = new TH2F(buff1,buff2, 500,0, 50000000, 66,0,66);
       se->registerHisto(buff1,h2_cellidndiffevt[imodule]);

     }
  }

  if(m_cellidadc>0){
    for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) {
      for(int isens=0; isens<6; isens++) {
        for(int i=0; i<2; i++) {
          //////////////
          sprintf(buff1,"h2_chipadc_%d_%d_%d", imodule, isens, i);
          sprintf(buff2,"ADC vs chip : module %d, sensor %d, kind %d", imodule, isens, i);
          h2_adcchan[imodule][isens][i] = new TH2F(buff1,buff2, 64,0,256, 12*128, 0, 12*128);
          se->registerHisto(buff1, h2_adcchan[imodule][isens][i]);
        }
      }
    }
  }


  return 0;
}

void SvxStripEventQA::fillCellIDHistoByDecoder(Event* event){
  if(m_cellidhist==0) return;

  if(event==NULL){
    cout<<PHWHERE<<" Event object is NULL"<<endl;
    return;
  }

  long evtseq = event->getEvtSequence();


  //int nwords=0;
  //int rawdata[5000];
  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) {

    //    int packetID = 24101 + imodule;
    int packetID = m_address->getStripPacketID(imodule); // packetID is 24101-24140

    Packet *packet = event->getPacket(packetID);

    if ( !packet ) {
      //     cout <<"No SVX STRIPIXEL packet : "<<packetID<<" , skipping..." << endl;
      continue;
    }
   
    int layer = m_address->getStripLayer(imodule);

    //////////////// 
    //  use martins decode function
//    int layer = m_address->getStripLayer(imodule);

    int nsens = (layer==2) ? SVXSENSORSLAYER2 : SVXSENSORSLAYER3;

    h1_cellidcount[imodule]->Reset();
    for(int isens=0; isens<nsens; isens++){
      for(int ichip=0; ichip<12; ichip++){
        int cellid = packet->iValue(isens, ichip, "CELLID");
        h1_cellidcount[imodule]->Fill(cellid);
      }
    }

    int maxbin = h1_cellidcount[imodule]->GetMaximumBin();
    int max    = h1_cellidcount[imodule]->GetBinLowEdge(maxbin);
    //cout<<maxbin<<" "<<max<<endl;
  
    int ndiff = 0;
    for(int isens=0; isens<nsens; isens++){
      for(int ichip=0; ichip<12; ichip++){
        int cellid = packet->iValue(isens, ichip, "CELLID");
        if(max!=cellid){
          //cout<<imodule<<" "<<isens<<" "<<ichip<<" : "<<cellid<<" "<<max<<endl;
          h3_cellidchipevt[imodule]->Fill(evtseq, isens*12+ichip, cellid);  // cellid=0-256, 0-50M/1000

          int diff = (cellid-max);
          if(abs(diff)>=65) diff=65;
          h3_celliddiffchipevt[imodule]->Fill(evtseq, isens*12+ichip, diff);  // cellid=0-256, 0-50M/1000
          ndiff++;
        }
      }
    }
    if(max>=65) max=65;
    h2_cellidavgevt[imodule]->Fill(evtseq, max);              // cellid=0-256, 0-50M/1000
    if(ndiff>=65) ndiff=65;
    h2_cellidndiffevt[imodule]->Fill(evtseq, ndiff);  // cellid=0-256, 0-50M/1000

    delete packet;
  }
}

void SvxStripEventQA::fillAdcChip(Event* event){
  if(m_cellidadc==0) return;

  if(event==NULL){
    cout<<PHWHERE<<" Event object is NULL"<<endl;
    return;
  }

  // fillADC

  for(int imodule=0; imodule<SVXNMODULESTRIP; imodule++) {

    //    int packetID = 24101 + imodule;
    int packetID = m_address->getStripPacketID(imodule); // packetID is 24101-24140

    Packet *packet = event->getPacket(packetID);

    if ( !packet ) {
      //     cout <<"No SVX STRIPIXEL packet : "<<packetID<<" , skipping..." << endl;
      continue;
    }

    //////////////// 
    //  use martins decode function
    int layer = m_address->getStripLayer(imodule);

    int nsens = (layer==2) ? SVXSENSORSLAYER2 : SVXSENSORSLAYER3;

    h1_cellidcount[imodule]->Reset();
    for(int isens=0; isens<nsens; isens++){
      for(int ichip=0; ichip<12; ichip++){
        int cellid = packet->iValue(isens, ichip, "CELLID");
        h1_cellidcount[imodule]->Fill(cellid);
      }
    }

    int maxbin = h1_cellidcount[imodule]->GetMaximumBin();
    int max    = h1_cellidcount[imodule]->GetBinLowEdge(maxbin);
    //cout<<maxbin<<" "<<max<<endl;
  
    for(int isens=0; isens<nsens; isens++){
      for(int ichip=0; ichip<12; ichip++){
        int cellid = packet->iValue(isens, ichip, "CELLID");

        for(int ich=0; ich<128; ich++){
          int ichannel = 128*ichip+ich;
          int adc = packet->iValue(isens, ichannel);

          int idx = (cellid==max) ? 0 : 1;

          // cout<<imodule<<" "<<isens<<" "<<ichip<<" "<<ich<<" "<<ichannel<<" "<<idx<<" "<<cellid<<" "<<max<<endl;
 
          h2_adcchan[imodule][isens][idx]->Fill(adc, ichannel); // adc:0-256, chan 0-11*128
        }
      }
    }

    delete packet;
  }
}
