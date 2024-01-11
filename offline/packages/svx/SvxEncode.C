// ===============
// FILE: SvxEncode.C
// ===============

// ******************************************************
//
// Class: SvxEncode implementation
//
// Author:  Sasha Lebedev (lebedev@iastate.edu)
// 
// Revisions: June 2010 - initial version
//
// ***************************************************************************

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <cstdio>

#include <SvxCommon.h>
#include <SvxEncode.h>
#include <SvxRawhitv4.h>
#include <SvxRawhitListv4.h>
#include <SvxPacket.h>
#include <Svx_stripixelPacket.h>
#include <SvxPacketList.h>
#include <Svx_stripixelPacketList.h>

//#include <svxAddress.hh>

#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>

#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHRawDataNode.h>
#include <getClass.h>


/////////////
#include "encodeString.h"
#include "packetConstants.h"
//#include "svxDetectorGeo.hh"
/////////////

#include <TRandom3.h>
#include <TFile.h>

using namespace std;

// Helpers for scanning Node Tree...
typedef PHIODataNode <PHObject>             PHObjectNode_t;
typedef PHIODataNode <SvxRawhitList>        SvxRawhitListNode_t;
typedef PHIODataNode <SvxPacketList>        SvxPacketListNode_t;
typedef PHIODataNode <Svx_stripixelPacketList>        Svx_stripixelPacketListNode_t;

//----------------------------------------------------------------------------------------------------

SvxEncode::SvxEncode(const string &name)
  : SubsysReco(name),
    d_rawhit(0x0),
    d_event(0x0), 
    d_event_stripixel(0x0),
    nSvxRawhits(0),  
    EventNumber(0)
{
  for (int i=0; i<60; i++) 
    m_rawDataArray[i] = 0x0;
  
  for (int i=0; i<40; i++)
    m_rawDataArray_stripixel[i] = 0x0;
}

//------------------------------------------------------------------------------------------------------

// Run-independent initialization
int SvxEncode::Init(PHCompositeNode *topNode)
{
  if(verbosity>0) cout << "SvxEncode::Init() Execution started." << endl;

  if(verbosity>0) cout << "SvxEncode::Init() Execution completed." << endl;

  return EVENT_OK;

}

//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxEncode::InitRun(PHCompositeNode *topNode)
{

  if(verbosity>0) cout << "SvxEncode::InitRun() Execution started.." << endl;

  int i = CreateNodeTree(topNode);
  if(verbosity>0) cout << "SvxEncode::InitRun() CreateNodeTree() returned " << i << endl;
  if(!(i==EVENT_OK)) {return EVENT_OK;}

  ///////////////////////
  // added by T.Hachiya 2011.06.17
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  //svxAddress& SvxAddressObject = *address;
  ///////////////////////


  if(verbosity>0) cout << "SvxEncode::InitRun() Node tree created." << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int SvxEncode::process_event(PHCompositeNode *topNode)
{

  if(verbosity>0) cout << "SvxEncode::process_event() Execution started..." <<endl;

  //int iError;
  if(verbosity>0 && EventNumber==0)  { cout << "SvxEncode topNode:"<< endl; topNode->print(); }

//  // Initialization
  nSvxRawhits        = 0;

  if(verbosity>0) cout << "SvxEncode::process_event() Getting raw hits..." <<endl;
  PHTypedNodeIterator<SvxRawhitList> iRAWHIT(topNode);
  SvxRawhitListNode_t *RAWHIT = iRAWHIT.find("SvxRawhitList");
  if (RAWHIT) d_rawhit = RAWHIT->getData();
  if (!d_rawhit) {
    cerr << PHWHERE << "SvxEncode ERROR: rawhit data not in the Node Tree" << endl;
    return EVENT_OK;
  }

  nSvxRawhits = d_rawhit->get_nRawhits();

  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  svxAddress& SvxAddressObject = *address;

  if(verbosity>0) cout << "SvxEncode::process_event() Getting PRDF node..." <<endl;
  PHTypedNodeIterator<SvxPacketList> iP(topNode);
  PHTypedNodeIterator<Svx_stripixelPacketList> iQ(topNode);

  SvxPacketListNode_t *P = iP.find("PRDFpixel");
  if (P) d_event = P->getData();
  if (!d_event) {
    cerr << PHWHERE << "SvxEncode ERROR: PRDF node not in the Node Tree" << endl;
    return EVENT_OK;
  }
  Svx_stripixelPacketListNode_t *Q = iQ.find("PRDFstrip");
  if (Q) d_event_stripixel = Q->getData();
  if (!d_event_stripixel) {
    cerr << PHWHERE << "SvxEncode ERROR: PRDF node not in the Node Tree" << endl;
    return EVENT_OK;
  }

  std::cout << "======== Event # " << EventNumber << ";     # of raw hits = " << nSvxRawhits << std::endl;

  /////////////////////////////////////
  // convert Hitdata to PRDF for pixel
  convertHitToPRDFPixel(SvxAddressObject);

  /////////////////////////////////////
  // convert Hitdata to PRDF for stripixel
  convertHitToPRDFStripixel(SvxAddressObject);

   /////////////////////////////////////
  // add by Yi Gu 2011.1.09
  // Copy PRDF data to SIMPRDF node
  copyToSIMPRDF_stripixel(topNode);
  
  /////////////////////////////////////
  // add by TH 2010.11.09
  // Copy PRDF data to SIMPRDF node
  copyToSIMPRDF(topNode);
  if(verbosity>0) { comparePRDFWithRawHit(topNode); }

  /////////////////////////////////////


  if(verbosity>0) {cout << "SvxEncode::process_event() Event processed." <<endl;}
  EventNumber++;
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------------------

// Create the data
int SvxEncode::CreateNodeTree(PHCompositeNode *topNode) 
{

  if(verbosity>0) cout << "SvxEncode::CreateNodeTree() Execution started." << endl;

  PHNodeIterator iter(topNode);

  // Looking for the DST node
  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) { cerr << PHWHERE << "DST node missing, doing nothing." << endl; return EVENT_OK; }

  // Find SVX node.
  PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*> (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode) { cerr << PHWHERE << "SVX node missing, doing nothing." << endl; return EVENT_OK; }
  
//  PHIODataNode<PHObject>* SvxRawhitListNode = NULL;
//  SvxRawhitListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxRawhitList");
//  if (!SvxRawhitListNode) { cerr << PHWHERE << "SvxRawhitList node missing, doing nothing." << endl; return EVENT_OK; }

  PHIODataNode<PHObject>* SvxPacketListNode = NULL;
  SvxPacketListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "PRDFpixel");
  if (!SvxPacketListNode)
    {
      //SvxPacketList* d_event = new SvxPacketListv1();
      d_event = new SvxPacketList();
      SvxPacketListNode =
        new PHIODataNode<PHObject>(d_event, "PRDFpixel", "PHObject");
      //topNode->addNode(SvxPacketListNode);
      svxNode->addNode(SvxPacketListNode);
      cout << PHWHERE << " PRDF node created for output." << endl;
    }

  
  PHIODataNode<PHObject>* Svx_stripixelPacketListNode = NULL;
  Svx_stripixelPacketListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "PRDFstrip");
  if (!Svx_stripixelPacketListNode)
    {
      //SvxPacketList* d_event = new SvxPacketListv1();
      d_event_stripixel = new Svx_stripixelPacketList();
      Svx_stripixelPacketListNode =
        new PHIODataNode<PHObject>(d_event_stripixel, "PRDFstrip", "PHObject");
      //topNode->addNode(SvxPacketListNode);
      svxNode->addNode(Svx_stripixelPacketListNode);
      cout << PHWHERE << " PRDF node created for output." << endl;
    }

  if(verbosity>0) cout << "SvxEncode::CreateNodeTree() Execution completed." << endl;




  return EVENT_OK;

}

//--------------------------------------------------------------------------------------------

int SvxEncode::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}


///////////////////////////////////
// convert Hitdata to DWORD in  PRDF data.
// add this function 2010.12.26 T.Hachiya  
// add argument      2011.06.17 T.Hachiya
int SvxEncode::convertHitToPRDFPixel(svxAddress& SvxAddressObject)
{
  if(verbosity>0) { cout << "SvxEncode::convertHitToPRDFPixel"<<endl; }

  if (!d_rawhit) {
    cerr << PHWHERE << "SvxEncode ERROR: rawhit data not in the Node Tree" << endl;
    return EVENT_OK;
  }

  if (!d_event) {
    cerr << PHWHERE << "SvxEncode ERROR: PRDF node not in the Node Tree" << endl;
    return EVENT_OK;
  }

  static const int baseword = 13;

  nSvxRawhits = d_rawhit->get_nRawhits();

  // initial setup of the Packets

  TRandom rnd = TRandom3(EventNumber*10+nSvxRawhits);

  for(int i=0; i<SVXNMODULEPIXEL; i++) {
    int a0=EventNumber;
    int a1=SVXDETECTORID;
    int a2=i;
    SvxPacket tmp = SvxPacket();
    tmp.setArray(0,a0);
    tmp.setArray(1,a1);
    tmp.setArray(2,a2);
    int temp1 = (int)rnd.Gaus(4000., 10.);
    int temp2 = (int)rnd.Gaus(4000., 10.);
    tmp.setArray(4110,temp1); // TH: index is wrong. changed (2010.11.15)  tmp.setArray(14,temp1);
    tmp.setArray(4111,temp2); // TH: index is wrong. changed (2010.11.15)  tmp.setArray(15,temp2);
    d_event->AddSvxPacket(&tmp);
 //   tmp.show();
  }


  //int rhcount=0;
  for(int i=0; i<nSvxRawhits; i++) {
    SvxRawhit* tmp = d_rawhit->get_Rawhit(i);
    int layer = tmp->get_layer();
    if(layer>1) continue;

    int ladder  = tmp->get_ladder();
    int sensor  = tmp->get_sensor();
    int section = tmp->get_sensorSection();
    int channel = tmp->get_channel();
    int module  = tmp->get_pixelModule();
    int chip    = tmp->get_pixelROC();

    int row    = SvxAddressObject.getPixelRocIX0(channel);
    int column = SvxAddressObject.getPixelRocIZ0(channel);
    int lsbmsb = column/16; // is it lsb or msb? 0 -> lsb, 1 -> msb
    int lsbmsbadd = (lsbmsb==0) ? 4 : 0; // first 4 msb then 4 lsb

    int chipadd     = ((chip%2)==0) ? 0 : 2048; // even numbered chips first, odd numbered chips after them
    int minichannel = column - lsbmsb*16; // channel in 16 bit word

    int iword = row*8 + chip/2 + lsbmsbadd + chipadd + baseword; // word number in a packet (must be from 13 to 4108)
    if(iword<baseword || iword>=(baseword+4096)) {
      std::cerr << PHWHERE << "SvxEncode ERROR: wrong word number: " << iword << std::endl;
    }

    if(verbosity>0) {
      cout<<"encodeed raw hit:"<<layer<<" "<<ladder<<" "<<sensor<<" "<<section<<" "<<chip<<" ";
      cout<<module<<" "<<channel<<" "<<row<<" "<<column<<" ";
      cout<<iword<<" "<<minichannel<<endl;
    }

    int theword = (((1 << minichannel) & 0xffff) ^ 0xffff); // fired bit = 0, the rest are 1, east arm

    int *tmparray = (d_event->getPacket(module))->getArray();
    int oldword   = tmparray[iword];
    int newword   = oldword & theword;
    tmparray[iword] = newword;
    d_event->setPacket(module, tmparray);
  }

  return 0;
}


///////////////////////////////////
// convert Hitdata to DWORD in  PRDF data for stripixel
// Yi Gu
// 01/07/2011


int SvxEncode::convertHitToPRDFStripixel(svxAddress& SvxAddressObject)
{
  if(verbosity>0) { cout << "SvxEncode::convertHitToPRDFStripixel"<<endl; }

  if (!d_rawhit) {
    cerr << PHWHERE << "SvxEncode ERROR: rawhit data not in the Node Tree" << endl;
    return EVENT_OK;
  }
  
  if (!d_event_stripixel) {
    cerr << PHWHERE << "SvxEncode ERROR: PRDF node not in the Node Tree" << endl;
    return EVENT_OK;
  }
  
  static const int baseword_stripixel = 20;
  int *tmparray_stripixel;
  nSvxRawhits = d_rawhit->get_nRawhits();

  // initial setup of the Packets

  
  for(int i=0; i<SVXNMODULESTRIP; i++) {//The stripixel uses 40 packet ID (24101 - 24140)
        
    int a0_stripixel=EventNumber;
    int a1_stripixel=SVXDETECTORID;
    int a2_stripixel=1; // bit 0:, Zero Supp, ON(1)/OFF(0)
    Svx_stripixelPacket tmp_stripixel = Svx_stripixelPacket();
    tmp_stripixel.setArray(0,a0_stripixel);
    tmp_stripixel.setArray(2,a1_stripixel);
    tmp_stripixel.setArray(1,a2_stripixel);
    tmp_stripixel.setArray(3,0); //Module ID, temperarily set to be 0
    tmp_stripixel.setArray(5,0); 
    tmp_stripixel.setArray(6,0); 
    tmp_stripixel.setArray(7,0); 
    
    tmp_stripixel.setArray(4662, 0xC03F);  // [15,14, 5-0]=[ADC/Chan, PedCorr On/Off, EnaRCC] added by TH. 2011.02.25
                                           // 15->1(Adc), 14->1(On), 5-0->(Ena)
   
    d_event_stripixel->AddSvx_stripixelPacket(&tmp_stripixel);
    
  }

   
  for(int i=0; i<nSvxRawhits; i++) {
    SvxRawhit* tmp = d_rawhit->get_Rawhit(i);
    int layer_stripixel = tmp->get_layer();
    //cout<<"  id: "<<i<<", layer="<<layer_stripixel<<endl;
    if(layer_stripixel<2) continue;

    int ladder_stripixel = tmp->get_ladder();
    int sensor_stripixel = tmp->get_sensor();
    int section_stripixel = tmp->get_sensorSection();
    int channel_stripixel = tmp->get_channel();
    int readout_stripixel = tmp->get_sensorReadout();
    int adc = tmp->get_adc();
    
    int chip_stripixel = 0;
    int invert_flag = 0; //0:normal; 1:invert
    
    // determine the chip number based on channel, section and "X"or"U" info;
    // section_stripixel=0->L, =1->R; readout_stripixel=0->X, =1->U;
    if(section_stripixel == 0 && (channel_stripixel>=256 && channel_stripixel<=383) && readout_stripixel==1) 
       {chip_stripixel = 0; invert_flag = 1;}
    if(section_stripixel == 0 && (channel_stripixel>=256 && channel_stripixel<=383) && readout_stripixel==0) 
       {chip_stripixel = 1; invert_flag = 1;}
    if(section_stripixel == 0 && (channel_stripixel>=128 && channel_stripixel<=255) && readout_stripixel==0) 
       {chip_stripixel = 2; invert_flag = 1;}
    if(section_stripixel == 1 && (channel_stripixel>=256 && channel_stripixel<=383) && readout_stripixel==0) 
       {chip_stripixel = 3; invert_flag = 1;}
    if(section_stripixel == 1 && (channel_stripixel>=256 && channel_stripixel<=383) && readout_stripixel==1) 
       {chip_stripixel = 4; invert_flag = 1;}
    if(section_stripixel == 1 && (channel_stripixel>=128 && channel_stripixel<=255) && readout_stripixel==1) 
       {chip_stripixel = 5; invert_flag = 1;}
    if(section_stripixel == 1 && (channel_stripixel>=0   && channel_stripixel<=127) && readout_stripixel==1) 
       {chip_stripixel = 6;}
    if(section_stripixel == 1 && (channel_stripixel>=0   && channel_stripixel<=127) && readout_stripixel==0) 
       {chip_stripixel = 7;}
    if(section_stripixel == 1 && (channel_stripixel>=128 && channel_stripixel<=255) && readout_stripixel==0) 
       {chip_stripixel = 8;}
    if(section_stripixel == 0 && (channel_stripixel>=0   && channel_stripixel<=127) && readout_stripixel==0) 
       {chip_stripixel = 9;}
    if(section_stripixel == 0 && (channel_stripixel>=0   && channel_stripixel<=127) && readout_stripixel==1) 
       {chip_stripixel = 10;}
    if(section_stripixel == 0 && (channel_stripixel>=128 && channel_stripixel<=255) && readout_stripixel==1) 
       {chip_stripixel = 11;}
    
    int RCCadd = 0 ;
    if (sensor_stripixel ==0 || sensor_stripixel ==1){RCCadd = 0;}
    if (sensor_stripixel ==2 || sensor_stripixel ==3){RCCadd = 1;}
    if (sensor_stripixel ==4 || sensor_stripixel ==5){RCCadd = 2;}
 
    int RocChannel_stripixel = 0;

  // Roc RocChan : channel : description
  //  0   0-127  : 383-256 :  U1L (step=2,inv=1)
  //  1   0-127  : 383-256 :  X1L (step=2,inv=1)
  //  2   0-127  : 255-128 :  X2L (step=1,inv=1)
  //  3   0-127  : 383-256 :  X1R (step=2,inv=1)
  //  4   0-127  : 383-256 :  U1R (step=2,inv=1)
  //  5   0-127  : 255-128 :  U2R (step=1,inv=1)
  //  6   0-127  :   0-127 :  U3R (step=0,inv=0)
  //  7   0-127  :   0-127 :  X3R (step=0,inv=0)
  //  8   0-127  : 128-255 :  X2R (step=1,inv=0)
  //  9   0-127  :   0-127 :  X3L (step=0,inv=0)
  // 10   0-127  :   0-127 :  U3L (step=0,inv=0)
  // 11   0-127  : 128-255 :  U2L (step=1,inv=0)

  // define step = 0:0-127, 1:128-255, 2:256-383
  // step0= 6,7,9,10 1=2,5,8,11 2=0,1,3,4
  // define invert = 0: normal, 1:invert
  // 0:6,7,8,9,10,11  1:0,1,2,3,4,5
 
    if (channel_stripixel<0 || channel_stripixel>383) continue;
   
    if (invert_flag == 1) 
         {RocChannel_stripixel = 127-channel_stripixel%128;}
    else {RocChannel_stripixel = channel_stripixel%128;}
    

    int chipadd_stripixel = (128*3*chip_stripixel)+(chip_stripixel*3);
    int channeladd = RocChannel_stripixel*3;

 
    int iword_stripixel =  chipadd_stripixel + channeladd + RCCadd + baseword_stripixel; // word number in a packet (from 20 to 4660)
    if(iword_stripixel<baseword_stripixel || iword_stripixel>=(baseword_stripixel+4644-3)) 
    {std::cerr << PHWHERE << "SvxEncode ERROR: wrong word number: " << iword_stripixel << std::endl;}


    int theword_stripixel = 0;

    if (!(sensor_stripixel%2==0)){
      theword_stripixel = (adc << 8); //ADC value from odd-numbered RCC stored in upper 8 bits 
    }
    if (sensor_stripixel%2==0)  {theword_stripixel = adc;}
    
    if(verbosity>0){ 
      cout<<"encoded raw hit: layer="<<layer_stripixel<<" ladder= "<<ladder_stripixel;
      cout<<" RCC= "<<sensor_stripixel<<" section(L/R)= "<<section_stripixel;
      cout<<" strip type= "<<readout_stripixel<<" read-in channel= "<<channel_stripixel;
      cout<<" roc channel(0-127)= "<<RocChannel_stripixel;
      cout<<" chip= "<<chip_stripixel<<" word number= "<<iword_stripixel<<" adc value= "<<adc<<endl;
    }
    
    int module_stripixel = (layer_stripixel-2)*16+ladder_stripixel;

    if(verbosity>0){
      std::cout << "encoding PRDF data on Packet No.: " << module_stripixel+24101<< " " << std::endl; 
    }

    tmparray_stripixel = (d_event_stripixel->getPacket(module_stripixel))->getArray();
    int oldword_stripixel = tmparray_stripixel[iword_stripixel];
    int newword_stripixel = oldword_stripixel | theword_stripixel;//OR operation
    tmparray_stripixel[iword_stripixel] = newword_stripixel;
    //cout<<"packet,iword,theword: "<<module<<" "<<iword<<" "<<oldword<<" "<<theword<<" "<<newword<<endl;
    d_event_stripixel->setPacket(module_stripixel, tmparray_stripixel);
    
    
  }
   

  if(verbosity>0){
   cout<<"Entry : "<<d_event_stripixel->GetSvx_stripixelPackets()->GetEntries()<<endl;
  
   Svx_stripixelPacket* tmp = d_event_stripixel->getPacket(0);
   tmp->show_stripixel();
  }

  return 0;
}


/////////////////////////////////
// copy PRDF data to SIMPRDF node
// T.Hachiya
// 2010.11.09
int SvxEncode::copyToSIMPRDF(PHCompositeNode *topNode)
{
  if(verbosity>0) cout<<PHWHERE<<" copyToSIMPRDF:"<<endl;

  PHNodeIterator iter(topNode);
  PHCompositeNode* prdfNode;
  PHNode *n1;

  static Int_t iCall = 0; // used to initialize the rawdata pointers


  //Find the PRDF node
  n1 = iter.findFirst("PHCompositeNode", "SIMPRDF");
  if (!n1) {
    cerr << "\n copyToSIMPRDF <E>: Unable to find SIMPRDF subnode, exiting \n\n" << endl;
    exit(1);
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
    cout<<PHWHERE<<"prdfNode is found"<<endl;
  }

  static const Int_t dcmRows = SVXNMODULEPIXEL; // this must define in the definition file

  // Fill PRDF
  static const Int_t numberOfWords = 4116;       // this was hardwired in packet
  static const Int_t bytesPerWord  = 4;          // 16bit/W
  static const Int_t hitFormat     = IDPXL_DCM0; // also hardwired
  Int_t dataID                     = 24000;      // also hardwired


  if(iCall == 0) {
    for(Int_t iRow=0; iRow<dcmRows; iRow++) {
      m_rawDataArray[iRow] = 0;
    }
  }  // initialization of rawDataArray pointers
  iCall++;


  char svxPRDF[11] = "sxpPRDF000";  // assumes maximum of 999 rows

  for (Int_t iRow=0; iRow<dcmRows; iRow++){

    encodeString(svxPRDF, iRow);
    dataID++;

    SvxPacket* tmp = d_event->getPacket(iRow);
    PHDWORD *dPtr = (PHDWORD*)tmp->getArray();
    if(m_rawDataArray[iRow] == 0) {
      m_rawDataArray[iRow] = new PHRawDataNode(dPtr, svxPRDF, numberOfWords, dataID, bytesPerWord, hitFormat);
      prdfNode->addNode(m_rawDataArray[iRow]);
    }
    else {
      PHRawDataNode *rPtr = m_rawDataArray[iRow];
      rPtr->setData(dPtr);
      rPtr->setLength(numberOfWords);
      rPtr->setID(dataID);
      rPtr->setWordLength(bytesPerWord);
      rPtr->setHitFormat(hitFormat);
    }
  } // loop over dcmRows

  return 0;
}

/////////////////////////////////
// copy PRDF data to SIMPRDF node
// YI GU
// 2010.1.13
int SvxEncode::copyToSIMPRDF_stripixel(PHCompositeNode *topNode)
{
  if(verbosity>0) cout<<PHWHERE<<" copyToSIMPRDF_stripixel:"<<endl;

  PHNodeIterator iter(topNode);
  PHCompositeNode* prdfNode;
  PHNode *n1;

  static Int_t iCall = 0; // used to initialize the rawdata pointers

  //Find the PRDF node
  n1 = iter.findFirst("PHCompositeNode", "SIMPRDF");
  if (!n1) {
    cout << "\n copyToSIMPRDF <E>: Unable to find SIMPRDF subnode, exiting \n\n" << endl;
    exit(1);
  }
  else {
    prdfNode = static_cast<PHCompositeNode*>(n1);
    cout<<PHWHERE<<"prdfNode is found"<<endl;
  }


  static const Int_t dcmRows = SVXNMODULESTRIP; // this must define in the definition file

  // Fill PRDF
  static const Int_t numberOfWords = 4668;       // this was hardwired in packet
  static const Int_t bytesPerWord  = 4;          // 16bit/W
//--  static const Int_t hitFormat     = 524; // temperarily set by hand
  static const Int_t hitFormat     = IDSTRIP_DCM0; // temperarily set by hand
  Int_t dataID                     = 24100;      // also hardwired


  if(iCall == 0) {
    for(Int_t iRow_stripixel=0; iRow_stripixel<dcmRows; iRow_stripixel++) {
      m_rawDataArray_stripixel[iRow_stripixel] = 0;
    }
  }  // initialization of rawDataArray pointers
  iCall++;


  //char svxPRDF_stripixel[17] = "svxPRDF_strip000";  // assumes maximum of 999 rows
  char svxPRDF_stripixel[11] = "sxsPRDF000";  // assumes maximum of 999 rows

  for (Int_t iRow_stripixel=0; iRow_stripixel<dcmRows; iRow_stripixel++){

    encodeString(svxPRDF_stripixel, iRow_stripixel);
    dataID++;

    Svx_stripixelPacket* tmp = d_event_stripixel->getPacket(iRow_stripixel);
    PHDWORD *dPtr = (PHDWORD*)tmp->getArray();
    if(m_rawDataArray_stripixel[iRow_stripixel] == 0) {
      m_rawDataArray_stripixel[iRow_stripixel] 
	= new PHRawDataNode(dPtr, svxPRDF_stripixel, numberOfWords, dataID, bytesPerWord, hitFormat);
      prdfNode->addNode(m_rawDataArray_stripixel[iRow_stripixel]);
    }
    else {
      PHRawDataNode *rawPtr = m_rawDataArray_stripixel[iRow_stripixel];
      rawPtr->setData(dPtr);
      rawPtr->setLength(numberOfWords);
      rawPtr->setID(dataID);
      rawPtr->setWordLength(bytesPerWord);
      rawPtr->setHitFormat(hitFormat);
    }
  } // loop over dcmRows

  return 0;
}




bool SvxEncode::comparePRDFWithRawHit(PHCompositeNode *topNode)
{
  if(verbosity>0) cout<<PHWHERE<<" comparePRDFWithRawHit:"<<endl;

  ///////////////////////
  // added by T.Hachiya 2011.06.17
  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  svxAddress& SvxAddressObject = *address;

  /////////////////////////////////////
  // get hit info from PRDF and check if the hit exist in RawHit

  static const int dcmRows=SVXNMODULEPIXEL;
  //int dataID = 0;
  for (Int_t iRow=0; iRow<dcmRows; iRow++){
    PHRawDataNode *rawDN = m_rawDataArray[iRow];

    //int id = rawDN->getID();
    int packetID = rawDN->getID();
    int id = SvxAddressObject.getPixelModuleID(packetID);
    PHDWORD* darry = rawDN->getData();

    //cout<<"ID: "<<id<<endl;

    SvxPacket pkt;
    pkt.setArray((int *)darry);

    ///////////// raw data
    int detid = pkt.iValue(0, "DETID");
    int modid = pkt.iValue(0, "MODADDR");

    int layer  = SvxAddressObject.getPixelLayer(id);
    int ladder = SvxAddressObject.getPixelLadder(id);
    int modaddr = id+1; // temporary conversion

    if(verbosity>2){
      cout<<"detector:"<<detid<<"  module:"<<modid<<", layer:ladder:modaddr = "<<layer;
      cout<<":"<<ladder<<":"<<modaddr<<endl;
    }
    
    for(int ichip=0; ichip<8; ichip++){
      for(int icol=0; icol<256; icol++){
        unsigned int data = pkt.iValue(ichip, icol);
        if(data>0){
          cout<<"ichip="<<setw(3)<<ichip<<"  0x"<<setw(8)<<hex<<data<<dec<<endl;
          for(int irow=0; irow<32; irow++){
            if(((data>>irow)&0x1)==0x1){
              int hit_ch = 32*icol+irow;

              if(verbosity>2){
                cout<<" P layer:ladder = "<<layer<<":"<<ladder;
                cout<<"  modaddr: "<<modaddr<<"  chip : "<<ichip;
                cout<<"  channel: "<<hit_ch;
                cout<<"  (icol="<<setw(4)<<icol;
                cout<<"  row="<<irow<<")"<<endl; 
              }

              bool flag=false;

              // search
              for(int iraw=0; iraw<d_rawhit->get_nRawhits(); iraw++){
//                int r_section   = d_rawhit->get_Rawhit(iraw)->get_svxSection();
                int r_layer     = d_rawhit->get_Rawhit(iraw)->get_layer();       // physical position
                int r_ladder    = d_rawhit->get_Rawhit(iraw)->get_ladder();      // physical position
//                int r_sensor    = d_rawhit->get_Rawhit(iraw)->get_sensor();
                int r_type      = d_rawhit->get_Rawhit(iraw)->get_sensorType();  // 1-9 pixel, 
                int r_channel   = d_rawhit->get_Rawhit(iraw)->get_channel();     // 0-8191 (32x256)
                int r_pixel_ldr = d_rawhit->get_Rawhit(iraw)->get_pixelModule() + 1; // must be 1-60, but currently 0-59. it is bug
                int r_pixel_chip= d_rawhit->get_Rawhit(iraw)->get_pixelROC();    // 0-7

                if((modaddr==r_pixel_ldr)&&(hit_ch==r_channel)){
                  if(verbosity>2){
                    cout<<"   layer:ladder = "<<r_layer<<":"<<r_ladder;
                    cout<<"  pladder: "<<r_pixel_ldr<<"  pchip: "<<r_pixel_chip<<"  channel: "<<r_channel;
                    cout<<", type: "<<r_type<<endl;
                  }
                  flag=true;
                }

                
              }
              if(!flag){
                cout<<"   Hit data does not exist in RawHit"<<endl;
              }

            }
          }
        }
      }
    }
  } // loop over dcmRows

  return true;
}

/////////////////////////////////

