//-----------------------------------------------------------------------------
//  Implementation of class TofEvtHeader
//
//  Author: Akio Kiyomichi
//-----------------------------------------------------------------------------
#include "TofEvtHeader.hh"

#include "dTofEvtHeaderWrapper.h"
#include "dEventHeaderWrapper.h"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"

#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;
typedef PHDataNode<Event>     EventNode_t;

TofEvtHeader::TofEvtHeader(){
  iDebug = 0;

  run = 0; date = 0; time = 0; evtseq = 0;
  scaledtrig = 0; rawtrig = 0; livetrig = 0;
}

PHBoolean TofEvtHeader::event(PHCompositeNode *root){
  // get Header info. from PRDF/Event
  PHNodeIterator iii(root),*jjj;
  PHCompositeNode *tofNode, *outNode ;
  TableNode_t *d;
  EventNode_t *evtNode;

  tofNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "TOF"));
  if (!tofNode) {
    tofNode = new PHCompositeNode("TOF");
    root->addNode(tofNode);
  }

  // Find the node with the Event object; it is called "PRDF".
  evtNode = static_cast<EventNode_t*>(iii.findFirst("PHDataNode", "PRDF"));
  if (!evtNode) {
    return False;
  }

  // Extract the Event object from the node.
  Event* event = evtNode->getData();
  if (!event) {
    return False;
  }

  // Find the dTofEvtHeader output table; if not found, create it.
  // Extract the data from the dTofEvtHeader
  dTofEvtHeaderWrapper *dTofEvtHeader;

  outNode = tofNode;
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofEvtHeader"));
  if (!d) {
     dTofEvtHeader = new dTofEvtHeaderWrapper("dTofEvtHeader", 1);
     if (!dTofEvtHeader) {
       return 1;
     }
     d = new TableNode_t(dTofEvtHeader,"dTofEvtHeader");
     outNode->addNode(d);
  } else {
    dTofEvtHeader = static_cast<dTofEvtHeaderWrapper*>(d->getData());
    if (!dTofEvtHeader){ cerr<<" Error"<< endl; exit(1);}
    dTofEvtHeader->SetMaxRowCount(1);
  }
  delete jjj;

  //=======================================================================
  // Main program
  //=======================================================================

  // Event info
  run = event->getRunNumber();
  date = event->getDate();
  time = event->getTime();

  evtseq = event->getEvtSequence();

  (*dTofEvtHeader)[0].run    = run;
  (*dTofEvtHeader)[0].date   = date;
  (*dTofEvtHeader)[0].time   = time;
  (*dTofEvtHeader)[0].evtseq = evtseq;

  // GL1 info
  Packet *pGL1;
  static const int id_gl1 = 14001;
  int id = id_gl1;
  if ((pGL1 = event->getPacket(id)) != 0) {

    scaledtrig = pGL1->iValue(0,"SCALEDTRIG");
    rawtrig    = pGL1->iValue(0,"RAWTRIG");
    livetrig   = pGL1->iValue(0,"LIVETRIG");

    (*dTofEvtHeader)[0].scaledtrig = scaledtrig;
    (*dTofEvtHeader)[0].rawtrig    = rawtrig;
    (*dTofEvtHeader)[0].livetrig   = livetrig;

    delete pGL1;
  }

  if(iDebug > 0){
    cout.setf(ios::showbase);
    cout<<" Run # "<<run<<" Event # "<<evtseq<<endl;
    cout<<" GL1 Trigger:";
    cout.setf(ios::hex, ios::basefield);
    cout<<"   scaled "<< scaledtrig;
    cout<<"   raw "<< rawtrig;
    cout<<"   live "<< livetrig;
    cout<<endl;
    cout.setf(ios::dec, ios::basefield);
  }

  dTofEvtHeader->SetRowCount(1);

  return True;
}

PHBoolean TofEvtHeader::setFromDst(PHCompositeNode *root){
  // get Header info. from DST/dEventHeader
  PHPointerList<PHNode> nodes;
  PHNodeIterator iii(root),*jjj;
  PHCompositeNode *dstNode, *tofNode, *outNode ;
  PHNode *n;
  TableNode_t *d;

  tofNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "TOF"));
  if (!tofNode) {
    tofNode = new PHCompositeNode("TOF");
    root->addNode(tofNode);
  }
  dstNode = static_cast<PHCompositeNode*>(iii.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) {
    dstNode = new PHCompositeNode("DST");
    root->addNode(dstNode);
  }

  // Extract the data from the dEventHeader
  dEventHeaderWrapper *dEventHeader;
  outNode = dstNode;
  n = iii.findFirst("PHIODataNode", "dEventHeader");
  if (!n) {
    cout << "ERROR:  'in' parameter dEventHeader not found" << endl;
    dEventHeader = new dEventHeaderWrapper("dEventHeader", 1);
    if (!dEventHeader){ return 1;}
    n = new TableNode_t(dEventHeader,"dEventHeader");
    outNode->addNode(n);
  }
  nodes.append(n);
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dEventHeader"));
  if (!d){ 
    cerr << "  Error "<< endl; return 1;
  } else {
    dEventHeader = static_cast<dEventHeaderWrapper*>(d->getData());
    if (!dEventHeader){ cerr<<" Error "<< endl; return 1;}
  }
  delete jjj;

  // Find the dTofEvtHeader output table; if not found, create it.
  // Extract the data from the dTofEvtHeader
  dTofEvtHeaderWrapper *dTofEvtHeader;
  outNode = tofNode;
  jjj = new PHNodeIterator(outNode);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode","dTofEvtHeader"));
  if (!d) {
     dTofEvtHeader = new dTofEvtHeaderWrapper("dTofEvtHeader", 1);
     if (!dTofEvtHeader) { return 1;}
     d = new TableNode_t(dTofEvtHeader,"dTofEvtHeader");
     outNode->addNode(d);
  } else {
    dTofEvtHeader = static_cast<dTofEvtHeaderWrapper*>(d->getData());
    if (!dTofEvtHeader){ cerr<<" Error "<< endl; exit(1);}
    dTofEvtHeader->SetMaxRowCount(1);
  }
  delete jjj;

  //=======================================================================
  // Main program
  //=======================================================================

  // Event info
  run    = (*dEventHeader)[0].run;
  date   = (*dEventHeader)[0].date;
  time   = (*dEventHeader)[0].time;
  evtseq = (*dEventHeader)[0].event;

  scaledtrig = (*dEventHeader)[0].trigScaled[0];
  rawtrig    = (*dEventHeader)[0].trigRaw[0];
  livetrig   = (*dEventHeader)[0].trigLive[0];

  (*dTofEvtHeader)[0].run    = run;
  (*dTofEvtHeader)[0].date   = date;
  (*dTofEvtHeader)[0].time   = time;
  (*dTofEvtHeader)[0].evtseq = evtseq;
  (*dTofEvtHeader)[0].scaledtrig = scaledtrig;
  (*dTofEvtHeader)[0].rawtrig    = rawtrig;
  (*dTofEvtHeader)[0].livetrig   = livetrig;

  return True;
}

PHBoolean TofEvtHeader::event(Event *e){

  run = e->getRunNumber();
  date = e->getDate();
  time = e->getTime();

  evtseq = e->getEvtSequence();
  
  scaledtrig = 0; rawtrig = 0; livetrig = 0;
  Packet *pGL1;
  if ((pGL1 = e->getPacket(14001)) != 0) {
    scaledtrig = pGL1->iValue(0,"SCALEDTRIG");
    rawtrig    = pGL1->iValue(0,"RAWTRIG");
    livetrig   = pGL1->iValue(0,"LIVETRIG");
    if(iDebug > 0){
      cout.setf(ios::showbase);
      cout<<" GL1 Trigger :";
      cout.setf(ios::hex, ios::basefield);
      cout<<"   scaled "<< scaledtrig;
      cout<<"   raw "<< rawtrig;
      cout<<"   live "<< livetrig;
      cout<<endl;
      cout.setf(ios::dec, ios::basefield);
    }
    delete pGL1;
  }
  return True;
}

int TofEvtHeader::getRunNumber(Event *e){
  run = e->getRunNumber();
  return run;
}
int TofEvtHeader::getDate(Event *e){
  date = e->getDate();
  return date;
}
int TofEvtHeader::getTime(Event *e){
  time = e->getTime();
  return time;
}
int TofEvtHeader::getEvtSequence(Event *e){
  evtseq = e->getEvtSequence();
  return evtseq;
}
long TofEvtHeader::getScaledTrig(Event *e){
  scaledtrig = 0;
  Packet *pGL1;
  if ((pGL1 = e->getPacket(14001)) != 0) {
    scaledtrig = pGL1->iValue(0,"SCALEDTRIG");
    delete pGL1;
  }
  return scaledtrig;
}
long TofEvtHeader::getRawTrig(Event *e){
  rawtrig = 0;
  Packet *pGL1;
  if ((pGL1 = e->getPacket(14001)) != 0) {
    rawtrig = pGL1->iValue(0,"RAWTRIG");
    delete pGL1;
  }
  return rawtrig;
}
long TofEvtHeader::getLiveTrig(Event *e){
  livetrig = 0;
  Packet *pGL1;
  if ((pGL1 = e->getPacket(14001)) != 0) {
    livetrig = pGL1->iValue(0,"LIVETRIG");
    delete pGL1;
  }
  return livetrig;
}

void TofEvtHeader::print(){
  cout.setf(ios::showbase);
  cout<<" GL1 Trigger :";
  cout.setf(ios::hex, ios::basefield);
  cout<<"   scaled "<< scaledtrig;
  cout<<"   raw "<< rawtrig;
  cout<<"   live "<< livetrig;
  cout<<endl;
  cout.setf(ios::dec, ios::basefield);
}
