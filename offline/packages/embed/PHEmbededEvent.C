// -----------------------------------------
// Created by:  Jiangyong Jia
//------------------------------------------
#include "PHEmbededEvent.hh"
#include "PHEmbedStat.h"
#include "BbcOut.h"
#include "CglTrack.h"
#include "EventHeader.h"
#include "RunHeader.h"
#include "ZdcOut.h"

#include "fkinWrapper.h"
#include "primaryWrapper.h"
#include "headerWrapper.h"
#include "dcghitWrapper.h"
#include "dDchGhitHitsWrapper.h"
#include "pcghitWrapper.h"
#include "dPadGhitClusWrapper.h"
#include "dTofGdigiWrapper.h"
#include "dTofGdigiRecWrapper.h" 

#include "dDchHitWrapper.h"
#include "DchHitLineTable.hh"
#include "DchTrack.h"
#include "dDchTracksWrapper.h"
#include "PadRaw.h"
#include "dPadRawWrapper.h"
#include "PadCluster.h"
#include "TofOut.h"
#include "dTofReconstructedWrapper.h"
#include "emcTowerContainer.h"
#include "emcClusterContainer.h"
#include "dEmcGeaClusterTrackWrapper.h"
#include "TecOutV1.hh"
#include "CrkHit.h"
#include "dCrkHitWrapper.h"
#include "AccRaw.h"
#include "TofwHit.h"
#include "PHTrackOut.h"
#include "PHDchTrackOut.h"
#include "PHGlobal.h"
#include "PHCentralTrack.h"

#include <iostream>
using namespace std;

typedef PHIODataNode<TObject> TableNode_t;
typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHDataNode<PHEmbededEvent>  PHEmbededEventNode_t;
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;

PHEmbededEvent::PHEmbededEvent() {
  verbose = 0;
  mc      = 1;
}

PHEmbededEvent::~PHEmbededEvent() {
}

std::vector<int>&   PHEmbededEvent::get_dchitEmbed()          {return embedStat->dchitE;}
std::vector<int>&   PHEmbededEvent::get_dchitEmbedStat()      {return embedStat->dchitStat;}
std::vector<int>&   PHEmbededEvent::get_pc1clusterEmbed()     {return embedStat->pc1clusterE;}
std::vector<int>&   PHEmbededEvent::get_pc1clusterEmbedStat() {return embedStat->pc1clusterStat;}
std::vector<int>&   PHEmbededEvent::get_pc2clusterEmbed()     {return embedStat->pc2clusterE;}
std::vector<int>&   PHEmbededEvent::get_pc2clusterEmbedStat() {return embedStat->pc2clusterStat;}
std::vector<int>&   PHEmbededEvent::get_pc3clusterEmbed()     {return embedStat->pc3clusterE;}
std::vector<int>&   PHEmbededEvent::get_pc3clusterEmbedStat() {return embedStat->pc3clusterStat;}
std::vector<int>&   PHEmbededEvent::get_crkhitEmbed()         {return embedStat->crkhitE;}
std::vector<int>&   PHEmbededEvent::get_crkhitEmbedStat()     {return embedStat->crkhitStat;}
std::vector<int>&   PHEmbededEvent::get_tofoutEmbed()         {return embedStat->tofoutE;}
std::vector<int>&   PHEmbededEvent::get_tofoutEmbedStat()     {return embedStat->tofoutStat;}
std::vector<int>&   PHEmbededEvent::get_emctowerEmbed()       {return embedStat->emctowerE;}
std::vector<int>&   PHEmbededEvent::get_emctowerEmbedStat()   {return embedStat->emctowerStat;}
std::vector<int>&   PHEmbededEvent::get_emcclusterEmbed()     {return embedStat->emcclusterE;}
std::vector<int>&   PHEmbededEvent::get_emcclusterEmbedStat() {return embedStat->emcclusterStat;}
std::vector<int>&   PHEmbededEvent::get_accrawEmbed()         {return embedStat->accrawE;}
std::vector<int>&   PHEmbededEvent::get_accrawEmbedStat()     {return embedStat->accrawStat;}
std::vector<int>&   PHEmbededEvent::get_tofwhitEmbed()         {return embedStat->tofwhitE;}
std::vector<int>&   PHEmbededEvent::get_tofwhitEmbedStat()     {return embedStat->tofwhitStat;}

PHBoolean PHEmbededEvent::eventSingleNode(PHCompositeNode* node1) {
  if(!node1) return False;
  PHNodeIterator iter1(node1);
  TableNode_t *tablenode;
  
  //geant tables
  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","fkin"));
  fkin1 = 0;
  if(tablenode) fkin1 = static_cast<fkinWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","primary"));
  primary1 = 0;
  if(tablenode) primary1 = static_cast<primaryWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","header"));              
  header1 = 0;
  if(tablenode) header1 = static_cast<headerWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dcghit"));              
  dcghit1 = 0;
  if(tablenode) dcghit1 = static_cast<dcghitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dDchGhitHits"));        
  dchghithit1 = 0;
  if(tablenode) dchghithit1  = static_cast<dDchGhitHitsWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","pc1ghit"));
  pc1ghit1    = 0;
  if(tablenode) pc1ghit1     = static_cast<pcghitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","pc2ghit"));
  pc2ghit1    = 0;
  if(tablenode) pc2ghit1     = static_cast<pcghitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","pc3ghit"));             
  pc3ghit1    = 0;
  if(tablenode) pc3ghit1     = static_cast<pcghitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dPc1GhitClus"));        
  pc1ghitclus1= 0;
  if(tablenode) pc1ghitclus1 = static_cast<dPadGhitClusWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dPc2GhitClus"));       
  pc2ghitclus1= 0;
  if(tablenode) pc2ghitclus1 = static_cast<dPadGhitClusWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dPc3GhitClus"));        
  pc3ghitclus1= 0;
  if(tablenode) pc3ghitclus1 = static_cast<dPadGhitClusWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dTofGdigi"));           
  tofGdigi1 = 0;
  if(tablenode) tofGdigi1 = static_cast<dTofGdigiWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dTofGdigiRec"));        
  tofGdigiRec1= 0;
  if(tablenode) tofGdigiRec1 = static_cast<dTofGdigiRecWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dDchTracksPerf"));
  dchperftrack1 = 0;
  if(tablenode){//perfect trackid is always 0, so need to fix it here !!!
    dchperftrack1= static_cast<dDchTracksWrapper*>(tablenode->getData());
    for(unsigned int i=0;i<dchperftrack1->RowCount();i++) dchperftrack1->set_trackid(i,i);
  }

  //subsystem reconstruction tables
  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","CglTrack"));
  cgltrack1   = 0;
  if(tablenode) cgltrack1 = static_cast<CglTrack*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dDchHit"));
  dchitwrap1  = 0;
  if(tablenode) dchitwrap1   = static_cast<dDchHitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","DchHitLineTable"));
  dchit1      = 0;
  if(tablenode) dchit1       = static_cast<DchHitLineTable*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","DchHitLineTablev1"));
  dchit1v1    = 0;
  if(tablenode) dchit1v1     = static_cast<DchHitLineTable*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","DchTrack"));
  dchtrack1   = 0;
  if(tablenode) dchtrack1    = static_cast<DchTrack*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","Pc1Raw"));
  pc1raw1     = 0;
  if(tablenode) pc1raw1      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","Pc2Raw"));
  pc2raw1     = 0;
  if(tablenode) pc2raw1      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","Pc3Raw"));
  pc3raw1     = 0;
  if(tablenode) pc3raw1      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","Pc1Cluster"));
  pc1cluster1 = 0;
  if(tablenode) pc1cluster1  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","Pc2Cluster"));
  pc2cluster1 = 0;
  if(tablenode) pc2cluster1  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","Pc3Cluster"));
  pc3cluster1 = 0;
  if(tablenode) pc3cluster1  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","CrkHit"));
  crkhit1     = 0;
  if(tablenode) crkhit1      = static_cast<CrkHit*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dCrkHit"));
  crkhitold1  = 0;
  if(tablenode) crkhitold1      = static_cast<dCrkHitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","TofOut"));
  tofout1     = 0;
  if(tablenode) tofout1      = static_cast<TofOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","TecOut")); 
  tecout1 = 0;
  if(tablenode) tecout1  = static_cast<TecOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","TecHitOut")); 
  techitout1 = 0;
  if(tablenode) techitout1  = static_cast<TecOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","emcTowerContainer"));
  emctower1= 0;
  if(tablenode) emctower1           = static_cast<emcTowerContainer*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","emcClusterContainer"));
  emccluster1          = 0;
  if(tablenode) emccluster1      = static_cast<emcClusterContainer*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","dEmcGeaClusterTrack"));
  emcgeaclustertrack1     = 0;
  if(tablenode) emcgeaclustertrack1      = static_cast<dEmcGeaClusterTrackWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","PHTrackOut"));
  phtrack1    = 0;
  if(tablenode) {
    phtrack1     = static_cast<PHTrackOut*>(tablenode->getData());    
  }

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","PHDchTrackOut"));
  phdchtrack1 = 0;
  if(tablenode) phdchtrack1  = static_cast<PHDchTrackOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","AccRaw"));
  accraw1    = 0;
  if(tablenode) accraw1     = static_cast<AccRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","TofwHit"));
  tofwhit1    = 0;
  if(tablenode) tofwhit1     = static_cast<TofwHit*>(tablenode->getData());

  PHDataNode<PHEmbedStat> *embedEventNode = (PHDataNode<PHEmbedStat>*)iter1.findFirst("PHDataNode","PHEmbedStat");
  if(embedEventNode) embedStat  = embedEventNode->getData();
  
  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","BbcOut"));
  bbcout1     = 0;
  if(tablenode) bbcout1      = static_cast<BbcOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter1.findFirst("PHIODataNode","ZdcOut"));
  zdcout1     = 0;
  if(tablenode) zdcout1      = static_cast<ZdcOut*>(tablenode->getData());

  return True;
}

PHBoolean   PHEmbededEvent::eventRealNode(PHCompositeNode* node2){
  if(!node2) return False;
  PHNodeIterator iter2(node2);
  TableNode_t *tablenode;

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","PHGlobal"));
  global2     = 0;
  if(tablenode) global2      = static_cast<PHGlobal*>(tablenode->getData());
  
  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","PHCentralTrack"));
  CNTtrk2     = 0;
  if(tablenode) CNTtrk2      = static_cast<PHCentralTrack*>(tablenode->getData());
  
  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","BbcOut"));
  bbcout2     = 0;
  if(tablenode) bbcout2      = static_cast<BbcOut*>(tablenode->getData());
  
  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","ZdcOut"));
  zdcout2     = 0;
  if(tablenode) zdcout2      = static_cast<ZdcOut*>(tablenode->getData());
  
  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","CglTrack"));
  cgltrack2= 0;
  if(tablenode) cgltrack2 = static_cast<CglTrack*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","dDchHit"));
  dchitwrap2  = 0;
  if(tablenode) dchitwrap2   = static_cast<dDchHitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","DchHitLineTablev1"));
  dchit2v1    = 0;
  if(tablenode) dchit2v1     = static_cast<DchHitLineTable*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","DchHitLineTable"));
  dchit2       = 0;
  if(tablenode) dchit2       = static_cast<DchHitLineTable*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","DchTrack"));
  dchtrack2    = 0;
  if(tablenode) dchtrack2    = static_cast<DchTrack*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","Pc1Raw"));
  pc1raw2     = 0;
  if(tablenode) pc1raw2      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","Pc2Raw"));
  pc2raw2     = 0;
  if(tablenode) pc2raw2      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","Pc3Raw"));
  pc3raw2     = 0;
  if(tablenode) pc3raw2      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","Pc1Cluster"));
  pc1cluster2 = 0;
  if(tablenode) pc1cluster2  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","Pc2Cluster"));
  pc2cluster2 = 0;
  if(tablenode) pc2cluster2  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","Pc3Cluster"));
  pc3cluster2 = 0;
  if(tablenode) pc3cluster2  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","CrkHit"));
  crkhit2     = 0;
  if(tablenode) crkhit2      = static_cast<CrkHit*>(tablenode->getData());
  
  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","dCrkHit"));
  crkhitold2   = 0;
  if(tablenode) crkhitold2      = static_cast<dCrkHitWrapper*>(tablenode->getData());
  
  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","TofOut"));
  tofout2     = 0;
  if(tablenode) tofout2      = static_cast<TofOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","TecOut"));
  tecout2 = 0;
  if(tablenode) tecout2  = static_cast<TecOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","TecHitOut"));
  techitout2 = 0;
  if(tablenode) techitout2  = static_cast<TecOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","emcTowerContainer"));
  emctower2     = 0;
  if(tablenode) emctower2    = static_cast<emcTowerContainer*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","emcClusterContainer"));
  emccluster2     = 0;
  if(tablenode) emccluster2  = static_cast<emcClusterContainer*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","PHTrackOut"));
  phtrack2    = 0;
  if(tablenode) phtrack2     = static_cast<PHTrackOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","PHDchTrackOut"));
  phdchtrack2 = 0;
  if(tablenode) phdchtrack2  = static_cast<PHDchTrackOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","AccRaw"));
  accraw2 = 0;
  if(tablenode) accraw2= static_cast<AccRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","TofwHit"));
  tofwhit2 = 0;
  if(tablenode) tofwhit2= static_cast<TofwHit*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","EventHeader"));
  eventheader2= 0;
  if(tablenode) eventheader2 = static_cast<EventHeader*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter2.findFirst("PHIODataNode","RunHeader"));
  runheader2  = 0;
  if(tablenode) runheader2   = static_cast<RunHeader*>(tablenode->getData());

  return True;
}

PHBoolean   PHEmbededEvent::eventMergedNode(PHCompositeNode* node3){
  if(!node3) return False;
  PHNodeIterator iter3(node3);
  TableNode_t *tablenode;

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","BbcOut"));
  bbcout3     = 0;
  if(tablenode) bbcout3      = static_cast<BbcOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","ZdcOut"));
  zdcout3     = 0;
  if(tablenode) zdcout3      = static_cast<ZdcOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","CglTrack"));
  cgltrack3= 0;
  if(tablenode) cgltrack3 = static_cast<CglTrack*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","dDchHit"));
  dchitwrap3  = 0;
  if(tablenode) dchitwrap3   = static_cast<dDchHitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","DchHitLineTablev1"));
  dchit3v1    = 0;
  if(tablenode) dchit3v1     = static_cast<DchHitLineTable*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","DchHitLineTable"));
  dchit3      = 0;
  if(tablenode) dchit3       = static_cast<DchHitLineTable*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","DchTrack"));
  dchtrack3   = 0;
  if(tablenode) dchtrack3    = static_cast<DchTrack*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","dPc1Raw"));
  pc1rawwrap3 = 0;
  if(tablenode) pc1rawwrap3      = static_cast<dPadRawWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","dPc2Raw"));
  pc2rawwrap3 = 0;
  if(tablenode) pc2rawwrap3      = static_cast<dPadRawWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","dPc3Raw"));
  pc3rawwrap3 = 0;
  if(tablenode) pc3rawwrap3      = static_cast<dPadRawWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","Pc1Raw"));
  pc1raw3     = 0;
  if(tablenode) pc1raw3      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","Pc2Raw"));
  pc2raw3     = 0;
  if(tablenode) pc2raw3      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","Pc3Raw"));
  pc3raw3     = 0;
  if(tablenode) pc3raw3      = static_cast<PadRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","Pc1Cluster"));
  pc1cluster3 = 0;
  if(tablenode) pc1cluster3  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","Pc2Cluster"));
  pc2cluster3 = 0;
  if(tablenode) pc2cluster3  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","Pc3Cluster"));
  pc3cluster3 = 0;
  if(tablenode) pc3cluster3  = static_cast<PadCluster*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","CrkHit"));
  crkhit3     = 0;
  if(tablenode) crkhit3      = static_cast<CrkHit*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","dCrkHit"));
  crkhitold3     = 0;
  if(tablenode) crkhitold3      = static_cast<dCrkHitWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","dTofReconstructed"));
  tofreconstructed3        =0;
  if(tablenode) tofreconstructed3 = static_cast<dTofReconstructedWrapper*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","TofOut"));
  tofout3     = 0;
  if(tablenode) tofout3        = static_cast<TofOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","TecOutV1")); 
  tecout3 = 0;
  if(tablenode) tecout3  = static_cast<TecOutV1*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","emcTowerContainer"));
  emctower3     = 0;
  if(tablenode) emctower3    = static_cast<emcTowerContainer*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","emcClusterContainer"));
  emccluster3     = 0;
  if(tablenode) emccluster3  = static_cast<emcClusterContainer*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","PHTrackOut"));
  phtrack3    = 0;
  if(tablenode) phtrack3     = static_cast<PHTrackOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","PHDchTrackOut"));
  phdchtrack3 = 0;
  if(tablenode) phdchtrack3  = static_cast<PHDchTrackOut*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","AccRaw"));
  accraw3 = 0;
  if(tablenode) accraw3        = static_cast<AccRaw*>(tablenode->getData());

  tablenode = static_cast<TableNode_t*>(iter3.findFirst("PHIODataNode","TofwHit"));
  tofwhit3 = 0;
  if(tablenode) tofwhit3        = static_cast<TofwHit*>(tablenode->getData());

  return True;
}

PHBoolean PHEmbededEvent::eventInputNodes(PHCompositeNode* node1, PHCompositeNode* node2) {
  PHBoolean ret1  = eventSingleNode(node1);
  PHBoolean ret2  = eventRealNode(node2);
  if(!ret2) ret1  = False;
  return  ret1;
}

PHBoolean PHEmbededEvent::event(PHCompositeNode* node1,
				PHCompositeNode* node2, 
				PHCompositeNode* node3) {
  PHBoolean ret1  = eventInputNodes(node1,node2);
  PHBoolean ret2  = eventMergedNode(node3);
  if(!ret2) ret1  = False;
  return  ret1;  
}


float   PHEmbededEvent::get_x1mPerfectTrack(int trkPerf) {
  if(!dchperftrack1) return -1;
  
  int numOfPerfTrack1 = dchperftrack1->RowCount();
  if(trkPerf<0 ||trkPerf>=numOfPerfTrack1) return -1;
  return -1;
}
float   PHEmbededEvent::get_x2mPerfectTrack(int trkPerf) {
  if(!dchperftrack1) return -1;
  
  int numOfPerfTrack1 = dchperftrack1->RowCount();
  if(trkPerf<0 ||trkPerf>numOfPerfTrack1) return -1;
  return -1;
}
int   PHEmbededEvent::get_x1hPerfectTrack(int trkPerf) {
  if(!dchperftrack1) return -1;
  
  int numOfPerfTrack1 = dchperftrack1->RowCount();
  if(trkPerf<0 ||trkPerf>=numOfPerfTrack1) return 0;

  int totalX1 = 0;
  for (int i=0; i < 40; i++) { // loop over X1 planes
    int id =  dchperftrack1->get_hits(i,trkPerf);
    if(id>-1)if(dcghit1->get_plane(id)<12) totalX1++;
  }
  return totalX1;
}
int   PHEmbededEvent::get_x2hPerfectTrack(int trkPerf) {
  if(!dchperftrack1) return -1;
  
  int numOfPerfTrack1 = dchperftrack1->RowCount();
  if(trkPerf<0 ||trkPerf>=numOfPerfTrack1) return 0;

  int totalX2 = 0;
  int plane;
  for (int i=0; i < 40; i++) { // loop over X2 planes
    int id =  dchperftrack1->get_hits(i,trkPerf);
    if(id>-1){
      plane = dcghit1->get_plane(id);
      if(plane>19&&plane<32)totalX2++;
    }
  }
  return totalX2; 
}
int   PHEmbededEvent::get_uv1hPerfectTrack(int trkPerf){
  if(!dchperftrack1) return -1;
  
  int numOfPerfTrack1 = dchperftrack1->RowCount();
  if(trkPerf<0 ||trkPerf>=numOfPerfTrack1) return 0;

  int totalUV1 = 0;
  int plane;
  for (int i=0; i < 40; i++) { // loop over UV1 planes
    int id =  dchperftrack1->get_hits(i,trkPerf);
    if(id>-1){
      plane = dcghit1->get_plane(id);
      if(plane>11&&plane<20)totalUV1++;
    }
  }
  return totalUV1;
}
int   PHEmbededEvent::get_uv2hPerfectTrack(int trkPerf){
  if(!dchperftrack1) return -1;
  
  int numOfPerfTrack1 = dchperftrack1->RowCount();
  if(trkPerf<0 ||trkPerf>=numOfPerfTrack1) return 0;

  int totalUV2 = 0;
  int plane;
  for (int i=0; i < 40; i++) { // loop over UV1 planes
    int id =  dchperftrack1->get_hits(i,trkPerf);
    if(id>-1){
      plane = dcghit1->get_plane(id);
      if(plane>31)totalUV2++;
    }
  }
  return totalUV2;
}

float   PHEmbededEvent::get_x1mS(int trkS){
  if(!dchtrack1) return -1;
  
  int numOfDchTrack1 = dchtrack1->get_DchNTrack();
  if(trkS<0 ||trkS>=numOfDchTrack1) return -1;

  int totalX1 = 0;
  float dist =0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X1 planes
    int id =  dchtrack1->get_hits(trkS,i);
    if(id>-1){
      plane = dchit1->getPlane(id);
      if(plane<12){
	dist += dchit1v1->getDistance(id);
	totalX1++;
      }
    }
  }
  if(totalX1>0) dist /= totalX1;
  else dist = -1.;
  return dist;
}
float   PHEmbededEvent::get_x2mS(int trkS){
  if(!dchtrack1) return -1;
  
  int numOfDchTrack1 = dchtrack1->get_DchNTrack();
  if(trkS<0 ||trkS>=numOfDchTrack1) return -1;

  int totalX2 = 0;
  float dist = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X2 planes
    int id =  dchtrack1->get_hits(trkS,i);
    if(id>-1){
      plane = dchit1->getPlane(id);
      if(plane>19&&plane<32){
	dist += dchit1v1->getDistance(id);
	totalX2++;
      }
    }
  }
  if(totalX2>0) dist /= totalX2;
  else dist = -1.;
  return dist;
}
int   PHEmbededEvent::get_x1hS(int trkS){
  if(!dchtrack1) return -1;
  
  int numOfDchTrack1 = dchtrack1->get_DchNTrack();
  if(trkS<0 ||trkS>=numOfDchTrack1) return 0;

  int totalX1 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X1 planes
    int id =  dchtrack1->get_hits(trkS,i);
    if(id>-1){
      plane = dchit1->getPlane(id);
      if(plane<12)totalX1++;
    }
  }
  return totalX1;
}
int   PHEmbededEvent::get_x2hS(int trkS){
  if(!dchtrack1) return -1;
  
  int numOfDchTrack1 = dchtrack1->get_DchNTrack();
  if(trkS<0 ||trkS>=numOfDchTrack1) return 0;

  int totalX2 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X1 planes
    int id =  dchtrack1->get_hits(trkS,i);
    if(id>-1){
      plane = dchit1->getPlane(id);
      if(plane>19&&plane<32)totalX2++;
    }
  }
  return totalX2;
}
int   PHEmbededEvent::get_uv1hS(int trkS){
  if(!dchtrack1) return -1;
  
  int numOfDchTrack1 = dchtrack1->get_DchNTrack();
  if(trkS<0 ||trkS>=numOfDchTrack1) return 0;

  int totalUV1 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over UV1 planes
    int id =  dchtrack1->get_hits(trkS,i);
    if(id>-1){
      plane = dchit1->getPlane(id);
      if(plane>11&&plane<20)totalUV1++;
    }
  }
  return totalUV1;
}
int   PHEmbededEvent::get_uv2hS(int trkS){
  if(!dchtrack1) return -1;
  
  int numOfDchTrack1 = dchtrack1->get_DchNTrack();
  if(trkS<0 ||trkS>=numOfDchTrack1) return 0;

  int totalUV2 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over UV2 planes
    int id =  dchtrack1->get_hits(trkS,i);
    if(id>-1){
      plane = dchit1->getPlane(id);
      if(plane>31)totalUV2++;
    }
  }
  return totalUV2;
}

float   PHEmbededEvent::get_x1mR(int trkR){
  if(!dchtrack3) return -1;
  
  int numOfDchTrack3 = dchtrack3->get_DchNTrack();
  if(trkR<0 ||trkR>=numOfDchTrack3) return -1;

  int totalX1 = 0;
  float dist =0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X1 planes
    int id =  dchtrack3->get_hits(trkR,i);
    if(id>-1){
      plane = dchit3->getPlane(id);
      if(plane<12){
	dist += dchit3v1->getDistance(id);
	totalX1++;
      }
    }
  }
  if(totalX1>0) dist /= totalX1;
  else dist = -1.;
  return dist;
}
float   PHEmbededEvent::get_x2mR(int trkR){
  if(!dchtrack3) return -1.;
  
  int numOfDchTrack3 = dchtrack3->get_DchNTrack();
  if(trkR<0 ||trkR>=numOfDchTrack3) return -1.;

  int totalX2 = 0;
  float dist =0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X2 planes
    int id =  dchtrack3->get_hits(trkR,i);
    if(id>-1){
      plane = dchit3->getPlane(id);
      if(plane>19&&plane<32){
	dist += dchit3v1->getDistance(id);
	totalX2++;
      }
    }
  }
  if(totalX2>0) dist /= totalX2;
  else dist = -1.;
  return dist;
}
int   PHEmbededEvent::get_x1hR(int trkR){
  if(!dchtrack3) return -1;
  
  int numOfDchTrack3 = dchtrack3->get_DchNTrack();
  if(trkR<0 ||trkR>=numOfDchTrack3) return 0;

  int totalX1 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X1 planes
    int id =  dchtrack3->get_hits(trkR,i);
    if(id>-1){
      plane = dchit3->getPlane(id);
      if(plane<12)totalX1++;
    }
  }
  return totalX1;
}
int   PHEmbededEvent::get_x2hR(int trkR){
  if(!dchtrack3) return -1;
  
  int numOfDchTrack3 = dchtrack3->get_DchNTrack();
  if(trkR<0 ||trkR>=numOfDchTrack3) return 0;

  int totalX2 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over X2 planes
    int id =  dchtrack3->get_hits(trkR,i);
    if(id>-1){
      plane = dchit3->getPlane(id);
      if(plane>19&&plane<32)totalX2++;
    }
  }
  return totalX2;
}
int   PHEmbededEvent::get_uv1hR(int trkR){
  if(!dchtrack3) return -1;
  
  int numOfDchTrack3 = dchtrack3->get_DchNTrack();
  if(trkR<0 ||trkR>=numOfDchTrack3) return 0;

  int totalUV1 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over UV1 planes
    int id =  dchtrack3->get_hits(trkR,i);
    if(id>-1){
      plane = dchit3->getPlane(id);
      if(plane>11&&plane<20)totalUV1++;
    }
  }
  return totalUV1;
}
int   PHEmbededEvent::get_uv2hR(int trkR){
  if(!dchtrack3) return -1;
  
  int numOfDchTrack3 = dchtrack3->get_DchNTrack();
  if(trkR<0 ||trkR>=numOfDchTrack3) return 0;

  int totalUV2 = 0;
  int plane;
  for (int i=0; i < 39; i++) { // loop over UV1 planes
    int id =  dchtrack3->get_hits(trkR,i);
    if(id>-1){
      plane = dchit3->getPlane(id);
      if(plane>31)totalUV2++;
    }
  }
  return totalUV2;
}

int   PHEmbededEvent::get_x1hAfterEmbedForPerfectTrack(int trkPerf){
  
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  unsigned int numOfHit1          =    dchit1->Entries();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;
 
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();

  //DDCHHIT_ST*      dDchHit1     = dchit1->TableData();
  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;

  int idGeant=-1,hitS[40],ghitid;
  for(int plane =0;plane<40;plane++) hitS[plane] = -1;
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      idGeant = dcghit1->get_mctrack(ghitid);
      break;
    }
  }
  //be careful with mirror hit!!!!!!, for x hits it is no problem
  int total=0;  
  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	int realplane = dcghit1->get_plane(ghitid);
	hitS[realplane] = dDchGhitHit1[i].hitsid;
	total++;
      }
    }
  }
  
  /*int total=0,total1=0;
  int hitS[40],ghits[40];
  for(int plane=0;plane<40;plane++){
    int ghitid = dchperftrack1->get_hits(plane,trkPerf);
    hitS[plane]=-1;ghits[plane]=ghitid;
    if(ghitid>-1){
      total1++;
      for(int i=0;i<numOfGhitHit1;i++){
	if(ghitid == dDchGhitHit1[i].ghitid){
	  hitS[plane] = dDchGhitHit1[i].hitsid;
	  total++;
	  break;
	}
      }
    }
  }
  if(verbose>30){  
    for(int plane=0;plane<40;plane++){
      cout<<ghits[plane]<<" "<<hitS[plane]<<" "<<(*dcghit1)[ghits[plane]].id<<" "<<(*dcghit1)[ghits[plane]].mctrack<<" "<<(*dcghit1)[ghits[plane]].plane<<endl;
    }
    cout<<"--------------------------------------------"<<endl;
    for(int i=0;i<numOfGhitHit1;i++){
      cout<<dDchGhitHit1[i].ghitid<<" "<<dDchGhitHit1[i].hitsid<<endl;
    }
    cout<<"--------------------------------------------"<<endl;
    for(int i=0;i<dcghit1->RowCount();i++)
      cout<<(*dcghit1)[i].id<<" "<<(*dcghit1)[i].mctrack<<" "<<(*dcghit1)[i].plane<<endl;
    
    cout<<"total "<<total<<" "<<total1<<" "<<numOfGhitHit1<<" "<<dcghit1->RowCount()<<endl;
    }*/
  //hitS is the list of simulated hits, which can be killed after embedding
  if(dchitE.size()!=numOfHit1){
    cout<<"PHEmbedEvent:: hit index outof bound"<<endl;
  }
  int hitid,realplane;
  int totalX1 = 0;
  for(int plane =0;plane<40;plane++){
    hitid = hitS[plane];
    if(hitid>-1){
      realplane =  dchit1->getPlane(hitid);
      if(realplane<12){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalX1++;
      }
    }
  }
  return totalX1;
}
int   PHEmbededEvent::get_x2hAfterEmbedForPerfectTrack(int trkPerf){
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  unsigned int numOfHit1          =    dchit1->Entries();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();

  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;
  int idGeant=-1,hitS[40],ghitid=-1;
  for(int plane =0;plane<40;plane++) hitS[plane] = -1;

 
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      idGeant = dcghit1->get_mctrack(ghitid);
      break;
    }
  }

  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	int realplane = dcghit1->get_plane(ghitid);
	hitS[realplane] = dDchGhitHit1[i].hitsid;
      }
    }
  }
  
  //hitS is the list of simulated hits, which can be killed after embedding
  if(dchitE.size()!=numOfHit1){
    cout<<"PHEmbedEvent:: hit index outof bound"<<endl;
  }
  int hitid,realplane;
  int totalX2 = 0;
  for(int plane =0;plane<40;plane++){
    hitid = hitS[plane];
    if(hitid>-1){
      realplane = dchit1->getPlane(hitid);
      if(realplane>19&&realplane<32){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalX2++;
      }
    }
  }
  return totalX2;
}
int   PHEmbededEvent::get_uv1hAfterEmbedForPerfectTrack(int trkPerf){
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  unsigned int numOfHit1          =    dchit1->Entries();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();

  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;
  int hitS[120],ghitid=-1,count=0;
  for(int i =0;i<120;i++) hitS[i] = -1;
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      for(int i=0;i<numOfGhitHit1;i++){
	if(ghitid == dDchGhitHit1[i].ghitid){
	  if(count>=120){
	    cout<<PHWHERE << "too many hits, array exceeded"<<endl;
	    break;
	  }
	  hitS[count] = dDchGhitHit1[i].hitsid;
	  count++;
	}
      }
    }
  }

  /*for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      idGeant = dcghit1->get_mctrack(ghitid);
      break;
    }
  }

  
  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	//int realplane = dcghit1->get_plane(ghitid);
	hitS[count] = dDchGhitHit1[i].hitsid;
	count++;
      }
    }
    }*/

  //hitS is the list of simulated hits, which can be killed after embedding
  if(dchitE.size()!=numOfHit1){
    cout<<"PHEmbedEvent:: hit index outof bound"<<endl;
  }
  int hitid,realplane;
  int totalUV1 = 0;
  for(int i =0;i<count;i++){
    hitid = hitS[i];
    if(hitid>-1){
      realplane = dchit1->getPlane(hitid);
      if(realplane>11&&realplane<20){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalUV1++;
      }
    }
  }
  return totalUV1;
}
int   PHEmbededEvent::get_uv2hAfterEmbedForPerfectTrack(int trkPerf){
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  unsigned int numOfHit1          =    dchit1->Entries();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();

  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;
  int hitS[120],ghitid=-1,count=0;
  for(int i =0;i<120;i++) hitS[i] = -1;
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      for(int i=0;i<numOfGhitHit1;i++){
	if(ghitid == dDchGhitHit1[i].ghitid){
	  if(count>=120){
	    cout<< PHWHERE << "too many hits, array exceeded" <<endl;
	    break;
	  }
	  hitS[count] = dDchGhitHit1[i].hitsid;
	  count++;
	}
      }
    }
  }
  /*
  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	//int realplane = dcghit1->get_plane(ghitid);
	hitS[count] = dDchGhitHit1[i].hitsid;
	count++;
      }
    }
  }
  */
  //hitS is the list of simulated hits, which can be killed after embedding
  if(dchitE.size()!=numOfHit1){
    cout<<"PHEmbedEvent:: hit index outof bound"<<endl;
  }
  int hitid,realplane;
  int totalUV2 = 0;
  for(int i =0;i<count;i++){
    hitid = hitS[i];
    if(hitid>-1){
      realplane = dchit1->getPlane(hitid);
      if(realplane>31){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalUV2++;
      }
    }
  }
  return totalUV2;
}
int   PHEmbededEvent::get_x1hBeforeEmbedForPerfectTrack(int trkPerf){
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();

  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;
  int idGeant=-1,hitS[40],ghitid=-1;
  for(int plane =0;plane<40;plane++) hitS[plane] = -1;
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      idGeant = dcghit1->get_mctrack(ghitid);
      break;
    }
  }

  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	int realplane = dcghit1->get_plane(ghitid);
	hitS[realplane] = dDchGhitHit1[i].hitsid;
      }
    }
  }
  int hitid,realplane;
  int totalX1 = 0;
  for(int plane =0;plane<40;plane++){
    hitid = hitS[plane];
    if(hitid>-1){
      realplane = dchit1->getPlane(hitid);
      if(realplane<12){
	totalX1++;
      }
    }
  }
  return totalX1;
}
int   PHEmbededEvent::get_x2hBeforeEmbedForPerfectTrack(int trkPerf){
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();

  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;

  int idGeant=-1,hitS[40],ghitid=-1;
  for(int plane =0;plane<40;plane++) hitS[plane] = -1;
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      idGeant = dcghit1->get_mctrack(ghitid);
      break;
    }
  }

  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	int realplane = dcghit1->get_plane(ghitid);
	hitS[realplane] = dDchGhitHit1[i].hitsid;
      }
    }
  }

  int hitid,realplane;
  int totalX2 = 0;
  for(int plane =0;plane<40;plane++){
    hitid = hitS[plane];
    if(hitid>-1){
      realplane = dchit1->getPlane(hitid);
      if(realplane>19&&realplane<32){
	totalX2++;
      }
    }
  }
  return totalX2;
}
int   PHEmbededEvent::get_uv1hBeforeEmbedForPerfectTrack(int trkPerf){
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();
  //DDCHHIT_ST*      dDchHit1     = dchit1->TableData();

  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;
  int hitS[120],ghitid=-1,count=0;
  for(int i =0;i<120;i++) hitS[i] = -1;
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      for(int i=0;i<numOfGhitHit1;i++){
	if(ghitid == dDchGhitHit1[i].ghitid){
	  if(count>=120){
	    cout << PHWHERE << " too many ghits, array exceeded" << endl;
	    break;
	  }
	  hitS[count] = dDchGhitHit1[i].hitsid;
	  count++;
	}
      }
    }
  }

  /*  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      idGeant = dcghit1->get_mctrack(ghitid);
      break;
    }
  }

  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	//int realplane = dcghit1->get_plane(ghitid);
	hitS[count] = dDchGhitHit1[i].hitsid;
	count++;
      }
    }
    }*/
  int hitid,realplane;
  int totalUV1 = 0;
  for(int i =0;i<count;i++){
    hitid = hitS[i];
    if(hitid>-1){
      realplane = dchit1->getPlane(hitid);
      if(realplane>11&&realplane<20){
	totalUV1++;
      }
    }
  }
  return totalUV1;
}
int   PHEmbededEvent::get_uv2hBeforeEmbedForPerfectTrack(int trkPerf){
  int numOfPerfTrack1    =    dchperftrack1->RowCount();
  int numOfGhitHit1      =    dchghithit1->RowCount();
  
  DDCHGHITHITS_ST *dDchGhitHit1 = dchghithit1->TableData();

  if(trkPerf<0||trkPerf>=numOfPerfTrack1) return 0;
  int hitS[120],ghitid=-1,count=0;
  for(int i =0;i<120;i++) hitS[i] = -1;
  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      for(int i=0;i<numOfGhitHit1;i++){
	if(ghitid == dDchGhitHit1[i].ghitid){
	  if(count>=120){
	    cout << PHWHERE << "too many ghits, aborting loop" << endl;
	    break;
	  }
	  hitS[count] = dDchGhitHit1[i].hitsid;
	  count++;
	}
      }
    }
  }
  /*  for(int plane=0;plane<40;plane++){
    ghitid = dchperftrack1->get_hits(plane,trkPerf);
    if(ghitid>-1){
      idGeant = dcghit1->get_mctrack(ghitid);
      break;
    }
  }

  for(int i=0;i<numOfGhitHit1;i++){
    ghitid =dDchGhitHit1[i].ghitid;
    if(ghitid>-1){
      if(idGeant == dcghit1->get_mctrack(ghitid)){
	//int realplane = dcghit1->get_plane(ghitid);
	hitS[count] = dDchGhitHit1[i].hitsid;
	count++;
      }
    }
    }*/
  int hitid,realplane;
  int totalUV2 = 0;
  for(int i =0;i<count;i++){
    hitid = hitS[i];
    if(hitid>-1){
      realplane =  dchit1->getPlane(hitid);
      if(realplane>31){
	totalUV2++;
      }
    }
  }
  return totalUV2;
}
int   PHEmbededEvent::get_x1hAfterEmbedForSimulatedTrack(int trkS){
  int numOfDchTrack1     =    dchtrack1->get_DchNTrack();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;

  if(trkS<0||trkS>=numOfDchTrack1) return 0;

  int realplane;
  int totalX1 = 0;
  for(int plane=0;plane<39;plane++){
    int hitid = dchtrack1->get_hits(plane,trkS);
    if(hitid>-1){
      realplane =  dchit1->getPlane(hitid);
      if(realplane<12){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalX1++;
      }
    }
  }
  return totalX1;
}
int   PHEmbededEvent::get_x2hAfterEmbedForSimulatedTrack(int trkS){
  int numOfDchTrack1     =    dchtrack1->get_DchNTrack();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;

  if(trkS<0||trkS>=numOfDchTrack1) return 0;

  int realplane;
  int totalX2 = 0;
  for(int plane=0;plane<39;plane++){
    int hitid = dchtrack1->get_hits(plane,trkS);
    if(hitid>-1){
      realplane =  dchit1->getPlane(hitid);
      if(realplane>19&&realplane<32){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalX2++;
      }
    }
  }
  return totalX2;
}
int   PHEmbededEvent::get_uv1hAfterEmbedForSimulatedTrack(int trkS){
  int numOfDchTrack1     =    dchtrack1->get_DchNTrack();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;

  if(trkS<0||trkS>=numOfDchTrack1) return 0;

  int realplane=-1;
  int totalUV1 = 0;
  for(int plane=0;plane<39;plane++){
    int hitid = dchtrack1->get_hits(plane,trkS);
    if(hitid>-1){
      realplane =  dchit1->getPlane(hitid);
      if(realplane>11&&realplane<20){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalUV1++;
      }
    }
  }
  return totalUV1;
}
int   PHEmbededEvent::get_uv2hAfterEmbedForSimulatedTrack(int trkS){
  int numOfDchTrack1     =    dchtrack1->get_DchNTrack();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;

  if(trkS<0||trkS>=numOfDchTrack1) return 0;

  int realplane=-1;
  int totalUV2 = 0;
  for(int plane=0;plane<39;plane++){
    int hitid = dchtrack1->get_hits(plane,trkS);
    if(hitid>-1){
      realplane =  dchit1->getPlane(hitid);
      if(realplane>31){
	if(dchitE[hitid]>-1&&(dchitStat[hitid]==0||dchitStat[hitid]==3))totalUV2++;
      }
    }
  }
  return totalUV2;
}

float PHEmbededEvent::getClosestApproachToBeamAxis(DDCHTRACKS_ST&dchTrack){
  float p1x        = dchTrack.point[0];
  float p1y        = dchTrack.point[1];
  //float p1z        = dchTrack.point[2];
  float p2x        = p1x + dchTrack.direction[0];
  float p2y        = p1y + dchTrack.direction[1];
  //float p2z        = p1z + dchTrack.direction[2];
  float slope;

  if ( p2x!=p1x ) slope = (p2y-p1y)/(p2x-p1x); else slope = 0.0;
  float intersect = p1y-slope*p1x;
  return fabs(intersect/sqrt(1+slope*slope));
}
float  PHEmbededEvent::meanDistanceBetweenTracks(DDCHTRACKS_ST&track1,DDCHTRACKS_ST&track2){
  float deltax = track1.point[0] - track2.point[0];
  float deltay = track1.point[1] - track2.point[1];
  return sqrt(deltax*deltax + deltay*deltay);
}
int    PHEmbededEvent::recohit2Simulatedhit(int recohitid){
  int numOfHit3     =    dchit3->Entries();
  std::vector<int>& dchitE      = embedStat->dchitE;
  std::vector<int>& dchitStat   = embedStat->dchitStat;
  
  if(recohitid<0||recohitid>=numOfHit3) return -1;
  int simuhitid = -1;
  for(unsigned int i=0;i<dchitE.size();i++){
    if(recohitid == dchitE[i]){
      if(dchitStat[i]==0||dchitStat[i]==3){//this hit comes from simulated hit
	simuhitid = i;
	break;
      }
    }
  }
  return simuhitid;
}
int    PHEmbededEvent::simuhit2Geanthit(int simuhitid){
  int numOfGhitHit1 =    dchghithit1->RowCount();
  int numOfHit1     =    dchit1->Entries();
  DDCHGHITHITS_ST *dDchGhitHit1 =    dchghithit1->TableData();
  if(simuhitid<0||simuhitid>=numOfHit1) return -1;
  int ghitid = -1;
  for(int i=0;i<numOfGhitHit1;i++){
    if(simuhitid == dDchGhitHit1[i].hitsid){
      ghitid = dDchGhitHit1[i].ghitid;
      break;
    }
  }
  return ghitid;
}
int    PHEmbededEvent::recohit2Geanthit(int recohitid){
  int simuhitid = recohit2Simulatedhit(recohitid);
  if(simuhitid<0) return -1;
  return simuhit2Geanthit(simuhitid);
}

int    PHEmbededEvent::geanthit2Perfecttrack(int ghitid){
  int numOfPerfTrack1       =    dchperftrack1->RowCount();
  int numOfDcghit1          =    dcghit1->RowCount();

  DDCHTRACKS_ST* dDchPerfTrack1  = dchperftrack1->TableData();

  if(ghitid<0||ghitid>=numOfDcghit1) return -1;
  int trackid = -1;
  for(int i =0;i<numOfPerfTrack1;i++){
    for(int plane=0;plane<40;plane++){
      if(ghitid == dDchPerfTrack1[i].hits[plane]){
	trackid = i;
	break;
      }
    }
  }
 return trackid;
 /*
   if(trackid>-1) return trackid;
   else{
   int idGeant = dcghit1->get_mctrack(ghitid);
   for(int i =0;i<numOfPerfTrack1;i++){
     for(int plane=0;plane<40;plane++){
       int hitid =  dDchPerfTrack1[i].hits[plane];
       if(hitid>-1&&dcghit1->get_mctrack(hitid) == idGeant){
	 trackid == i;
	 goto End;
       }
     }
   }
 End:
   return trackid;
 }
 */
}
int    PHEmbededEvent::simuhit2Perfecttrack(int simuhitid){
  int ghitid = simuhit2Geanthit(simuhitid);
  if(ghitid<0) return -1;
  return geanthit2Perfecttrack(ghitid);
}
int    PHEmbededEvent::recohit2Perfecttrack(int recohitid){
  int simuhitid = recohit2Simulatedhit(recohitid);
  if(simuhitid<0) return -1;
  return simuhit2Perfecttrack(simuhitid);
}
int    PHEmbededEvent::recohit2Simulatedtrack(int recohitid){
  int simuhitid = recohit2Simulatedhit(recohitid);
  if(simuhitid<0) return -1;
  return simuhit2Simulatedtrack(simuhitid);
}
int    PHEmbededEvent::simuhit2Simulatedtrack(int simuhitid){
  if(simuhitid<0)return -1;
  int numOfDchTrack1         = dchtrack1->get_DchNTrack();
  int numOfDchHit1           = dchit1->Entries();

  //DDCHTRACKS_ST* dDchTrack1  = dchtrack1->TableData();
  if(simuhitid<0||simuhitid>=numOfDchHit1) return -1;
  
  int trackid = -1;
  for(int i=0;i<numOfDchTrack1;i++){
    for(int plane=0;plane<39;plane++){
      if(simuhitid == dchtrack1->get_hits(i,plane)){
	trackid = i;
	goto end;
      }
    }
  }
end:
  return trackid;
}
int    PHEmbededEvent::recohit2Reconstructedtrack(int recohitid){
  int numOfDchTrack3         = dchtrack3->get_DchNTrack();
  int numOfDchHit3           = dchit3->Entries();

  //DDCHTRACKS_ST* dDchTrack3  = dchtrack3->TableData();
  if(recohitid<0||recohitid>=numOfDchHit3) return -1;
  
  int trackid = -1;
  for(int i=0;i<numOfDchTrack3;i++){
    for(int plane=0;plane<39;plane++){
      if(recohitid == dchtrack3->get_hits(i,plane)){
	trackid = i;
	break;
      }
    }
  }
  return trackid;
}
int  PHEmbededEvent::pcghit2Cluster(int pc,int pcghitid){
  int numOfPcxGhitClus1;
  DPADGHITCLUS_ST*        dPcxGhitClus1;

  if(pc==1){
    numOfPcxGhitClus1  =     pc1ghitclus1->RowCount();
    dPcxGhitClus1      =     pc1ghitclus1->TableData();
  }else if(pc==2){
    numOfPcxGhitClus1  =     pc2ghitclus1->RowCount();
    dPcxGhitClus1      =     pc2ghitclus1->TableData();
  }else if(pc==3){
    numOfPcxGhitClus1  =     pc3ghitclus1->RowCount();
    dPcxGhitClus1      =     pc3ghitclus1->TableData();
  }else return -1;

  int pcxclusterid = -1;
  for(int i=0;i<numOfPcxGhitClus1;i++){
    if(pcghitid == dPcxGhitClus1[i].ghitid){
      pcxclusterid = dPcxGhitClus1[i].clusid;
	break;
    }
  }
  return pcxclusterid;
}
int   PHEmbededEvent::get_pcClusterAfterEmbedForGeantTrack(int pc,int trkG){
  return -1;//for now not used
  int numOfPcxCluster1;
  int numOfPcxGhitClus1;
  int numOfPcxghit1;

  PadCluster*              PcxCluster1;
  DPADGHITCLUS_ST*         dPcxGhitClus1;
  PCGHIT_ST*               dPcxGhit1;
  vector<int>&             pcxclusterE     = embedStat->pc1clusterE;
  vector<int>&             pcxclusterStat  = embedStat->pc1clusterStat ;
  
  if(pc==1){
    PcxCluster1        =     pc1cluster1;
    numOfPcxCluster1   =     pc1cluster1->get_PadNCluster();
    numOfPcxGhitClus1  =     pc1ghitclus1->RowCount();
    numOfPcxghit1      =     pc1ghit1->RowCount();
    dPcxGhitClus1      =     pc1ghitclus1->TableData();
    dPcxGhit1          =     pc1ghit1->TableData();
    pcxclusterE        =     embedStat->pc1clusterE;
    pcxclusterStat     =     embedStat->pc1clusterStat;
  }else if(pc==2){
    PcxCluster1        =     pc2cluster1;
    numOfPcxCluster1   =     pc2cluster1->get_PadNCluster();
    numOfPcxGhitClus1  =     pc2ghitclus1->RowCount();
    numOfPcxghit1      =     pc2ghit1->RowCount();
    dPcxGhitClus1      =     pc2ghitclus1->TableData();
    dPcxGhit1          =     pc2ghit1->TableData();
    pcxclusterE        =     embedStat->pc2clusterE;
    pcxclusterStat     =     embedStat->pc2clusterStat;
  }else if(pc==3){
    PcxCluster1        =     pc3cluster1;
    numOfPcxCluster1   =     pc3cluster1->get_PadNCluster();
    numOfPcxGhitClus1  =     pc3ghitclus1->RowCount();
    numOfPcxghit1      =     pc3ghit1->RowCount();
    dPcxGhitClus1      =     pc3ghitclus1->TableData();
    dPcxGhit1          =     pc3ghit1->TableData();
    pcxclusterE        =     embedStat->pc3clusterE;
    pcxclusterStat     =     embedStat->pc3clusterStat;
  }else {
    cout<<"Not a valid PC number"<<endl;
    return -1;
  }
  int pcxclusterid  = -1;
  int pcghitid = -1;
  for(int i=0;i<numOfPcxghit1;i++){
    if(trkG == dPcxGhit1[i].mctrack){
      pcghitid = i;
      break;
    }
  }
  if(pcghitid<0)     return  -1;
  for(int i=0;i<numOfPcxGhitClus1;i++){
    if(pcghitid == dPcxGhitClus1[i].ghitid){
      pcxclusterid = dPcxGhitClus1[i].clusid;
      break;
    }
  }
  if(pcxclusterid<0) return -1;
  /*
   *now pcxclusterid is the cluster id in PcxCluster1, we need to find out it's embeded id.
   */
  int  pcxclusteridE=-1;
  int stat = -1;
  for(int i=0;i<numOfPcxCluster1;i++){
    if(pcxclusterid == PcxCluster1->get_id(i)){
      pcxclusteridE = pcxclusterE[i];
      stat = pcxclusterStat[i];
      break;
    }
  }
  if(stat==0) return pcxclusteridE;
  else{
    cout<<"should not happn"<<endl;
    return -1;
  }
}

