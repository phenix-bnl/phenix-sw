// -----------------------------------------
// Created by:  Jiangyong Jia
//------------------------------------------
#ifndef _PHEMBEDEDEVENT_H_
#define _PHEMBEDEDEVENT_H_

#include "dDchTracks.h"
#include "phool.h"
#include "PHPointerList.h"
#include <vector>

class BbcOut;
class CglTrack;
class dEmcGeaClusterTrackWrapper;
class EventHeader;
class PHTrackOut;
class RunHeader;
class ZdcOut;

class fkinWrapper;
class primaryWrapper;
class headerWrapper;
class dcghitWrapper;
class dDchGhitHitsWrapper;
class pcghitWrapper;
class dPadGhitClusWrapper;
class dTofGdigiWrapper;
class dTofGdigiRecWrapper;

class dDchHitWrapper;
class dDchTracksWrapper;
class DchHitLineTable;
class DchTrack;

class PadRaw;
class dPadRawWrapper;
class PadCluster;
class dCrkHitWrapper;
class CrkHit;
class TofOut;
class dTofReconstructedWrapper;
class emcTowerContainer;
class emcClusterContainer;
class PHDchTrackOut;
class PHCompositeNode;
class PHDchCalibrationObject;
class TecOut;
class TecOutV1;
class PHGlobal;
class PHCentralTrack;
class PHEmbedStat;
class AccRaw;
class TofwHit;

class PHEmbededEvent{
 protected:
  //1 :test particle node, 2 real DST event ,3: merged event
  fkinWrapper                   *fkin1;
  primaryWrapper                *primary1;
  headerWrapper                 *header1;
  dcghitWrapper                 *dcghit1;
  dDchGhitHitsWrapper           *dchghithit1;
  pcghitWrapper                 *pc1ghit1;
  pcghitWrapper                 *pc2ghit1;
  pcghitWrapper                 *pc3ghit1;
  dPadGhitClusWrapper           *pc1ghitclus1;
  dPadGhitClusWrapper           *pc2ghitclus1;
  dPadGhitClusWrapper           *pc3ghitclus1;
  dTofGdigiWrapper              *tofGdigi1;
  dTofGdigiRecWrapper           *tofGdigiRec1;

  PHGlobal                      *global2;
  PHCentralTrack                *CNTtrk2;
  //subsystem information
  BbcOut                        *bbcout1,*bbcout2,*bbcout3;
  ZdcOut                        *zdcout1,*zdcout2,*zdcout3;
  
  CglTrack                      *cgltrack1,          *cgltrack2,          *cgltrack3;

  //DC information

  dDchHitWrapper                *dchitwrap1,         *dchitwrap2,         *dchitwrap3;
  DchHitLineTable               *dchit1v1,           *dchit2v1,           *dchit3v1;
  //DchHitLineTable               *dchitout1v2,        *dchitout2v2,        *dchitout3v2;
  DchHitLineTable               *dchit1,             *dchit2,             *dchit3;
  //std::vector<int>                    dchitE;//dchitE.size() == dchit1->Entries();
  //std::vector<int>                    dchitStat;
  dDchTracksWrapper             *dchperftrack1,      *dchperftrack2,      *dchperftrack3;
  //dDchTracksWrapper             *dchtrack1,          *dchtrack2,          *dchtrack3;
  DchTrack                      *dchtrack1,          *dchtrack2,          *dchtrack3;

  //PC information
  dPadRawWrapper                *pc1rawwrap3,        *pc2rawwrap3,        *pc3rawwrap3;
  PadRaw                        *pc1raw1,            *pc1raw2,            *pc1raw3;
  //std::vector<int>                    pc1rawE;
  //std::vector<int>                    pc1rawStat;
  PadRaw                        *pc2raw1,            *pc2raw2,            *pc2raw3;
  //std::vector<int>                    pc2rawE;
  //std::vector<int>                    pc2rawStat;
  PadRaw                        *pc3raw1,            *pc3raw2,            *pc3raw3;
  //std::vector<int>                    pc3rawE;
  //std::vector<int>                    pc3rawStat;
  
  //dPadClusterWrapper            *pc1cluster1,        *pc1cluster2,        *pc1cluster3;
  PadCluster                    *pc1cluster1,        *pc1cluster2,        *pc1cluster3;
  //std::vector<int>                    pc1clusterE;
  //std::vector<int>                    pc1clusterStat;
  //dPadClusterWrapper            *pc2cluster1,        *pc2cluster2,        *pc2cluster3;
  PadCluster                    *pc2cluster1,        *pc2cluster2,        *pc2cluster3;
  //std::vector<int>                    pc2clusterE;
  //std::vector<int>                    pc2clusterStat;
  //dPadClusterWrapper            *pc3cluster1,        *pc3cluster2,        *pc3cluster3;
  PadCluster                    *pc3cluster1,        *pc3cluster2,        *pc3cluster3;
  //std::vector<int>                    pc3clusterE;
  //std::vector<int>                    pc3clusterStat;
  
  //TOF information
  dTofReconstructedWrapper      *tofreconstructed3;
  TofOut                        *tofout1,            *tofout2,            *tofout3;
  //std::vector<int>                    tofoutE;
  //std::vector<int>                    tofoutStat;
  
  
  //CRK information
  CrkHit                        *crkhit1,            *crkhit2,            *crkhit3;
  dCrkHitWrapper                *crkhitold1,         *crkhitold2,         *crkhitold3;
  //std::vector<int>                    crkhitE;
  //std::vector<int>                    crkhitStat;

  TecOut                        *tecout1, *tecout2;
  TecOut                        *techitout1, *techitout2;
  TecOutV1                      *tecout3; 

  //EMC information
  emcTowerContainer             *emctower1,          *emctower2,          *emctower3;
  //std::vector<int>                    emctowerE;
  //std::vector<int>                    emctowerStat;
  emcClusterContainer           *emccluster1,        *emccluster2,        *emccluster3;
  //std::vector<int>                    emcclusterE;
  //std::vector<int>                    emcclusterStat;
  dEmcGeaClusterTrackWrapper    *emcgeaclustertrack1;

  //ACC infomation
  AccRaw                        *accraw1, *accraw2, *accraw3;
  
  //Tofw information
  TofwHit                       *tofwhit1, *tofwhit2, *tofwhit3;

  EventHeader                   *eventheader2;
  RunHeader                     *runheader2;
  //post reconstruction stuff,should have a coresponding geant record in same format 
  PHTrackOut                    *phtrack1,           *phtrack2,           *phtrack3;
  PHDchTrackOut                 *phdchtrack1,        *phdchtrack2,        *phdchtrack3;

public:
  //new tables

private:
  int verbose;
  int mc;
  
public:
  PHBoolean            event(PHCompositeNode* node1,PHCompositeNode* node2,PHCompositeNode* node3);
  PHBoolean            eventSingleNode(PHCompositeNode* node1);
  PHBoolean            eventRealNode(PHCompositeNode* node2);
  PHBoolean            eventInputNodes(PHCompositeNode* node1,PHCompositeNode* node2);
  PHBoolean            eventMergedNode(PHCompositeNode* node3);

  int                  get_verbose() { return verbose;}
  int                  get_EmbedMode() { return mc;}
  void                 set_verbose(int val){verbose = val;}

public:
  PHEmbededEvent();
  virtual ~PHEmbededEvent();
  
public:
  fkinWrapper*                   get_fkin1()           { return fkin1; }
  primaryWrapper*                get_primary1()        { return primary1;}
  headerWrapper*                 get_header1 ()        { return header1;}
  dcghitWrapper*                 get_dcghit1()         { return dcghit1;}
  dDchGhitHitsWrapper*           get_dchghithit1()     { return dchghithit1;}
  pcghitWrapper*                 get_pc1ghit1()        { return pc1ghit1;}
  pcghitWrapper*                 get_pc2ghit1()        { return pc2ghit1;}
  pcghitWrapper*                 get_pc3ghit1()        { return pc3ghit1;}
  dTofGdigiWrapper*              get_tofGdigi1()       { return tofGdigi1;}
  dTofGdigiRecWrapper*           get_tofGdigiRec1()    { return tofGdigiRec1;}
  dPadGhitClusWrapper*           get_pc1ghitclus1()    { return pc1ghitclus1;}
  dPadGhitClusWrapper*           get_pc2ghitclus1()    { return pc2ghitclus1;}
  dPadGhitClusWrapper*           get_pc3ghitclus1()    { return pc3ghitclus1;}
  dDchTracksWrapper*             get_dchperftrack1()   { return dchperftrack1;}
  dDchTracksWrapper*             get_dchperftrack2()   { return dchperftrack2;}
  dDchTracksWrapper*             get_dchperftrack3()   { return dchperftrack3;}
  

  PHGlobal*                      get_global2()          { return global2;}
  PHCentralTrack*                get_CNTtrk2()          { return CNTtrk2;}

  //subsystem information
  BbcOut*                get_bbcout1()         { return bbcout1;}
  BbcOut*                get_bbcout2()         { return bbcout2;}
  BbcOut*                get_bbcout3()         { return bbcout3;}
  ZdcOut*                get_zdcout1()         { return zdcout1;}
  ZdcOut*                get_zdcout2()         { return zdcout2;}
  ZdcOut*                get_zdcout3()         { return zdcout3;}
  
  CglTrack*              get_cgltrack1()       { return cgltrack1;}
  CglTrack*              get_cgltrack2()       { return cgltrack2;}
  CglTrack*              get_cgltrack3()       { return cgltrack3;}

  //DC
  DchTrack*                      get_dchtrack1()       { return dchtrack1;}
  DchTrack*                      get_dchtrack2()       { return dchtrack2;}
  DchTrack*                      get_dchtrack3()       { return dchtrack3;}
  dDchHitWrapper*                get_dchitwrap1()      { return dchitwrap1;}
  dDchHitWrapper*                get_dchitwrap2()      { return dchitwrap2;}
  dDchHitWrapper*                get_dchitwrap3()      { return dchitwrap3;}
  DchHitLineTable*               get_dchit1v1()        { return dchit1v1;}
  DchHitLineTable*               get_dchit2v1()        { return dchit2v1;}
  DchHitLineTable*               get_dchit3v1()        { return dchit3v1;}
  DchHitLineTable*               get_dchit1()          { return dchit1;}
  DchHitLineTable*               get_dchit2()          { return dchit2;}
  DchHitLineTable*               get_dchit3()          { return dchit3;}
  std::vector<int>&              get_dchitEmbed();
  std::vector<int>&              get_dchitEmbedStat();

  //PC information
  PadRaw*                        get_pc1raw1()         { return pc1raw1;}
  PadRaw*                        get_pc1raw2()         { return pc1raw2;}
  PadRaw*                        get_pc1raw3()         { return pc1raw3;}
  PadRaw*                        get_pc2raw1()         { return pc2raw1;}
  PadRaw*                        get_pc2raw2()         { return pc2raw2;}
  PadRaw*                        get_pc2raw3()         { return pc2raw3;}
  PadRaw*                        get_pc3raw1()         { return pc3raw1;}
  PadRaw*                        get_pc3raw2()         { return pc3raw2;}
  PadRaw*                        get_pc3raw3()         { return pc3raw3;}
  dPadRawWrapper*                get_pc1rawwrap3()     { return pc1rawwrap3;}
  dPadRawWrapper*                get_pc2rawwrap3()     { return pc2rawwrap3;}
  dPadRawWrapper*                get_pc3rawwrap3()     { return pc3rawwrap3;}

  PadCluster*                    get_pc1cluster1()     { return pc1cluster1;}
  PadCluster*                    get_pc1cluster2()     { return pc1cluster2;}
  PadCluster*                    get_pc1cluster3()     { return pc1cluster3;}
  std::vector<int>&              get_pc1clusterEmbed();
  std::vector<int>&              get_pc1clusterEmbedStat();
  PadCluster*                    get_pc2cluster1()     { return pc2cluster1;}
  PadCluster*                    get_pc2cluster2()     { return pc2cluster2;}
  PadCluster*                    get_pc2cluster3()     { return pc2cluster3;}
  std::vector<int>&              get_pc2clusterEmbed();
  std::vector<int>&              get_pc2clusterEmbedStat();
  PadCluster*                    get_pc3cluster1()     { return pc3cluster1;}
  PadCluster*                    get_pc3cluster2()     { return pc3cluster2;}
  PadCluster*                    get_pc3cluster3()     { return pc3cluster3;}
  std::vector<int>&              get_pc3clusterEmbed();
  std::vector<int>&              get_pc3clusterEmbedStat();
  
  CrkHit*                        get_crkhit1()         { return crkhit1;}
  CrkHit*                        get_crkhit2()         { return crkhit2;}
  CrkHit*                        get_crkhit3()         { return crkhit3;}
  dCrkHitWrapper*                get_crkhitold1()      { return crkhitold1;}
  dCrkHitWrapper*                get_crkhitold2()      { return crkhitold2;}
  dCrkHitWrapper*                get_crkhitold3()      { return crkhitold3;}
  std::vector<int>&              get_crkhitEmbed();
  std::vector<int>&              get_crkhitEmbedStat();
  //dTofReconstructedWrapper*      get_tofreconstructed1(){ return tofreconstructed1;}
  //dTofReconstructedWrapper*      get_tofreconstructed2(){ return tofreconstructed2;}
  dTofReconstructedWrapper*      get_tofreconstructed3(){ return tofreconstructed3;}
  TofOut*                        get_tofout1()         { return tofout1;}
  TofOut*                        get_tofout2()         { return tofout2;}
  TofOut*                        get_tofout3()         { return tofout3;}
  std::vector<int>&              get_tofoutEmbed() ;
  std::vector<int>&              get_tofoutEmbedStat();

  TecOut*                        get_tecout1()         { return tecout1;}
  TecOut*                        get_tecout2()         { return tecout2;}
  TecOut*                        get_techitout1()      { return techitout1;}
  TecOut*                        get_techitout2()      { return techitout2;}
  TecOutV1*                      get_tecout3()         { return tecout3;}
  
  emcTowerContainer*             get_emctower1()        { return emctower1;}
  emcTowerContainer*             get_emctower2()        { return emctower2;}
  emcTowerContainer*             get_emctower3()        { return emctower3;}
  std::vector<int>&              get_emctowerEmbed();
  std::vector<int>&              get_emctowerEmbedStat();
  emcClusterContainer*           get_emccluster1()       { return emccluster1;}
  emcClusterContainer*           get_emccluster2()       { return emccluster2;}
  emcClusterContainer*           get_emccluster3()       { return emccluster3;}
  std::vector<int>&              get_emcclusterEmbed();
  std::vector<int>&              get_emcclusterEmbedStat();
  dEmcGeaClusterTrackWrapper*    get_emcgeaclustertrack1() { return emcgeaclustertrack1;}

  AccRaw*                        get_accraw1()       { return accraw1;}
  AccRaw*                        get_accraw2()       { return accraw2;}
  AccRaw*                        get_accraw3()       { return accraw3;}

  std::vector<int>&              get_accrawEmbed();
  std::vector<int>&              get_accrawEmbedStat();

  TofwHit*                        get_tofwhit1()       { return tofwhit1;}
  TofwHit*                        get_tofwhit2()       { return tofwhit2;}
  TofwHit*                        get_tofwhit3()       { return tofwhit3;}

  std::vector<int>&              get_tofwhitEmbed();
  std::vector<int>&              get_tofwhitEmbedStat();

  
  EventHeader*                   get_eventheader2()    { return eventheader2;}
  RunHeader*                     get_runheader2()      { return runheader2;}
  //post reconstruction stuff, should have a coresponding geant record in same format 
  PHTrackOut*                    get_phtrack1()        { return phtrack1;}
  PHTrackOut*                    get_phtrack2()        { return phtrack2;}
  PHTrackOut*                    get_phtrack3()        { return phtrack3;}
  PHDchTrackOut*                 get_phdchtrack1()     { return phdchtrack1;}
  PHDchTrackOut*                 get_phdchtrack2()     { return phdchtrack2;}
  PHDchTrackOut*                 get_phdchtrack3()     { return phdchtrack3;}

  PHEmbedStat*                   embedStat;
  PHDchCalibrationObject* dchCalibrationObject;
public:
  /*
   *  DCH Functions
   */
  //get geant hit information
  float            get_x1mPerfectTrack(int trkPerf);
  float            get_x2mPerfectTrack(int trkPerf);
  int              get_x1hPerfectTrack(int trkPerf);
  int              get_x2hPerfectTrack(int trkPerf);
  int              get_uv1hPerfectTrack(int trkPerf);
  int              get_uv2hPerfectTrack(int trkPerf);
  //get response information
  float            get_x1mS(int trkS);
  float            get_x2mS(int trkS);
  int              get_x1hS(int trkS);
  int              get_x2hS(int trkS);
  int              get_uv1hS(int trkS);
  int              get_uv2hS(int trkS);
  //get embed information
  int              get_x1hAfterEmbedForPerfectTrack(int trkPerf);
  int              get_x2hAfterEmbedForPerfectTrack(int trkPerf);
  int              get_uv1hAfterEmbedForPerfectTrack(int trkPerf);
  int              get_uv2hAfterEmbedForPerfectTrack(int trkPerf);
  int              get_x1hBeforeEmbedForPerfectTrack(int trkPerf);
  int              get_x2hBeforeEmbedForPerfectTrack(int trkPerf);
  int              get_uv1hBeforeEmbedForPerfectTrack(int trkPerf);
  int              get_uv2hBeforeEmbedForPerfectTrack(int trkPerf);
  int              get_x1hAfterEmbedForSimulatedTrack(int trkS);
  int              get_x2hAfterEmbedForSimulatedTrack(int trkS);
  int              get_uv1hAfterEmbedForSimulatedTrack(int trkS);
  int              get_uv2hAfterEmbedForSimulatedTrack(int trkS);
  //get reconstruction information
  float            get_x1mR(int trkR);
  float            get_x2mR(int trkR);
  int              get_x1hR(int trkR);
  int              get_x2hR(int trkR);
  int              get_uv1hR(int trkR);
  int              get_uv2hR(int trkR);

  float            getClosestApproachToBeamAxis(DDCHTRACKS_ST&dchTrack);
  float            meanDistanceBetweenTracks(DDCHTRACKS_ST&,DDCHTRACKS_ST&);
  int              recohit2Geanthit(int recohitid);
  int              simuhit2Geanthit(int simuhitid);
  int              recohit2Simulatedhit(int recohitid);
  int              geanthit2Perfecttrack(int ghitid);
  int              simuhit2Perfecttrack(int simuid);
  int              recohit2Perfecttrack(int recoid);  
  int              simuhit2Simulatedtrack(int simuhitid);
  int              recohit2Simulatedtrack(int recohitid);
  int              recohit2Reconstructedtrack(int recohitid);
  /*
   * PC functions
   */
  int              pcghit2Cluster(int pc,int pcghitid);
  //maybe will not be used if we merge at raw level.
  int              get_pcClusterAfterEmbedForGeantTrack(int pc,int trkG);
};
#endif
