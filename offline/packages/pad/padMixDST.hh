//----------------------------------------------------------------------- 
// Class: padMixDST
// 
// Created by: David Silvermyr
// 
// Description: 
// This class is designed for mixing DSTs. Presently only on the cluster 
// level.
//------------------------------------------------------------------------

#ifndef __PADMIXDST_HH__
#define __PADMIXDST_HH__

#include "phool.h"

class PHCompositeNode;

const int PADMIXDST_MAXHITS=1000;

class padMixDST 
{ 

public:
  // save possibility for the user to use a specific seed value
  padMixDST(long seedval=123456789);                             // constructor
  virtual ~padMixDST();                            // destructor

  // take the PC cluster contents on topNode0 and topNode1
  virtual PHBoolean event(PHCompositeNode *topNode0, PHCompositeNode *topNode1);      
  // and store the merge on one of them, if the user so desires..
  
public:

  // Data member access methods

  // Print out what's going on/has been done
  // general information
  void print();

  // set debug level (0 = do not debug)
  void setDebugLevel(short debug) { Debug = debug; } 

  // get number of hits in PC1/3 (only the arms we are reconstructing tracks
  // from)
  short getNumAccHits(short isrc, short ipc) const 
  { return ( ((isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2)) ? numAccHits[isrc][ipc] : -1 ); }

  short getNumRemovedHits(short isrc, short ipc) const 
  { return ( ((isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2)) ? removedHits[isrc][ipc] : -1 ); }

  // access methods to rec. hits data
  short getHitId(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].id : -1); }
  short getHitArm(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].arm : -1); }
  short getHitSector(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].sector : -1); }
  short getHitWire(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].wire : -1); }
  short getHitCell(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].cell : -1); }
  short getHitType(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].type : -1); }
  short getHitAccepted(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].accepted : -1); }

  float getHitX(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].xyz[0] : -999.99); }
  float getHitY(short isrc, short ipc, short ihit) const	  
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].xyz[1] : -999.99); }
  float getHitZ(short isrc, short ipc, short ihit) const	  
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].xyz[2] : -999.99); }

  float getHitdX(short isrc, short ipc, short ihit) const
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].dxyz[0] : -999.99); }
  float getHitdY(short isrc, short ipc, short ihit) const	  
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].dxyz[1] : -999.99); }
  float getHitdZ(short isrc, short ipc, short ihit) const	  
  { return ( ( (isrc>=0) && (isrc<=1) && (ipc>=0) && (ipc<=2) && (ihit>=0) && (ihit<numAccHits[isrc][ipc]) ) ? padClust[isrc][ipc][ihit].dxyz[2] : -999.99); }

  void storeOnNode(short si) { StoreOnNode = si; } // put merge result on node si
  void doNotStoreOnNode() { StoreOnNode = -1; } // don't put merge result on one of the input nodes

  void ignoreOverlap() { IgnoreOverlap = 1; } // don't care about overlap between clusters
  void doNotIgnoreoverlap() { IgnoreOverlap = -1; } // care about overlap between clusters

  void setWireCut(float f) { WireCut = f; } // cuts are based on type value (size)
  void setZCut(float f) { ZCut = f; }
  float getWireCut() { return WireCut; }
  float getZCut() { return ZCut; }

  void setMinClusterType(short itype) { minClusterType = itype; }
  void setMaxClusterType(short itype) { maxClusterType = itype; }

  PHBoolean getInfo(PHCompositeNode *topNode, short nodeindex);      
  PHBoolean setInfo(PHCompositeNode *topNode, short nodeindex);      
  PHBoolean compare01();      

private:

  // Debug flag (0 = do not debug) 
  short Debug;

  // counter of how many acc. hits we have from the two sources
  // indices are src (0-1), pc (0-2)
  short numAccHits[2][3];  
  short removedHits[2][3];

  typedef struct
  {
    short id;  
    short arm; 
    short sector;
    short wire;  
    short cell;  
    float xyz[3];
    float dxyz[3];
    short type;
    short accepted;
  } padhit;
  padhit padClust[2][3][PADMIXDST_MAXHITS]; 
  // indices are src (0-1), pc (0-2), hit (0-(PADMIXDST_MAXHITS-1))

  // a switch for determining which node (if) we should store merge results
  short StoreOnNode;
 
  // a switch for determining if we should store all hits (value=1) or the one's that don't overlap (!=1)
  short IgnoreOverlap;

  // cut variables
  float WireCut,ZCut;

  // for selecting only certain clustertype (e.g avoid using too large or split clusters)
  short minClusterType,maxClusterType;

  // seed for utiRandom
  long utiseed;
}; 

#endif /* __PADMIXDST_HH__ */
