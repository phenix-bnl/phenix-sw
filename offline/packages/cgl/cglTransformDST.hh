// Class: cglTransformDST
// Created by: Jeffery T. Mitchell
// Description: Central arm global hit association module
// Details: This class will transform DST hits and tracks from one
// reference frame to another.

#ifndef __CGLTRANSFORMDST_HH__
#define __CGLTRANSFORMDST_HH__

class PHCompositeNode;

class dDchHitWrapper;
class dDchTracksWrapper;
class dPadClusterWrapper;
class TecOutV1;
class dTofReconstructedWrapper;
class dEmcClusterLocalWrapper;
class dEmcClusterLocalExtWrapper;
class BbcOut;
class ZdcOut;
class VtxOut;

class PHFrame;
class PHVector;
class PHMatrix;
class PHPoint;

class cglTransformDST { 

public:
  cglTransformDST();                             // constructor
  virtual ~cglTransformDST(){}                            // destructor
  
public:

  // Data member access methods
  void set_Verbose(short);         // Set Verbose
  short get_Verbose();             // Get Verbose

  // Transform all of the DSTs.  arm and sector are the last 2 arguments
  void transform(PHCompositeNode *,PHFrame &f1,PHFrame &f2,short,short);

  // Transform all DSTs, but use cglDetectorGeo frames to do it
  void transformPhenix(PHCompositeNode *);
  void transformEast(PHCompositeNode *);
  void transformWest(PHCompositeNode *);

  // For testing only.  transform a retracted arm geometry
  void transformRetracted(PHCompositeNode *);

  // single entry transformation methods from matrices
  // The first argument is the entry number in the table
  void transform(long, BbcOut *, PHMatrix &m, PHVector &v);
  void transform(long, ZdcOut *, PHMatrix &m, PHVector &v);
  void transform(long, VtxOut *, PHMatrix &m, PHVector &v);
  void transform(long, dPadClusterWrapper *, PHMatrix &m, PHVector &v);
  void transform(long, dDchHitWrapper *, PHMatrix &m, PHVector &v);
  void transform(long, dDchTracksWrapper *, PHMatrix &m, PHVector &v);
  void transform(long, TecOutV1 *, PHMatrix &m, PHVector &v);
  void transform(long, dTofReconstructedWrapper *, PHMatrix &m, PHVector &v);
  void transform(long, dEmcClusterLocalWrapper *, PHMatrix &m, PHVector &v);
  void transform(long, dEmcClusterLocalExtWrapper *, PHMatrix &m, PHVector &v);

  // single entry transformation methods from frames
  // These methods should not be called if they must be called often in a loop
  // The first argument is the entry number in the table
  void transform(long, BbcOut *, PHFrame &f1, PHFrame &f2);
  void transform(long, ZdcOut *, PHFrame &f1, PHFrame &f2);
  void transform(long, VtxOut *, PHFrame &f1, PHFrame &f2);
  void transform(long, dPadClusterWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(long, dDchHitWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(long, dDchTracksWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(long, TecOutV1 *, PHFrame &f1, PHFrame &f2);
  void transform(long, dTofReconstructedWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(long, dEmcClusterLocalWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(long, dEmcClusterLocalExtWrapper *, PHFrame &f1, PHFrame &f2);

  // All entry transformation methods
  void transform(BbcOut *, PHFrame &f1, PHFrame &f2);
  void transform(ZdcOut *, PHFrame &f1, PHFrame &f2);
  void transform(VtxOut *, PHFrame &f1, PHFrame &f2);
  void transform(dPadClusterWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(dDchHitWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(dDchTracksWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(TecOutV1 *, PHFrame &f1, PHFrame &f2);
  void transform(dTofReconstructedWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(dEmcClusterLocalWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(dEmcClusterLocalExtWrapper *, PHFrame &f1, PHFrame &f2);

  // Arm and sector transformation methods
  // The first argument is the arm (0=east, 1=west, 2=both)
  // If there is a second argument, it is the sector
  // If the second argument is negative, then the entire arm will be moved
  void transform(short, short, dPadClusterWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(short, dDchHitWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(short, dDchTracksWrapper *, PHFrame &f1, PHFrame &f2);
  void transform(short, short, TecOutV1 *, PHFrame &f1, PHFrame &f2);
  void transform(short, short, dTofReconstructedWrapper *, 
		 PHFrame &f1, PHFrame &f2);
  void transform(short, short, dEmcClusterLocalWrapper *, 
		 PHFrame &f1, PHFrame &f2);
  void transform(short, short, dEmcClusterLocalExtWrapper *, 
		 PHFrame &f1, PHFrame &f2);

private:

  // Verbosity output level
  short Verbose;

}; 

#endif /* __CGLTRANSFORMDST_HH__ */
