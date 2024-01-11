#ifndef TCRKPROJECTOR_H_INCLUDED
#define TCRKPROJECTOR_H_INCLUDED

/*
 * Header file for family of CrkProjectors.
 * The purpose of CrkProjector is to associate reconstructed track to
 * RICH. From the tracking chambers information, a "projection" of track for
 * each of reconstructed tracks are produced. A projector object is passed
 * to TCrkModule::pid() member function and used internally in pid().
 * 
 *
 * Usage:
 *  TCrkProjector *projector = new TCrkxxxxProjector();
 *  TCrkModule::pid(topNode, projector);
 *  delete projector
 *
 * Inheritance tree  ( Notation: public base <--- derived class)
 *
 * TCrkProjector <---- TCrkDchProjector       (do Dch fit by itself. Assume
 *                                             Zvertx = 0.0)
 *               <---- TCrkDchTrackProjector  (use contents of dDchTracks)
 *               <---- TCrkCglInVecProjector  (use contents of dCglInVector)
 *               <---- TCrkCglProjProjector   (use contents of dCglProjection)
 *               (More to follow)
 *
 * TCrkProjector is an abstract base class of all CrkProjector family. 
 *
 * Author: Y. Akiba
 * Initial Date:   February 19, 2000
 * Last Revised:   February 20, 2000
 */
#include <iostream>
#include <vector>

class PHCompositeNode;

struct CrkProj { // struct for "projection".
  short track_id;
  float x[3];
  float u[3];
};

class TCrkProjector { // abstract base class of all CrkProjectors
public:
  virtual ~TCrkProjector();
  virtual CrkProj *projection(PHCompositeNode *top, int *p_nproj)=0;
};

class TCrkDchProjector: public TCrkProjector {
public:
  TCrkDchProjector() {}
  virtual ~TCrkDchProjector() {}
  virtual CrkProj *projection(PHCompositeNode *top, int *p_nproj);
};

class TCrkDchTrackProjector: public TCrkProjector {
public:
  TCrkDchTrackProjector() {}
  virtual ~TCrkDchTrackProjector() {}
  virtual CrkProj *projection(PHCompositeNode *top, int *p_nproj);
};

class TCrkCglProjector: public TCrkProjector {
public:
  TCrkCglProjector();
  virtual ~TCrkCglProjector();
  virtual CrkProj *projection(PHCompositeNode *top, int *p_nproj);
};


#endif

