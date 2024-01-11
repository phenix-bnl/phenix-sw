//--------------------------------------------------------
//
// Class: PadRecModule (definition)
//
// Created by: Paul B Nilsson
//
// Description: PC Cluster Reconstruction
//
// Details: This is the Pad Chamber cluster reconstruction
//          module class that is used as an interface to
//          the cluster reconstruction algorithm.
//          The interface is necessary since cint doesn't
//          support some of the used C++ features
//--------------------------------------------------------

#ifndef __PADRECMODULE_HH__
#define __PADRECMODULE_HH__

#include "PHCompositeNode.h"
#include "padDetectorGeo.hh"

class PadRec;

class PadRecModule {

 public:
  PadRecModule(short pc); // Constructor
  virtual ~PadRecModule(); // Destructor

  PHBoolean event(short, padDetectorGeo *, PHCompositeNode *); // interface to reconstruction algorithm
  void setEventNumber(const short e) { eventNumber = e; };
  void setDebugLevel(const short d) { debug = d; };
  void setSplitMax(const short s) { splitMax = s; };
  void setSplitMode(const short m) { mode = m; };
  void setPadTypeLimit(const short l) { padTypeLimit = l; };
  void setClusterSizes(short, short, short, short, short, short, short, short, short);

 private:

  PadRec *padRecObj;
  short eventNumber, debug, mode, padTypeLimit, splitMax;
  short oneW, oneZ, oneL, twoW, twoZ, twoL, threeW, threeZ, threeL;

};

#endif
