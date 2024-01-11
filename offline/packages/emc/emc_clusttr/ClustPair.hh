#ifndef ClustPair_HH
#define ClustPair_HH

#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TObject.h>
#include "Global.hh"
#include "Clust.hh"
#include "Track.hh"
#include "Evt.hh"


class ClustPair : public TObject {
public:
  // Global Information
  Global glb0;
  Global glb1;
  int clt_num0;
  int clt_num1;

  // Pair Information
  float m,e,pt,px,py,pz,mv0,m0;
  float cosine,asym;

  // 2 Clust Information
  int clt_id0;
  int clt_id1;
  Clust clt0;
  Clust clt1;

  // Tracking Information
  int trk_num0;
  int trk_num1;
  float dist0[5];
  float dist1[5];
  float dist0xyz[5][3];
  float dist1xyz[5][3];
  int trk_id0[5];
  int trk_id1[5];
  Track trk0;
  Track trk1;

public:
  ClustPair();
  void Reset();

  ClassDef(ClustPair,1)
};
//
#endif
//
