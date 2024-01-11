#ifndef Gg2_HH
#define Gg2_HH

#ifdef COMPILE
#include <cstdlib>
#include <stdio.h>
#include <unistd.h>
#include <cstring>
#include <iostream.h>
#include <cmath>
#include <Rtypes.h>
#include <TObject.h>
#endif
#include "ClustTr.hh"

class Gg2 : public TObject {
public:
  // Global Information
  float evcut;
  float mip1,hitnum1,rej1;
  float mip2,hitnum2,rej2;
  float cor_evn,cor_mip;
  float nemc1,nemc2;

  // Pair Information
  float m,e,pt,px,py,pz,m_vert;
  float cosine,asym;
  float scale;

  // 2 gamma Information
  ClustTr g1;
  ClustTr g2;


public:
  Gg2();
  void Reset();

  ClassDef(Gg2,1)
};
//
#endif
//
