
#include "Gg2.hh"

ClassImp(Gg2)

Gg2::Gg2(){

};
void Gg2::Reset(){
  // Global Information
  evcut = 0;
  mip1 = 0;
  hitnum1 = 0;
  rej1 = 0;
  mip2 = 0;
  hitnum2 = 0;
  rej2 = 0;
  cor_evn = 0;
  cor_mip = 0;
  nemc1 = 0;
  nemc2 = 0;

  // Pair Information
  m = 0;
  e = 0;
  pt = 0;
  px = 0;
  py = 0;
  pz = 0;
  m_vert = 0;
  cosine = 0;
  asym = 0;
  scale = 0;

  // 2 gamma Information
  g1.Reset();
  g2.Reset();

};
//

