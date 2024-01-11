#include "MPCFPLTSnglPisaHitv1.h"

ClassImp(MPCFPLTSnglPisaHitv1)
 int MPCFPLTSnglPisaHitv1::MPCFPLTCount = 0;
 int MPCFPLTSnglPisaHitv1::MPCFPLT1Count = 0;
 int MPCFPLTSnglPisaHitv1::MPCFPLT2Count = 0;

MPCFPLTSnglPisaHitv1::MPCFPLTSnglPisaHitv1()
{
  evt = -9999;
  id   = -9999 ;
  arm = -9999 ;
  mctrack= -9999 ;
  dedx = -9999 ;
  track = -9999; 
  incc = -9999 ;
  isubevent = -9999 ;
  nfile = -9999 ;
  return;
}

MPCFPLTSnglPisaHitv1::MPCFPLTSnglPisaHitv1(MPCFPLTSnglPisaHitv1 *PHtrack)
{

  if (!PHtrack) return;

  MPCFPLTCount = GetMPCFPLTCount ();
  MPCFPLT1Count = GetMPCFPLT1Count ();
  MPCFPLT2Count = GetMPCFPLT2Count ();

  evt = GetNEvent(); 
  id = GetId(); 
  arm = GetIarm ();
  mctrack = GetMctrack(); 
  dedx = GetDedx(); 
  track = GetNtrack ();
  incc = GetIncc(); 
  isubevent = GetIsubevent ();
  nfile = GetNfile ();

  return;
}
