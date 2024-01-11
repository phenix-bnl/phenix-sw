#include "MPCEXABSSnglPisaHitv1.h"

ClassImp(MPCEXABSSnglPisaHitv1)
 int MPCEXABSSnglPisaHitv1::MPCEXABSCount = 0;
 int MPCEXABSSnglPisaHitv1::MPCEXABS1Count = 0;
 int MPCEXABSSnglPisaHitv1::MPCEXABS2Count = 0;

MPCEXABSSnglPisaHitv1::MPCEXABSSnglPisaHitv1()
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

MPCEXABSSnglPisaHitv1::MPCEXABSSnglPisaHitv1(MPCEXABSSnglPisaHitv1 *PHtrack)
{

  if (!PHtrack) return;

  MPCEXABSCount = GetMPCEXABSCount ();
  MPCEXABS1Count = GetMPCEXABS1Count ();
  MPCEXABS2Count = GetMPCEXABS2Count ();

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
