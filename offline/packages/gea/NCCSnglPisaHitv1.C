#include "NCCSnglPisaHitv1.h"

ClassImp(NCCSnglPisaHitv1)
 int NCCSnglPisaHitv1::NCCCount = 0;
 int NCCSnglPisaHitv1::NCC1Count = 0;
 int NCCSnglPisaHitv1::NCC2Count = 0;

NCCSnglPisaHitv1::NCCSnglPisaHitv1()
{
  mctrack= -9999 ;
  xx   = -9999 ;
  yy   = -9999 ;
  zz   = -9999 ;
  id   = -9999 ;
  dedx = -9999 ;
  Xe   = -9999 ;
  Ye   = -9999 ;
  Pmom = -9999 ;
  P_id = -9999 ;
  PNum = -9999 ;
  track = -9999 ;
  arm = -9999 ;
  ipc = -9999 ;
  isubevent = -9999 ;
  nfile = -9999 ;
  return;
}

NCCSnglPisaHitv1::NCCSnglPisaHitv1(NCCSnglPisaHitv1 *PHtrack)
{

  if (!PHtrack) return;

  NCCCount = GetNCCCount ();
  NCC1Count = GetNCC1Count ();
  NCC2Count = GetNCC2Count ();
  arm = GetIarm ();
  isubevent = GetIsubevent ();
  track = GetNtrack ();
  id = GetId ();
  mctrack = GetMctrack ();
  ipc = GetIpc ();
  xx          = GetXin ();
  yy          = GetYin ();
  zz          = GetZin ();
  dedx        = GetDedx ();
  Xe          = GetXe ();
  Ye          = GetYe ();
  Pmom        = GetPmom ();
  P_id        = GetP_id ();
  PNum        = GetPNum ();
  //  pathLength = GetPathLength ();
  nfile = GetNfile ();
 return;
}
