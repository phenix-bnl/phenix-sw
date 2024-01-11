#include "NCCSnglPisaHitv2.h"

ClassImp(NCCSnglPisaHitv2)
 int NCCSnglPisaHitv2::NCCCount = 0;
 int NCCSnglPisaHitv2::NCC1Count = 0;
 int NCCSnglPisaHitv2::NCC2Count = 0;

NCCSnglPisaHitv2::NCCSnglPisaHitv2()
{
  evt = -9999;
  id   = -9999 ;
  arm = -9999 ;
  mctrack= -9999 ;
  dedx = -9999 ;
  track = -9999; 
  incc = -9999 ;
  twrid = -9999; 
  senid = -9999;
  tof = -9999;
  isubevent = -9999 ;
  nfile = -9999 ;
  return;
}

NCCSnglPisaHitv2::NCCSnglPisaHitv2(NCCSnglPisaHitv2 *PHtrack)
{

  if (!PHtrack) return;

  NCCCount = GetNCCCount ();
  NCC1Count = GetNCC1Count ();
  NCC2Count = GetNCC2Count ();

  evt = GetNEvent(); 
  id = GetId(); 
  arm = GetIarm ();
  mctrack = GetMctrack(); 
  dedx = GetDedx(); 
  track = GetNtrack ();
  incc = GetIncc(); 
  twrid = GetTWRID(); 
  senid = GetSENID(); 
  tof = GetTOFIN();   
  isubevent = GetIsubevent ();
  nfile = GetNfile ();

  return;
}
