#include "MPCEXENTSnglPisaHitv1.h"

ClassImp(MPCEXENTSnglPisaHitv1)
 int MPCEXENTSnglPisaHitv1::MPCEXENTCount = 0;

MPCEXENTSnglPisaHitv1::MPCEXENTSnglPisaHitv1()
{
  mctrack= -9999 ;
  vx = -9999 ;
  vy = -9999 ;
  vz = -9999 ;
  px = -9999 ;
  py = -9999 ;
  pz = -9999 ;
  isubevent = -9999; 
  ntrack = -9999; 
  nfile = -9999;
  return;
}

MPCEXENTSnglPisaHitv1::MPCEXENTSnglPisaHitv1(MPCEXENTSnglPisaHitv1 *PHtrack)
{

  if (!PHtrack) return;

  MPCEXENTCount = PHtrack->GetMPCEXENTCount();

  mctrack = PHtrack->GetMctrack(); 

  vx = PHtrack->GetVx(); 
  vy = PHtrack->GetVy(); 
  vz = PHtrack->GetVz(); 
  px = PHtrack->GetPx(); 
  py = PHtrack->GetPy(); 
  pz = PHtrack->GetPz(); 

  isubevent = PHtrack->GetIsubevent(); 
  ntrack = PHtrack->GetNtrack(); 
  nfile = PHtrack->GetNtrack(); 

  return;
}
