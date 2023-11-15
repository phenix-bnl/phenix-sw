// $Id: CrkPISAHit.cc,v 1.2 2007/11/13 22:27:43 hpereira Exp $

/*!
\file  CrkPISAHit.cc
\brief container for RICH pisa hits
\author  T. K. Ghosh
\version $Revision: 1.2 $
\date    $Date: 2007/11/13 22:27:43 $
*/

#include "CrkPISAHit.h"

using namespace std;

ClassImp(CrkPISAHit)

//____________________________________________________________________________
vector<CrkPISAHit> CrkPISAHit::_hits;
  
//____________________________________________________________________________
CrkPISAHit::CrkPISAHit(
  Short_t argpmt, Float_t argx, Float_t argy, Float_t argz, 
  Float_t argtof, Float_t argpx, Float_t argpy, Float_t argpz, Short_t argpid, 
  Int_t argtra, Int_t argparent, Int_t argnbf, Int_t argbi1, Int_t argbi2, 
  Float_t argbp1, Float_t argbp2, Int_t argmctrack,
  Int_t argnfile, Int_t argisubevent )
{
  pmt = argpmt;
  pos[0] = argx;
  pos[1] = argy;
  pos[2] = argz;
  tof = argtof;
  mom[0] = argpx;
  mom[1] = argpy;
  mom[2] = argpz;
  pid = argpid;
  tra = argtra;
  parent = argparent;
  nbf = argnbf;
  bi1 = argbi1;
  bi2 = argbi2;
  bp1 = argbp1;
  bp2 = argbp2;
  mctrack = argmctrack;
  nfile= argnfile;
  isubevent = argisubevent;
  
}
