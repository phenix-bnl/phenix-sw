#include "TDirectory.h"

#include "CtrPISAHit.h"

/////////////////////////////////////////////////////////////////////////
//                                                                     //
//                                                                     //
// Implementation of EmcPISAHit event header, event, and PISA hits in event  //
//                                                                     //
//////////////////////////////////////////////////////////////////////////

ClassImp(CtrPISAHit)

CtrPISAHit *CtrPISAHit::CtrHitEvt = 0;
  Int_t CtrPISAHit::CtrCount = 0;



CtrPISAHit:: CtrPISAHit( Int_t argdetector, Float_t argx, Float_t argy, Float_t argz, 
     Short_t argpid, Int_t argitra, Float_t argpvx, Float_t argpvy, Float_t argpvz, 
     Float_t argvx, Float_t argvy, Float_t argvz, Int_t argmctrack, Int_t argnfile,
     Int_t argisubevent) : TObject()


{
      detector = argdetector;
      pos[0] = argx;
      pos[1] = argy;
      pos[2] = argz;
      pid= argpid;
      itra= argitra;
      momv[0] = argpvx;
      momv[1] = argpvy;
      momv[2] = argpvz;
      v[0]= argvx;
      v[1]= argvy;
      v[2]= argvz;
      mctrack = argmctrack;
      nfile = argnfile;
      isubevent = argisubevent;

  // cerr << "\n PISAHit constructor called" << endl;
}












