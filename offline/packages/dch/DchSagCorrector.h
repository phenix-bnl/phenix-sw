#ifndef __DCHSAGCORRECTOR_H__
#define __DCHSAGCORRECTOR_H__

#include "SubsysReco.h"

class PHCompositeNode;
class DchTrack;
class DchHitLineTable;
class PHDchAddressObject;
class PHDchGeometryObject;
class PHLine;
class DchSnglTrackv1;
//
//  Hello True Believers:
//    This is a simple Reco Module whose purpose to measure
//    wire sag with respect to zed in the drift chamber.
//                                                Nuf' said
//                                                STJL
//                                                9-1-2003
//


class DchSagCorrector: public SubsysReco
{
 public:
  DchSagCorrector(const char *name = "DchSagCorrector");
  virtual ~DchSagCorrector() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  void Set_ENO (const float val) { ENO = val; return;}
  void Set_ENE (const float val) { ENE = val; return;}
  void Set_ESO (const float val) { ESO = val; return;}
  void Set_ESE (const float val) { ESE = val; return;}
  void Set_WNO (const float val) { WNO = val; return;}
  void Set_WNE (const float val) { WNE = val; return;}
  void Set_WSO (const float val) { WSO = val; return;}
  void Set_WSE (const float val) { WSE = val; return;}

 protected:
  int FillHistos(int arm,int side,int even,float dx, float dy,float para, float ortho,float zed);
  int CalcResidual(int plane, DchSnglTrackv1 *track,PHLine trackline,float zed);
  //DataNodes  -- ala Akiba...
  DchTrack            * dchtracks;     
  DchHitLineTable     * dchhits;     
 private:
  float ENO;
  float ENE;
  float ESO;
  float ESE;
  float WNO;
  float WNE;
  float WSO;
  float WSE;
  PHDchAddressObject * DAO;
  PHDchGeometryObject * DGO;
};

#endif /* __DCHSAGCORRECTOR_H__ */
