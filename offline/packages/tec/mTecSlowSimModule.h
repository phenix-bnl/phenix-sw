#ifndef __MTECSLOWSIMMODULE_H__
#define __MTECSLOWSIMMODULE_H__

#include "TecBasicObject.hh"
#include "phool.h"

class PHCompositeNode;
class TecAddressObject;

/** Detailed TEC detector responce simulation.
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/slowsim.html}
*/

class mTecSlowSimModule
{
public:
///
  mTecSlowSimModule();
///
  virtual ~mTecSlowSimModule(){}
///
  PHBoolean event(PHCompositeNode*);
///
  PHBoolean event(PHCompositeNode*, TecAddressObject*);
///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_RandSeed(long rs){RandSeed=rs;}
///
  void set_GasType(int gt){GasType=gt;}
///
  void set_GasGain(float gg){GasGain=gg; 
                             for(int i=0; i<TECMAXINDEX; i++) PlaneGasGain[i]=gg;}
///
  void set_TimeSampling(float ts){TimeSampling=ts;}
///
  void set_DriftVelocity(float dv){DriftVelocity=dv;}
///
  void set_FillTecGhitRaw(int ftgr){FillTecGhitRaw=ftgr;}
///
  void set_FillTecGHitList(int i){FillTecGHitList=i;}
///
  void set_ChangePar(int chp){ChangePar=chp;}
///
  void set_Noise(int noise){Noise=noise;}
///
  void set_TrdON(int uo){TrdON=uo;}
///
  void set_PlaneGasGain(int plane, float gain){PlaneGasGain[plane]=gain;}

private:
///
  int TecTimeResponce(float *ltime, float *lampl);
///
  float PriIon( int *lgastype, int *lparticle, float *lmomentum);
///
  int SecIon(long *lseed, int *lgastype, int *lparticle, float *lmomentum);
///
  int TrdIon(int lgastype, float ee);
///
  int Verbose;
///
  long RandSeed;
///
  int GasType;
///
  float GasGain;
///
  float PlaneGasGain[TECMAXINDEX];
///
  float TimeSampling;
///
  float DriftVelocity;
///
  int FillTecGhitRaw;
///
  int FillTecGHitList;
///
  int ChangePar;
///
  int Noise;
///
  int TrdON;

};
#endif /*__MTECSLOWSIMMODULE_H__*/

