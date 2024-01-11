// ===============
// FILE: SvxApplyHotDead.h
// ===============
#ifndef __SVXAPPLYHOTDEAD_H__
#define __SVXAPPLYHOTDEAD_H__

#include <SubsysReco.h>

#include <phool.h>
#include <PHTimeServer.h>

class PHCompositeNode;
class svxAddress;
class SvxPixelHotDeadMap;
class SvxPixelHotDeadMapv2;
class SvxDeadMap; // strips
class SvxDaqErrorMap; // strips

/**
 * @brief  A SubsysReco module to apply hot and dead map for rawhit. 
 * @date  Created by Takashi Hachiya in Nov 2011
 *
 */

////////////////////////
// comment
// The hotdead map objects are own by SvxParManager.
// The pointer of the map is taken from SvxParManager.
// Using the map, this class just adds hot and dead flag to SvxRawhit.

// the status flag in the rawhit is updated using the hot dead map.
// DAQerror also updates the status flag which combine both hotdead and DAQ error.   

class SvxApplyHotDead : public SubsysReco
{
 enum SvxRawStatusAdditionalError {DAQERROR=10};

 public:

  SvxApplyHotDead(const std::string &name = "SVXAPPLYHOTDEAD");
  virtual ~SvxApplyHotDead();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void set_applytoMerged(bool a) {m_applytoMerged=a;}

  virtual void setMaskDaqErrorFlag(bool flag) { m_maskDaqErrorFlag=flag;}

  void set_SimClusterCorrection(bool a) {m_simclustercorrection=a;}
  // set the fraction of rawhits you wish to keep in each layer, I.E. if you wish to discard
  // 15% of raw hits in layer b0 use command set_SimRawHitLimits(0.85,1,1,1)
  void set_SimRawHitLimits(double a,double b, double c, double d) {b0threshold=a;
    b1threshold = b; b2threshold = c;b3threshold = d;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

  void maskDaqError(PHCompositeNode *topNode);

  
 private:
  svxAddress         *m_address;        // Hardware <-> Software channel conversion
  SvxPixelHotDeadMap *m_pixelhotdead;   // Pixel HotDead Map
  SvxPixelHotDeadMapv2 *m_pixelhotdead2;   // Pixel HotDead Map
  SvxDeadMap         *m_striphotdead;   // Strip HotDead Map
  SvxDaqErrorMap     *m_daqerr;         // DaqErrorMap

  bool m_applytoMerged;
  // daq error flag. true=DAQerror, false=noDAQerror, [2]:0=pixel, 1=strip, [60]:moduleID
  bool m_maskDaqErrorFlag;
  bool m_simclustercorrection;
  double b0threshold;
  double b1threshold;
  double b2threshold;
  double b3threshold;


  PHTimeServer::timer _timer;   ///< Timer
};
#endif 


