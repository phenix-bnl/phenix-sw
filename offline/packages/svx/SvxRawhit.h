// =================
// FILE: SvxRawhit.h
// =================

#ifndef __SVXRAWHIT_HH_
#define __SVXRAWHIT_HH_

#include <SvxHit.h>
#include <SvxRawhitList.h>

/**
 * @brief  Temporary(?) implementation of Silicon raw hit
 *
 * This will be superceded by additions to offline/packages/gea.
 * @date  Created by V. L. Rykov on 08-Feb-2004
 */
class SvxRawhit : public SvxHit
{

 public:
  SvxRawhit(SvxRawhit* rawhit = NULL) : SvxHit(rawhit)
    { /*std::cout << "SvxRawhit object created" << std::endl;*/ }
  virtual ~SvxRawhit()
    { /*std::cout << "SvxRawhit object destroyed" << std::endl;*/ }

  // Standard functions of all inheritors of PHObject classes...
  // """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  virtual void identify(std::ostream &os=std::cout) const {
    os << "Identify yourself: virtual SvxRawhit object" << std::endl;
  }

  // Set the values in the SvxRawhit...
  // """"""""""""""""""""""""""""""""""
  virtual void set_sensorSection     (const int val ) {PHOOL_VIRTUAL_WARN("sensorSection"        );}
  virtual void set_sensorReadout     (const int val ) {PHOOL_VIRTUAL_WARN("sensorReadout"        );}
  virtual void set_sensorType        (const int val ) {PHOOL_VIRTUAL_WARN("sensorType"           );}
  virtual void set_adc               (const int val ) {PHOOL_VIRTUAL_WARN("adc"                  );}
  virtual void set_channel           (const int val ) {PHOOL_VIRTUAL_WARN("channel"              );}
  virtual void set_pixelModule       (const int val ) {PHOOL_VIRTUAL_WARN("pixelModule"          );}
  virtual void set_pixelROC          (const int val ) {PHOOL_VIRTUAL_WARN("pixelROC"             );}
  virtual void set_HotDeadFlag       (const int val ) {PHOOL_VIRTUAL_WARN("HotDeadFlag"          );}
  virtual void set_isOkForClustering (const bool val) {PHOOL_VIRTUAL_WARN("set_isOkForClustering");}

  // Get the values from the SvxRawhit...
  // """"""""""""""""""""""""""""""""""""
  virtual int get_sensorSection() const
    { PHOOL_VIRTUAL_WARN("sensorSection"); return -9999; }
  virtual int get_sensorReadout() const
    { PHOOL_VIRTUAL_WARN("sensorReadout"); return -9999; }
  virtual int get_sensorType   () const
    { PHOOL_VIRTUAL_WARN("sensorType"   ); return -9999; }
  virtual int get_adc     () const { PHOOL_VIRTUAL_WARN("adc"     ); return -9999 ;}
  virtual int get_channel () const { PHOOL_VIRTUAL_WARN("channel" ); return -9999 ;}
  virtual int get_pixelModule () const { PHOOL_VIRTUAL_WARN("pixelModule" ); return -9999 ;}
  virtual int get_pixelROC () const { PHOOL_VIRTUAL_WARN("pixelROC" ); return -9999 ;}
  virtual int get_HotDeadFlag () const { PHOOL_VIRTUAL_WARN("HotDeadFlag"); return -9999 ; }
  virtual bool get_isOkForClustering () const {PHOOL_VIRTUAL_WARN("get_isOkForClustering"); return false;}

  // Methods
  // """""""
  virtual SvxHit* Clone()           { PHOOL_VIRTUAL_WARN("Clone()"); return 0; }
  virtual void    Copy(SvxHit* hit) { PHOOL_VIRTUAL_WARN("Copy()"); }

  ClassDef(SvxRawhit, 1);
};
#endif
