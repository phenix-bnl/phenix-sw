// $Id: SvxSnglPisaHit.h,v 1.3 2014/01/27 19:04:23 bbannier Exp $ 
#ifndef __SVXSNGLPisaHIT_HH_
#define __SVXSNGLPisaHIT_HH_

#include <PHObject.h>
#include <phool.h>

#include <iostream>

/*!
  \file SvxSnglPisaHit.h
  \brief Forward vertex phool interface to pisa hit
  \author Sasha Lebedev (lebedev@iastate.edu)
  \version $Revision: 1.3 $
  \date $Date: 2014/01/27 19:04:23 $
*/

//! Forward vertex phool interface to pisa hit
/*! 
  This is the base class with virtual functions only.
  True functions are implemented in the versionned daughter classes
*/
class SvxSnglPisaHit : public PHObject
{

  public:

  virtual ~SvxSnglPisaHit()
  {}

  virtual int GetSvxCount() const
  {PHOOL_VIRTUAL_WARN("SvxCount"); return -9999;}
  
  virtual void SetSvxCount( const int val)
  {PHOOL_VIRTUAL_WARN("SvxCount");}

  virtual int GetMctrack() const            
  {PHOOL_VIRTUAL_WARN("mctrack"); return -9999;}
  
  virtual void SetMctrack(const int val)
  {PHOOL_VIRTUAL_WARN("mctrack");}
  
  virtual int GetIdPart() const
  {PHOOL_VIRTUAL_WARN("IdPart"); return -9999;}
  
  virtual void SetIdPart(const int val)
  {PHOOL_VIRTUAL_WARN("IdPart");}
  
  virtual float GetXGlobal() const
  {PHOOL_VIRTUAL_WARN("XGlobal"); return -9999;}
  
  virtual void SetXGlobal(const float val)
  {PHOOL_VIRTUAL_WARN("XGlobal");}
  
  virtual float GetYGlobal() const
  {PHOOL_VIRTUAL_WARN("YGlobal"); return -9999;}
  
  virtual void SetYGlobal(const float val)
  {PHOOL_VIRTUAL_WARN("YGlobal");}
  
  virtual float GetZGlobal() const
  {PHOOL_VIRTUAL_WARN("ZGlobal"); return -9999;}
  
  virtual void SetZGlobal(const float val)
  {PHOOL_VIRTUAL_WARN("ZGlobal");}
  
  virtual float GetDele() const
  {PHOOL_VIRTUAL_WARN("Dele"); return -9999;}
  
  virtual void SetDele(const float val)
  {PHOOL_VIRTUAL_WARN("Dele");}
  
  virtual float GetPmomX() const
  {PHOOL_VIRTUAL_WARN("PmomX"); return -9999;}
  
  virtual void SetPmomX(const float val)
  {PHOOL_VIRTUAL_WARN("PmomX");}
  
  virtual float GetPmomY() const
  {PHOOL_VIRTUAL_WARN("PmomY"); return -9999;}
  
  virtual void SetPmomY(const float val)
  {PHOOL_VIRTUAL_WARN("PmomY");}
  
  virtual float GetPmomZ() const
  {PHOOL_VIRTUAL_WARN("PmomZ"); return -9999;}
  
  virtual void SetPmomZ(const float val)
  {PHOOL_VIRTUAL_WARN("PmomZ");}
  
  virtual float GetTof() const
  {PHOOL_VIRTUAL_WARN("Tof"); return -9999;}
  
  virtual void SetTof(const float val)
  {PHOOL_VIRTUAL_WARN("Tof");}

  virtual float GetXLocalIn() const
  {PHOOL_VIRTUAL_WARN("XLocalIn"); return -9999;}
  
  virtual void SetXLocalIn(const float val)
  {PHOOL_VIRTUAL_WARN("XLocalIn");}
  
  virtual float GetYLocalIn() const
  {PHOOL_VIRTUAL_WARN("YLocalIn"); return -9999;}
  
  virtual void SetYLocalIn(const float val)
  {PHOOL_VIRTUAL_WARN("YLocalIn");}
  
  virtual float GetZLocalIn() const
  {PHOOL_VIRTUAL_WARN("ZLocalIn"); return -9999;}
  
  virtual void SetZLocalIn(const float val)
  {PHOOL_VIRTUAL_WARN("XLocalIn");}
  
  virtual float GetXLocalOut() const
  {PHOOL_VIRTUAL_WARN("XLocal"); return -9999;}
  
  virtual void SetXLocalOut(const float val)
  {PHOOL_VIRTUAL_WARN("XLocalOut");}
  
  virtual float GetYLocalOut() const
  {PHOOL_VIRTUAL_WARN("YLocalOut"); return -9999;}
  
  virtual void SetYLocalOut(const float val)
  {PHOOL_VIRTUAL_WARN("YLocalOut");}
  
  virtual float GetZLocalOut() const
  {PHOOL_VIRTUAL_WARN("ZLocalOut"); return -9999;}
  
  virtual void SetZLocalOut(const float val)
  {PHOOL_VIRTUAL_WARN("XLocalOut");}

  virtual float GetXGlobalIn() const 
  {PHOOL_VIRTUAL_WARN("XGlobalIn"); return -9999;}
  
  virtual void SetXGlobalIn(const float val)
  {PHOOL_VIRTUAL_WARN("XGlobalIn");}
  
  virtual float GetYGlobalIn() const 
  {PHOOL_VIRTUAL_WARN("YGlobalIn"); return -9999;}
  
  virtual void SetYGlobalIn(const float val)
  {PHOOL_VIRTUAL_WARN("YGlobalIn");}
  
  virtual float GetZGlobalIn() const
  {PHOOL_VIRTUAL_WARN("ZGlobalIn"); return -9999;}
  
  virtual void SetZGlobalIn(const float val)
  {PHOOL_VIRTUAL_WARN("ZGlobalIn");}
  
  virtual float GetXGlobalOut() const
  {PHOOL_VIRTUAL_WARN("XGlobalOut"); return -9999;}
  
  virtual void SetXGlobalOut(const float val) 
  {PHOOL_VIRTUAL_WARN("XGlobalOut");}
  
  virtual float GetYGlobalOut() const
  {PHOOL_VIRTUAL_WARN("YGlobalOut"); return -9999;}
  
  virtual void SetYGlobalOut(const float val) 
  {PHOOL_VIRTUAL_WARN("YGlobalOut");}
  
  virtual float GetZGlobalOut() const
  {PHOOL_VIRTUAL_WARN("ZGlobalOut"); return -9999;}
  
  virtual void SetZGlobalOut(const float val) 
  {PHOOL_VIRTUAL_WARN("ZGlobalOut");}
  
  
  virtual int GetHitVolume(const short int i) const
  {PHOOL_VIRTUAL_WARN("HitVolume"); return -9999;}
  
  virtual void SetHitVolume(const short int i, const int val)
  {PHOOL_VIRTUAL_WARN("HitVolume");}
  
  virtual int GetTrack() const
  {PHOOL_VIRTUAL_WARN("Track"); return -9999;}
  
  virtual void SetTrack(const int val)
  {PHOOL_VIRTUAL_WARN("Track");}
  
  virtual int GetLayer() const
  {PHOOL_VIRTUAL_WARN("Layer"); return -9999;}
  
  virtual void SetLayer(const int val)
  {PHOOL_VIRTUAL_WARN("Layer");}
  
  virtual int GetIsubevent() const
  {PHOOL_VIRTUAL_WARN("isubevent"); return -9999;}
  
  virtual void SetIsubevent(const int val)
  {PHOOL_VIRTUAL_WARN("isubevent");}
  
  virtual int GetNfile() const
  {PHOOL_VIRTUAL_WARN("nfile"); return -9999;}
  
  virtual void SetNfile(const int val)
  {PHOOL_VIRTUAL_WARN("nfile");}

  ClassDef(SvxSnglPisaHit, 1);
};
#endif
