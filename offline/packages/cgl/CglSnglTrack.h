#ifndef __CGLSNGLTRACK_H
#define __CGLSNGLTRACK_H

#include "PHObject.h"
#include "phool.h"

class CglSnglTrack : public PHObject
{
 public:
  CglSnglTrack() {}
  virtual ~CglSnglTrack() {}

  virtual short get_arm() const {return -999;}
  virtual void set_arm(const short ival) {return;}

  virtual short get_id() const {return -999;}
  virtual void set_id(const short ival) {return;}

  virtual short get_dctracksid() const {return -999;}
  virtual void set_dctracksid(const short ival) {return;}

  virtual short get_tectrackid() const {return -999;}
  virtual void set_tectrackid(const short ival) {return;}

  virtual short get_pc1clusid() const {return -999;}
  virtual void set_pc1clusid(const short ival) {return;}

  virtual short get_pc2clusid() const {return -999;}
  virtual void set_pc2clusid(const short ival) {return;}

  virtual short get_pc3clusid() const {return -999;}
  virtual void set_pc3clusid(const short ival) {return;}

  virtual short get_tofrecid() const {return -999;}
  virtual void set_tofrecid(const short ival) {return;}

  virtual short get_accrecid() const {return -999;}
  virtual void set_accrecid(const short ival) {return;}

  virtual short get_svxclusid(const short ilayer) const {return -999;}
  virtual void set_svxclusid(const short ilayer, const short ival) {return;}

  virtual short get_mrpcrecid() const {return -999;}
  virtual void set_mrpcrecid(const short ival) {return;}
  
  virtual short get_tofwrecid() const {return -999;}
  virtual void set_tofwrecid(const short ival) {return;}

  virtual short get_hbdblobid() const {return -999;}
  virtual void set_hbdblobid(const short ival) {return;}

  virtual short get_emcclusid() const {return -999;}
  virtual void set_emcclusid(const short ival) {return;}

  virtual short get_richringid() const {return -999;}
  virtual void set_richringid(const short ival) {return;}

  virtual short get_tzrrecid() const {return -999;}
  virtual void set_tzrrecid(const short ival) {return;}

  virtual short get_pcrrecid() const {return -999;}
  virtual void set_pcrrecid(const short ival) {return;}

  virtual short get_trackModel() const {return -999;}
  virtual void set_trackModel(const short ival) {return;}

  virtual float get_quality() const;
  virtual void set_quality(const float rval) {return;}

  void Reset();
  void ShutUp(const int i);

 protected:
  void Copy(const CglSnglTrack &src);
  void Copy(TObject&) const { PHOOL_VIRTUAL_WARNING; }

  ClassDef(CglSnglTrack, 1);
};

#endif /* __CGLSNGLTRACK_H */
