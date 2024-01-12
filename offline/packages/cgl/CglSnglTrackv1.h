#ifndef __CGLSNGLTRACKV1_H
#define __CGLSNGLTRACKV1_H

#include "PHObject.h"

class CglSnglTrackv1 : public TObject
{
 public:
  CglSnglTrackv1();
  virtual ~CglSnglTrackv1() {}

  short get_arm() const {return arm;}
  void set_arm(const short ival) {arm = ival; return;}

  short get_id() const {return id;}
  void set_id(const short ival) {id = ival; return;}

  short get_dctracksid() const {return dctracksid;}
  void set_dctracksid(const short ival) {dctracksid = ival; return;}

  short get_tectrackid() const {return tectrackid;}
  void set_tectrackid(const short ival) {tectrackid = ival; return;}

  short get_pc1clusid() const {return pc1clusid;}
  void set_pc1clusid(const short ival) {pc1clusid = ival; return;}

  short get_pc2clusid() const {return pc2clusid;}
  void set_pc2clusid(const short ival) {pc2clusid = ival; return;}

  short get_pc3clusid() const {return pc3clusid;}
  void set_pc3clusid(const short ival) {pc3clusid = ival; return;}

  short get_tofrecid() const {return tofrecid;}
  void set_tofrecid(const short ival) {tofrecid = ival; return;}

  short get_emcclusid() const {return emcclusid;}
  void set_emcclusid(const short ival) {emcclusid = ival; return;}

  short get_richringid() const {return richringid;}
  void set_richringid(const short ival) {richringid = ival; return;}

  short get_trackModel() const {return trackModel;}
  void set_trackModel(const short ival) {trackModel = ival; return;}

  float get_quality() const {return quality;}
  void set_quality(const float rval) {quality = rval; return;}


 protected:
  short arm;
  short id;
  short dctracksid;
  short emcclusid;
  short pc1clusid;
  short pc2clusid;
  short pc3clusid;
  short richringid;
  short tectrackid;
  short tofrecid;
  short trackModel;
  float quality;

  ClassDef(CglSnglTrackv1,1)
};

#endif /* __CGLSNGLTRACKV1_H */