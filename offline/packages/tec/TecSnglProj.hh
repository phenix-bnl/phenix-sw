#ifndef TECSNGLPROJ_H
#define TECSNGLPROJ_H 

#include <PHObject.h>
#include <PHCompositeNode.h>
#include <cmath>


class TecSnglProj : public PHObject {
 
 public:
 
/// Destructor
  virtual ~TecSnglProj() { }

  virtual int get_cgltrackid() const {return -9999;}
  virtual void set_cgltrackid() const {return;};

  virtual float get_pstartx() const {return -9999.;}
  virtual float get_pstarty() const {return -9999.;}
  virtual float get_pstartz() const {return -9999.;}

  virtual void set_pstartx(const float a) {return;}
  virtual void set_pstarty(const float a) {return;}
  virtual void set_pstartz(const float a) {return;}

  virtual float get_pendx() const {return -9999.;}
  virtual float get_pendy() const {return -9999.;}
  virtual float get_pendz() const {return -9999.;}

  virtual void set_pendx(const float a) {return;}
  virtual void set_pendy(const float a) {return;}
  virtual void set_pendz(const float a) {return;}

  virtual int get_tecclusterid(const unsigned iplane) {return -9999;}
  virtual void set_tecclusterid(const unsigned iplane, const int tecclusterid) {return;}

///
  virtual void identify(std::ostream& os = std::cout) const;
  TecSnglProj(const TecSnglProj &source) {}

 protected:
/// Default constructor
  TecSnglProj() {}

  ClassDef(TecSnglProj,1)

};

#endif // TECSNGLPROJ_H
