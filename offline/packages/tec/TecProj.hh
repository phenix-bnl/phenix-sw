#ifndef __TECPROJ_H
#define __TECPROJ_H

#include <PHObject.h>
#include <iostream>

class TecProj : public PHObject
{
 public:
  virtual ~TecProj() {}

  virtual void Reset();
  virtual int isValid() const;
  virtual void identify(std::ostream &os = std::cout) const;

  virtual unsigned int get_TecNProj() const {return 0;}
  virtual void set_TecNProj(const unsigned int nring) {return;}

  virtual int set_TClonesArraySize(const unsigned int ntrk) {return 0;}
  virtual void AddTecProj(const unsigned int iproj) {return;}

  virtual float get_pstartx(const unsigned int iproj) const {warning("pstartx  "); return -999;}
  virtual void set_pstartx(const unsigned int iproj, const float ival) {warning("pstartx  "); return;}

  virtual float get_pstarty(const unsigned int iproj) const {warning("pstarty  "); return -999;}
  virtual void set_pstarty(const unsigned int iproj, const float ival) {warning("pstarty  "); return;}

  virtual float get_pstartz(const unsigned int iproj) const {warning("pstartz  "); return -999;}
  virtual void set_pstartz(const unsigned int iproj, const float ival) {warning("pstartz  "); return;}

  virtual float get_pendx(const unsigned int iproj) const {warning("pendx  "); return -999;}
  virtual void set_pendx(const unsigned int iproj, const float ival) {warning("pendx  "); return;}

  virtual float get_pendy(const unsigned int iproj) const {warning("pendy  "); return -999;}
  virtual void set_pendy(const unsigned int iproj, const float ival) {warning("pendy  "); return;}

  virtual float get_pendz(const unsigned int iproj) const {warning("pendz  "); return -999;}
  virtual void set_pendz(const unsigned int iproj, const float ival) {warning("pendz  "); return;}

  virtual short get_cgltrackid(const unsigned int iproj) const {warning("track  "); return -999;} 
  virtual void  set_cgltrackid(const unsigned int iproj, const unsigned int ival) {warning("trackid  "); return;} 

  virtual int get_teclusterid(const unsigned int iproj, const unsigned iplane) const {warning("teclusterid  "); return -999;}
  virtual void set_tecclusterid(const unsigned iproj, const unsigned int iplane, const int tecclusterid) {warning("teclusterid  "); return;}

 private:
  void warning(const char* field) const;

  ClassDef(TecProj,1)

};

#endif /* __TECPROJ_H */
