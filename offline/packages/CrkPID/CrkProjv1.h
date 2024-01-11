#ifndef __CRKPROJV1_H
#define __CRKPROJV1_H

#include <CrkProj.h>
#include <iostream>

class TClonesArray;

class CrkProjv1 : public CrkProj
{
 public:
  CrkProjv1();
  virtual ~CrkProjv1();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

  unsigned int get_CrkNProj() const {return CrkNRing;}
  void set_CrkNProj(const unsigned int nring) {CrkNRing = nring; return;}

  int set_TClonesArraySize(const unsigned int ntrk);
  void AddCrkProj(const unsigned int iring);

  float get_pstartx(const unsigned int iring) const ;
  void set_pstartx(const unsigned int iring, const float ival) ;

  float get_pstarty(const unsigned int iring) const ;
  void set_pstarty(const unsigned int iring, const float ival) ;

  float get_pstartz(const unsigned int iring) const ;
  void set_pstartz(const unsigned int iring, const float ival) ;

  float get_pendx(const unsigned int iring) const ;
  void set_pendx(const unsigned int iring, const float ival) ;

  float get_pendy(const unsigned int iring) const ;
  void set_pendy(const unsigned int iring, const float ival) ;

  float get_pendz(const unsigned int iring) const ;
  void set_pendz(const unsigned int iring, const float ival) ;

  short get_ringid(const unsigned int iring) const ;
  void  set_ringid(const unsigned int iring, const unsigned int ival) ;

  short get_cgltrackid(const unsigned int iring) const ;
  void  set_cgltrackid(const unsigned int iring, const unsigned int ival) ;

 protected:
  TClonesArray *GetCrkProj() const {return CrkProj;}
  unsigned int CrkNRing; 
  TClonesArray *CrkProj;
  ClassDef(CrkProjv1,1)

};

#endif /* __CRKPROJV1_H */
