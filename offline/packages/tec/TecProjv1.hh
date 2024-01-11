#ifndef __TECPROJV1_H
#define __TECPROJV1_H

#include "TecProj.hh"
#include <iostream>

class TClonesArray;

class TecProjv1 : public TecProj
{
 public:
  TecProjv1();
  virtual ~TecProjv1();

  void Reset();
  int isValid() const;
  void identify(std::ostream &os = std::cout) const;

  unsigned int get_TecNProj() const {return TecNProj;}
  void set_TecNProj(const unsigned int nproj) {TecNProj = nproj; return;}

  int set_TClonesArraySize(const unsigned int ntrk);
  void AddTecProj(const unsigned int iring);

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

  short get_cgltrackid(const unsigned int iring) const ;
  void  set_cgltrackid(const unsigned int iring, const unsigned int ival) ;

  int get_teclusterid(const unsigned int iproj, const unsigned iplane) const ;
  void set_tecclusterid(const unsigned iproj, const unsigned int iplane, const int tecclusterid) ;

 private:
  void warning(char* field) const;

 protected:
  TClonesArray *GetTecProj() const {return TecProj;}
  unsigned int TecNProj; 
  TClonesArray *TecProj;
  ClassDef(TecProjv1,1)

};

#endif /* __TECPROJV1_H */
