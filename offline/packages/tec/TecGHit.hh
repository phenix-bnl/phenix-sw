#ifndef TECGHIT_H
#define TECGHIT_H 

#include "PHObject.h"

#include "TecCalibrationObject.hh"
#include <iostream>

/** Relates TecHit with Geant information */
class TecGHit : public TObject {
 
 public:
 
/// Default constructor
  TecGHit(); 
/// Constructors
  TecGHit(const TecGHit* tecghit);
  TecGHit(int ihitid, int itecghitid, int ifkinid);
  TecGHit(int ihitid, int itecghitid, int ifkinid, float ffraction);

/// Destructor
  virtual ~TecGHit() { }

///
  void identify(std::ostream& os = std::cout) const;

///
  int get_hitid()  {return hitid;}
///
  int get_tecghitid() {return tecghitid[0];}
///
  int get_fkinid()   {return fkinid[0];}
///
  int get_tecghitid(int i) {if(i>-1 && i<3) return tecghitid[i];
                              else return -1;}
///
  int get_fkinid(int i)   {if(i>-1 && i<3) return fkinid[i];
			     else return -1;}
///
  float get_fraction()   {return fraction[0];}
///
  float get_fraction(int i)   {if(i>-1 && i<3) return fraction[i];
			       else return 0.;}

///
  void set_hitid(int i)  {hitid=i;}
///
  void set_tecghitid(int i)   {tecghitid[0]=i;}
///
  void set_fkinid(int i)   {fkinid[0]=i;}
///
  void set_tecghitid(int i, int ih)   {if(i>-1 && i<3) tecghitid[i]=ih;}
///
  void set_fkinid(int i, int ih)   {if(i>-1 && i<3) fkinid[i]=ih;}
///
  void set_fraction(float a)   {fraction[0]=a;}
///
  void set_fraction(int i, float a)   {if(i>-1 && i<3) fraction[i]=a;}


 protected:

///
  int hitid;
///
  int tecghitid[3];
///
  int fkinid[3];
///
  float fraction[3];

  ClassDef(TecGHit,1)

};

#endif /* TECGHIT_H */                                                         

