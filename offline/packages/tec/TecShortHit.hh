#ifndef TECSHORTHIT_H
#define TECSHORTHIT_H 

#include "TecCalibrationObject.hh"

#include "PHObject.h"

#include <iostream>

class TecHit;

/** Represents one fired Tec time bin -- short version */
class TecShortHit : public TObject {
 
 public:
 
/// Default constructor
  TecShortHit(); 
/// Constructors
  TecShortHit(int iindex, int iadc, int itrack);
/// Constructors
  TecShortHit(int iindex, int iadc, float fcharge, float* fxyz, int itrack);
///
  TecShortHit(int iindex, int iwire, int ibin,
              int iadc, float fcharge, float* fxyz, int itrack);
///
  TecShortHit(int isector, int iside, int iplane, int iwire, int ibin,
              int iadc, float fcharge, float* fxyz, int itrack);
///
  TecShortHit(TecHit* techit);

/// Destructor
  virtual ~TecShortHit() { }

///
  void identify(std::ostream& os = std::cout) const;

///
  int get_globalindex()  {return index;}
///
  int get_index()  {return index/100000;}
///
  int get_sector();
///
  int get_side();
///
  int get_plane();
///
  int get_wire();
///
  int get_bin();
///
  short int get_adc()  {return adc;}            
///
  float get_charge()  {return 0.;}            
///
  float get_x()  {return 0.;}            
///
  float get_y()  {return 0.;}            
///
  float get_z()  {return 0.;}            
///
  short int get_trackid()  {return trackid;}            
///
  short int get_trackid(int i)  {return trackid;}            

///
  void set_globalindex(int i) {index = i;}  
///
  void set_index(int i);  
///
  void set_wire(int i);
///
  void set_bin(int i);
///
  void set_adc(short int a) {adc=a;}            
///
  void set_charge(float a) { }            
///
  void set_x(float a) { }            
///
  void set_y(float a) { }            
///
  void set_z(float a) { }            
///
  void set_trackid(short int i) {trackid=i;}            
///
  void set_trackid(int j, short int i) {trackid=i;}            

 protected:

/// index=(sector*12+plane+2+side)*100000 + wire*100 + bin
  int index;
///
  short int adc;
///
  short int trackid;

  ClassDef(TecShortHit,1)

};

#endif // TECSHORTHIT_H                                                          

