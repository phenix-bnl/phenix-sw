#ifndef TECLONGHIT_H
#define TECLONGHIT_H 

#include "PHObject.h"

#include "TecCalibrationObject.hh"

/** Represents one fired Tec time bin -- long version */
class TecLongHit : public TObject {
 
 public:
 
/// Default constructor
  TecLongHit(); 
/// Constructors
  TecLongHit(int iindex, int iwire, int ibin,
         int adc, float charge, float* xyz, int itrack);
///
  TecLongHit(int isector, int iside, int iplane, int iwire, int ibin,
         int adc, float charge, float* xyz, int itrack);
///
  TecLongHit(int isector, int iside, int iplane, int iwire, int ibin,
         int adc, float charge,
         TecGeometryObject* TGO, TecCalibrationObject* TCO, int itrack);

/// Destructor
  virtual ~TecLongHit() { }

///
  void identify(ostream& os = cout) const;

///
  int get_index()  {return index;}
///
  int get_sector() {return index/(TECMAXPLANE*TECMAXSIDE);}
///
  int get_side()   {return index%2;}
///
  int get_plane()  {return (index-(index/(TECMAXPLANE*TECMAXSIDE))*(TECMAXPLANE*TECMAXSIDE))/TECMAXSIDE;}
///
  int get_wire()   {return wire;}
///
  int get_bin()   {return bin;}
///
  int get_adc()  {return adc;}            
///
  float get_charge()  {return charge;}            
///
  float get_x()  {return xyz[0];}            
///
  float get_y()  {return xyz[1];}            
///
  float get_z()  {return xyz[2];}            
///
  int get_trackid()  {return trackid[0];}            
///
  int get_trackid(int i)  {return trackid[i];}            

///
  void set_index(int i)  {index=i;}
///
  void set_wire(int i)   {wire=i;}
///
  void set_bin(int i)   {bin=i;}
///
  void set_adc(int a) {adc=a;}            
///
  void set_charge(float a) {charge=a;}            
///
  void set_x(float a) {xyz[0]=a;}            
///
  void set_y(float a) {xyz[1]=a;}            
///
  void set_z(float a) {xyz[2]=a;}            
///
  void set_trackid(int i) {trackid[0]=i;}            
///
  void set_trackid(int j, int i) {trackid[j]=i;}            

 protected:

/// index=sector*12+plane+2+side
  int index;
///
  int wire;
///
  int bin;
///
  int adc;
///
  float charge;
///
  float xyz[3];
///
  int trackid[3];

  ClassDef(TecLongHit,1)

};

#endif /* TECHIT_H */                                                         

