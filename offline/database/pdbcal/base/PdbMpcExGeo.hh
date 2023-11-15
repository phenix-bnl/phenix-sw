#ifndef __PDBMPCEXGEO_HH__
#define __PDBMPCEXGEO_HH__

#include "PdbCalChan.hh"

class PdbMpcExGeo : public PdbCalChan {
 public:

  //! number of arms
  enum { NumberOfArms = 2 };
  
  //! number of layers
  enum { NumberOfLayers = 8 };
  
  //!number of types
  enum { NumberOfTypes = 2 };
  
  //!number of sensorsx
  enum { NumberOfSensorsX = 6 };
  
  //!number of sensorsy
  enum { NumberOfSensorsY = 6 };

  PdbMpcExGeo();
  virtual ~PdbMpcExGeo(){}

  void set_layer_z(int iArm, int iType, int iLayer, float new_z)
  {if( (iArm>=0 && iArm<NumberOfArms) && (iType>=0 && iType<NumberOfTypes) && 
       (iLayer>=0 && iLayer<NumberOfLayers) ) layer_z[iArm][iType][iLayer] = new_z;}

  void set_sensorcenter_x(int iArm, int iX, int iY, float new_x)
  {if( (iArm>=0 && iArm<NumberOfArms) && (iX>=0 && iX<NumberOfSensorsX) && 
       (iY>=0 && iY<NumberOfSensorsY) ) sensorcenter_x[iArm][iX][iY] = new_x;}

  void set_sensorcenter_y(int iArm, int iX, int iY, float new_y)
  {if( (iArm>=0 && iArm<NumberOfArms) && (iX>=0 && iX<NumberOfSensorsX) && 
       (iY>=0 && iY<NumberOfSensorsY) ) sensorcenter_y[iArm][iX][iY] = new_y;}

  void set_minipadwidth(float new_width){minipadwidth = new_width;}
  void set_minipadlength(float new_length){minipadlength = new_length;}

  float get_layer_z(int iArm, int iType, int iLayer)
  {if( (iArm>=0 && iArm<NumberOfArms) && (iType>=0 && iType<NumberOfTypes) && 
       (iLayer>=0 && iLayer<NumberOfLayers) ) return layer_z[iArm][iType][iLayer]; else return -9999.0;}

  float get_sensorcenter_x(int iArm, int iX, int iY)
  {if( (iArm>=0 && iArm<NumberOfArms) && (iX>=0 && iX<NumberOfSensorsX) && 
       (iY>=0 && iY<NumberOfSensorsY) ) return sensorcenter_x[iArm][iX][iY]; else return -9999.0;}

  float get_sensorcenter_y(int iArm, int iX, int iY)
  {if( (iArm>=0 && iArm<NumberOfArms) && (iX>=0 && iX<NumberOfSensorsX) && 
       (iY>=0 && iY<NumberOfSensorsY) ) return sensorcenter_y[iArm][iX][iY]; else return -9999.0;}
 
  float get_minipadwidth(){return minipadwidth;}
  float get_minipadlength(){return minipadlength;}

  virtual void Reset();
  virtual void print() const;
  
private:

  float layer_z[NumberOfArms][NumberOfTypes][NumberOfLayers]; 
  float sensorcenter_x[NumberOfArms][NumberOfSensorsX][NumberOfSensorsY]; 
  float sensorcenter_y[NumberOfArms][NumberOfSensorsX][NumberOfSensorsY]; 
  float minipadwidth; 
  float minipadlength; 

  ClassDef(PdbMpcExGeo,1);

};

#endif /* __PDBMPCEXGEO_HH__ */
