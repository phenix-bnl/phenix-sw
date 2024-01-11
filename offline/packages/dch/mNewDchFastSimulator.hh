#ifndef __MNEWDCHFASTSIMULATOR_H__
#define __MNEWDCHFASTSIMULATOR_H__

#include "phool.h"
#include "PHCompositeNode.h"
#include "PHPointerList.h"
#include "PdbDchWire.hh"

class PHDchHistogrammer;
class PHDchGeometryObject; 
class PHDchAddressObject;
class PHDchNoiseObject;
class PHDchCalibrationObject;

class mNewDchFastSimulator
{
public:
  mNewDchFastSimulator();
  virtual ~mNewDchFastSimulator();
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
  
public:  
  const PHDchAddressObject* getDAO() const { return dchAddressObject;}
  const PHDchGeometryObject* getDGO() const { return dchGeometryObject;}
  const PHDchCalibrationObject* getDCO() const { return dchCalibrationObject;}
  const PHDchNoiseObject* getDNO() const { return dchNoiseObject;}
  const PHDchHistogrammer* getHistogrammer() const { return dchHistogrammer;}

  void setHistogrammer(PHDchHistogrammer* histo) {dchHistogrammer = histo;}
 
private:
  PHDchGeometryObject*    dchGeometryObject;
  PHDchAddressObject*     dchAddressObject;
  PHDchCalibrationObject* dchCalibrationObject;
  PHDchNoiseObject*       dchNoiseObject;
  PHDchHistogrammer*      dchHistogrammer;

public:

  int initialize(PHCompositeNode* root);
  float getZmax( int arm, int plane) {return Zmax[arm][plane];}
  float getDeltaX( int arm, int plane) {return DX[arm][plane];}
  float getDeltaX2( int arm, int plane) {return DX2[arm][plane];}
  float getDeltaPhi( int arm, int plane) {return Phi[arm][plane];}
  float getnorth_x( int arm, int plane) {return north_x[arm][plane];}
  float getnorth_y( int arm, int plane) {return north_y[arm][plane];}
  float getnorth_z( int arm, int plane) {return north_z[arm][plane];}
  float getsign( int arm, int plane) {return sign[arm][plane];}
  int   getSide( int arm, int plane) {return my_sidexx[arm][plane];}
  void  SetPropRegion(float dummy ) { PropRegion = dummy;}
  void  SetBackdriftCutoff(float dummy) { BackdriftCutoff = dummy;}
  void  SetWidthOfDriftChannel(float dummy) { WidthofDriftChannel = dummy ;}
  float getPropRegion() {return PropRegion;}
  float getBackdriftCutoff() {return BackdriftCutoff;}
  float getWidthOfDriftChannel() {return WidthofDriftChannel;}
  void  SetMirrorHits( int dummy ) {mirrorHits = dummy;}
  int   getMirrorHits() { return mirrorHits;}
  void  setSigmaProp( float val ) { sigmaprop = val; }
  void  setSigmaDrift( float val ) { sigmadrift = val; }

  int print();  
  int print( int armxx, int planemin, int planemax );
  int init;

private:
  
  int numberOfPlanesxx;
  float Zmax[2][40];
  float DX[2][40];
  float DX2[2][40];
  float Phi[2][40];
  float north_x[2][40];
  float north_y[2][40];
  float north_z[2][40];
  float sign[2][40];
  int my_sidexx[2][40];
  float length[2][40];
  float PropRegion;
  float BackdriftCutoff;
  float WidthofDriftChannel;
  int mirrorHits;
  float sigmaprop;
  float sigmadrift;

};

#endif /*__MNEWDCHFASTSIMULATOR_H__*/
