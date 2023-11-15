#include "PdbMpcExGeo.hh"

#include <iostream>

using namespace std;

PdbMpcExGeo::PdbMpcExGeo()
{
  Reset();
}

void PdbMpcExGeo::print() const
{

  // Prints out values in same format/order as the PISA geometry file

  for(int iArm=0 ; iArm<PdbMpcExGeo::NumberOfArms ; iArm++) {
    for(int iType=0 ; iType<PdbMpcExGeo::NumberOfTypes ; iType++) {
      for(int iLayer=0 ; iLayer<PdbMpcExGeo::NumberOfLayers ; iLayer++) {
	cout << layer_z[iArm][iType][iLayer] << endl; 
      } //iArm
    } //iType
  } //iLayer
  
  for(int iArm=0 ; iArm<PdbMpcExGeo::NumberOfArms ; iArm++) {
    for(int iX=0 ; iX<PdbMpcExGeo::NumberOfSensorsX ; iX++) {
      for(int iY=0 ; iY<PdbMpcExGeo::NumberOfSensorsY ; iY++) {
	cout << sensorcenter_x[iArm][iX][iY] << endl; 
      } //iArm
    } //iX
  } //iY

  for(int iArm=0 ; iArm<PdbMpcExGeo::NumberOfArms ; iArm++) {
    for(int iX=0 ; iX<PdbMpcExGeo::NumberOfSensorsX ; iX++) {
      for(int iY=0 ; iY<PdbMpcExGeo::NumberOfSensorsY ; iY++) {
	cout << sensorcenter_y[iArm][iX][iY] << endl;
      } //iArm
    } //iX
  } //iY

  cout << minipadwidth << endl; 
  cout << minipadlength << endl;

}

void PdbMpcExGeo::Reset()
{

  for(int iArm=0 ; iArm<PdbMpcExGeo::NumberOfArms ; iArm++) {
    for(int iType=0 ; iType<PdbMpcExGeo::NumberOfTypes ; iType++) {
      for(int iLayer=0 ; iLayer<PdbMpcExGeo::NumberOfLayers ; iLayer++) {
	layer_z[iArm][iType][iLayer] = -9999.0;
      } //iArm
    } //iType
  } //iLayer
  
  for(int iArm=0 ; iArm<PdbMpcExGeo::NumberOfArms ; iArm++) {
    for(int iX=0 ; iX<PdbMpcExGeo::NumberOfSensorsX ; iX++) {
      for(int iY=0 ; iY<PdbMpcExGeo::NumberOfSensorsY ; iY++) {
	sensorcenter_x[iArm][iX][iY] = -9999.0;
      } //iArm
    } //iX
  } //iY

  for(int iArm=0 ; iArm<PdbMpcExGeo::NumberOfArms ; iArm++) {
    for(int iX=0 ; iX<PdbMpcExGeo::NumberOfSensorsX ; iX++) {
      for(int iY=0 ; iY<PdbMpcExGeo::NumberOfSensorsY ; iY++) {
	sensorcenter_y[iArm][iX][iY] = -9999.0;
      } //iArm
    } //iX
  } //iY

  minipadwidth = -9999.0;
  minipadlength = -9999.0;

}
