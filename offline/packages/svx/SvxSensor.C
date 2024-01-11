// ===================
// FILE: SvxSensor.C
// ===================
// ****************************************************************************
// Implementation of SvxSensor.
// ---
// Created  by V. L. Rykov on 12-Feb-2003
// Modified by V. L. Rykov on 22-Apr-2004
//      Local<->Global transformation methods moved here.
// ****************************************************************************

#include "SvxSensor.h"

ClassImp(SvxSensor)

// Constructor(s)
// """"""""""""""
SvxSensor::SvxSensor(const int sc, const int lr, const int ld, const int sn)
{
  svxSection = (short) sc;
  layer      = (short) lr;
  ladder     = (short) ld;
  sensor     = (short) sn;

  for (int i=0; i<3; i++) {
    transVector[i] = 0.;
    for (int j=0; j<3; j++) {
      rotMatrix[i][j] = (i == j) ? 1. : 0. ;
    }

    m_originOffset[i] = 0.; // initial value is 0
  }
  //std::cout << "SvxSensor object created" << std::endl;

  sAQ = 0;
}

// Standard functions of all inheritors of PHObject classes...
//""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
void SvxSensor::Reset() { return; }

int SvxSensor::isValid() const {
  return ( (svxSection < 0) ||
	   (layer      < 0) ||
	   (ladder     < 0) ||
	   (sensor     < 0) ) ? 0 : 1;
}

void SvxSensor::identify(std::ostream &os) const {
  os << "Identify yourself: SvxSensor object: isValid() = "
     << isValid() << std::endl; 
}

// Check hit's sensor ID
// """""""""""""""""""""
bool SvxSensor::checkID(const SvxHit* hit)       const {
  bool flag = false;
  if (hit->get_ladder() == (int) ladder              ) {
    if (hit->get_sensor() == (int) sensor            ) {
      if (hit->get_layer() == (int) layer            ) {
	if (hit->get_svxSection() == (int) svxSection) flag = true;
      }                         // layer
    }                         // ladder
  }                         // sensor
  return flag;
}

// Global to local transformations and back
// """"""""""""""""""""""""""""""""""""""""
void SvxSensor::vector_global2local(const double vg[], double vl[]) const {
  for ( int i = 0; i < 3; i++ ) {
    vl[i]= 0.;
    for ( int j = 0; j < 3; j++ ) {
      vl[i] += rotMatrix[i][j]*vg[j];
    }
  }
}

void SvxSensor::position_global2local(const double pg[], double pl[]) const {
  double pgsh[3];
  for ( int i = 0; i < 3; i++ ) {
//    pgsh[i] = pg[i] - transVector[i];
    pgsh[i] = pg[i] - get_correctedTransVector(i);
  }
  vector_global2local(pgsh, pl);
}

void SvxSensor::matrix_global2local(const double mg[][3], double ml[][3]) const {
  for ( int i = 0; i < 3; i++ ) {
    for ( int j = 0; j < 3; j++ ) {
      ml[i][j]= 0.;
      for ( int k = 0; k < 3; k++ ) {
	double sm = 0;
	for ( int m = 0; m < 3; m++ ) {
	  sm += mg[k][m]*rotMatrix[j][m];
	}
	ml[i][j] += rotMatrix[i][k]*sm;
      }
    }
  }
}

// ---
void SvxSensor::vector_local2global(const double vl[], double vg[]) const {
  for ( int i = 0; i < 3; i++ ) {
    vg[i]= 0.;
    for ( int j = 0; j < 3; j++ ) {
      vg[i] += rotMatrix[j][i]*vl[j];
    }
  }
}

void SvxSensor::position_local2global(const double pl[], double pg[]) const {
  vector_local2global(pl, pg);
  for ( int i = 0; i < 3; i++ ) {
//    pg[i] += transVector[i];
    pg[i] += get_correctedTransVector(i);
  }
}

void SvxSensor::matrix_local2global(const double ml[][3], double mg[][3]) const {
  for ( int i = 0; i < 3; i++ ) {
    for ( int j = 0; j < 3; j++ ) {
      mg[i][j]= 0.;
      for ( int k = 0; k < 3; k++ ) {
	double sm = 0;
	for ( int m = 0; m < 3; m++ ) {
	  sm += ml[k][m]*rotMatrix[m][j];
	}
	mg[i][j] += rotMatrix[k][i]*sm;
      }
    }
  }
}

// Print sensor parameters
// """""""""""""""""""""""
void SvxSensor::printID() const
{
  std::cout << "SvxSensor object:";
  std::cout << " svxSection #" << svxSection;
  std::cout << " layer #" << layer;
  std::cout << " ladder #" << ladder;
  std::cout << " sensor #" << sensor;
  std::cout << std::endl;
}

void SvxSensor::printPositioning() const
{
  std::cout << "SvxSensor object:";
  std::cout << " Translation vector =";
  for (int i=0; i<3; i++) {
    std::cout << " " << transVector[i];
  }
  std::cout << std::endl;
  std::cout << "  Rotation matrix =";
  for (int i=0; i<3; i++) {
    for (int j=0; j<3; j++) {
      std::cout << " " << rotMatrix[i][j];
    }
    std::cout << std::endl << "                   ";
  }
  std::cout << std::endl;
  std::cout << " Origin offset =";
  for (int i=0; i<3; i++) {
    std::cout << " " << get_originOffset(i);
  }
  std::cout << std::endl;
}

void SvxSensor::printPar() const
{
  std::cout << std::endl;
  printID();
  printPositioning();
}
