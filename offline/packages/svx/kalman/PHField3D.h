//
//    *************************************      
//    *                                   *    
//    *          PHField3D.hh             *        
//    *                                   *   
//    *************************************   
//
// **************************************************************
// 
// GEANT Field Map, for use in converting ROOT maps of 
// PHENIX magnetic field.  Written by Michael Stone, July 2011
// 
// **************************************************************
//
// The structure of the indices has little to do with physics and 
// has much more to do with the way the PHENIX field map is formatted
// in SimMap3D++.root i.e.  The z value is incremented only after
// every phi and r point has been accounted for in that plane.

#ifndef __PHFIELD3D_H__
#define __PHFIELD3D_H__

#ifndef __CINT__
#ifdef USEG4
#include <G4MagneticField.hh>
#include <globals.hh>
#include <G4ios.hh>
#endif // USEG4
#endif // __CINT__

//root framework
#include <TFile.h>
#include <TNtuple.h>

#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <map> 

#ifndef __CINT__
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>
#endif // __CINT__


#ifndef __CINT__

class PHField3D
#ifdef USEG4
 : public G4MagneticField
#endif
{
  typedef boost::tuple<float,float,float> trio;
  
 public:
  
  PHField3D(const char* filename, int verb=0);
  virtual ~PHField3D() {}
  
  void GetFieldValue( const double Point[4],    double *Bfield ) const;
  void GetFieldCyl  ( const double CylPoint[4], double *Bfield ) const;

  void GetFieldValueKiloGauss( const double Point[4], double *Bfield ) const
  {
    GetFieldValue( Point, Bfield );
    for(int i=0; i<6; ++i) Bfield[i] /= 1000.;
  }

  void set_scale_factor(const float &scale) {scale_factor_ = scale;}
  float get_scale_factor() const {return scale_factor_;}

  void set_verbosity(const unsigned int &i) {verb_ = i;}
  unsigned int get_verbosity() const {return verb_;}
  
 protected:

  float scale_factor_;

  // Field map is stored in cm, radians, and Gauss  
  // < i, j, k > , this allows i and i+1 to be neighbors ( <i,j,k>=<z,r,phi> )
  std::vector< std::vector< std::vector<float> > > BFieldZ_; 
  std::vector< std::vector< std::vector<float> > > BFieldR_;
  std::vector< std::vector< std::vector<float> > > BFieldPHI_;
  
  // maps indices to values z_map[i] = z_value that corresponds to ith index
  std::vector<float>   z_map_;   // < i > 
  std::vector<float>   r_map_;   // < j > 
  std::vector<float>   phi_map_; // < k > 
  
  float maxz_, minz_;    // boundaries of magnetic field map cyl
  unsigned int verb_;

 private:

  bool bin_search( const std::vector<float>& vec, unsigned start, unsigned end, const float& key, unsigned& index ) const;
  void print_map( std::map<trio,trio>::iterator& it ) const;

};

#endif // __CINT__


//---------------------------------------------------
// A wrapper so the class plays nicely with ROOT dictionary.  This can
// be used in a ROOT macro for quick checking of the map.
//
class PHField3DWrapper
{
 public:
  PHField3DWrapper(const char* filename);
  
  double GetBx(double x, double y, double z);
  double GetBy(double x, double y, double z);
  double GetBz(double x, double y, double z);
  double GetBCylZ(double z, double r, double phi);
  double GetBCylR(double z, double r, double phi); 
  double GetBCylPHI(double z, double r, double phi); 

  void set_verbosity(int i) {field.set_verbosity(i);}

#ifndef  __CINT__
 private:
  PHField3D field;
#endif // __CINT__
};

#endif // __PHFIELD3D_H
