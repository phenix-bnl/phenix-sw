// $Id: PHBFieldMap.cc,v 1.8 2009/05/17 11:04:33 hpereira Exp $

/*!
   \file PHBFieldMap.cc
   \brief Cell based linear interpolation of PHENIX B Field 
   \author Sean Kelly, Hugo Pereira Da Costa
   \version $Revision: 1.8 $
   \date $Date: 2009/05/17 11:04:33 $
*/


#include <iostream> 
#include <cmath>
#include <PHBFieldMap.h>
#include <TFile.h>
#include <TTree.h>

#include <string>
#include <climits>
#include <stdexcept>

#include <gsl/gsl_math.h>

using namespace std;

// Definition of non const static data members
const float PHBFieldMap::CELL_WIDTH_R = 4.0;    // cm
const float PHBFieldMap::CELL_WIDTH_PHI = 3.0;  // degrees
const float PHBFieldMap::CELL_WIDTH_Z = 4.0;    // cm
const float PHBFieldMap::RAD_TO_DEG = 180.0/M_PI;
const float PHBFieldMap::DEG_TO_RAD = M_PI/180.0;

long PHBFieldMap::MIN_R = LONG_MAX;
long PHBFieldMap::MIN_PHI = LONG_MAX;
long PHBFieldMap::MIN_Z = LONG_MAX;
long PHBFieldMap::MAX_R = LONG_MIN;
long PHBFieldMap::MAX_PHI = LONG_MIN;
long PHBFieldMap::MAX_Z = LONG_MIN;

//____________________________________________________________________
void PHBFieldMap::initialize( const string& filename )
{
  
  cout << "PHBFieldMap::initialize - loading file " << filename << endl;

  TFile f( filename.c_str() );
  if(!f.IsOpen()){
    throw runtime_error( "PHBFieldMap::initialize - can't find input file " );
  }
  
  TTree* tree = (TTree*)gDirectory->Get("map");
  if(!tree){
    throw runtime_error("PHBFieldMap::initialize - can't find input file tree: map");
  }

  // Setup locals for tree access
  //
  Float_t z=0;
  Float_t r=0;
  Float_t phi=0;
  Float_t bz=0;
  Float_t br=0;
  Float_t bphi=0;
  
  tree->SetBranchAddress("z",&z);
  tree->SetBranchAddress("r",&r);
  tree->SetBranchAddress("phi",&phi);
  tree->SetBranchAddress("bz",&bz);
  tree->SetBranchAddress("br",&br);
  tree->SetBranchAddress("bphi",&bphi);
  
  // Determine the bounds of the lookup
  //
  Stat_t nentries = tree->GetEntries();
  for (int jentry=0; jentry<nentries;jentry++) {
    tree->GetEntry(jentry);   
    update_bounds(r,phi,z);
  }

  // Allocate the resources
  initialize_lookup( nentries );
  
  cout << "PHBFieldMap::initialize - r window  : [" << MIN_R << "," << MAX_R << "]" << " _size_r  : " << _size_r << endl;
  cout << "PHBFieldMap::initialize - phi window: [" << MIN_PHI << "," << MAX_PHI << "]" << " _size_phi: " << _size_phi << endl;
  cout << "PHBFieldMap::initialize - z window  : [" << MIN_Z << "," << MAX_Z << "]" << " _size_z  : " << _size_z << endl;
  
  // Set the root field point
  for (int jentry=0; jentry<nentries;jentry++) {
    tree->GetEntry(jentry);   
    add_point(r,phi,z,br,bphi,bz);    
  }
  _initialized = true;

  // clear tree and close TFile
  delete tree;
  f.Close();
  cout << "PHBFieldMap::initialize - entries read: " << nentries << endl;
    
}

//____________________________________________________________________
const PHBFieldMap::BField& PHBFieldMap::get_field( float x, float y, float z) 
{
  
  // check (once) if map is initialized. Throw exception if not.
  static bool first __attribute__ ((unused)) = check_initialized();
  
  // convert to cynlidrical coordinates
  float r = sqrt( square(x) + square(y) );  
  float phi = atan2(y,x);
  phi = phi < 0 ? RAD_TO_DEG*(phi+2*M_PI) : RAD_TO_DEG*phi; 

  // retrieve index
  _index = get_index( r, phi, z );

  if( !_index.valid ) return null_field();
  
  // load field for all 8 points of the cell
  size_t key = hash_function( _index );
  const FieldPoint& r_lo_phi_lo_z_lo = _field_values[_lookup[key]];
  if( !r_lo_phi_lo_z_lo.valid ) return null_field();

  key = hash_function( _index, 0, 0, 1);
  const FieldPoint& r_lo_phi_lo_z_hi = _field_values[_lookup[key]];
  if( !r_lo_phi_lo_z_hi.valid ) return null_field();
  
  key = hash_function( _index, 0, 1, 0);
  const FieldPoint& r_lo_phi_hi_z_lo = _field_values[_lookup[key]];
  if( !r_lo_phi_hi_z_lo.valid ) return null_field();
  
  key = hash_function( _index, 0, 0, 1);
  const FieldPoint& r_lo_phi_hi_z_hi = _field_values[_lookup[key]];
  if( !r_lo_phi_hi_z_hi.valid ) return null_field();
  
  key = hash_function( _index, 1, 0, 0);
  const FieldPoint& r_hi_phi_lo_z_lo = _field_values[_lookup[key]];
  if( !r_hi_phi_lo_z_lo.valid ) return null_field();
  
  key = hash_function( _index, 1, 0, 1);
  const FieldPoint& r_hi_phi_lo_z_hi = _field_values[_lookup[key]];
  if( !r_hi_phi_lo_z_hi.valid ) return null_field();
  
  key = hash_function( _index, 1, 1, 0);
  const FieldPoint& r_hi_phi_hi_z_lo = _field_values[_lookup[key]];
  if( !r_hi_phi_hi_z_lo.valid ) return null_field();
  
  key = hash_function( _index, 1, 1, 1);
  const FieldPoint& r_hi_phi_hi_z_hi = _field_values[_lookup[key]];
  if( !r_hi_phi_hi_z_hi.valid ) return null_field();
    
  float t = (r - r_lo_phi_lo_z_lo.r)/CELL_WIDTH_R;
  float u = (phi - r_lo_phi_lo_z_lo.phi)/CELL_WIDTH_PHI;
  float v = (z - r_lo_phi_lo_z_lo.z)/CELL_WIDTH_Z;
  
  // bx interpolation
  if( _mode == BILINEAR )
  {
    
    // Bilinear interpolation in z phi plane at r_lo
    float z1 = (1.0-v) * r_lo_phi_lo_z_lo.bx + v * r_lo_phi_lo_z_hi.bx;
    float z2 = (1.0-v) * r_lo_phi_hi_z_lo.bx + v * r_lo_phi_hi_z_hi.bx;              
    float z_phi_1 = (1.0-u) * z1 + u * z2;
    
    // Bilinear interpolation in z phi plane at r_hi
    float z3 = (1.0-v) * r_hi_phi_lo_z_lo.bx + v * r_hi_phi_lo_z_hi.bx;
    float z4 = (1.0-v) * r_hi_phi_hi_z_lo.bx + v * r_hi_phi_hi_z_hi.bx;              
    float z_phi_2 = (1.0-u) * z3 + u * z4;
    
    // Now interpolate between the two r planes 
    _field.bx = (1.0-t) * z_phi_1 + t * z_phi_2;
    
  } else {
    
    // linear extrapolation
    float dbxdr = (r_hi_phi_lo_z_lo.bx - r_lo_phi_lo_z_lo.bx);
    float dbxdphi = (r_lo_phi_hi_z_lo.bx - r_lo_phi_lo_z_lo.bx);
    float dbxdz = (r_lo_phi_lo_z_hi.bx - r_lo_phi_lo_z_lo.bx);
    
    _field.bx = r_lo_phi_lo_z_lo.bx + dbxdr*t + dbxdphi*u + dbxdz*v;
    
  }
  
  // by interpolation
  if( _mode == BILINEAR )
  {
    
    // Bilinear interpolation in z phi plane at r_lo
    //
    float z1 = (1.0-v) * r_lo_phi_lo_z_lo.by + v * r_lo_phi_lo_z_hi.by;
    float z2 = (1.0-v) * r_lo_phi_hi_z_lo.by + v * r_lo_phi_hi_z_hi.by;              
    float z_phi_1 = (1.0-u) * z1 + u * z2;
    
    // Bilinear interpolation in z phi plane at r_hi
    //
    float z3 = (1.0-v) * r_hi_phi_lo_z_lo.by + v * r_hi_phi_lo_z_hi.by;
    float z4 = (1.0-v) * r_hi_phi_hi_z_lo.by + v * r_hi_phi_hi_z_hi.by;              
    float z_phi_2 = (1.0-u) * z3 + u * z4;
    
    // Now interpolate between the two r planes 
    _field.by = (1.0-t) * z_phi_1 + t * z_phi_2;
  
  } else {
    
    // linear extrapolation
    float dbydr = (r_hi_phi_lo_z_lo.by - r_lo_phi_lo_z_lo.by);
    float dbydphi = (r_lo_phi_hi_z_lo.by - r_lo_phi_lo_z_lo.by);
    float dbydz = (r_lo_phi_lo_z_hi.by - r_lo_phi_lo_z_lo.by);
    
    _field.by = r_lo_phi_lo_z_lo.by + dbydr*t + dbydphi*u + dbydz*v;
    
  }
  
  // bz interpolation
  if( _mode == BILINEAR )
  {
  
    // Bilinear interpolation in z phi plane at r_lo
    float z1 = (1.0-v) * r_lo_phi_lo_z_lo.bz + v * r_lo_phi_lo_z_hi.bz;
    float z2 = (1.0-v) * r_lo_phi_hi_z_lo.bz + v * r_lo_phi_hi_z_hi.bz;              
    float z_phi_1 = (1.0-u) * z1 + u * z2;
    
    // Bilinear interpolation in z phi plane at r_hi
    float z3 = (1.0-v) * r_hi_phi_lo_z_lo.bz + v * r_hi_phi_lo_z_hi.bz;
    float z4 = (1.0-v) * r_hi_phi_hi_z_lo.bz + v * r_hi_phi_hi_z_hi.bz;              
    float z_phi_2 = (1.0-u) * z3 + u * z4;
    
    // Now interpolate between the two r planes 
    _field.bz = (1.0-t) * z_phi_1 + t * z_phi_2;
  
  } else {
    
    // linear extrapolation
    float dbzdr = (r_hi_phi_lo_z_lo.bz - r_lo_phi_lo_z_lo.bz);
    float dbzdphi = (r_lo_phi_hi_z_lo.bz - r_lo_phi_lo_z_lo.bz);
    float dbzdz = (r_lo_phi_lo_z_hi.bz - r_lo_phi_lo_z_lo.bz);
    
    _field.bz = r_lo_phi_lo_z_lo.bz + dbzdr*t + dbzdphi*u + dbzdz*v;
    
  }
  
  return _field;
  
}

//____________________________________________________________________
void PHBFieldMap::add_point(
  float r, float phi, float z,
  float br, float bphi, float bz)
{

  // Convert to cartesian
  float sin_phi = sin(DEG_TO_RAD*phi);
  float cos_phi = cos(DEG_TO_RAD*phi);
  
  float bx = br*cos_phi - bphi*sin_phi;
  float by = br*sin_phi + bphi*cos_phi;

  // create point and insert in lookup
  FieldPoint point( r,phi,z,bx,by,bz, true );  
  _field_values.push_back( point );
  _lookup[hash_function(r,phi,z)] = _field_values.size()-1;  
  
}

//____________________________________________________________________
void PHBFieldMap::update_bounds(float r, float phi, float z)
{
  long i_r = static_cast<int>(floor(r/CELL_WIDTH_R));
  long i_phi = static_cast<int>(floor(phi/CELL_WIDTH_PHI));
  long i_z = static_cast<int>(floor(z/CELL_WIDTH_Z));

  MIN_R = min(i_r,MIN_R);
  MIN_PHI = min(i_phi,MIN_PHI);
  MIN_Z = min(i_z,MIN_Z);
  MAX_R = max(i_r,MAX_R);
  MAX_PHI = max(i_phi,MAX_PHI);
  MAX_Z = max(i_z,MAX_Z);

}

//____________________________________________________________________
void PHBFieldMap::initialize_lookup( Stat_t size )
{
  
  // Offset 2 is (1 for distance calc + 1 for empty border)
  _size_r = (MAX_R-MIN_R + 2); 
  _size_z = (MAX_Z-MIN_Z + 2); 
  
  // phi, since it is an angle, is circular. Thus there should be no empty border
  _size_phi = (MAX_PHI-MIN_PHI + 1); 
  
  // allocate lookup
  _lookup = vector<unsigned int>(_size_r*_size_phi*_size_z, 0);  
  
  // clear values
  _field_values.clear();
  _field_values.reserve( (size_t)size+1 );
  
  // insert null field as the first point in the field_values, so that it gets returned
  // for all poisitions that corresponds to _lookup = 0
  _field_values.push_back( FieldPoint() );
  
}

//____________________________________________________________________
bool PHBFieldMap::check_initialized( void )
{
  
  // dump interpolation mode
  cout << "PHBFieldMap::check_initialized - interpolation mode is " << ( _mode == LINEAR ? "linear":"bilinear" ) << endl;
  
  // throw exception if initialization not done.
  if( !_initialized ) 
  { throw runtime_error("PHBFieldMap::check_initialized - lookup not initialized."); }
  
  return _initialized; 
}

//____________________________________________________________________
inline PHBFieldMap::Index PHBFieldMap::get_index(float r, float phi, float z) const
{
  
  // hash cell r 
  /* 
  note: one needs to check both i_r and i_r + 1, 
  because i_r if negative is translated to a very big number when turned to an unsigned, 
  where as i_r+1 might actually be 0 and pass the cut
  */
  size_t i_r = static_cast<int>(floor(r/CELL_WIDTH_R)) - MIN_R;
  if( i_r >= _size_r || i_r + 1 >= _size_r ) return Index( 0, 0, 0, false );
  
  // hash cell z
  /* same remark for boundaries */
  size_t i_z = static_cast<int>(floor(z/CELL_WIDTH_Z)) - MIN_Z;
  if( i_z > _size_z || i_z + 1 > _size_z ) return Index( 0, 0, 0, false );

  // hash cell phi
  /* no boundary check because phi being an angle, it is circular */
  size_t i_phi = static_cast<int>(floor(phi/CELL_WIDTH_PHI)) - MIN_PHI;
  
  // return index
  return Index( i_r, i_phi, i_z, true );
    
}
   
//____________________________________________________________________
inline size_t PHBFieldMap::hash_function(const PHBFieldMap::Index& index , size_t r_offset, size_t phi_offset, size_t z_offset) const
{ 
  
  // phi being an angle i_phi must be circular
  size_t i_phi( index.i_phi + phi_offset );
  while( i_phi >= _size_phi ) i_phi -= _size_phi ;
  
  // compute index
  return (index.i_r + r_offset ) + _size_r*( i_phi + _size_phi*( index.i_z + z_offset ) ); 
  
}
