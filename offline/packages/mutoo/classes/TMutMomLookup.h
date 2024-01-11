//////////////////////////////////////////////////////////////////
//
// Utility class: TMutMomLookup.h
// Author: S.Kelly 
// Date: 4/16/02
// Description: Cell based linear interpolation of PHENIX B Field 
//                            
//////////////////////////////////////////////////////////////////

#ifndef __PHBFIELDMAP_H__
#define __PHBFIELDMAP_H__

#include<TDataType.h>
#include<MUTOO.h>
#include<PHTimer.h>
#include<PHException.h>
#include<TMutTrkMap.h>

#include<vector>
#include<iomanip>
#include<set>
#include<climits>

#include<boost/smart_ptr.hpp>
#include<boost/array.hpp>

/*! \ingroup classes */

//! Momentum lookup

/*!
*/

class TMutMomLookup
{
 public: 

  /*! Default constructor */
  TMutMomLookup(const std::string& south_filename="lu_data_south.root",
		const std::string& north_filename="lu_data_north.root") : 
    _south_filename(south_filename),
    _north_filename(north_filename),
    _timer("TMutMomLookup Initialization"),
    _initialized(false)
    {
      initialize();
    }
    
  double get_us_momentum(unsigned short arm, const double& theta, const double& phi12, const double& phi23) const;

  void print(std::ostream& os=std::cout);

 private:
  
  // Read ROOT file into lookup
  //
  void initialize();

  size_t hash_function_south(const double& theta, const double& phi12, const double& phi23,
			     int theta_offset=0, int phi12_offset=0, int phi23_offset=0) const;
  
  
  size_t hash_function_north(const double& theta, const double& phi12, const double& phi23,
			     int theta_offset=0, int phi12_offset=0, int phi23_offset=0) const;
  
  // Update the bounds of the lookup table
  //
  void update_bounds_south(const double& theta, const double& phi12, const double& phi23);
  void update_bounds_north(const double& theta, const double& phi12, const double& phi23);  

  // Initialize lookup table
  //
  void initialize_lookup_south();
  void initialize_lookup_north();

  // Add points to lookup
  //
  void add_point_south(const double& theta, const double& phi12, const double& phi23, 
		       const double& ptotus, const double& ptotvx);
  
  void add_point_north(const double& theta, const double& phi12, const double& phi23, 
		       const double& ptotus, const double& ptotvx);
  
  
  // Data struct stored in lookup
  //
  struct Node 
  {    
    float theta;
    float phi12;
    float phi23;
    float ptotus;
    float ptotvx;
    float count;
  };
  
  // Declare non-int static data members
  //
  static const double CELL_WIDTH_THETA;
  static const double CELL_WIDTH_PHI12;
  static const double CELL_WIDTH_PHI23;

  static long MIN_THETA_SOUTH;
  static long MIN_PHI12_SOUTH;
  static long MIN_PHI23_SOUTH;
  static long MAX_THETA_SOUTH;
  static long MAX_PHI12_SOUTH;
  static long MAX_PHI23_SOUTH;
  static long MIN_THETA_NORTH;
  static long MIN_PHI12_NORTH;
  static long MIN_PHI23_NORTH;
  static long MAX_THETA_NORTH;
  static long MAX_PHI12_NORTH;
  static long MAX_PHI23_NORTH;

  size_t _size_theta_south;
  size_t _size_phi12_south;
  size_t _size_phi23_south;
  size_t _size_theta_north;
  size_t _size_phi12_north;
  size_t _size_phi23_north;

  std::vector<Node>* _south;
  std::vector<Node>* _north;
    
  std::string _south_filename;
  std::string _north_filename;

  PHTimer _timer;  
  
  // State booleans
  //
  bool _initialized;
};

// Singleton
//
class TMutMomLU
{
 public:

  static double get_us_momentum(unsigned short arm, double theta, double phi12, double phi23) {
    // Instantiate upon first use
    //
    static TMutMomLookup *lu = new TMutMomLookup();
    return lu->get_us_momentum(arm,theta,phi12,phi23);
  }

  static double get_ephi12(TMutTrkMap::const_pointer);
  static double get_ephi23(TMutTrkMap::const_pointer);  
};


#endif



