#ifndef __PHBFIELDMAP_H__
#define __PHBFIELDMAP_H__
// $Id: PHBFieldMap.h,v 1.6 2008/10/16 19:45:31 pinkenbu Exp $

/*!
   \file PHBFieldMap.h
   \brief Cell based linear interpolation of PHENIX B Field 
   \author Sean Kelly, Hugo Pereira Da Costa
   \version $Revision: 1.6 $
   \date $Date: 2008/10/16 19:45:31 $
*/

#include<TDataType.h>

#include<vector>
#include<iomanip>
#include<iostream>
#include<climits>

/*! \ingroup classes */

/*!
  \class PHBFieldMap
  \brief Cell based linear interpolation of PHENIX B Field .
  Upon construction PHBFieldMap reads the PHENIX 3D magnetic field and populates
  an lookup table.  The input B field map is a ROOT format file that is specified
  at construction time.
  <br>
  The pitch of the input grid is delta r = 4cm, delta z = 4cm delta phi = 3 degrees.
  If the input files is of a different grid the code will not work -- ie the input
  grid spacing is hardwired not determined at runtime from the input file.
  <br>
*/

//! magnetic field map point storage
class PHBField
{
  
  public:
  
  //! constructor
  PHBField() : bx(0),by(0),bz(0),error(true){}
  
  //! field along x
  float bx;
  
  //! field along y
  float by;
  
  //! field along z
  float bz;
  
  //! true if field could not be calculated properly
  bool error;    
  
};

class PHBFieldMap
{
 public: 

  //! shortcut
  typedef PHBField BField;
  
  //! interpolation mode
  enum Mode
  {
    //! linear interpolation
    /*! uses 4 out of the 8 magnetic field values of a cell */
    LINEAR,

    //! bi-linear interpolation
    /*! uses all 8 magnetic field values of a cell. More precise but twice slower */
    BILINEAR
  };
    
  /*! Default constructor */
  PHBFieldMap( void ) : 
    _initialized(false),
    _mode( LINEAR )
  { 
    /* 
    set the error of the output field
    to false. Only when nullField is returned
    is the error equals to true.
    */
    _field.error = false; 
  }
  
  //! destructor
  ~PHBFieldMap( void )
  {}
  
  //! set interpolation mode
  void set_interpolation_mode( const Mode& mode )
  { _mode = mode; }
  
  //! set interpolation mode
  const Mode& interpolation_mode( void ) const
  { return _mode; }
  
  //! true if initialized
  const bool& initialized( void ) const
  { return _initialized; }
  
  //! Read ROOT file into lookup
  void initialize( const std::string& filename );
    
  //! return field at position (x,y,z)
  const BField& get_field( float x, float y, float z);  
  
  //! store r, phi, z indices separately
  struct Index
  {

    //! constructor
    Index( void ):
      i_r( 0 ), i_phi( 0 ), i_z( 0 ),
      valid( false )
    {}
    
    //! constructor
    Index( const size_t& i_r, const size_t i_phi, const size_t& i_z, const bool& valid ):
      i_r( i_r ), i_phi( i_phi ), i_z( i_z ),
      valid( valid )
    {}
       
    //! r index
    size_t i_r;

    //! phi index
    size_t i_phi;
    
    //! z index
    size_t i_z;
    
    //! validity
    bool valid;
    
  };

  //! return current index of the cell being used.
  const Index& index( void ) const
  { return _index; }
  
  private:
  
  //! calculate r, phi and z indices; check boundaries
  Index get_index(float r, float phi, float z) const;
  
  //! return index in lookup table for given r, phi and z value. Second value is true if out of bound
  size_t hash_function(float r, float phi, float z) const
  { return hash_function( get_index( r, phi, z ) ); }
  
  //! return index in lookup table for given r, phi and z value. Second value is true if out of bound
  size_t hash_function(const Index& index, size_t r_offset = 0, size_t phi_offset = 0, size_t z_offset = 0) const;
  
  //! Update the bounds of the lookup table at initialization stage
  void update_bounds(float r, float phi, float z);
  
  //! Initialize lookup table
  void initialize_lookup( Stat_t size );

  //! check if initialized
  bool check_initialized( void );   
  
  //! Add given point to lookup
  void add_point(
    float r, float phi, float z,
    float br, float bphi, float bz);
  
  //! square
  template < typename T >
    T square( const T& x ) 
  { return x*x; }
  
  //! Data struct stored in lookup
  struct FieldPoint 
  {    
    
    //! constructor
    FieldPoint( 
      float r = 0, float phi = 0, float z = 0, 
      float bx = 0, float by = 0, float bz = 0, 
      bool valid = false ):
      r(r), phi(phi), z(z),
      bx(bx), by(by), bz(bz),
      valid(valid)
    {}
    
    //!@name position (cylindrical coordinates)
    //@{
    float r;
    float phi;
    float z;
    //@}
        
    //!@name magnetic field (cartesian coordinates)
    //@{
    float bx;
    float by;
    float bz;
    //@}
    
    //! true if point was set.
    bool valid;
    
    //! streamer
    void print(std::ostream& os=std::cout) const {

      // for conversion to cylindrical 
      os << std::setw(10) << std::setprecision(5) << std::setiosflags(std::ios::showpoint);

      // print point cylindrical coordinates
      os << "x_cyln = {" << r << ", " << phi << ", " << z << "}" << std::endl;

      // print the voxel
      os << "B = {" << bx << ", " << by << ", " << bz << "}" << std::endl;

    }
  };
  
  //!@name cell size. These numbers are hard coded. They <b>must</b> match the input root file.
  //@{
  static const float CELL_WIDTH_R;
  static const float CELL_WIDTH_PHI;
  static const float CELL_WIDTH_Z;
  //@}
  
  //! rad to degrees conversion
  static const float RAD_TO_DEG;
  
  //! degrees to radian conversion
  static const float DEG_TO_RAD;
    
  //!@name grid limits. The numbers are updated on fly while reading the map
  //@{
  static long MIN_R;
  static long MAX_R;

  static long MIN_PHI;
  static long MAX_PHI;

  static long MIN_Z;  
  static long MAX_Z;
  //@}
  
  //!@name grid size. The numbers are updated on fly while reading the map
  //@{
  size_t _size_r;
  size_t _size_phi;
  size_t _size_z;
  //@}
  
  //! map location to index in field_vector
  /*! this allows to store the field values in a "hole-free" container */
  std::vector<unsigned int> _lookup;
  
  //! field values
  std::vector<FieldPoint> _field_values;
  
  //! temporary index storage
  Index _index;
  
  //! temporary storage for return value 
  BField _field;    
  
  //! Null Field with error set
  const BField& null_field()
  {
    static BField null;
    return null;
  }
  
  //! true when initialized
  bool _initialized;
  
  //! interpolation mode
  Mode _mode;
  
};

#endif

