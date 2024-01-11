//////////////////////////////////////////////////////////////////
//
// Utility class: PHKey
// Author: S.Kelly 
// Date: 1/12/01
// Description: 
//							
//////////////////////////////////////////////////////////////////

#ifndef __PHMAPBASE_H__
#define __PHMAPBASE_H__

// ROOT headers
#include<TDataType.h>

// STL/GSL headers
#include<algorithm>
#include <iostream>
#include<vector>
#include<gsl/gsl_rng.h>

// PH headers
#include<PHObject.h>
#include<PHCompositeNode.h>

// Forward
class TClonesArray;
class PHMapManager;

/*! \ingroup classes */

//! Map Key - unique identifier for container

/*! 
  PHMap objects are keyed with a unique identifier.
  This class provides the key generation and interface 
  and registers the map with the map manager.
*/

class PHMapBase : public PHObject
{			
 public:
  
  // typedef PHMapManager::key_type key_type;
  typedef ULong_t key_type;

  //! Construct with a unique key 
  PHMapBase();

  //! Construct with provided key 
  PHMapBase(key_type key);
  
  //! map key
  key_type get_map_key() const 
  { return _key; }
  
  //! change map key
  virtual void change_map_key( key_type );
  
  //! Clear the map 
  virtual void clear() = 0;

  //! Clear the map 
  virtual void update_statistics() = 0;
  
  //! map name 
  virtual std::string get_name() const 
  { return ""; }
  
  //!@name map statistics
  //@{
  
  //! current size 
  /*! 
    note that the method is called "count" rather than
    size because it conflicts with the stl map "size" member
    for the PHMap implementation
  */
  virtual size_t count() const 
  { return 0; }
      
  //! accumulated size 
  virtual size_t get_accumulated_size() const 
  { return 0; }
  
  //! number of cycles 
  virtual size_t get_ncycle() const 
  { return 0; }

  //@}
  
  //! Dump map contents to specified output stream 
  virtual void print(std::ostream& os = std::cout) const 
  {;}
  
  // persistency stuff
  //! set map as writable (to output)
  virtual bool is_writeable() const = 0;
  
  //! set map as readable (from input)
  virtual bool is_readable() const = 0;

  //! write map to TClonesArray output
  virtual void write_array() = 0;
  
  //! read map from TClonesArray input
  virtual void read_array(PHCompositeNode*) = 0;

  //! clear IO TClonesArray
  virtual void clear_array() = 0;
  
  virtual void print_array(std::ostream& os = std::cout) const = 0;
  
  virtual TClonesArray* get_array() = 0;
  
  virtual ~PHMapBase() {;}

  // phool interface stuff
  //

  //! PHOOL interface requirement 
  virtual void identify(std::ostream& os = std::cout) const {os << "PHMapBase";}

  //! PHOOL interface requirement -- null op 
  virtual void Reset(){;}

  //! PHOOL interface requirement -- always returns 1 
  virtual int isValid() const;

  protected:

  //! allow PHMapManager access to destroy 
  friend class PHMapManager;

  //! changes the map key.
  virtual void set_key( const key_type& key )
  { _key = key; }
  
  private:	

  //! globally unique identifier used as map key 
  static key_type get_gui();
    
  //! the map key 
  key_type _key;

};


#endif
