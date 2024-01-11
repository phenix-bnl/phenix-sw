//////////////////////////////////////////////////////////////////
//
// Utility class: TMutActivePacketSet
// Author: S.Kelly 
// Date: 3/24/03
// Description: Statically scoped class for archiving active
//              packet set
//
//////////////////////////////////////////////////////////////////

#ifndef __TMUTACTIVEPACKETSET_H__
#define __TMUTACTIVEPACKETSET_H__

#include<TDataType.h>
#include<MUTOO.h>
#include<iostream>
#include<sstream>

/*! \ingroup classes */
//! Statically scoped class for managing active packet set
/*! 
  This class manages a set of active packet id's from given run.  The
  class can be initialized from or written to the database.  In addition
  the class provides a add_packet_id method used by the mMutUnpack module
  to populate the active from raw data.
*/

class TMutActivePacketSet
{
 public:

  /*! Add a packet id to the set */
  static void add_packet_id(ULong_t packet_id) { packet_set().insert(packet_id); }
  
  /*! Returns a colon delimeted string of packet ids in current set */
  static std::string get_stringofied_packet_list() {
    std::ostringstream stream;
    packet_set_type::const_iterator iter = packet_set().begin();    
    for(;iter!=packet_set().end();++iter){
      stream << *iter << ":";
    }
    return stream.str();
  }
  
  /*! Clear active packet set */
  static void clear() { packet_set().clear(); }
  
  /*! Is given packet it in current set */
  static bool is_packet_active(ULong_t packet_id) { 
    return packet_set().find(packet_id) != packet_set().end();
  }
  
  /*! Dummy routine (Please Write Me) */
  static void initialize_from_database(ULong_t run_number) {
  }
  
  /*! Dummy routine (Please Write Me) */
  static void write_to_database(ULong_t run_number) {
  }
  
 private:

  // Name for packet storage
  //
  typedef std::set<ULong_t> packet_set_type;
  
  // Instantiate upon first use semantics for packet set
  //
  static packet_set_type& packet_set(){
    static packet_set_type* lcl = new packet_set_type;
    return *lcl;
  }
  
};

#endif








