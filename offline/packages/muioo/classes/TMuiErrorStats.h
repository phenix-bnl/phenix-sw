//////////////////////////////////////////////////////////////////
//
// Utility class: TMuiErrorStats.h
// based on TMutErrorStats
// Description: Maintains statistics on MUIOO exceptions
//              
//////////////////////////////////////////////////////////////////

#ifndef __TMUIERRORSTATS_H__
#define __TMUIERRORSTATS_H__

#include<PHException.h>
#include<MUIOO.h>
#include<list>

/*! \ingroup classes */
//! Maintains statistics on MUIOO exceptions
/*! 
  Maintains statistics on MUIOO exceptions
*/
class PHCompositeNode;
class TFile;
class TNtuple;

class TMuiErrorStats
{
 public: 

  enum ErrorType { NO_ERROR, ROAD_BIFURCATE };
  
  static void clear_event();
  static unsigned long get_n_accumulated_errors();
  static unsigned long get_n_event_errors();
  
  static bool is_error(UShort_t type); // return whether there was an error of type "type"
  static bool is_error(); //return whether there was an error of either type
  
  static void set_error(ErrorType error, UShort_t arm,
			unsigned long road_occ=0, UShort_t time_ms=0);
  
  static void set_event_stats(PHCompositeNode* top_node, UShort_t time);
  
  static void make_ntuple();
  static void print_event();
  static void print_cummulate();
  static bool initialize_ntuple();
  static void finish();
  static void write_event();
  
 private: 
  
  struct error {

    error(ErrorType type, UShort_t arm, 
	  unsigned long road_occ=0, UShort_t time_ms=0) :
      type(type),
      arm(arm),
      road_occ(road_occ),
      time_ms(time_ms){}
    
    void print() const
    {
      if(type == NO_ERROR) {
	std::cout << "NO_ERROR "; 
      } else if(type==ROAD_BIFURCATE) {
	std::cout << "ROAD_BIFURCATE ";
      } 
      std::cout << "arm:" << arm 
                << " roads: " << road_occ << " time:" << time_ms << std::endl;
    }
    UShort_t type;
    UShort_t arm;
    unsigned long road_occ;
    UShort_t time_ms;
  };

  static TFile* _file;
  static TNtuple* _tuple_event;
  static TNtuple* _tuple_error;
  static std::list<error> _event_errors;
  static std::list<error> _cummulate_errors;

};

#endif



