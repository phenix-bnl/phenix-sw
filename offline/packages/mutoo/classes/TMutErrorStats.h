// $Id: TMutErrorStats.h,v 1.9 2011/12/24 04:48:21 slash Exp $
//////////////////////////////////////////////////////////////////
/*
  \class TMutErrorStat.h
  \author S.Kelly
  \brief Maintains statistics on MUIOO/MUTOO exceptions
	\version $Revision: 1.9 $
	\date		$Date: 2011/12/24 04:48:21 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __TMUTERRORSTATS_H__
#define __TMUTERRORSTATS_H__

#include<PHException.h>
#include<MUTOO.h>
#include<list>
#include<string>


class PHCompositeNode;
class TNtuple;

/*! \ingroup classes */
//! Maintains statistics on MUIOO/MUTOO exceptions
class TMutErrorStats
{
 public:

  //! supported error types
  enum ErrorType { NO_ERROR, STUB_BIFURCATE, CLONE_TRACK };

  //! output filename
  static void set_filename( const std::string& filename )
  { _filename = filename; }

  //! output filename
  static std::string get_filename( void )
  { return _filename; }

  //! reset event errors
  static void clear_event()
  { _event_errors.clear(); }

  //! returns event number of errors
  static ULong_t get_n_event_errors()
  { return _event_errors.size(); }

  /*!
    return TRUE if there was an error of type
    can also be used with type=0 to tell if there was no error
  */
  static bool is_error(unsigned short type);

  //! return whether there was an error of either type
  static bool is_error();

  //! adds an error to current error
  static void set_error(
    ErrorType error, unsigned short arm, unsigned short station, unsigned short octant,
		ULong_t hit_occ=0, ULong_t stub_occ=0, ULong_t trk_occ=0, unsigned short time_ms=0);

  //! sets event statistics
  static void set_event_stats(PHCompositeNode* top_node, unsigned short time);

  //! print current event error statistics
  static void print_event();

  //! initialize ntuples
  static bool initialize_ntuple( void );

  //! write ntuples
  static void finish();

  //! store event statistics to ntuple
  static void write_event();

 private:

	//! stores usefull informations every time an exception is caught
  struct Error {

    Error(ErrorType type, unsigned short arm, unsigned short station, unsigned short octant,
	  ULong_t hit_occ=0, ULong_t stub_occ=0, ULong_t trk_occ=0, unsigned short time_ms=0) :
      type(type),
      arm(arm),
      station(station),
      octant(octant),
      hit_occ(hit_occ),
      stub_occ(stub_occ),
      trk_occ(trk_occ),
      time_ms(time_ms){}

		//! print error to stdout
    void print() const
    {
      switch( type ) {
        case NO_ERROR: std::cout << "NO_ERROR "; break;
        case STUB_BIFURCATE: std::cout << "STUB_BIFURCATE "; break;
        case CLONE_TRACK: std::cout << "CLONE_TRACK "; break;
        default: std::cout << "UNKNOWN "; break;
      }

      std::cout << "arm:" << arm << " sta:" << station << " oct:" << octant << " hits: " << hit_occ << " stubs: " << stub_occ
                << " trks: " << trk_occ << " time:" << time_ms << std::endl;
    }

    unsigned short type;
    unsigned short arm;
    unsigned short station;
    unsigned short octant;
    ULong_t hit_occ;
    ULong_t stub_occ;
    ULong_t trk_occ;
    unsigned short time_ms;
  };

  static std::string _filename;
  static bool _initialized;
  static TNtuple* _tuple_event;
  static TNtuple* _tuple_error;

  //! list of current event errors
  static std::list<Error> _event_errors;

};

#endif



