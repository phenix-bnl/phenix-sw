// $Id: MuonDisalign.h,v 1.7 2010/08/25 00:55:09 hpereira Exp $
#ifndef __MuonDisalign_h__
#define __MuonDisalign_h__

/*!
\file		MuonDisalign.h
\brief	 load disalignments from file, update mutr/muid geometry consequently
Call MuonDisalign::add_corrections(file_name);
Then call MuonDisalign("MUONDISALIGN").
\author	Hugo Pereira/Catherine Silvestre
\version $Revision: 1.7 $
\date		$Date: 2010/08/25 00:55:09 $
*/

#include <SubsysReco.h>

#include <string>
#include <list>

//! load disalignments from files, update mutr/muid geometry consequently
class MuonDisalign: public SubsysReco
{
  public:

  //! constructor
  MuonDisalign( const char* name = "MUONDISALIGN", const char* file = 0 ):
    SubsysReco( "MUONDISALIGN" ),
    _flags( NONE )
  { if( file ) add_corrections( file ); }

  //! destructor
  virtual ~MuonDisalign()
  {}

  //!run initialization
  int InitRun(PHCompositeNode *top_node);

  //! add a file correction to the list
  void add_corrections( const char* file = "alignment_correction.txt" );

  //! event method
  int process_event(PHCompositeNode *top_node)
  { return 0; }

  //! finish run
  int End(PHCompositeNode *top_node)
  { return 0; }

  //! flags
  enum Flag {

    //! no flag
    NONE = 0,

    //! convert geometry to DB format and print
    CONVERT_TO_DB = 1<<0,

    //! print MuTR alignment parameters
    PRINT_MUTR = 1<<1,

    //! print MuID alignment parameters
    PRINT_MUID = 1<<2,

    //! print full MuTR geometry
    PRINT_MUTR_GEOM = 1<<3

  };

  //! flags
  void set_flags( const unsigned long int& value )
  { _flags = value; }

  //! flags
  void set_flag( const Flag& flag, const bool& value )
  {
    if( value ) { _flags |= flag; }
    else { _flags &= (~flag); }
  }

  //! flags
  bool get_flag( const Flag& flag ) const
  { return _flags & flag; }

  //! db output file
  void set_db_output_file( const char* file )
  { if( file ) _db_output_file = file; }

  private:

  //! short cut list of files to read
  typedef std::list< std::string > file_list;

  //! correction files list
  file_list _file;

  //! patttern recognition configuration flags
  /*! it is a bitwise or of the Flags enumaration */
  unsigned long int _flags;

  //! db output file
  std::string _db_output_file;

};


#endif
