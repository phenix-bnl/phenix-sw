// $Id: FvtxDisalign.h,v 1.2 2012/08/23 21:19:10 jinhuang Exp $

#ifndef __FvtxDisalign_h__
#define __FvtxDisalign_h__

/*!
\file		FvtxDisalign.h
\brief	 load disalignments from file, update mutr/muid geometry consequently
Call FvtxDisalign::add_corrections(file_name);
Then call FvtxDisalign("FVTXDISALIGN").
\author	Zhengyun You
\version $Revision: 1.2 $
\date		$Date: 2012/08/23 21:19:10 $
*/

#include <SubsysReco.h>

#include <string>
#include <list>

//! load disalignments from files, update mutr/muid geometry consequently
class FvtxDisalign: public SubsysReco
{
  public:

  //! constructor
  FvtxDisalign( const char* name = "FVTXDISALIGN", const char* file = 0 ):
    SubsysReco( "FVTXDISALIGN" ),
    _flags( NONE )
  { if( file ) add_corrections( file ); }

  //! destructor
  virtual ~FvtxDisalign()
  {}

  //!run initialization
  int Init(PHCompositeNode *top_node);

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

    //! print FVTX alignment parameters
    PRINT_FVTX = 1<<1,

    //! print full FVTX geometry
    PRINT_FVTX_GEOM = 1<<2

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
