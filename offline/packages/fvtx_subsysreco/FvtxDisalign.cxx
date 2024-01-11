// $Id: FvtxDisalign.cxx,v 1.2 2012/08/23 21:19:10 jinhuang Exp $

/*!
   \file    FvtxDisalign.h
   \brief   load disalignments from file, update fvtx geometry consequently
            Call FvtxDisalign::add_corrections(file_name) as much as u want
   	    Then call FvtxDisalign("FVTXDISALIGN")
   \author  Zhengyun You
   \version $Revision: 1.2 $
   \date    $Date: 2012/08/23 21:19:10 $
*/

#include "FvtxDisalign.h"
#include "TFvtxAlign.h"
#include <FVTXGEOM.h>
#include <FVTXOO.h>

#include <iostream>
#include <fstream>

using namespace std;

//________________________________________________________
int FvtxDisalign::Init(PHCompositeNode *top_node)
{

  FVTXOO::PRINT( cout, "FvtxDisalign::InitRun" );

  // insert file correction in the list
  // iterate until the end of the list
  if(_file.empty()) FVTXOO::PRINT( cout, "FvtxDisalign::InitRun - add_corrections not called" );

  TFvtxAlign::reset();

  for(file_list::iterator file_iter = _file.begin(); file_iter!= _file.end();file_iter++)
  {
    //! load mutr alignment corrections
    TFvtxAlign::init( *file_iter );
  }

  //! convert geometry to DB format
  if( get_flag( CONVERT_TO_DB ) ) 
  {
    if( _db_output_file.empty() ) TFvtxAlign::convert_to_db();
    else 
    {
      cout << "FvtxDisalign::InitRun - writting db corrections to: " << _db_output_file << endl;
      ofstream fout( _db_output_file.c_str() );
      TFvtxAlign::convert_to_db( fout );
      fout.close();
    }
  }
  //! update geometry
  TFvtxAlign::update_geometry();

  //! print alignment corrections
  if( get_flag( PRINT_FVTX ) ) TFvtxAlign::print_parameters();

  //! print full fvtx geometry
  if( get_flag( PRINT_FVTX_GEOM ) )
  {
    TFvtxAlign::print_geometry();
  }

  FVTXOO::PRINT( cout, "**" );
  return 0;

}

//________________________________________________________
void FvtxDisalign::add_corrections( const char* file )
{ if( file ) _file.push_back(file);	}



