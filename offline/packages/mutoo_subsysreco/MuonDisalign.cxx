// $Id: MuonDisalign.cxx,v 1.8 2010/08/25 00:55:09 hpereira Exp $

/*!
   \file    MuonDisalign.h
   \brief   load disalignments from file, update mutr/muid geometry consequently
      			Call MuonDisalign::add_corrections(file_name) as much as u want
   					Then call MuonDisalign("MUONDISALIGN")
   \author  Hugo Pereira/Catherine Silvestre
   \version $Revision: 1.8 $
   \date    $Date: 2010/08/25 00:55:09 $
*/

#include "MuonDisalign.h"
#include "TMutAlign.h"
#include "TMuiAlign.h"
#include <MUTOO.h>
#include <MuiGeomClasses.hh>
#include <TMuiGeometry.hh>

#include <iostream>
#include <fstream>

using namespace std;

//________________________________________________________
int MuonDisalign::InitRun(PHCompositeNode *top_node)
{

  MUTOO::PRINT( cout, "MuonDisalign::InitRun" );

  // insert file correction in the list
  // iterate until the end of the list
  if(_file.empty()) MUTOO::PRINT( cout, "MuonDisalign::InitRun - add_corrections not called" );

  TMutAlign::reset();
  TMuiAlign::reset();

  for(file_list::iterator file_iter = _file.begin(); file_iter!= _file.end();file_iter++)
  {
    //! load mutr alignment corrections
    TMutAlign::init( *file_iter );

    //! load muid alignment corrections
    TMuiAlign::init( *file_iter );
  }

  //! convert geometry to DB format
  if( get_flag( CONVERT_TO_DB ) ) 
  {
    if( _db_output_file.empty() ) TMutAlign::convert_to_db();
    else 
    {
      cout << "MuonDisalign::InitRun - writting db corrections to: " << _db_output_file << endl;
      ofstream fout( _db_output_file.c_str() );
      TMutAlign::convert_to_db( fout );
      fout.close();
    }
  }
  //! update geometry
  TMutAlign::update_geometry();
  TMuiAlign::update_geometry();

  //! print alignment corrections
  if( get_flag( PRINT_MUTR ) ) TMutAlign::print_parameters();
  if( get_flag( PRINT_MUID ) ) TMuiAlign::print_parameters();

  //! print full mutoo geometry
  if( get_flag( PRINT_MUTR_GEOM ) )
  {
    TMutAlign::print_geometry();
    TMuiGeometry::Geom()->dump_panel_geometry();
  }

  MUTOO::PRINT( cout, "**" );
  return 0;

}

//________________________________________________________
void MuonDisalign::add_corrections( const char* file )
{ if( file ) _file.push_back(file);	}



