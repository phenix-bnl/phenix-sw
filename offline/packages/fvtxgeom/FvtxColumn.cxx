// $Id: FvtxColumn.cxx,v 1.22 2015/09/14 15:28:52 snowball Exp $

/*!
  \file FvtxColumn.cxx
  \brief Forward vertex Column geometry
  Initialize and provide access to FVTX strips
  \author Hugo Pereira da costa
  \version $Revision: 1.22 $
  \date $Date: 2015/09/14 15:28:52 $
*/

#include "FvtxGeom.h"
#include "FvtxColumn.h"
#include "FvtxSector.h"

using namespace std;

//_____________________________________________________________
bool FvtxColumn::contains( const PHPoint& point ) const
{
  //PHPoint local = global_to_local(point);

  //return _shape.Contains(local);
  return _shape.Contains(point);
}

//_____________________________________________________________
FvtxStrip* FvtxColumn::find_strip( const PHPoint& point ) const
{
  //PHPoint point_local = global_to_local(point); // transform position of point to local coordinate of Column
    PHPoint point_local = point; // transform position of point to local coordinate of Column
  
    double dist_low_edge = point_local.getZ() + _shape.fDz;
    int strip_id = int( dist_low_edge / get_strip(0)->get_width() );
    if( strip_id < 0 || strip_id >= static_cast<int>(get_n_strips()) ) return 0;

    if( FvtxGeom::get_verbosity() >= FVTXGEOM::ALOT )
    { cout << "FvtxColumn::find_strip - " << index() << " strip_id: " << strip_id << endl; }

    FvtxStrip &strip( *get_strip( strip_id ) );
    return &strip;
}

//_____________________________________________________________
list<FvtxStrip*> FvtxColumn::find_strips( const PHPoint& point_in, const PHPoint& point_out ) const
{
  list< FvtxStrip* > out;
  FvtxStrip *strip_in( find_strip( point_in ) );
  FvtxStrip *strip_out( find_strip( point_out ) );

  unsigned int edge_max = 5;
  unsigned int n_strips = get_n_strips();
  if( strip_in && strip_out )
  {
    if( FvtxGeom::get_verbosity() >= FVTXGEOM::ALOT ) {
      cout << "strip in  " << strip_in->get_strip_index() << endl
           << "strip out " << strip_out->get_strip_index() << endl;
    }
    unsigned int strip_id_in( strip_in->get_strip_index() );
    unsigned int strip_id_out( strip_out->get_strip_index() );
    if( strip_id_in > strip_id_out ) swap( strip_id_in, strip_id_out );
    for( unsigned int strip_id = strip_id_in; strip_id <= strip_id_out; strip_id++ )
    out.push_back( get_strip( strip_id ) );	
  }
  else if( strip_in )
  {
    unsigned int strip_id_in( strip_in->get_strip_index() );
    if( FvtxGeom::get_verbosity() >= FVTXGEOM::ALOT ) cout << "only strip_in " << strip_id_in << endl;
    if( strip_id_in < edge_max ) 
      for ( unsigned int strip_id = 0; strip_id <= strip_id_in; strip_id++ ) {
        if( FvtxGeom::get_verbosity() >= FVTXGEOM::ALOT ) cout << "add " << strip_id << endl; 
        out.push_back( get_strip(strip_id) );
      }
    else if ( n_strips - 1 - strip_id_in < edge_max )
      for ( unsigned int strip_id = strip_id_in; strip_id < n_strips; strip_id++ ) {
        if( FvtxGeom::get_verbosity() >= FVTXGEOM::ALOT ) cout << "add " << strip_id << endl; 
        out.push_back( get_strip(strip_id) );
    }
    else
      out.push_back( strip_in );
  }
  else if( strip_out ) 
  {
    if( FvtxGeom::get_verbosity() >= FVTXGEOM::ALOT ) cout << "only strip_out " << endl;
    out.push_back(strip_out );
  }
  return out;
}
