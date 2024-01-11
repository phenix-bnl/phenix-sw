// $Id: FvtxStation.cxx,v 1.10 2013/11/05 00:04:47 jinhuang Exp $
/*!
  \file FvtxStation.cxx
  \brief Forward vertex Station geometry
  Initialize and provide access to FVTX sectors
  \author Hugo Pereira da costa
  \version $Revision: 1.10 $
  \date $Date: 2013/11/05 00:04:47 $
*/

#include "FvtxStation.h"
#include "FvtxGeom.h"

using namespace std;

//______________________________________________________________
list<FvtxSector*> FvtxStation::find_sectors( const PHPoint& point ) const
{
  check_phy_node("FvtxStation::find_sectors");
  
  double phi( atan2( point.getY(), point.getX() ) - get_phi0() );
  if( phi < 0 ) phi += 2*M_PI;

  list< FvtxSector* > out;
  FvtxSector* sector( 0 );

  for (int sector_id = 0; sector_id < FVTXGEOM::NumberOfSectors; sector_id++)
  {
    if( ( sector = get_sector( sector_id ) )->contains( point ) ) out.push_back( sector );
  }  

  // now sector found
  if( FvtxGeom::get_verbosity() >= FVTXGEOM::SOME && out.size() )
  {
    cout << "FvtxStation::find_sectors - phi: " << phi << " [ ";
    for( list< FvtxSector* >::iterator iter = out.begin(); iter != out.end(); iter++ )
    cout << (*iter)->index().sector() << " ";
    cout << "]" << endl;
  }	
  
  return out;
      
}
