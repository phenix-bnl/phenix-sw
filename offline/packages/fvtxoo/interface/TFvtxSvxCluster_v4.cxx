// $Id: TFvtxSvxCluster_v4.cxx,v 1.1 2012/10/01 04:28:01 keyaaron Exp $

/*!
  \file TFvtxSvxCluster_v4.cxx
  \brief The forward vertex hit object 
  \author Hugo Pereira Da Costa
  \version $Revision: 1.1 $
  \date $Date: 2012/10/01 04:28:01 $
*/

#include <FVTXOO.h>
#include <TFvtxSvxCluster_v4.h>
ClassImp( TFvtxSvxCluster_v4 );
using namespace std; 

double TFvtxSvxCluster_v4::PIXEL_ZLENGTH = 0.0425;   // Pixel length in Z direction (cm)
double TFvtxSvxCluster_v4::PIXEL_PHILENGTH = 0.0050; // Pixel width in Phi direction (cm)
double TFvtxSvxCluster_v4::PIXEL_RLENGTH = 0.0200;   // Pixel thickness in R direction (cm)

//____________________________________________________
TFvtxSvxCluster_v4::TFvtxSvxCluster_v4( void ):
  _index(0)
{}

//_____________________________________________
TFvtxSvxCluster_v4::TFvtxSvxCluster_v4( const Key& key, const unsigned short& index ):
  TFvtxSvxCluster( key ),
  _index( index )
{}

//_____________________________________________ 
TFvtxSvxCluster_v4::TFvtxSvxCluster_v4(const TFvtxSvxCluster& base_ref ):
  TFvtxSvxCluster(base_ref)
{ 
  set_index( base_ref.get_index() );
  set_cluster( base_ref.get_cluster() ); 
}

//_____________________________________________ 
TFvtxSvxCluster_v4::TFvtxSvxCluster_v4(const TFvtxSvxCluster* base_ptr ):
  TFvtxSvxCluster(*base_ptr)
{ 
  set_index( base_ptr->get_index() );
  set_cluster( base_ptr->get_cluster() ); 
}

//_______________________________________________________
void TFvtxSvxCluster_v4::print( ostream& os ) const
{
  FVTXOO::PRINT(os,GetName());
  
  os << "index: " << get_index() << endl;
  os << " global position=("
     << get_cluster()->get_xyz_global(0) << "," 
     << get_cluster()->get_xyz_global(1) << "," 
     << get_cluster()->get_xyz_global(2) << ")" << endl;
  os << " local position=("
     << get_cluster()->get_xyz_local(0) << "," 
     << get_cluster()->get_xyz_local(1) << "," 
     << get_cluster()->get_xyz_local(2) << ")" << endl;
  os << " layer=" << get_cluster()->get_layer() << endl;
  os << " volume= [ ";
  os << get_cluster()->get_hitID() << " "
     << get_cluster()->get_svxSection() << " "
     << get_cluster()->get_layer() << " "
     << get_cluster()->get_ladder() << " " 
     << get_cluster()->get_sensor() << " ";
  os << "]" << endl;
    
  FVTXOO::PRINT(os,"**");
}

void
TFvtxSvxCluster_v4::set_line(const PHLine& line)
{
  _point0[0] = line.getBasepoint().getX();
  _point0[1] = line.getBasepoint().getY();
  _point0[2] = line.getBasepoint().getZ();
  _point1[0] = _point0[0] + line.getDirection().getX();
  _point1[1] = _point0[1] + line.getDirection().getY();
  _point1[2] = _point0[2] + line.getDirection().getZ();
}
