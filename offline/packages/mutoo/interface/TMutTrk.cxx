// $Id: TMutTrk.cxx,v 1.31 2018/08/08 02:44:38 slash Exp $

/*!
  \file TMutTrk.cxx
  \brief The Muon tracker Track object
  \author S. Kelly
  \version $Revision: 1.31 $
  \date    $Date: 2018/08/08 02:44:38 $
*/

#include <TMutGeo.h>
#include <TMutTrackUtil.h>

#include "TMutCoordMap.h"
#include "TMutGapCoordMap.h"
#include "TMutStubMap.h"
#include "TMutTrk.hh"

#include <MUTOO.h>
#include <float.h>
ClassImp(TMutTrk)

using namespace std;

//
PHClassId::id_type TMutTrk::_class_id( 0 );
//_________________________________________________________________________
void TMutTrk::print_status( ostream& os ) const
{
  os << " status: ";
  if( get_no_fit() ) os << "NO_FIT ";
  if( get_global_fit() ) os << "GLOBAL_FIT ";
  if( get_global_fail() ) os << "GLOBAL_FAIL ";
  if( get_kalman_fit() ) os << "KALMAN_FIT ";
  if( get_kalman_fail() ) os << "KALMAN_FAIL ";
  if( get_reco_success() ) os << "RECO_SUCCESS ";
  if( get_reco_min_hits() ) os << "RECO_MIN_HITS ";
  if( get_no_estimate() ) os << "NO_ESTIMATE ";
  if( get_ghost() ) os << "GHOST ";
  if( get_low_mom() ) os << "LOW_MOMENTUM ";
  if( get_bp_fit2() ) os << "BP_FIT2 ";
  if( get_bp_fit3() ) os << "BP_FIT3 ";
  if( get_status( STRAIGHT_TRACK ) ) os << "STRAIGHT_TRACK ";
  if( get_status( BOTH_ARMS ) ) os << "BOTH_ARMS ";
  os << std::endl;
}

//_________________________________________________________________________
const TMutTrkPar* TMutTrk::get_trk_par_station( UShort_t arm, UShort_t station) const
{

  // check there are some track parameters in the list
  const trk_par_list* local_list( get_trk_par_list() );
  if( !local_list ) return get_trk_par();

  // Loop over assoicate TMutCoord in the station and use the minimal mean_z of TMutCoord.
  double target_z = DBL_MAX;
  TMutCoordMap::const_key_iterator coord_iter = get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next())
  { 
    if( 
      coord_ptr->get()->get_arm() == arm &&
      coord_ptr->get()->get_station() == station &&
      fabs( coord_ptr->get()->get_mean_z()) < fabs( target_z ) 
      ) 
    { target_z = coord_ptr->get()->get_mean_z(); }
  }
    
  // loop ovet the track_par_list and search closest track parameters
  double min_delta_z = DBL_MAX;
  const TMutTrkPar* trk_par_ptr=0;

  // Loop over trk par list and TMutTrkPar with z reference closest to
  // gap 0 of request station
  for( trk_par_list::const_iterator trk_iter = local_list->begin();trk_iter != local_list->end();++trk_iter){
    double delta_z = fabs(target_z-trk_iter->get_z());
    if(delta_z < min_delta_z) {
      min_delta_z = delta_z;
      trk_par_ptr = &(*trk_iter);
    }
  }

  // If TMutTrkPar* is still null then return the reference track pars
  return (trk_par_ptr) ? trk_par_ptr : get_trk_par();
}

//___________________________________________________
size_t TMutTrk::get_n_coord() const
{ return get_associated<TMutCoord>().count(); }

//___________________________________________________
size_t TMutTrk::get_n_gap_coord() const
{ return get_associated<TMutGapCoord>().count(); }

//___________________________________________________
size_t TMutTrk::get_ndf() const
{ return get_associated<TMutCoord>().count() - 5; }

//___________________________________________________
double TMutTrk::get_rapidity( void ) const
{

  if( !get_trk_par_vtx() ) {
    cerr << "TMutTrk::get_rapidity - parameters at vertex not set.\n";
    return 0;
  }

  double px = get_trk_par_vtx()->get_px();
  double py = get_trk_par_vtx()->get_py();
  double pz = get_trk_par_vtx()->get_pz();
  float E = sqrt(
    MUTOO::SQUARE( px )+
    MUTOO::SQUARE( py )+
    MUTOO::SQUARE( pz )+
    MUTOO::MASS_MUON_SQUARE);
  return 0.5*log( (E+pz)/(E-pz) );

}

//_________________________________________
UShort_t TMutTrk::get_hit_pattern() const
{

  UShort_t out( 0 );
  TMutCoordMap::const_key_iterator coord_iter = get_associated<TMutCoord>();
  while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
  {

    // calculate plane index from the coordinate
    UShort_t index =
      coord_ptr->get()->get_cathode() + MUTOO::NumberOfCathodePlanes*(
        coord_ptr->get()->get_gap() + MUTOO::NumberOfGaps*(
          coord_ptr->get()->get_station() ) );

    // update the pattern
    out |= ( 1 << index );
  }

  return out;

}

//_________________________________________
bool TMutTrk::has_stub(UShort_t station) const
{
  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
    if(stub_ptr->get()->get_station() == station) return true;
  }
  return false;
}

//_________________________________________
double TMutTrk::get_rdphi_12() const
{
  // Check that station1 and station2 stubs exist
  if(!has_stub(MUTOO::Station1) || !has_stub(MUTOO::Station2)) return 0;

  // Capture pointers to station 1 and 2 stubs associated with this track
  TMutStubMap::const_pointer stub_sta1=0, stub_sta2=0;
  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
    if(stub_ptr->get()->get_station() == MUTOO::Station1) stub_sta1 = stub_ptr;
    if(stub_ptr->get()->get_station() == MUTOO::Station2) stub_sta2 = stub_ptr;
  }

  // Extrapolate station 1 stub to first gap in station 2
  double z = TMutGeo::get_anode_plane_position(stub_sta1->get()->get_arm(),
    MUTOO::Station2,
    stub_sta1->get()->get_octant(),
    stub_sta1->get()->get_half_octant(),
    MUTOO::Gap1).getZ();

  PHPoint stub_sta1_point = TMutTrackUtil::linear_track_model(stub_sta1->get()->get_fit_par(),z);

  double r = sqrt(MUTOO::SQUARE(stub_sta1_point.getX()) + MUTOO::SQUARE(stub_sta1_point.getX()));
  double stub_sta1_phi = atan2(stub_sta1_point.getY(),stub_sta1_point.getX());
  double stub_sta2_phi = atan2(stub_sta2->get()->get_fit_par()->get_y(), stub_sta2->get()->get_fit_par()->get_x());

  // Return R * DELTA PHI
  return r*(stub_sta2_phi - stub_sta1_phi);
}

//_____________________________________________
double TMutTrk::get_rdphi_23() const
{
  // Check that station1 and station2 stubs exist
  if(!has_stub(MUTOO::Station2) || !has_stub(MUTOO::Station3)) return 0;

  // Capture pointers to station 1 and 2 stubs associated with this track
  TMutStubMap::const_pointer stub_sta2=0, stub_sta3=0;
  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
    if(stub_ptr->get()->get_station() == MUTOO::Station2) stub_sta2 = stub_ptr;
    if(stub_ptr->get()->get_station() == MUTOO::Station3) stub_sta3 = stub_ptr;
  }

  // Extrapolate station 2 stub to first gap in station 3
  double z = TMutGeo::get_anode_plane_position(stub_sta2->get()->get_arm(),
    MUTOO::Station3,
    stub_sta2->get()->get_octant(),
    stub_sta2->get()->get_half_octant(),
    MUTOO::Gap1).getZ();

  PHPoint stub_sta2_point = TMutTrackUtil::linear_track_model(stub_sta2->get()->get_fit_par(),z);

  double r = sqrt(MUTOO::SQUARE(stub_sta2_point.getX()) + MUTOO::SQUARE(stub_sta2_point.getX()));
  double stub_sta2_phi = atan2(stub_sta2_point.getY(),stub_sta2_point.getX());
  double stub_sta3_phi = atan2(stub_sta3->get()->get_fit_par()->get_y(), stub_sta3->get()->get_fit_par()->get_x());

  // Return R * DELTA PHI
  return r*(stub_sta3_phi - stub_sta2_phi);
}

//________________________________________________________________
double TMutTrk::calculate_momentum_point_point( void ) const
{

  // Track must have stub in stations 2 and 3
  if(!(has_stub(MUTOO::Station2) && has_stub(MUTOO::Station3))) return -1;

  boost::array<TMutStubMap::const_pointer,3> stubs = {{0}};
  boost::array<const TMutFitPar*,3>  fit_pars = {{0}};
  boost::array<PHPoint,3> points;

  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()) {
    UShort_t station = stub_ptr->get()->get_station();
    fit_pars[station] = stub_ptr->get()->get_fit_par();
    points[station] = fit_pars[station]->get_point();
    stubs[station] = stub_ptr;
  }

  double mod1 = sqrt(MUTOO::SQUARE(points[1].getX()) + MUTOO::SQUARE(points[1].getY()));
  double mod2 = sqrt(MUTOO::SQUARE(points[2].getX()) + MUTOO::SQUARE(points[2].getY()));

  // Calculate theta from station 2 stub
  double theta = MUTOO::RAD_TO_DEG*atan2(mod1,fabs(fit_pars[1]->get_point().getZ()));

  // Delta phi station 23
  double phi23 = MUTOO::RAD_TO_DEG*acos( (1.0/(mod1*mod2)) * (points[1].getX()*points[2].getX() + points[1].getY()*points[2].getY()));
  return TMutTrackUtil::get_us_mom_point_point( get_arm(), theta, phi23);
}


//____________________________________________________________________________
double TMutTrk::calculate_momentum_point_point2( void ) const
{
  // Track must have stub in all 3 stations
  if(!(
    has_stub(MUTOO::Station1) &&
    has_stub(MUTOO::Station2) &&
    has_stub(MUTOO::Station3))) return -1;


  boost::array<TMutStubMap::const_pointer,3> stubs = {{0}};
  boost::array<const TMutFitPar*,3>  fit_pars = {{0}};
  boost::array<PHPoint,3> points;

  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()) {
    UShort_t station = stub_ptr->get()->get_station();
    fit_pars[station] = stub_ptr->get()->get_fit_par();
    points[station] = fit_pars[station]->get_point();
    stubs[station] = stub_ptr;
  }

  //  If the station3 stub has no point then return default estimate
  if(!stubs[MUTOO::Station3]->get()->get_n_gap_coord()) return -1;

  double mod0 = sqrt(MUTOO::SQUARE(points[0].getX()) + MUTOO::SQUARE(points[0].getY()));
  double mod2 = sqrt(MUTOO::SQUARE(points[2].getX()) + MUTOO::SQUARE(points[2].getY()));

  PHVector vector01(
    points[1].getX()-points[0].getX(),
    points[1].getY()-points[0].getY(),
    points[1].getZ()-points[0].getZ());

  vector01.normalize();

  double deltaz = points[2].getZ() - points[1].getZ();

  PHPoint point012;
  point012.setX(points[1].getX() + (vector01.getX()/vector01.getZ())*deltaz);
  point012.setY(points[1].getY() + (vector01.getY()/vector01.getZ())*deltaz);
  point012.setZ(points[2].getZ());

  double mod012 = sqrt(MUTOO::SQUARE(point012.getX()) + MUTOO::SQUARE(point012.getY()));
  double edphi012 = MUTOO::RAD_TO_DEG*acos( (1.0/(mod012*mod2)) * (point012.getX()*points[2].getX() + point012.getY()*points[2].getY()));
  double theta1 = MUTOO::RAD_TO_DEG*atan2(mod0,fabs(points[0].getZ()));

  return TMutTrackUtil::get_us_mom_point_point2(get_arm(), theta1, edphi012);

}

//____________________________________________________________________________

pair<bool,PHVector> TMutTrk::estimate_tangent( UShort_t station ) const
{
  if(station == MUTOO::Station1 || station == MUTOO::Station2) return estimate_tangent_sta12(station);
  else if(station == MUTOO::Station3) return estimate_tangent_sta3();
  else throw invalid_argument(DESCRIPTION("estimate_tangent -- bad station specifier"));

}

//__________________________________________________________________________________
pair<bool,PHVector> TMutTrk::estimate_tangent_sta12( UShort_t station ) const
{
  // Track must have stub in sta1 and sta2
  //
  if(!( has_stub(station) && has_stub(station+1))) return make_pair(false, PHVector());

  TMutStubMap::const_pointer sta1_stub_ptr = 0;
  PHPoint sta1_point, sta2_point;
  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next())
  {
    if(stub_ptr->get()->get_station() == station)
    {
      sta1_stub_ptr = stub_ptr;
      sta1_point = stub_ptr->get()->get_fit_par()->get_point();
    } else if (stub_ptr->get()->get_station() == station+1)
    {
      sta2_point = stub_ptr->get()->get_fit_par()->get_point();
    }
  }

  // Construct the w, r, z coordinate axis
  PHVector z_dir(0,0,1);
  PHVector w_dir = sta1_stub_ptr->get()->get_anode_direction();
  PHVector r_dir = w_dir.cross(z_dir);

  // Stub tangent
  PHVector stub_dir = sta1_stub_ptr->get()->get_fit_par()->get_tangent( sta1_stub_ptr->get()->get_arm() );

  // Normalized vector in the direction of the line that connects
  // station 1 and 2 points

  PHVector r(
    sta2_point.getX()-sta1_point.getX(),
    sta2_point.getY()-sta1_point.getY(),
    sta2_point.getZ()-sta1_point.getZ());

  r.normalize();

  double s_w = stub_dir.dot(w_dir);
  double r_w = r.dot(w_dir);
  double s_z = stub_dir.dot(z_dir);
  double r_z = r.dot(z_dir);
  double r_r = r.dot(r_dir);

  // Angle between stub and r projection onto wz plane
  double mod1 = sqrt(MUTOO::SQUARE(s_w) + MUTOO::SQUARE(s_z));
  double mod2 = sqrt(MUTOO::SQUARE(r_w) + MUTOO::SQUARE(r_z));
  double cos_phi = 1.0/(mod1*mod2)*(s_w*r_w + s_z*r_z);
  double sin_phi = sqrt(1-MUTOO::SQUARE(cos_phi));

  // Sense of rotation from the cross of the projections
  double sgn = (r_z*s_w - r_w*s_z) > 0 ? -1 : 1;

  // Rotatate r until wz projection is parallel to stub
  double r_z_p = cos_phi*r_z + sgn*sin_phi*r_w;
  double r_w_p = -1.0*sgn*sin_phi*r_z + cos_phi*r_w;

  // Construct the tangent
  PHVector tangent = z_dir*r_z_p + w_dir*r_w_p + r_dir*r_r;

  return make_pair(true,tangent);
}

//__________________________________________________________________
pair<bool,PHVector> TMutTrk::estimate_tangent_sta3( void ) const
{
  // Track must have stub in sta2 and sta3
  if(!( has_stub(MUTOO::Station2) && has_stub(MUTOO::Station3))) return make_pair(false, PHVector());

  TMutStubMap::const_pointer sta3_stub_ptr = 0;
  PHPoint sta2_point, sta3_point;
  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next())
  {
    if(stub_ptr->get()->get_station() == MUTOO::Station3)
    {
      sta3_stub_ptr = stub_ptr;
      sta3_point = stub_ptr->get()->get_fit_par()->get_point();
    } else if (stub_ptr->get()->get_station() == MUTOO::Station2)
    {
      sta2_point = stub_ptr->get()->get_fit_par()->get_point();
    }
  }

  // Construct the w, r, z coordinate axis
  PHVector z_dir(0,0,1);
  PHVector w_dir = sta3_stub_ptr->get()->get_anode_direction();
  PHVector r_dir = w_dir.cross(z_dir);

  // Stub tangent
  PHVector stub_dir =
    sta3_stub_ptr->get()->get_fit_par()->get_tangent( sta3_stub_ptr->get()->get_arm() );

  // Normalized vector in the direction of the line that connects
  // station 1 and 2 points
  PHVector r(sta3_point.getX()-sta2_point.getX(),
       sta3_point.getY()-sta2_point.getY(),
       sta3_point.getZ()-sta2_point.getZ());

  r.normalize();

  double s_w = stub_dir.dot(w_dir);
  double r_w = r.dot(w_dir);
  double s_z = stub_dir.dot(z_dir);
  double r_z = r.dot(z_dir);
  double r_r = r.dot(r_dir);

  // Angle between stub and r projection onto wz plane
  double mod1 = sqrt(MUTOO::SQUARE(s_w) + MUTOO::SQUARE(s_z));
  double mod2 = sqrt(MUTOO::SQUARE(r_w) + MUTOO::SQUARE(r_z));
  double cos_phi = 1.0/(mod1*mod2)*(s_w*r_w + s_z*r_z);
  double sin_phi = sqrt(1-MUTOO::SQUARE(cos_phi));

  // Sense of rotation from the cross of the projections
  double sgn = (r_z*s_w - r_w*s_z) > 0 ? -1 : 1;

  // Rotatate r until wz projection is parallel to stub
  double r_z_p = cos_phi*r_z + sgn*sin_phi*r_w;
  double r_w_p = -1.0*sgn*sin_phi*r_z + cos_phi*r_w;

  // Construct the tangent
  PHVector tangent = z_dir*r_z_p + w_dir*r_w_p + r_dir*r_r;

  return make_pair(true,tangent);
}

//____________________________________________________________________________
double TMutTrk::calculate_momentum_stub_point( void ) const
{

  // Track must have stub in all 3 stations
  if(!(
    has_stub(MUTOO::Station1) &&
    has_stub(MUTOO::Station2) &&
    has_stub(MUTOO::Station3))) return -1;


  boost::array<TMutStubMap::const_pointer,3> stubs = {{0}};
  boost::array<const TMutFitPar*,3>  fit_pars = {{0}};
  boost::array<PHPoint,3> points;

  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next())
  {
    UShort_t station = stub_ptr->get()->get_station();
    fit_pars[station] = stub_ptr->get()->get_fit_par();
    points[station] = fit_pars[station]->get_point();
    stubs[station] = stub_ptr;
  }

  double mod1 = sqrt(MUTOO::SQUARE(points[0].getX()) + MUTOO::SQUARE(points[0].getY()));
  double mod3 = sqrt(MUTOO::SQUARE(points[2].getX()) + MUTOO::SQUARE(points[2].getY()));

  // Estimate tangent
  pair<bool,PHVector> tangent_pair = estimate_tangent();
  PHVector tangent = tangent_pair.second;

  double deltaz = points[2].getZ() - points[1].getZ();

  // Extrapolate tangent to station 2
  PHPoint point23(
    points[1].getX() + (tangent.getX()/tangent.getZ())*deltaz,
    points[1].getX() + (tangent.getY()/tangent.getZ())*deltaz,
    points[2].getZ());

  double mod23 = sqrt(
    MUTOO::SQUARE(point23.getX()) +
    MUTOO::SQUARE(point23.getY()));


  // Calculate theta from station 1 stub
  double theta = MUTOO::RAD_TO_DEG*atan2(mod1,fabs(fit_pars[0]->get_point().getZ()));
  double edphi23 = MUTOO::RAD_TO_DEG*acos((1.0/(mod23*mod3)) *
    (point23.getX()*points[2].getX()
    + point23.getY()*points[2].getY()));

  return TMutTrackUtil::get_us_mom_stub_point( get_arm(), theta,  edphi23);

}

//____________________________________________________________________________
pair<bool,int> TMutTrk::estimate_charge( void ) const
{
  // Attempt to estimate the charge using local tangents in station 1 and 2
  pair<bool,double> charge12_pair = estimate_charge_sta12();

  // Attempt to estimate the charge using local tangents in station 2 and 3
  pair<bool,double> charge23_pair = estimate_charge_sta23();

  // If estimate succeeds for both pairs of stations then return the one with the biggest
  // magnitude (that is the largest projection of delta tangent upon phi hat).
  // If only one estimate succeeds then return the estimate.
  if(charge12_pair.first && charge23_pair.first)
  {
    if(fabs(charge23_pair.second) > fabs(charge12_pair.second))
    {
      int charge = charge23_pair.second < 0 ? -1 : 1;
      return make_pair(true,charge);
    } else {
      int charge = charge12_pair.second < 0 ? -1 : 1;
      return make_pair(true,charge);
    }
  } else if(charge23_pair.first)
  {
    int charge = charge23_pair.second < 0 ? -1 : 1;
    return make_pair(true,charge);
  } else if(charge12_pair.first)
  {
    int charge = charge12_pair.second < 0 ? -1 : 1;
    return make_pair(true,charge);
  }
  return make_pair(false,0);
}

//____________________________________________________________________________
pair<bool,double> TMutTrk::estimate_charge_sta12( void ) const
{
  // Check for station1 and station2 stubs
  if(!( has_stub(MUTOO::Station1) && has_stub(MUTOO::Station2))) return make_pair(false,0);

  // Loop over associated stubs and capture TMutFitPar pointer from station1
  // and station2 stubs.
  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  //const TMutFitPar* stub1=0;
  const TMutFitPar* stub2=0;
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next())
  {
    //    if(stub_ptr->get()->get_station() == MUTOO::Station1) stub1 = stub_ptr->get()->get_fit_par();
    //    else 
    if (stub_ptr->get()->get_station() == MUTOO::Station2) stub2 = stub_ptr->get()->get_fit_par();
  }
  
  pair<bool,PHVector> stub1_pair = estimate_tangent( MUTOO::Station1 );
  pair<bool,PHVector> stub2_pair = estimate_tangent( MUTOO::Station2 );

  if (!stub1_pair.first || !stub2_pair.first) make_pair(false,0);

  // Vector difference between the normalized track tangents
  PHVector delta_stub = stub2_pair.second - stub1_pair.second;

  // Vector phi hat -- (-sin(phi), cos(phi), 0)
  double r = sqrt(MUTOO::SQUARE(stub2->get_x()) + MUTOO::SQUARE(stub2->get_y()));
  PHVector phi_hat(-1.0*stub2->get_y()/r, stub2->get_x()/r,0);

  // Dot delta_stub onto phi hat
  double dot = phi_hat.dot(delta_stub);

  // Succcess
  return make_pair(true,dot);
}

//____________________________________________________________________________
pair<bool,double> TMutTrk::estimate_charge_sta23( void ) const
{
  // Check for station1 and station2 stubs
  if(!( has_stub(MUTOO::Station2) && has_stub(MUTOO::Station3))) return make_pair(false,0);

  // Loop over associated stubs and capture TMutFitPar pointer from station1
  // and station2 stubs.  If either stub has fewer than 2 gap coords then we
  // don't trust the local tangent and the estimate fails
  TMutStubMap::const_key_iterator stub_iter = get_associated<TMutStub>();
  const TMutFitPar* stub=0;
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next())
    {
      if(stub_ptr->get()->get_station() == MUTOO::Station2) {
	if(stub_ptr->get()->get_n_gap_coord() < 2) return make_pair(false,0);
	stub = stub_ptr->get()->get_fit_par();
      } else 
	if (stub_ptr->get()->get_station() == MUTOO::Station3) {
	  if(stub_ptr->get()->get_n_gap_coord() < 2) return make_pair(false,0);
	  stub = stub_ptr->get()->get_fit_par();
	}
    }

  pair<bool,PHVector> stub2_pair = estimate_tangent( MUTOO::Station2 );
  pair<bool,PHVector> stub3_pair = estimate_tangent( MUTOO::Station3 );
  
  if (!stub2_pair.first || !stub3_pair.first) return make_pair(false,0);
  
  // Vector difference between the normalized track tangents
  PHVector delta_stub = stub3_pair.second - stub2_pair.second;
  
  // Vector phi hat -- (-sin(phi), cos(phi), 0)
  if (!stub) return make_pair(false,0);

  double r = sqrt(MUTOO::SQUARE(stub->get_x()) + MUTOO::SQUARE(stub->get_y()));
  PHVector phi_hat(-1.0*stub->get_y()/r, stub->get_x()/r,0);

  // Dot delta_stub onto phi hat
  double dot = phi_hat.dot(delta_stub);

  // Succcess
  return make_pair(true,dot);
}
