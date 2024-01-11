// $Id: TMutBPUtil.cxx,v 1.21 2011/12/24 04:48:20 slash Exp $

/*!
\file TMutBPUtil.cxx
\brief Utility class for track bend plane fit
\author S.Kelly
\version $Revision: 1.21 $
\date $Date: 2011/12/24 04:48:20 $
*/

#include "TMutBPUtil.h"
#include "TMutNode.h"
#include "TMutGeo.h"
#include "MUTOO.h"
#include "TMutStubFinder.h"
#include "PHGslMatrix.h"

#include <boost/array.hpp>
#include <TMutStubMap.h>
#include <TMutTrackUtil.h>

using namespace std;

// Static data member definitions
TMutBPUtil::Mode TMutBPUtil::_mode = TMutBPUtil::NORMAL;
const double TMutBPUtil::SOUTH_PISTON_ANGLE = 12;
const double TMutBPUtil::NORTH_PISTON_ANGLE = 10;
MUTOO::Verbosity TMutBPUtil::_verbosity = MUTOO::NONE;

//________________________________________________________________________
// Bend plane z between stations 1 and 2
double TMutBPUtil::get_zbp_sta12(int arm, float theta)
{
  typedef boost::array<double,4> parameters;
  static const parameters north_par = 
  {{265.9469, 0.8882387, -0.5315443E-01, 0.7741023E-03}};
  
  static const parameters south_par = 
  {{-244.7470, -0.4432710, 0.3247451E-01,-0.4720373E-03}};
  
  sanity_check(arm);	
  
  const parameters& pars = (arm == MUTOO::South) ? south_par : north_par;
  
  // theta in degrees minus the piston angle, should be positive 
  //
  double theta_degrees = (arm==MUTOO::South) ?
    MUTOO::RAD_TO_DEG*std::fabs(theta) - SOUTH_PISTON_ANGLE :
    MUTOO::RAD_TO_DEG*std::fabs(theta) - NORTH_PISTON_ANGLE;
  
  double zb_sta12 = 0.;
  for( unsigned int i=0; i<pars.size(); ++i)
    zb_sta12 += pars.at(i) * std::pow(theta_degrees, static_cast<double>( i ) );
  
  return zb_sta12;
}

//________________________________________________________________________
// Pt kick between stations 1 and 2
double TMutBPUtil::get_pt_kick_sta12(int arm, float theta)
{	
  typedef boost::array<double,5> parameters;
  
  static const parameters north_par = 
  {{ 0.1673813, -0.1364463E-01, 0.7520387E-03, -0.2309152E-04, 0.2969341E-06}};
  
  static const parameters south_par = 
  {{0.1561845, -0.1236906E-01, 0.6223816E-03, -0.1734021E-04, 0.2012057E-06}};
  
  sanity_check(arm);	
  
  const parameters& pars = (arm == MUTOO::South) ? south_par : north_par;
  
  // theta in degrees minus the piston angle, should be positive 
  double theta_deg = (arm==MUTOO::South) ?
    MUTOO::RAD_TO_DEG*std::fabs(theta) - SOUTH_PISTON_ANGLE :
    MUTOO::RAD_TO_DEG*std::fabs(theta) - NORTH_PISTON_ANGLE;	
  
  double pt_kick_sta12 = 0.;
  for( unsigned int i=0; i<pars.size(); ++i)
    pt_kick_sta12 += pars.at(i) * std::pow(theta_deg, static_cast<double>( i ) );
  
  return (_mode == NORMAL) ? pt_kick_sta12 : -1.0*pt_kick_sta12;
}

//________________________________________________________________________
// Bend plane z between stations 2 and 3
double TMutBPUtil::get_zbp_sta23(int arm, float theta)
{
  typedef boost::array<double,4> parameters;
  static const parameters north_par = 
  {{460.5233, -2.328246, 0.5208058E-01, -0.1066571E-02}};
  static const parameters south_par = 
  {{-373.5550, 1.841457, -0.3306052E-01, -0.1861432E-03}};		
  
  sanity_check(arm);	
  
  const parameters& pars = (arm == MUTOO::South) ? south_par : north_par;
  
  // theta in degrees minus the piston angle, should be positive 
  //
  double theta_deg = (arm==MUTOO::South) ?
    MUTOO::RAD_TO_DEG*std::fabs(theta) - SOUTH_PISTON_ANGLE :
    MUTOO::RAD_TO_DEG*std::fabs(theta) - NORTH_PISTON_ANGLE;
  
  double zb_sta23 = 0.;
  for(unsigned int i=0; i<pars.size(); ++i) 
    zb_sta23 += pars.at(i) * std::pow(theta_deg,static_cast<double>(i) );
  
  return zb_sta23;
}

//________________________________________________________________________
// Pt kick between stations 2 and 3
double TMutBPUtil::get_pt_kick_sta23(int arm, float theta)
{	
  typedef boost::array<double,5> parameters;
  static const parameters north_par = 
  {{ 0.1818613, -0.2236482E-01, 0.1394652E-02, -0.4559191E-04, 0.5990854E-06}};
  
  static const parameters south_par = 
  {{ 0.1402123, -0.1859624E-01, 0.1233965E-02, -0.4306643E-04, 0.6135089E-06}};
  
  sanity_check(arm);	
  
  const parameters& pars = (arm == MUTOO::South) ? south_par : north_par;
  
  // theta in degrees minus the piston angle, should be positive 
  double theta_deg = (arm==MUTOO::South) ?
    MUTOO::RAD_TO_DEG*std::fabs(theta) - SOUTH_PISTON_ANGLE :
    MUTOO::RAD_TO_DEG*std::fabs(theta) - NORTH_PISTON_ANGLE;
  
  double pt_kick_sta23 = 0.;
  for( unsigned int i=0; i<pars.size(); ++i)
    pt_kick_sta23 += pars.at(i) * std::pow( theta_deg, static_cast<double>(i) );
  
  return (_mode == NORMAL) ? pt_kick_sta23:-pt_kick_sta23;
}

//________________________________________________________________________
void TMutBPUtil::get_bp_window(TMutTrkMap::const_pointer trk_ptr, unsigned short station)
{
  
  static double maskw_the[6] = {10.0, 12.0, 24.0, 10.0, 12.0, 24.0};
  static double maskw_phi[6] = {5.0, 12.0, 25.0, 5.0, 12.0, 25.0};
  static double pzmint = 0.5;
  static double nsigpz = 7.0;
  static double PI = 3.141593;
  
  // Protect against bad input (should not happen)
  if(!trk_ptr->get()) throw std::logic_error(DESCRIPTION("null track pointer"));
  
  // given a track in a station, project to next station forward
  // start with a momentum vector at a station
  // for station-3 use the stubfit, for station-2 use the 2-station bend-plane fit
  
  unsigned short arm = trk_ptr->get()->get_arm();
  int iang = arm*3 + station;
  double theta = 0;
  double phi = 0;
  double phires = 0;
  double theres = 0;
  double sigphi = 0;
  
  double tmp;
  
  if (_verbosity >= MUTOO::SOME ) 
    cout << "get_bp_window: arm,station = " << arm << " " << station << endl;
  
  // Get the station stub pointers
  TMutStubMap::key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
  TMutStubMap::pointer stub_sta1 = 0;
  TMutStubMap::pointer stub_sta2 = 0;
  TMutStubMap::pointer stub_sta3 = 0;
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    if (stub_ptr->get()->get_station() == MUTOO::Station1)			stub_sta1 = stub_ptr;
    else if (stub_ptr->get()->get_station() == MUTOO::Station2) stub_sta2 = stub_ptr;
    else if (stub_ptr->get()->get_station() == MUTOO::Station3) stub_sta3 = stub_ptr;
  }
  
  if (_verbosity >= MUTOO::SOME ) {
    cout << "theta3 = " << stub_sta3->get()->get_theta() << endl;
    if(station == MUTOO::Station1) cout << "theta2 = " << stub_sta2->get()->get_theta() << endl;
  }
  
  // Get the z and kick of the bend planes
  double z_bp_23( get_zbp_sta23(arm, stub_sta3->get()->get_theta()) );
  double pt_kick_23( get_pt_kick_sta23(arm, stub_sta3->get()->get_theta()) );
  
  double z_bp_12 = 0;
  double pt_kick_12 = 0;
  
  if(station==MUTOO::Station2) {
    
    z_bp_12 = get_zbp_sta12(arm, stub_sta3->get()->get_theta());
    pt_kick_12 = get_pt_kick_sta12(arm, stub_sta3->get()->get_theta());
    
  } else if( station==MUTOO::Station1 ) {
    
    z_bp_12 = get_zbp_sta12(arm, stub_sta2->get()->get_theta());
    pt_kick_12 = get_pt_kick_sta12(arm, stub_sta2->get()->get_theta());
    
  }
  
  if (_verbosity >= MUTOO::SOME ) {
    cout << "get_bp_window: z_bp_23,pt_kick_23 = " << z_bp_23 << " " << pt_kick_23 << endl;
    cout << "get_bp_window: z_bp_12,pt_kick_12 = " << z_bp_12 << " " << pt_kick_12 << endl;
  }
  
  // extract some station 3 stub fit parameters
  double x3 = stub_sta3->get()->get_fit_par()->get_x();
  double y3 = stub_sta3->get()->get_fit_par()->get_y();
  double z3 = stub_sta3->get()->get_fit_par()->get_z();
  
  double x3err = sqrt( stub_sta3->get()->get_fit_par()->get_covar(0,0) );
  double y3err = sqrt( stub_sta3->get()->get_fit_par()->get_covar(2,2) );
  double phi3err = sqrt( 1.0/(MUTOO::SQUARE( x3 ) + MUTOO::SQUARE( y3 ))*( MUTOO::SQUARE( y3err ) + MUTOO::SQUARE( y3*x3err ) ) );
  
  double dphi3sq = (x3 != 0.) ?	
    MUTOO::SQUARE(1./(1. + MUTOO::SQUARE(y3/x3))) *
    ( MUTOO::SQUARE( y3err/x3 ) +
    ( MUTOO::SQUARE( x3err*y3/MUTOO::SQUARE(x3))) ) : 0 ;
  
  double rho3 = sqrt(x3*x3 + y3*y3);
  double phi3 = stub_sta3->get()->get_phi();
  double the3 = stub_sta3->get()->get_theta();
  
  //variables for looping over uncertainty in charge and pz:
  double ppz[2], qq[2];
  double phi1n[2], phi2n[2];
  double x2, y2, z2;
  double z1;
  int itry;
  
  // projection from station-3 to station-2 using station-3 stub
  if(station==MUTOO::Station2) {
    
    // get vector at station-3
    double x3p = stub_sta3->get()->get_fit_par()->get_dxdz();
    double y3p = stub_sta3->get()->get_fit_par()->get_dydz();
    double phi3p = (-x3p*sin(phi3)+y3p*cos(phi3))/rho3;
    
    if (_verbosity >= MUTOO::SOME ) {
      cout << "get_bp_window: x3,y3,z3 = " << x3 << " " << y3 << " " << z3 << endl;
      cout << "get_bp_window: rho3,phi3,the3 = " << rho3 << " " << phi3 << " " << the3 << endl;
      cout << "get_bp_window: x3p,y3p,phi3p = " << x3p << " " << y3p << " " << phi3p << endl;
    }
    
    // project to 23 bendplane
    double x23 = x3 + (z_bp_23 - z3)*x3p;
    double y23 = y3 + (z_bp_23 - z3)*y3p;
    double rho23 = std::sqrt(x23*x23 + y23*y23);
    double the23 = the3;
    double phi23 = std::atan2(y23,x23);
    
    if (_verbosity >= MUTOO::SOME ) 
      cout << "get_bp_window: after projection to bendplane" << endl;
    
    // calculate phi angle after bend
    double phi23p1 = (-x3p*sin(phi23)+y3p*cos(phi23))/rho23;
    
    if (_verbosity >= MUTOO::SOME ) 
      cout << "get_bp_window: after getting phi angle after bend" << endl;
    
    // guess at momentum and charge from phi angle at station-3
    // _mode == NORMAL	& arm
    double chg = 0;
    double pz = 0;
    double phi3pd = MUTOO::RAD_TO_DEG*std::fabs(phi3p);
    
    if (_verbosity >= MUTOO::SOME ) 
      cout << "phi3pd,phi3p,rho3 = " << phi3pd << " " << phi3p << " " << rho3 << endl;
    
    double revbp = ( _mode == NORMAL ) ? 1:-1;
    
    if((phi3p*rho3)!=0) pz = fabs((pt_kick_12+pt_kick_23)/(phi3p*rho3)/1.6);
    else pz = 10000;
    if( arm == MUTOO::South ) pz *= -1;
    
    double dpz( fabs( 0.5*pz ) );
    double abspz( fabs(pz) );
    double signpz( pz/abspz );
    
    if(phi3pd > 0.02) chg = revbp;
    else if(phi3pd < -0.02) chg = -revbp;
    if( arm == MUTOO::South ) chg *= -1;
    
    double chg_trk = trk_ptr->get()->get_trk_par()->get_charge();
    double pz_trk = trk_ptr->get()->get_trk_par()->get_pz();
    
    if (_verbosity >= MUTOO::SOME ) 
      cout << "chg_trk,pz_trk, chg,pz = " << chg_trk << " " << pz_trk << " " << chg << " " << pz << endl;
    
    // *** Put in loop over charge if not known, and uncertainty in p ***
    if(chg != 0) {
      
      // charge is known
      qq[0] = chg;
      qq[1] = chg;
      
      if ((abspz-dpz)>pzmint) ppz[0] = signpz*(abspz-dpz);
      else ppz[0] = signpz*pzmint;
      
      
      if ((abspz+dpz)>pzmint) ppz[1] = signpz*(abspz+dpz);
      else ppz[1] = signpz*pzmint;
      
    } else {
      
      // charge not known
      qq[0] = -1;
      qq[1] = +1;
      
      if ((abspz-dpz)>pzmint) ppz[0] = signpz*(abspz-dpz);
      else ppz[0] = signpz*pzmint;
      ppz[1] = ppz[0];
      
    }
    
    for (itry=0; itry<2; itry++){
      
      // phi angle before bend
      double phi23p2 = asin( (rho23*sin(phi23p1) - (qq[itry]*pt_kick_23)/ppz[itry])/rho23 );
      double x23p = tan(the23)*cos(phi23) - rho23*sin(phi23)*phi23p2;
      double y23p = tan(the23)*sin(phi23) + rho23*cos(phi23)*phi23p2;
      
      if (_verbosity >= MUTOO::SOME ) 
        cout << "get_bp_window: after getting phi angle bend" << endl;
      
      // project to station-2
      unsigned short oct = stub_sta3->get()->get_octant();
      unsigned short hoct = stub_sta3->get()->get_half_octant();
      z2 = TMutGeo::get_cathode_plane_position(arm,station,oct,hoct,0,0).getZ();
      
      // this is the mystery divide by two
      x2 = x23 + (z2 - z_bp_23)*x23p/2;
      y2 = y23 + (z2 - z_bp_23)*y23p/2;
      phi2n[itry] = std::atan2(y2,x2);
      
    }
    
    double the2 = the3;
    
    // compare to stub position for evaluation
    if( fabs(phi2n[0]-phi2n[1]) > PI && phi2n[0]*phi2n[1] < 0.) {
      
      // near discontinuity between +pi and -pi
      phi = (phi2n[0]+phi2n[1])/2. + PI;
      if(phi > PI) phi = phi - 2*PI;
      tmp = fabs( fabs(phi2n[0]-phi2n[1]) - 2*PI )/2.;
      
    } else {
      
      // not near discontinuity
      phi = (phi2n[0]+phi2n[1])/2.;
      tmp = fabs( fabs(phi2n[0]-phi2n[1]) )/2.;
      
    }
    
    theta = the2;
    phires = fabs(maskw_phi[iang]/(z2*tan(theta)));
    theres = fabs(maskw_the[iang]/(z2*cos(theta)));
    
    sigphi = sqrt( 
      MUTOO::SQUARE( tmp ) + 
      MUTOO::SQUARE( phires ) +
      MUTOO::SQUARE( phi3err ) 
      );
    
    if (_verbosity >= MUTOO::SOME )
      cout 
      << "TMutBPUtil::get_bp_window -"
      << " sigphi=" << sigphi
      << " tmp=" << tmp 
      << " phires=" << phires 
      << " phi3err=" << phi3err
      << endl; 
    
    // projection from station-2 to station-1 using station-2 & 3 bend-plane fit
    
  } else if(station==MUTOO::Station1) {
    
    // get vector at station-2 from station-2 & 3 bend-plane fit
    double x2 = trk_ptr->get()->get_trk_par()->get_x();
    double y2 = trk_ptr->get()->get_trk_par()->get_y();
    double z2 = trk_ptr->get()->get_trk_par()->get_z(); 
    
    double phi2 = std::atan2(y2,x2);
    double rho2 = std::sqrt(x2*x2+y2*y2);
    double the2 = std::atan2(rho2,fabs(z2));
    double x2p = trk_ptr->get()->get_trk_par()->get_px()/trk_ptr->get()->get_trk_par()->get_pz();
    double y2p = trk_ptr->get()->get_trk_par()->get_py()/trk_ptr->get()->get_trk_par()->get_pz(); 
    
    // or from the station-2 stub
    double x2a = stub_sta2->get()->get_fit_par()->get_x(); //??
    double y2a = stub_sta2->get()->get_fit_par()->get_y();
    double z2a = stub_sta2->get()->get_fit_par()->get_z();
    double x2aerr = sqrt(stub_sta2->get()->get_fit_par()->get_covar(0,0)); //??
    double y2aerr = sqrt(stub_sta2->get()->get_fit_par()->get_covar(2,2));
    double dphi2sq = (x2a == 0) ?	 0:
      MUTOO::SQUARE(1./(1.+ MUTOO::SQUARE(y2a/x2a))) * (
      MUTOO::SQUARE(y2aerr/x2a) +
      MUTOO::SQUARE(x2aerr*y2/MUTOO::SQUARE(x2a))
      );
    
    double phi2a = stub_sta2->get()->get_phi();
    double the2a = stub_sta2->get()->get_theta();
    double x2pa = stub_sta2->get()->get_fit_par()->get_dxdz();
    double y2pa = stub_sta2->get()->get_fit_par()->get_dydz();
    
    if (_verbosity >= MUTOO::SOME ) {
      cout << "get_bp_window: x2,y2,z2 = " << x2 << " " << y2 << " " << z2 << endl;
      cout << "get_bp_window: rho2,phi2,the2 = " << rho2 << " " << phi2 << " " << the2 << endl;
      cout << "get_bp_window: x2p,y2p = " << x2p << " " << y2p << endl;
      cout << "get_bp_window: x2a,y2a,z2a = " << x2a << " " << y2a << " " << z2a << endl;
      cout << "get_bp_window: phi2a,the2a = " << phi2a << " " << the2a << endl;
      cout << "get_bp_window: x2pa,y2pa = " << x2pa << " " << y2pa << endl;
    }
    
    // project to 12 bendplane
    double x12 = x2 + (z_bp_12 - z2)*x2p;
    double y12 = y2 + (z_bp_12 - z2)*y2p;
    double rho12 = std::sqrt(x12*x12 + y12*y12);
    double the12 = the2;
    double phi12 = std::atan2(y12,x12);
    
    // calculate phi angle after bend
    double phi12p1 = (-x2p*sin(phi12)+y2p*cos(phi12))/rho12;
    
    // get best guess at charge and pz
    double chg = trk_ptr->get()->get_trk_par()->get_charge();
    double pz = trk_ptr->get()->get_trk_par()->get_pz();
    double dpz = trk_ptr->get()->get_bp_par()->get_err_pz_st2();
    dpz *= nsigpz;
    double abspz = fabs(pz);
    double signpz = pz/abspz;
    
    if(chg != 0) {
      
      // charge is known
      qq[0] = chg;
      qq[1] = chg;
      
      if ((abspz-dpz)>pzmint) ppz[0] = signpz*(abspz-dpz);
      else ppz[0] = signpz*pzmint;
      
      if ((abspz+dpz)>pzmint) ppz[1] = signpz*(abspz+dpz);
      else ppz[1] = signpz*pzmint;
      
    } else {
      
      // charge not known
      qq[0] = -1;
      qq[1] = +1;
      
      if ((abspz-dpz)>pzmint) ppz[0] = signpz*(abspz-dpz);
      else ppz[0] = signpz*pzmint;
      
      ppz[1] = ppz[0];
      
    }
    
    
    if (_verbosity >= MUTOO::SOME ) 
      cout << "chg,pz = " << chg << " " << pz << endl;
    
    for (int itry=0; itry<2; itry++){
      
      // phi angle before bend
      double phi12p2 = asin( (rho12*sin(phi12p1) - (qq[itry]*pt_kick_12)/ppz[itry])/rho12 );
      double x12p = tan(the12)*cos(phi12) - rho12*sin(phi12)*phi12p2;
      double y12p = tan(the12)*sin(phi12) + rho12*cos(phi12)*phi12p2;
      
      // project to station-1
      unsigned short oct = stub_sta2->get()->get_octant();
      unsigned short hoct = stub_sta2->get()->get_half_octant();
      z1 = TMutGeo::get_cathode_plane_position(arm,station,oct,hoct, MUTOO::Gap3,MUTOO::Cathode1).getZ();
      
      // this is the mystery divide by two
      double x1 = x12 + (z1 - z_bp_12)*x12p/2;
      double y1 = y12 + (z1 - z_bp_12)*y12p/2;
      phi1n[itry] = std::atan2(y1,x1);
      
    }
    
    double the1 = the2;
    
    // If using bend-plane results, calculate average phi and error 
    if( fabs( phi1n[0]-phi1n[1] ) > PI && phi1n[0]*phi1n[1] < 0.) {
      
      // near discontinuity between +pi and -pi
      phi = (phi1n[0]+phi1n[1])/2. + PI;
      if(phi > PI) phi = phi - 2*PI;
      tmp = fabs( fabs( phi1n[0]-phi1n[1] ) - 2*PI )/2.;
      
    } else {
      
      // not near discontinuity
      phi = (phi1n[0]+phi1n[1])/2.;
      tmp = fabs( phi1n[0]-phi1n[1] )/2.;
      
    }
    
    //	Simpler but more robust phi calculation using stub fit results
    //	from stations 2 and 3 (this is what is used in old-framework:
    if( fabs((double)(phi3-phi2a)) > PI && phi3*phi2a < 0.) {
      
      // near discontinuity between +pi and -pi
      if ((phi3-phi2a)>0.) phi = phi2a + (z1-z2)*(phi3-phi2a-(2.*PI))/(z3-z2);
      else phi = phi2a + (z1-z2)*(phi3-phi2a+(2.*PI))/(z3-z2);
      
    } else phi = phi2a + (z1-z2)*(phi3-phi2a)/(z3-z2);
    
    theta = the1;
    
    double tmp2 = sqrt( 
      MUTOO::SQUARE(1-((z1-z2)/(z3-z2)))*dphi2sq + 
      MUTOO::SQUARE((z1-z2)/(z3-z2))*dphi3sq);
    
    phires = fabs(maskw_phi[iang]/(z1*(float)tan((double)theta)));
    theres = fabs(maskw_the[iang]/(z1*(float)cos((double)theta)));
    
    sigphi = sqrt( 
      MUTOO::SQUARE( tmp ) + 
      MUTOO::SQUARE( phires ) + 
      MUTOO::SQUARE( tmp2 ));
    
  }
  
  // Define phi interval around extrapolated point
  double delphi = sigphi;
  double delthe = theres;
  
  TMutStubFinder::stub_window _theta_window;
  TMutStubFinder::stub_window _phi_window;
  _phi_window = std::make_pair(phi - delphi,
    phi + delphi);
  
  // Write the phi window data to the track object
  trk_ptr->get()->set_phi_min(station,_phi_window.first);
  trk_ptr->get()->set_phi_max(station,_phi_window.second);
  
  // Define theta window
  _theta_window = std::make_pair(theta - delthe, theta + delthe);
  
  // Write the theta window data to the track object
  trk_ptr->get()->set_theta_min(station,_theta_window.first);
  trk_ptr->get()->set_theta_max(station,_theta_window.second);
  
  return;
  
}

//________________________________________________________
boost::array<double,3> TMutBPUtil::get_mom_vtx(const PHPoint& vtx_point, TMutTrkMap::const_pointer trk_ptr)
{
  unsigned short arm = trk_ptr->get()->get_arm();
  
  // Constants for e-loss through the absorber. "cu"
  // represents the copper nosecone and "fe" represents 
  // the central magnet poleface
  const double zincu	= (arm == MUTOO::North) ? 40:-40;
  const double zoutcu = (arm == MUTOO::North) ? 60:-60;
  const double zinfe	= (arm == MUTOO::North) ? 60:-60;
  const double zoutfe = (arm == MUTOO::North) ? 120:-120;
  const double dedxcu = 12.9e-3;
  const double fudge = 1.15;
  
  // Fit Momentum at Station 1
  //
  double x = trk_ptr->get()->get_trk_par()->get_x();
  double y = trk_ptr->get()->get_trk_par()->get_y();
  double z = trk_ptr->get()->get_trk_par()->get_z();
  double px = trk_ptr->get()->get_trk_par()->get_px();
  double py = trk_ptr->get()->get_trk_par()->get_py();
  double pz = trk_ptr->get()->get_trk_par()->get_pz();
  double ptot = trk_ptr->get()->get_trk_par()->get_ptot();
  double xp = px/pz;
  double yp = py/pz;
  
  // Projecting Station 1 Momentum vector to the m.s. b.p.
  //
  const double zms = (arm == MUTOO::North ) ? 85:-85;
  double xms = x + xp*(zms - z);
  double yms = y + yp*(zms - z);
  
  // Location of the Vertex
  //
  double xvtx = vtx_point.getX();
  double yvtx = vtx_point.getY();
  double zvtx = vtx_point.getZ();
  
  // Slopes of line between vtx point and mult. scat. point
  // at the vertex
  double xpv = (xms - xvtx)/(zms - zvtx);
  double ypv = (yms - yvtx)/(zms - zvtx);
  
  // Calculating path length through copper nosecone. Assume 
  // effective m.s. z is inside of fe and at larger z than
  // nosecone.
  //
  double xin = xvtx + xpv*(zincu - zvtx);
  double yin = yvtx + ypv*(zincu - zvtx);
  double xout = xvtx + xpv*(zoutcu - zvtx);
  double yout = yvtx + ypv*(zoutcu - zvtx);
  double loss_cu = std::sqrt( (xout - xin)*(xout - xin) +
    (yout - yin)*(yout - yin) +
    (zoutcu - zincu)*(zoutcu - zincu) );
  
  // Iron now
  xin = xvtx + xpv*(zinfe - zvtx);
  yin = yvtx + ypv*(zinfe - zvtx);
  xout = xvtx + xpv*(zms - zvtx);
  yout = yvtx + ypv*(zms - zvtx);
  double loss_fe = std::sqrt( (xout - xin)*(xout - xin) +
    (yout - yin)*(yout - yin) +
    (zms - zinfe)*(zms - zinfe) );
  
  xin = x + xp*(zms - z);
  yin = y + yp*(zms - z);
  xout = x + xp*(zoutfe - z);
  yout = y + yp*(zoutfe - z);
  loss_fe += std::sqrt( (xout - xin)*(xout - xin) +
    (yout - yin)*(yout - yin) +
    (zoutfe - zms)*(zoutfe - zms) );
  
  double decu = loss_cu * dedxcu;
  double defe = loss_fe * dedxcu;
  double de = fudge*(decu + defe);
  
  // Neglecting Muon mass, changing the total momentum
  //
  ptot += de;
  
  // Calculate the new Vertex Momentum Values
  //
  double pz_v = ptot;
  if (pz<0) pz_v = -ptot;
  
  double px_v = xpv*pz_v;
  double py_v = ypv*pz_v;
  double pnew = std::sqrt(px_v*px_v + py_v*py_v + pz_v*pz_v);
  
  boost::array<double,3> vtx_mom = 
  {
    {
      ptot/pnew * px_v,
      ptot/pnew * py_v,
      ptot/pnew * pz_v
    }
  };
  return vtx_mom;
}
  
//________________________________________________________
int TMutBPUtil::get_distcls(
  const PHPoint& xx1, 
  const PHPoint& xx2, 
  const double tanx1[2], 
  const double tanx2[2],
  PHPoint& x1c, PHPoint& x2c)
  
  // This routine calculates the postions on two lines between
  // which the distance is the shortest
  // inputs:
  //	 x1[3] and x2[3] are the coordinates on any point on line 1
  //	 and 2, respectively. tanx1[2] and tanx2[2] are the tanx and
  //	 tany for the two lines
  // outputs:
  //	 x1c[3] and x2c[3] are the points on the two lines for which
  //	 the distance is the shortest
  //	 ifail is nonzero if the matrix inversion failed
  
{
  double b[6], c[6];
  
  double x1[3] = {xx1.getX(),xx1.getY(),xx1.getZ()};
  double x2[3] = {xx2.getX(),xx2.getY(),xx2.getZ()};
  
  x1c.setX(0);
  x2c.setX(0);
  x1c.setY(0);
  x2c.setY(0);
  x1c.setZ(0);
  x2c.setZ(0);
  
  PHGslMatrix n( 6, 6 );
  
  // fill e matrix
  PHGslMatrix e( 6, 6 );
  e( 0, 0 ) = tanx1[0];
  e( 1, 0 ) = tanx2[0];
  e( 3, 0 ) = 1;
  
  e( 0, 1 ) = -e( 0, 0 );
  e( 1, 1 ) = -e( 1, 0 );
  e( 5, 1 ) = 1;
  
  e( 0, 2 ) = tanx1[1];
  e( 1, 2 ) = tanx2[1];
  e( 2, 2 ) = 1;
  
  e( 0, 3 ) = -e( 0, 2 );
  e( 1, 3 ) = -e( 1, 2 );
  e( 4, 3 ) = 1;
  e( 0, 4 ) = 1;
  e( 1, 4 ) = 1;
  e( 2, 4 ) = -e( 0, 2 );
  e( 3, 4 ) = -e( 0, 0 );
  e( 0, 5 ) = -1;
  e( 1, 5 ) = -1;
  e( 4, 5 ) = -e( 1, 2 );
  e( 5, 5 ) = -e( 1, 0 );
  
  try {
    PHGslMatrix n( e.invert() );	 
    b[0] = 0;
    b[1] = 0;
    b[2] = x1[1] - tanx1[1] * x1[2];
    b[3] = x1[0] - tanx1[0] * x1[2];
    b[4] = x2[1] - tanx2[1] * x2[2];
    b[5] = x2[0] - tanx2[0] * x2[2];
    
    for( int i = 0; i < 6; i++) {
      c[i] = 0;
      for( int j = 0; j < 6; j++) 
      { c[i] += n( i, j) * b[j];	}
      
    }
    
    x1c.setX(c[0]);
    x2c.setX(c[1]);
    x1c.setY(c[2]);
    x2c.setY(c[3]);
    x1c.setZ(c[4]);
    x2c.setZ(c[5]);
    return 0;
    
  } catch( exception &e ) { cout << e.what() << endl; }
  
  return -1;
}

