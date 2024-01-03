// $Id: PHMuoTracksOut.C,v 1.10 2011/04/04 18:07:13 bbannier Exp $

/*!
	\file PHMuoTracksOut.C
	\brief base class for Dimuon nanoDST container
*/

#include <stdexcept>
#include <sstream>

#include <PHMuoTracksOut.h>


ClassImp(PHMuoTracksOut)
using namespace std;

static int shutup = 0;

//________________________________________________________________________
TObject* PHMuoTracksOut::GetSingleParticle(unsigned int /*ipart*/)
{
  cout << PHWHERE << "GetSingleParticle not implemented by your version" << endl;
  return 0;
}

//________________________________________________________________________
TObject* PHMuoTracksOut::GetSingleDimuon(unsigned int /*ipart*/)
{
  cout << PHWHERE << "GetSingleDimuon not implemented by your version" << endl;
  return 0;
}

//________________________________________________________________________
void PHMuoTracksOut::AddPHParticle(unsigned int /*ipart*/, TObject *)
{
  cout << PHWHERE << "AddPHParticle(i,TObject*) not implemented by your version" << endl;
  
}

//________________________________________________________________________
void PHMuoTracksOut::AddPHDimuon(unsigned int /*ipart*/, TObject *)
{ cout << PHWHERE << "AddPHDimuon(i,TObject*) not implemented by your version" << endl; }

//________________________________________________________________________
PHMuoTracksOut* PHMuoTracksOut::clone() const
{
  cout << PHWHERE << "clone() not implemented by your version" << endl;
  return 0;
}
  
/*
Following are the base class implementation of the accessors and modifiers needed to store missing information needed for event mixing
in MWG track object. These implementations overwrite some (largely unused) existing members of the track object.
For recent enough versions of MWG track objects, these methods are re-implemented using dedicated variables.
*/

//________________________________________________________________________
unsigned int PHMuoTracksOut::get_n_primitives( const unsigned int itrk ) const
{ 
  unsigned int count(0);
  for( int i = 0; i<5; i++ )
  {
    if( get_muID_proj_hit_dist(i, 1, 0, itrk) == 8888 ) count++;
    else break;
  }
  
  return count;
}

//________________________________________________________________________
void PHMuoTracksOut::set_n_primitives( const unsigned int /*itrk*/, const unsigned int& /*value */)
{ return; }
  
//________________________________________________________________________
double PHMuoTracksOut::get_level2_phi( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 0, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_phi - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 1, itrk);
  
}

//________________________________________________________________________
void PHMuoTracksOut::set_level2_phi( const unsigned int itrk, const unsigned int&i, const double& value )
{ 
  set_muID_proj_hit_dist(i, 1, 0, itrk, 8888 );
  set_muID_proj_hit_dist( i, 0, 1, itrk, value);
  return;
}  
  
//________________________________________________________________________
double PHMuoTracksOut::get_level2_theta( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 0, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_theta - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 0, itrk);
  
}

//________________________________________________________________________
void PHMuoTracksOut::set_level2_theta( const unsigned int itrk, const unsigned int& i, const double& value )
{ 
  set_muID_proj_hit_dist(i, 1, 0, itrk, 8888 );
  set_muID_proj_hit_dist( i, 0, 0, itrk, value);
  return; 
}  

//________________________________________________________________________
unsigned int PHMuoTracksOut::get_n_mutr_primitives( const unsigned int itrk ) const
{ 
  unsigned int count(0);
  for( int i = 0; i<5; i++ )
  {
    if( get_muID_proj_hit_dist(i, 1, 2, itrk) == 8888 ) count++;
    else break;
  }
  
  return count;
}

//________________________________________________________________________
void PHMuoTracksOut::set_n_mutr_primitives( const unsigned int /*itrk*/, const unsigned int& /*value */)
{ return; }

//________________________________________________________________________
void PHMuoTracksOut::set_level2_pmin_x( const unsigned int itrk, const unsigned int& i, const double& value )
{
  set_muID_proj_hit_dist(i, 1, 2, itrk, 8888.0);
  set_muID_proj_hit_dist(i, 0, 2, itrk, value);
  return; 
}

//________________________________________________________________________
void PHMuoTracksOut::set_level2_pmin_y( const unsigned int itrk, const unsigned int& i, const double& value )
{
  set_muID_proj_hit_dist(i, 1, 2, itrk, 8888);
  set_muID_proj_hit_dist(i, 0, 3, itrk, value);
  return; 
}

//________________________________________________________________________
void PHMuoTracksOut::set_level2_pmin_z( const unsigned int itrk, const unsigned int& i, const double& value )
{
  set_muID_proj_hit_dist(i, 1, 2, itrk, 8888.0);
  set_muID_proj_hit_dist(i, 0, 4, itrk, value);
  return; 
}

//________________________________________________________________________
double PHMuoTracksOut::get_level2_pmin_x( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 2, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_pmin_x - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 2, itrk);
  
}

//________________________________________________________________________
double PHMuoTracksOut::get_level2_pmin_y( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 2, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_pmin_y - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 3, itrk);
  
}


//________________________________________________________________________
double PHMuoTracksOut::get_level2_pmin_z( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 2, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_pmin_z - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 4, itrk);
  
}
  
//________________________________________________________________________
void PHMuoTracksOut::set_level2_pmax_x( const unsigned int itrk, const unsigned int& i, const double& value )
{
  set_muID_proj_hit_dist(i, 1, 2, itrk, 8888.0);
  set_muID_proj_hit_dist(i, 0, 5, itrk, value);
  return; 
}

//________________________________________________________________________
void PHMuoTracksOut::set_level2_pmax_y( const unsigned int itrk, const unsigned int& i, const double& value )
{
  set_muID_proj_hit_dist(i, 1, 2, itrk, 8888.0);
  set_muID_proj_hit_dist(i, 0, 6, itrk, value);
  return; 
}

//________________________________________________________________________
void PHMuoTracksOut::set_level2_pmax_z( const unsigned int itrk, const unsigned int& i, const double& value ) 
{
  set_muID_proj_hit_dist(i, 1, 2, itrk, 8888.0);
  set_muID_proj_hit_dist(i, 0, 7, itrk, value);
  return; 
}

//________________________________________________________________________
double PHMuoTracksOut::get_level2_pmax_x( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 2, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_pmax_x - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 5, itrk);
  
}

//________________________________________________________________________
double PHMuoTracksOut::get_level2_pmax_y( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 2, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_pmax_y - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 6, itrk);
  
}


//________________________________________________________________________
double PHMuoTracksOut::get_level2_pmax_z( const unsigned int itrk, const unsigned int& i ) const
{ 
  
  if( get_muID_proj_hit_dist(i, 1, 2, itrk) != 8888 ) 
  {
    cout << "PHMuoTracksOut::get_level2_pmax_z - Data for track " << itrk << ", primitive " << i << " was not filled." << endl;
    return 1.e7;
  }
		
  return get_muID_proj_hit_dist( i, 0, 7, itrk);
  
}

//________________________________________________________________________
double PHMuoTracksOut::get_event_vertex_z( const unsigned int itrk ) const
{ 
  if( get_px(3,itrk) != 8888 )
  {
    cout << "PHMuoTracksOut::get_event_vertex_z - Data for track " << itrk << " was not filled." << endl;
    return 1e7;
  }
  
  return get_py(3, itrk ); 
}
  
//________________________________________________________________________
double PHMuoTracksOut::get_event_vertex_z_error( const unsigned int itrk ) const
{ 
  if( get_px(3,itrk) != 8888 )
  {
    cout << "PHMuoTracksOut::get_event_vertex_z - Data for track " << itrk << " was not filled." << endl;
    return 1e7;
  }
  
  return get_pz(3, itrk ); 
}
  
//________________________________________________________________________
void PHMuoTracksOut::set_event_vertex_z( const unsigned int itrk, const double& value )
{ 
  set_px( 3, itrk, 8888 );
  set_py( 3, itrk, value );
  return; 
}

//________________________________________________________________________
void PHMuoTracksOut::set_event_vertex_z_error( const unsigned int itrk, const double& value )
{ 
  set_px( 3, itrk, 8888 );
  set_pz( 3, itrk, value );
  return; 
}

//________________________________________________________________________
double PHMuoTracksOut::get_event_rp_angle( const unsigned int itrk, const unsigned int& arm ) const
{ 
  
  if( get_delta_q_error(arm, itrk) != 8888 )
  { 
    ostringstream what;
    what << "PHMuoTracksOut::get_event_rp_angle - Data for track " << itrk << " was not filled." << endl;
    throw std::runtime_error( what.str() );
  }
  
  return get_delta_q( arm, itrk );
}
 
//________________________________________________________________________
void PHMuoTracksOut::set_event_rp_angle( const unsigned int itrk, const unsigned int& arm, const double& value )
{ 
  set_delta_q_error(arm, itrk, 8888);
  set_delta_q( arm, itrk, value );
  return; 
}



void
PHMuoTracksOut::ShutUp(const int i)
{
  shutup = i;
  PHParticle::ShutUp(i);
}

void
PHMuoTracksOut::warning(const char *what) const
{
  if (!shutup)
    {
      cout << PHWHERE << "using virtual function, doing nothing" << endl;
      cout << "Offending field == " << what << endl;
    }
  return ;
}
