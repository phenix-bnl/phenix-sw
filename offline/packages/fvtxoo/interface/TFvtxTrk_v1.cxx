// $Id: TFvtxTrk_v1.cxx,v 1.13 2015/04/08 17:26:18 slash Exp $

/*!
  \file TFvtxTrk_v1.cxx
  \brief The Forward Silicon (FVTX) Track object 
  \author M. Brooks
  \version $Revision: 1.13 $
  \date $Date: 2015/04/08 17:26:18 $
*/

#include<TFvtxTrk_v1.h>
#include<TDataType.h>
#include<TFvtxCoordMap.h>
#include<TFvtxSvxClusterMap.h>
#include<TMutTrackUtil.h>
#include<PHGeometry.h>
#include<algorithm>
#include<iomanip>

ClassImp(TFvtxTrk_v1)

using namespace std;

//_______________________________________
TFvtxTrk_v1::TFvtxTrk_v1() :
  _arm(0),
  _index(0),
  _w_chi_square(0),
  _r_chi_square(0),
  _ndf(0),
  _hit_pattern(0),
  _charge(1),
  _status(0)
{
  for (int istation=0; istation<WINDOWS_SIZE; istation++){
      _phi_min[istation] = 0;
      _phi_max[istation] = 0;
      _theta_min[istation]= 0;
      _theta_max[istation] = 0;
  }
}


//_______________________________________
TFvtxTrk_v1::TFvtxTrk_v1(const Key& key,
  const unsigned short& arm,
  const unsigned short& index) :
  TFvtxTrk(key),
  _arm(arm),
  _index(index),
  _w_chi_square(0),
  _r_chi_square(0),
  _ndf(0),
  _hit_pattern(0),
  _charge(1),
  _status(0)
{
  for (int istation=0; istation<WINDOWS_SIZE; istation++){
      _phi_min[istation] = 0;
      _phi_max[istation] = 0;
      _theta_min[istation] = 0;
      _theta_max[istation] = 0;
  }
}

//_______________________________________
TFvtxTrk_v1::TFvtxTrk_v1(const TFvtxTrk* base_ptr) :
  TFvtxTrk(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index()),
  _trk_par(*(base_ptr->get_trk_par())),
  _trk_par_vtx(*(base_ptr->get_trk_par_vtx())),
  _trk_par_mutr(*(base_ptr->get_trk_par_mutr())),
  _trk_par_list(*(base_ptr->get_trk_par_list())),
  _w_residual_list(*(base_ptr->get_w_residual_list())),
  _r_residual_list(*(base_ptr->get_r_residual_list())),
  _w_chi_square(base_ptr->get_w_chi_square()),
  _r_chi_square(base_ptr->get_r_chi_square()),
  _ndf(base_ptr->get_ndf()),
  _hit_pattern(base_ptr->get_hit_pattern()),
  _charge(base_ptr->get_charge()),
  _status(base_ptr->get_status())
{

  for(int i=0;i<WINDOWS_SIZE;++i){
      _phi_min[i]=base_ptr->get_phi_min(i);
      _phi_max[i]=base_ptr->get_phi_max(i);
      _theta_min[i]=base_ptr->get_theta_min(i);
      _theta_max[i]=base_ptr->get_theta_max(i);
  }

}

//_______________________________________
TFvtxTrk_v1::TFvtxTrk_v1(const TFvtxTrk& base_ref) :
  TFvtxTrk(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index()),
  _trk_par(*(base_ref.get_trk_par())),
  _trk_par_vtx(*(base_ref.get_trk_par_vtx())),
  _trk_par_mutr(*(base_ref.get_trk_par_mutr())),
  _trk_par_list(*(base_ref.get_trk_par_list())),
  _w_residual_list(*(base_ref.get_w_residual_list())),
  _r_residual_list(*(base_ref.get_r_residual_list())),
  _w_chi_square(base_ref.get_w_chi_square()),
  _r_chi_square(base_ref.get_r_chi_square()),
  _ndf(base_ref.get_ndf()),
  _hit_pattern(base_ref.get_hit_pattern()),
  _charge(base_ref.get_charge()),
  _status(base_ref.get_status())
{

  for(int i=0;i<WINDOWS_SIZE;++i){
      _phi_min[i]=base_ref.get_phi_min(i);
      _phi_max[i]=base_ref.get_phi_max(i);
      _theta_min[i]=base_ref.get_theta_min(i);
      _theta_max[i]=base_ref.get_theta_max(i);
  }

}

//_______________________________________
void TFvtxTrk_v1::print(std::ostream& os, bool max) const
{
  FVTXOO::PRINT(os,GetName());

  os
    << " key: " << get_key().get_obj_key()
    << " arm: " << _arm << "          ";

  // Dump the status
  //
  os << " status: ";
  if(get_no_fit()) os << "NO_FIT ";
  if(get_kalman_fit()) os << "KALMAN_FIT ";
  if(get_kalman_fail()) os << "KALMAN_FAIL ";
  if(get_reco_success()) os << "RECO_SUCCESS ";
  if(get_reco_min_hits()) os << "RECO_MIN_HITS ";
  if(get_ghost()) os << "GHOST ";
  os << std::endl;

  // dump track parameters
  //
  os << " track par = {";

  os << setw(5) << setprecision(3) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);

  os << _trk_par.get_x() << ", ";
  os << _trk_par.get_y() << ", ";
  os << _trk_par.get_px() << ", ";
  os << _trk_par.get_py() << ", ";
  os << _trk_par.get_pz() << "}" << std::endl;
  os << " ptot:" << _trk_par.get_ptot() << "	 z ref: " << _trk_par.get_z() << "	";
  os << " chi_w (full):" << get_w_chi_square() << " ";
  os << " ndf:" << get_ndf() << " ";
  os << " charge: " << get_charge() << std::endl;

  os << " TFvtxCoord:" << get_associated<TFvtxCoord>().count()	<< " ";
  os << " TFvtxSvxCluster:" << get_associated<TFvtxSvxCluster>().count() << " ";

  // reco parameters
  os << " n_trk_par - " << _trk_par_list.size() << std::endl;

  if(max) {

    // dump track parameter list
    //
    trk_par_list::const_iterator trk_list_iter = _trk_par_list.begin();

    for(;trk_list_iter!=_trk_par_list.end();++trk_list_iter) 
    {
      os << " track par = {";
      os << trk_list_iter->get_x() << ", ";
      os << trk_list_iter->get_y() << ", ";
      os << trk_list_iter->get_px() << ", ";
      os << trk_list_iter->get_py() << ", ";
      os << trk_list_iter->get_pz() << "}" << "	 ";
      os << " z reference: " << trk_list_iter->get_z() << std::endl;
    }

    // dump vtx track parameters
    //
    os << " track parameters vertex = {";
    os << "x:" << _trk_par_vtx.get_x() << ", ";
    os << "y:" << _trk_par_vtx.get_y() << ", ";
    os << "px: " << _trk_par_vtx.get_px() << ", ";
    os << "py: " << _trk_par_vtx.get_py() << ", ";
    os << "pz: " << _trk_par_vtx.get_pz() << "}" << std::endl;
    os << " ptot: = " << _trk_par_vtx.get_ptot() << "	 z reference: " << _trk_par_vtx.get_z() << std::endl;

    os << std::endl;

    // extrapolate the track to gap0 MUID and dump coordinate
    //

    if(max)
    {
      // dump residuals
      //
      os << " residuals" << std::endl;
      residual_list::const_iterator residual_iter = _w_residual_list.begin();
      for(;residual_iter != _w_residual_list.end(); ++residual_iter)
      {
        residual_iter->print();
      }
    }
  }
  os << std::fixed;
  FVTXOO::PRINT(os,"**");
}

//_______________________________________
double TFvtxTrk_v1::get_w_chi_square_pdf() const 
{ 
  int ndf( get_ndf() );
  return (ndf > 0 ) ? _w_chi_square/ndf:0;
}

//_______________________________________
double TFvtxTrk_v1::get_r_chi_square_pdf() const 
{ 
  int ndf( get_ndf() );
  return (ndf > 0 ) ? _r_chi_square/ndf:0;
}

//_______________________________________
unsigned short TFvtxTrk_v1::get_hit_pattern() const
{
  if (_hit_pattern) return _hit_pattern;
  unsigned short hit_pattern = 0;
  TFvtxCoordMap::const_key_iterator coord_iter = get_associated<TFvtxCoord>();
  while( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {	    
      // calculate plane index from the coordinate
      unsigned short index = 
	(1-coord_ptr->get()->get_arm()+
	 coord_ptr->get()->get_sector())%2 + 
	2 * (coord_ptr->get()->get_station() );
      // update the pattern
      hit_pattern |= ( 1 << index );
    }
  return hit_pattern; 
}
