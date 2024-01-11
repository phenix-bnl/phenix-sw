// $Id: TRpcTrk_v1.cxx,v 1.2 2008/08/28 00:50:25 kempel Exp $

/*!
	\file TRpcTrk_v1.cxx
	\brief The RPC Track object 
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:25 $
*/

#include "TRpcTrk_v1.h"
#include "RPCOO.h"

#include <iomanip>

using namespace std;

//_______________________________________
TRpcTrk_v1::TRpcTrk_v1() :
  _arm(0),
  _index(0),
  _chi_square(0),
  _ndf(0),
  _hit_pattern(0),
  _status(0)
{}

//_______________________________________
TRpcTrk_v1::TRpcTrk_v1(const Key& key, UShort_t arm, UShort_t index) :
  TRpcTrk(key),
  _arm(arm),
  _index(index),
  _chi_square(0),
  _ndf(0),
  _hit_pattern(0),
  _status(0)
{}

//_______________________________________
TRpcTrk_v1::TRpcTrk_v1(const TRpcTrk* base_ptr) :
  TRpcTrk(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index()),
  _trk_par(*(base_ptr->get_trk_par())),
  _trk_par_vtx(*(base_ptr->get_trk_par_vtx())),
  _trk_par_list(*(base_ptr->get_trk_par_list())),
  _fit_par(*(base_ptr->get_fit_par())),
  _chi_square(base_ptr->get_chi_square()),
  _ndf(base_ptr->get_ndf()),
  _hit_pattern(base_ptr->get_hit_pattern()),
  _status(base_ptr->get_status())
{}

//_______________________________________
TRpcTrk_v1::TRpcTrk_v1(const TRpcTrk& base_ref) :
  TRpcTrk(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index()),
  _trk_par(*(base_ref.get_trk_par())),
  _trk_par_vtx(*(base_ref.get_trk_par_vtx())),
  _trk_par_list(*(base_ref.get_trk_par_list())),
  _fit_par(*(base_ref.get_fit_par())),
  _chi_square(base_ref.get_chi_square()),
  _ndf(base_ref.get_ndf()),
  _hit_pattern(base_ref.get_hit_pattern()),
  _status(base_ref.get_status())
{}

//_______________________________________
void TRpcTrk_v1::print(std::ostream& os, bool max) const
{
  RPCOO::PRINT(os,GetName());
  
  os 
    << " key: " << get_key().get_obj_key() 
    << " arm: " << _arm << "     ";

  // dump track parameters
  os << " track par = {";

  os << setw(5) << setprecision(3) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);  

  os << _trk_par.get_x() << ", ";
  os << _trk_par.get_y() << ", ";
  os << _trk_par.get_px() << ", ";
  os << _trk_par.get_py() << ", ";
  os << _trk_par.get_pz() << "}" << std::endl;    
  os << " ptot:" << _trk_par.get_ptot() << "   z ref: " << _trk_par.get_z() << "  ";  
  os << " chi2 (full):" << get_chi_square() << " ";
  os << " ndf:" << get_ndf() << " ";
  os << " charge: " << get_charge() << std::endl;
  
  os << " TRpcCoord:" << get_n_coord()  << " ";

	// reco parameters
	os << " n_trk_par - " << get_n_trk_par() << std::endl; 

  if(max) {

    // dump track parameter list
    trk_par_list::const_iterator trk_list_iter = _trk_par_list.begin();
  
    for(;trk_list_iter!=_trk_par_list.end();++trk_list_iter) {
      os << " track par = {";
      os << trk_list_iter->get_x() << ", ";
      os << trk_list_iter->get_y() << ", ";
      os << trk_list_iter->get_px() << ", ";
      os << trk_list_iter->get_py() << ", ";
      os << trk_list_iter->get_pz() << "}" << "   ";
      os << " z reference: " << trk_list_iter->get_z() << std::endl;  
    }
    
  
  	// dump fit parameters
  	os << " fit parameters = {";
  	os << "x:" << _fit_par.get_x() << ", ";
  	os << "y:" << _fit_par.get_y() << ", ";
  	os << "dxdz: " << _fit_par.get_dxdz() << ", ";
  	os << "dydz: " << _fit_par.get_dydz() << "}" << std::endl;    
	    
    // dump vtx track parameters
    os << " track parameters vertex = {";
    os << "x:" << _trk_par_vtx.get_x() << ", ";
    os << "y:" << _trk_par_vtx.get_y() << ", ";
    os << "px: " << _trk_par_vtx.get_px() << ", ";
    os << "py: " << _trk_par_vtx.get_py() << ", ";
    os << "pz: " << _trk_par_vtx.get_pz() << "}" << std::endl;    
    os << " ptot: = " << _trk_par_vtx.get_ptot() << "   z reference: " << _trk_par_vtx.get_z() << std::endl;  
    
    os << std::endl;

  }    
	
  RPCOO::PRINT(os,"**");
}

