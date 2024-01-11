/*!
	\file TRpcMuoTrk_v1.cxx
	\brief The RPC Track object 
	\author Richard Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2012/04/03 18:47:21 $
*/

#include "TRpcMuoTrk_v1.h"
#include "RPCOO.h"

#include <iomanip>

using namespace std;

//_______________________________________
TRpcMuoTrk_v1::TRpcMuoTrk_v1() :
  _arm(0),
  _index(0)
{
  _muo_trk_number = -9999;
  _muo_trk_px = -9999;
  _muo_trk_py = -9999;
  _muo_trk_pz = -9999;

  for(int irpc=0 ; irpc<2 ; irpc++)
    {
      for(int imutr=0 ; imutr<2 ; imutr++)
	{
	  _extrap_trk_hit_x[imutr][irpc] = 0;
	  _extrap_trk_hit_y[imutr][irpc] = 0;
	  _dca_trk[imutr][irpc] = 0;
	  _rpc_clus_time[imutr][irpc] = 0;
	  _rpc_clus_hit_pos[imutr][irpc] = 0;
	}
      
      _extrap_vtx_hit_x[irpc] = 0;
      _extrap_vtx_hit_y[irpc] = 0;
      _extrap_muid_hit_x[irpc] = 0;
      _extrap_muid_hit_y[irpc] = 0;
      _dca_vtx[irpc] = 0;
      _dca_muid[irpc] = 0;
      _rpc_clus_vtx_time[irpc] = 0;
      _rpc_clus_muid_time[irpc] = 0;
      _rpc_clus_vtx_hit_pos[irpc] = 0;
      _rpc_clus_muid_hit_pos[irpc] = 0; 
    }
}

//_______________________________________
TRpcMuoTrk_v1::TRpcMuoTrk_v1(const Key& key, UShort_t arm, UShort_t index) :
  TRpcMuoTrk(key),
  _arm(arm),
  _index(index)
{
  _muo_trk_number = -9999;
  _muo_trk_px = -9999;
  _muo_trk_py = -9999;
  _muo_trk_pz = -9999;

  for(int irpc=0 ; irpc<2 ; irpc++)
    {
      for(int imutr=0 ; imutr<2 ; imutr++)
	{
	  _extrap_trk_hit_x[imutr][irpc] = 0;
	  _extrap_trk_hit_y[imutr][irpc] = 0;
	  _dca_trk[imutr][irpc] = 0;
	  _rpc_clus_time[imutr][irpc] = 0;
	  _rpc_clus_hit_pos[imutr][irpc] = 0;
	}
      
      _extrap_vtx_hit_x[irpc] = 0;
      _extrap_vtx_hit_y[irpc] = 0;
      _extrap_muid_hit_x[irpc] = 0;
      _extrap_muid_hit_y[irpc] = 0;
      _dca_vtx[irpc] = 0;
      _dca_muid[irpc] = 0;
      _rpc_clus_vtx_time[irpc] = 0;
      _rpc_clus_muid_time[irpc] = 0;
      _rpc_clus_vtx_hit_pos[irpc] = 0;
      _rpc_clus_muid_hit_pos[irpc] = 0; 
    }  
}

//_______________________________________
TRpcMuoTrk_v1::TRpcMuoTrk_v1(const TRpcMuoTrk* base_ptr) :
  TRpcMuoTrk(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index())
{
  _muo_trk_number = base_ptr->get_muo_trk_number();
  _muo_trk_px = base_ptr->get_muo_trk_momentum(0);
  _muo_trk_py = base_ptr->get_muo_trk_momentum(1);
  _muo_trk_pz = base_ptr->get_muo_trk_momentum(2);

  for(int irpc=0 ; irpc<2 ; irpc++)
    {
      for(int imutr=0 ; imutr<2 ; imutr++)
	{
	  _extrap_trk_hit_x[imutr][irpc] = base_ptr->get_trk_extrapolated_hit_x(imutr,irpc);
	  _extrap_trk_hit_y[imutr][irpc] = base_ptr->get_trk_extrapolated_hit_y(imutr,irpc);
	  _dca_trk[imutr][irpc] = base_ptr->get_dca_trk(imutr,irpc);
	  _rpc_clus_time[imutr][irpc] = base_ptr->get_rpcclus_time(imutr,irpc);
	  _rpc_clus_hit_pos[imutr][irpc] = base_ptr->get_rpcclus_hitpos(imutr,irpc);
	}
      
      _extrap_vtx_hit_x[irpc] = base_ptr->get_vtx_extrapolated_hit_x(irpc);
      _extrap_vtx_hit_y[irpc] = base_ptr->get_vtx_extrapolated_hit_y(irpc);
      _extrap_muid_hit_x[irpc] = base_ptr->get_muid_extrapolated_hit_x(irpc);
      _extrap_muid_hit_y[irpc] = base_ptr->get_muid_extrapolated_hit_y(irpc);
      _dca_vtx[irpc] = base_ptr->get_dca_vtx(irpc);
      _dca_muid[irpc] = base_ptr->get_dca_muid(irpc);
      _rpc_clus_vtx_time[irpc] = base_ptr->get_rpcclus_time_vtx(irpc);
      _rpc_clus_muid_time[irpc] = base_ptr->get_rpcclus_time_muid(irpc);
      _rpc_clus_vtx_hit_pos[irpc] = base_ptr->get_rpcclus_hitpos_vtx(irpc);
      _rpc_clus_muid_hit_pos[irpc] = base_ptr->get_rpcclus_hitpos_muid(irpc);
    }
}

//_______________________________________
TRpcMuoTrk_v1::TRpcMuoTrk_v1(const TRpcMuoTrk& base_ref) :
  TRpcMuoTrk(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index())
{
  _muo_trk_number = base_ref.get_muo_trk_number();
  _muo_trk_px = base_ref.get_muo_trk_momentum(0);
  _muo_trk_py = base_ref.get_muo_trk_momentum(1);
  _muo_trk_pz = base_ref.get_muo_trk_momentum(2);

  for(int irpc=0 ; irpc<2 ; irpc++)
    {
      for(int imutr=0 ; imutr<2 ; imutr++)
	{
	  _extrap_trk_hit_x[imutr][irpc] = base_ref.get_trk_extrapolated_hit_x(imutr,irpc);
	  _extrap_trk_hit_y[imutr][irpc] = base_ref.get_trk_extrapolated_hit_y(imutr,irpc);
	  _dca_trk[imutr][irpc] = base_ref.get_dca_trk(imutr,irpc);
	  _rpc_clus_time[imutr][irpc] = base_ref.get_rpcclus_time(imutr,irpc);
	  _rpc_clus_hit_pos[imutr][irpc] = base_ref.get_rpcclus_hitpos(imutr,irpc);
	}
      
      _extrap_vtx_hit_x[irpc] = base_ref.get_vtx_extrapolated_hit_x(irpc);
      _extrap_vtx_hit_y[irpc] = base_ref.get_vtx_extrapolated_hit_y(irpc);
      _extrap_muid_hit_x[irpc] = base_ref.get_muid_extrapolated_hit_x(irpc);
      _extrap_muid_hit_y[irpc] = base_ref.get_muid_extrapolated_hit_y(irpc);
      _dca_vtx[irpc] = base_ref.get_dca_vtx(irpc);
      _dca_muid[irpc] = base_ref.get_dca_muid(irpc);
      _rpc_clus_vtx_time[irpc] = base_ref.get_rpcclus_time_vtx(irpc);
      _rpc_clus_muid_time[irpc] = base_ref.get_rpcclus_time_muid(irpc);
      _rpc_clus_vtx_hit_pos[irpc] = base_ref.get_rpcclus_hitpos_vtx(irpc);
      _rpc_clus_muid_hit_pos[irpc] = base_ref.get_rpcclus_hitpos_muid(irpc);
    }
}

//_______________________________________
void TRpcMuoTrk_v1::print(std::ostream& os, bool max) const
{
  /*  RPCOO::PRINT(os,GetName());
  
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
  */
  RPCOO::PRINT(os,"**");
}

