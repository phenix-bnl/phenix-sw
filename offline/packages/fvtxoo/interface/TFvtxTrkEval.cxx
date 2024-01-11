#include <iostream>
#include <TFvtxTrkEval.h>
#include <FVTXOO.h>

ClassImp(TFvtxTrkEval);

TFvtxTrkEval::TFvtxTrkEval():
  _n_total_true_hits(0),
  _n_masked_true_hits(0),
  _n_reco_true_hits(0),
  _n_reco_ghost_hits(0),
  _best_track(0),
  _charge_true(0),
  _charge_reco(0),
  _px_true_vx(0),
  _py_true_vx(0),
  _pz_true_vx(0),
  _ptot_true_vx(0),
  _px_reco_vx(0),
  _py_reco_vx(0),
  _pz_reco_vx(0),
  _ptot_reco_vx(0),
  _px_true_us(0),
  _py_true_us(0),
  _pz_true_us(0),
  _ptot_true_us(0),
  _px_reco_us(0),
  _py_reco_us(0),
  _pz_reco_us(0),
  _ptot_reco_us(0),
  _px_true_ds(0),
  _py_true_ds(0),
  _pz_true_ds(0),
  _ptot_true_ds(0),
  _px_reco_ds(0),
  _py_reco_ds(0),
  _pz_reco_ds(0),
  _ptot_reco_ds(0)
  {;}

void TFvtxTrkEval::print(std::ostream& os) const
{

  FVTXOO::PRINT(os,"global");

  std:: cout << " total hits true " << get_n_total_true_hits() << std::endl;
  std:: cout << " total true hits found " << get_n_reco_true_hits() << std::endl;
  std:: cout << " total ghost hits found " << get_n_reco_ghost_hits() << std::endl;
  std:: cout << " best track flag " << get_best_track() << std::endl;
  os << " charge true: " << get_charge_true() << std::endl;
  os << " charge reco: " << get_charge_reco() << std::endl;

  FVTXOO::PRINT(os,"vertex");

  os << " p_true = {" 
     << get_px_true_vx() << ", "
     << get_py_true_vx() << ", "
     << get_pz_true_vx() << "}" << " ptot: " << get_ptot_true_vx() << std::endl;
  
  os << " p_reco = {" 
     << get_px_reco_vx() << ", "
     << get_py_reco_vx() << ", "
     << get_pz_reco_vx() << "}" << " ptot: " << get_ptot_reco_vx() << std::endl;
  
  os << " dp/p: " << get_delta_ptot_vx() << std::endl;
  os << " dpx/px: " << get_delta_px_vx() << std::endl;
  os << " dpy/py: " << get_delta_py_vx() << std::endl;
  os << " dpz/pz: " << get_delta_pz_vx() << std::endl;
  

  FVTXOO::PRINT(os,"upstream gap");

  os << " p_true = {" 
     << get_px_true_us() << ", "
     << get_py_true_us() << ", "
     << get_pz_true_us() << "}" << " ptot: " << get_ptot_true_us() << std::endl;
  
  os << " p_reco = {" 
     << get_px_reco_us() << ", "
     << get_py_reco_us() << ", "
     << get_pz_reco_us() << "}" << " ptot: " << get_ptot_reco_us() << std::endl;

  os << " dp/p: " << get_delta_ptot_us() << std::endl;
  os << " dpx/px: " << get_delta_px_us() << std::endl;
  os << " dpy/py: " << get_delta_py_us() << std::endl;
  os << " dpz/pz: " << get_delta_pz_us() << std::endl;

  FVTXOO::PRINT(os,"downstream gap");

  os << " p_true = {" 
     << get_px_true_ds() << ", "
     << get_py_true_ds() << ", "
     << get_pz_true_ds() << "}" << " ptot: " << get_ptot_true_ds() << std::endl;
  
  os << " p_reco = {" 
     << get_px_reco_ds() << ", "
     << get_py_reco_ds() << ", "
     << get_pz_reco_ds() << "}" << " ptot: " << get_ptot_reco_ds() << std::endl;

  os << " dp/p: " << get_delta_ptot_ds() << std::endl;
  os << " dpx/px: " << get_delta_px_ds() << std::endl;
  os << " dpy/py: " << get_delta_py_ds() << std::endl;
  os << " dpy/py: " << get_delta_pz_ds() << std::endl;

}
















