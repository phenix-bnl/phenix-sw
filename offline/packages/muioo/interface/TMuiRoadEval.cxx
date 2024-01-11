#include<TMuiRoadEval.hh>
#include<MUIOO.h>

ClassImp(TMuiRoadEval)

TMuiRoadEval::TMuiRoadEval():
  _n_true_hits(0),
  _n_masked_hits(0),
  _n_reco_true_hits(0),
  _n_reco_ghost_hits(0),
  _px_true_vx(0),
  _py_true_vx(0),
  _pz_true_vx(0),
  _ptot_true_vx(0)
{;}

void TMuiRoadEval::print(std::ostream& os) const
{

  MUIOO::PRINT(os,"TMuiRoadEval");

  std:: cout << " total hits true " << get_n_true_hits() << std::endl;
  std:: cout << " total true hits found " << get_n_reco_true_hits() << std::endl;	     
  std:: cout << " total ghost hits found " << get_n_reco_ghost_hits() << std::endl;
  std:: cout << " total masked hits found " << get_n_masked_hits() << std::endl;
	     
  MUIOO::PRINT(os,"vertex");

  os << " p_true = {" 
     << get_px_true_vx() << ", "
     << get_py_true_vx() << ", "
     << get_pz_true_vx() << "}" << " ptot: " 
     << get_ptot_true_vx() << std::endl;  

  MUIOO::PRINT(os,"**");
}
















