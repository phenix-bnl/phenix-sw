#include<TMutEval_v1.hh>
#include<TDataType.h>

ClassImp(TMutEval)
ClassImp(TMutEval_v1)

TMutEval_v1::TMutEval_v1() :
  _arm(0),
  _octant(0),
  _index(0)
{;}

TMutEval_v1::TMutEval_v1(const Key& key,
			 UShort_t arm,
			 UShort_t octant,
			 UShort_t index) :
  TMutEval(key),
  _arm(arm),
  _octant(octant),
  _index(index)
{;}

TMutEval_v1::TMutEval_v1(const TMutEval* base_ptr):
  TMutEval(*base_ptr),
  _arm(base_ptr->get_arm()),
  _octant(base_ptr->get_octant()),
  _index(base_ptr->get_index()),
  _eval_res_list(*(base_ptr->get_eval_res_list())),
  _trk_eval(*(base_ptr->get_trk_eval())){;}

TMutEval_v1::TMutEval_v1(const TMutEval& base_ref):
  TMutEval(base_ref),
  _arm(base_ref.get_arm()),
  _octant(base_ref.get_octant()),
  _index(base_ref.get_index()),
  _eval_res_list(*(base_ref.get_eval_res_list())),
  _trk_eval(*(base_ref.get_trk_eval())){;}

void 
TMutEval_v1::print(std::ostream& os, bool max) const 
{
  MUTOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << " octant: " << _octant 
     << " index: " << _index << std::endl;
  _trk_eval.print(os);
  MUTOO::PRINT(os,"**");
}




