#include<TMuiEval.hh>
#include<TDataType.h>

ClassImp(TMuiEval)
ClassImp(TMuiEval_v1)

TMuiEval_v1::TMuiEval_v1() :
  _arm(0),
  _index(0)
{;}

TMuiEval_v1::TMuiEval_v1(const Key& key,
			 UShort_t arm,
			 UShort_t index) :
  TMuiEval(key),
  _arm(arm),
  _index(index)
{;}

TMuiEval_v1::TMuiEval_v1(const TMuiEval* base_ptr):
  TMuiEval(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index()),
  _eval_res_list(*(base_ptr->get_eval_res_list())),
  _road_eval(*(base_ptr->get_road_eval())){;}

TMuiEval_v1::TMuiEval_v1(const TMuiEval& base_ref):
  TMuiEval(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index()),
  _eval_res_list(*(base_ref.get_eval_res_list())),
  _road_eval(*(base_ref.get_road_eval())){;}

void 
TMuiEval_v1::print(std::ostream& os, bool max) const 
{
  MUIOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << " index: " << _index << std::endl;
  _road_eval.print(os);
  MUIOO::PRINT(os,"**");
}




