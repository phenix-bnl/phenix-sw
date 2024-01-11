#include<TFvtxEval_v1.h>
#include<TDataType.h>

ClassImp(TFvtxEval)
ClassImp(TFvtxEval_v1)

TFvtxEval_v1::TFvtxEval_v1() :
  _arm(0),
  _cage(0),
  _index(0)
{;}

TFvtxEval_v1::TFvtxEval_v1(const Key& key,
			 unsigned short arm,
			 unsigned short cage,
			 unsigned short index) :
  TFvtxEval(key),
  _arm(arm),
  _cage(cage),
  _index(index)
{;}

TFvtxEval_v1::TFvtxEval_v1(const TFvtxEval* base_ptr):
  TFvtxEval(*base_ptr),
  _arm(base_ptr->get_arm()),
  _cage(base_ptr->get_cage()),
  _index(base_ptr->get_index()),
  _trk_eval(*(base_ptr->get_trk_eval())){;}

TFvtxEval_v1::TFvtxEval_v1(const TFvtxEval& base_ref):
  TFvtxEval(base_ref),
  _arm(base_ref.get_arm()),
  _cage(base_ref.get_cage()),
  _index(base_ref.get_index()),
  _trk_eval(*(base_ref.get_trk_eval())){;}

void 
TFvtxEval_v1::print(std::ostream& os, bool max) const 
{
  FVTXOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << " cage: " << _cage 
     << " index: " << _index << std::endl;
  _trk_eval.print(os);
  FVTXOO::PRINT(os,"**");
}




