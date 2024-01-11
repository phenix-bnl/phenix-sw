#ifndef __MFVTXFINDSVXCLUSTERSPAR_HH__
#define __MFVTXFINDSVXCLUSTERSPAR_HH__

#include<PHObject.h>
#include<FVTXOO.h>
#include<TFvtxParBase.h>

class mFvtxFindSvxClustersPar : public TFvtxParBase
{
  
public:
  
  // default constructor
  
  mFvtxFindSvxClustersPar() : 
    _do_evaluation(false)
  {
  }

  ~mFvtxFindSvxClustersPar() {}

  // print method
  void print(std::ostream& out = std::cout) const {
    FVTXOO::PRINT(out, "mFvtxFindSvxClustersPar");
    out << "do_evaluation = " << _do_evaluation << std::endl;
    FVTXOO::PRINT(out, "***");
  }

  void set_do_evaluation(const bool val) { _do_evaluation = val; }

  bool get_do_evaluation() const { return _do_evaluation; }

private:

  bool _do_evaluation;

  ClassDef(mFvtxFindSvxClustersPar,1);
};

#endif /* __MFVTXFINDSVXCLUSTERSPAR_HH__ */
