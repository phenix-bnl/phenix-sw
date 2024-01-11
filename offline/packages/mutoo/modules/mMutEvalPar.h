#ifndef __MMUTEVALPAR_HH__
#define __MMUTEVALPAR_HH__

#include<MUTOO.h>
#include<PHObject.h>
#include<TMutParBase.h>

/*! 
Runtime parameter object for mMutEval analysis module
*/
class mMutEvalPar : public TMutParBase
{
 public:
  /*! Default constructor */

  mMutEvalPar() : 
    _pr_mode(NORMAL){}

  /*! Destructor */ ~mMutEvalPar() {;}

  /*! Pattern recognition mode */
  enum PRMode {PERFECT,NORMAL};
  /*! Pattern recognition mode */
  PRMode get_pr_mode() const { return _pr_mode; } 
  /*! Pattern recognition mode */
  void set_pr_mode(PRMode mode){_pr_mode = mode;}

 private:

  PRMode _pr_mode;

};

#endif /* __MMUTEVALPAR_HH__*/













