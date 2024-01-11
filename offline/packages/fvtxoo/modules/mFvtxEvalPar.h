#ifndef __mFvtxEvalPar_HH__
#define __mFvtxEvalPar_HH__

#include<MUTOO.h>
#include<PHObject.h>
#include<TMutParBase.h>

/*! 
Runtime parameter object for mFvtxEvalPar analysis module
*/
class mFvtxEvalPar : public TMutParBase
{
 public:
  /*! Default constructor */

  mFvtxEvalPar() : 
    _pr_mode(NORMAL){}

  /*! Destructor */ ~mFvtxEvalPar() {;}

  /*! Pattern recognition mode */
  enum PRMode {PERFECT,NORMAL};
  /*! Pattern recognition mode */
  PRMode get_pr_mode() const { return _pr_mode; } 
  /*! Pattern recognition mode */
  void set_pr_mode(PRMode mode){_pr_mode = mode;}

 private:

  PRMode _pr_mode;

	ClassDef(mFvtxEvalPar, 1);
};

#endif /* __mFvtxEvalPar_HH__*/













