#ifndef __MMUIEVALOPAR_H__
#define __MMUIEVALOPAR_H__

#include<MUIOO.h>
#include<PHObject.h>
#include<TMuiParBase.h>

class mMuiEvalOPar : public TMuiParBase
{
 public:
  /*! Default constructor */

  mMuiEvalOPar() : 
    _pr_mode(NORMAL){}

  /*! Destructor */ ~mMuiEvalOPar() {;}

  /*! Pattern recognition mode */
  enum PRMode {PERFECT,NORMAL};
  /*! Pattern recognition mode */
  PRMode get_pr_mode() const { return _pr_mode; } 
  /*! Pattern recognition mode */
  void set_pr_mode(PRMode mode){_pr_mode = mode;}

 private:

  PRMode _pr_mode;

};

#endif /* __MMUIEVALOPAR_H__*/













