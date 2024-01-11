#ifndef __MMUISLOWSIMPAR_HH__
#define __MMUISLOWSIMPAR_HH__

#include<PHObject.h>
#include<TMuiParBase.h>

/*! 
Runtime parameter object for mMuiSlowSim analysis module
*/
class mMuiSlowSimPar : public TMuiParBase
{
  
 public: 

  /*! default constructor */
  mMuiSlowSimPar(){;}
  
  /*! destructor */
  ~mMuiSlowSimPar(){;}
  
  /*! PHOOL inteface requirement */
  void identify( std::ostream& os = std::cout ) const 
  { os << "mMuiSlowSimPar";}
  
  // ADD GETTERS AND SETTERS FOR RUNTIME PARAMETERS HERE
  //
  
 private:  
  
  // ADD RUNTIME PARAMETERS HERE
  //
  
};

#endif /* __MMUISLOWSIMPAR_HH__ */







