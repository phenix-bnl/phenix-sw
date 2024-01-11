#ifndef __MMUIROADASSOCPAR_HH__
#define __MMUIROADASSOCPAR_HH__

#include<PHObject.h>
#include<TMutParBase.h>

/*! 
Runtime parameter object for mMuiRoadAssoc analysis module
*/
class mMuiRoadAssocPar : public TMutParBase
{
  
 public: 

  /*! default constructor */
  mMuiRoadAssocPar(){;}
  
  /*! destructor */
  ~mMuiRoadAssocPar(){;}
  
  /*! PHOOL inteface requirement */
  void identify( std::ostream& os = std::cout) const 
  { os << "mMuiRoadAssocPar";}
  
  // ADD GETTERS AND SETTERS FOR RUNTIME PARAMETERS HERE
  //

 private:  

  // ADD RUNTIME PARAMETERS HERE
  //
  
};

#endif /* __MMUIROADASSOCPAR_HH__ */







