#ifndef __MMUTBPVERTEXPAR_HH__
#define __MMUTBPVERTEXPAR_HH__

#include<PHObject.h>
#include<TMutParBase.h>

/*! 
Runtime parameter object for mMutBPVertex analysis module
*/
class mMutBPVertexPar : public TMutParBase
{
  
 public: 

  /*! default constructor */
  mMutBPVertexPar()
  {;}
  
  /*! destructor */
  ~mMutBPVertexPar()
  {;}
  
  /*! PHOOL inteface requirement */
  void identify( std::ostream& os = std::cout ) const 
  { os << "mMutBPVertexPar";}
  
  // ADD GETTERS AND SETTERS FOR RUNTIME PARAMETERS HERE
  //

 private:  

  // ADD RUNTIME PARAMETERS HERE
  //
  
};

#endif /* __MMUTBPVERTEXPAR_HH__ */

