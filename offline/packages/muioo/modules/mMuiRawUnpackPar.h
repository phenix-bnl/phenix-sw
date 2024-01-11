#ifndef __MMUIRAWUNPACKPAR_HH__
#define __MMUIRAWUNPACKPAR_HH__

#include<PHObject.h>
#include<MUIOO.h>
#include<TMuiParBase.h>

//!  Runtime parameter object for mMuiRawUnpack analysis module
/*! 
*/
class mMuiRawUnpackPar : public TMuiParBase
{
  
 public: 
  
  /*! default constructor */
  mMuiRawUnpackPar() : 
    _id_base(12001)
    {
      _module_id[0] = 0x0000;
      _module_id[1] = 0x0010;
      _module_id[2] = 0x1000;
      _module_id[3] = 0x1010;
    }
  
  /*! destructor */
  ~mMuiRawUnpackPar(){;}
    
  /*! Minimum cluster width */
  unsigned int get_id_base() const {return _id_base;} 
  
  /*! Maximum cluster width */
  unsigned int get_module_id(int dcm) const {return _module_id[dcm];} 
  
  /*! Minimum cluster width */
  void set_id_base(unsigned int val) {_id_base = val;}
  
  /*! Maximum cluster width */
  void set_module_id(int dcm, unsigned int val) {_module_id[dcm] = val;}
  
 private:  
  
  unsigned int _id_base;
  unsigned int _module_id[MUIOO::kFEMsTotal];

};

#endif /* __MMUIRAWUNPACKPAR_HH__ */





