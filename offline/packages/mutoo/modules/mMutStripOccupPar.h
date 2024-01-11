#ifndef __MMUTSTRIPOCCUPPAR_HH__
#define __MMUTSTRIPOCCUPPAR_HH__

#include <MUTOO.h>
#include <PHObject.h>
#include <TMutParBase.h>

/*! 
Runtime parameter object for mMutStripOccup analysis module
*/
class mMutStripOccupPar : public TMutParBase
{
 public:
  /*! default constructor */
  mMutStripOccupPar():
    _switch_on(0),
    _charge_cut(0)
    {}

  /*! destructor */
  ~mMutStripOccupPar()
    {}

  int get_switch_on() const { return _switch_on;}
  float get_charge_cut() const { return _charge_cut;}
  void set_switch_on(int switch_on) { _switch_on = switch_on;}
  void set_charge_cut( float charge_cut) { _charge_cut = charge_cut;}
 private:
  
  unsigned short _switch_on;
  float _charge_cut;
};

#endif /* __MMUTSTRIPOCCUPPAR_HH__*/









