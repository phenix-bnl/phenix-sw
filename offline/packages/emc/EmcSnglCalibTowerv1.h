#ifndef __EMCSNGLCALIBTOWERV1_H__
#define __EMCSNGLCALIBTOWERV1_H__

#include "PHObject.h"

/** (OLD) Kind of interim object between STAF times and Fun4All times.
Relevant object is now emcTowerContainer / emcTowerContent
@ingroup deprecated 
*/

class EmcSnglCalibTowerv1 : public TObject
{

 public:
  EmcSnglCalibTowerv1();
  virtual ~EmcSnglCalibTowerv1() {}

  short get_id() const {return id;}
  void set_id(const short ival) {id = ival; return;}

  short get_type() const {return type;}
  void set_type(const short ival) {type = ival; return;}

  int get_deadmap() const {return deadmap;}
  void set_deadmap(const int ival) {deadmap = ival; return;}

  int get_hwkey() const {return hwkey;}
  void set_hwkey(const int ival) {hwkey = ival; return;}

  int get_index() const {return index;}
  void set_index(const int ival) {index = ival; return;}

  int get_swkey() const {return swkey;}
  void set_swkey(const int ival) {swkey = ival; return;}

  int get_warnmap() const {return warnmap;}
  void set_warnmap(const int ival) {warnmap = ival; return;}


  float get_adc() const {return adc;}
  void set_adc(const float rval) {adc = rval; return;}

  float get_ecal() const {return ecal;}
  void set_ecal(const float rval) {ecal = rval; return;}

  float get_tac() const {return tac;}
  void set_tac(const float rval) {tac = rval; return;}

  float get_tof() const {return tof;}
  void set_tof(const float rval) {tof = rval; return;}


 protected:
  short id;
  short type;

  int deadmap;
  int hwkey;
  int index;
  int swkey;
  int warnmap;

  float adc;
  float ecal;
  float tac;
  float tof;

   ClassDef(EmcSnglCalibTowerv1,1)
};

#endif /*__EMCSNGLCALIBTOWERV1_H__*/
