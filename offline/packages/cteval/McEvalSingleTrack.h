#ifndef MCEVALSINGLETRACK_H
#define MCEVALSINGLETRACK_H

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class RecoEvalSingleList;
class RecoEvalSingleTrack;

class McEvalSingleTrack : public PHObject {
public:
  McEvalSingleTrack() {};
  McEvalSingleTrack(McEvalSingleTrack *track) {};
  virtual ~McEvalSingleTrack() {};

  virtual void set_eventid(int val) = 0;
  virtual void set_mctrackid(int val) = 0;
  virtual void set_generation(int val) = 0;
  virtual void set_particleid(int val) = 0;
  virtual void set_parentid(int val) = 0;
  virtual void set_primaryid(int val) = 0;
  virtual void set_vertexx(float val) = 0;
  virtual void set_vertexy(float val) = 0;
  virtual void set_vertexz(float val) = 0;
  virtual void set_parentvertexx(float val) = 0;
  virtual void set_parentvertexy(float val) = 0;
  virtual void set_parentvertexz(float val) = 0;
  virtual void set_primaryvertexx(float val) = 0;
  virtual void set_primaryvertexy(float val) = 0;
  virtual void set_primaryvertexz(float val) = 0;
  virtual void set_momentumx(float val) = 0;
  virtual void set_momentumy(float val) = 0;
  virtual void set_momentumz(float val) = 0;
  virtual void set_parentmomentumx(float val) = 0;
  virtual void set_parentmomentumy(float val) = 0;
  virtual void set_parentmomentumz(float val) = 0;
  virtual void set_primarymomentumx(float val) = 0;
  virtual void set_primarymomentumy(float val) = 0;
  virtual void set_primarymomentumz(float val) = 0;
  virtual void set_quality(int val) = 0;
  virtual void set_momentumr(double val) = 0;
  virtual void set_theta0(double val) = 0;
  virtual void set_phi0(double val) = 0;
  virtual void set_phi(double val) = 0;
  virtual void set_alpha(double val) = 0;
  virtual void set_zed(double val) = 0;
  virtual void set_beta(double val) = 0;
  virtual void set_RecoEvalSingleList(RecoEvalSingleList *list) = 0;

  virtual void add_RecoEvalSingleTrack(RecoEvalSingleTrack *track) = 0;

  virtual int get_eventid() = 0;
  virtual int get_mctrackid() = 0;
  virtual int get_generation() = 0;
  virtual int get_particleid() = 0;
  virtual int get_parentid() = 0;
  virtual int get_primaryid() = 0;
  virtual float get_vertexx() = 0;
  virtual float get_vertexy() = 0;
  virtual float get_vertexz() = 0;
  virtual float get_parentvertexx() = 0;
  virtual float get_parentvertexy() = 0;
  virtual float get_parentvertexz() = 0;
  virtual float get_primaryvertexx() = 0;
  virtual float get_primaryvertexy() = 0;
  virtual float get_primaryvertexz() = 0;
  virtual float get_momentumx() = 0;
  virtual float get_momentumy() = 0;
  virtual float get_momentumz() = 0;
  virtual float get_parentmomentumx() = 0;
  virtual float get_parentmomentumy() = 0;
  virtual float get_parentmomentumz() = 0;
  virtual float get_primarymomentumx() = 0;
  virtual float get_primarymomentumy() = 0;
  virtual float get_primarymomentumz() = 0;
  virtual int get_quality() = 0;
  virtual double get_momentumr() = 0;
  virtual double get_theta0() = 0;
  virtual double get_phi0() = 0;
  virtual double get_phi() = 0;
  virtual double get_alpha() = 0;
  virtual double get_zed() = 0;
  virtual double get_beta() = 0;
  virtual RecoEvalSingleList *get_RecoEvalSingleList() = 0;

protected:
  virtual void Init() {};
  ClassDef(McEvalSingleTrack, 1)
};

#endif /*__MCEVALSINGLETRACK_H*/
