#ifndef MCEVALSINGLETRACK_v2_H
#define MCEVALSINGLETRACK_v2_H

#include "PHObject.h"
#include "McEvalSingleTrack.h"

class RecoEvalSingleList;
class RecoEvalSingleTrack;

class McEvalSingleTrack_v2 : public McEvalSingleTrack
{
public:
  McEvalSingleTrack_v2();
  McEvalSingleTrack_v2(McEvalSingleTrack *track);  
  virtual ~McEvalSingleTrack_v2();

  void set_eventid(int val) {EVENTID = val;}
  void set_mctrackid(int val) {MCTRACKID = val;}
  void set_generation(int val) {GENERATION = val;}
  void set_particleid(int val) {PARTICLEID = val;}
  void set_parentid(int val) {PARENTID = val;}
  void set_primaryid(int val) {PRIMARYID = val;}
  void set_vertexx(float val) {VERTEXX = val;}
  void set_vertexy(float val) {VERTEXY = val;}
  void set_vertexz(float val) {VERTEXZ = val;}
  void set_parentvertexx(float val) {PARENTVERTEXX = val;}
  void set_parentvertexy(float val) {PARENTVERTEXY = val;}
  void set_parentvertexz(float val) {PARENTVERTEXZ = val;}
  void set_primaryvertexx(float val) {PRIMARYVERTEXX = val;}
  void set_primaryvertexy(float val) {PRIMARYVERTEXY = val;}
  void set_primaryvertexz(float val) {PRIMARYVERTEXZ = val;}
  void set_momentumx(float val) {MOMENTUMX = val;}
  void set_momentumy(float val) {MOMENTUMY = val;}
  void set_momentumz(float val) {MOMENTUMZ = val;}
  void set_parentmomentumx(float val) {PARENTMOMENTUMX = val;}
  void set_parentmomentumy(float val) {PARENTMOMENTUMY = val;}
  void set_parentmomentumz(float val) {PARENTMOMENTUMZ = val;}
  void set_primarymomentumx(float val) {PRIMARYMOMENTUMX = val;}
  void set_primarymomentumy(float val) {PRIMARYMOMENTUMY = val;}
  void set_primarymomentumz(float val) {PRIMARYMOMENTUMZ = val;}
  void set_quality(int val) {QUALITY = val;}
  void set_momentumr(double val) {MOMENTUMR = val;}
  void set_theta0(double val) {THETA0 = val;}
  void set_phi0(double val) {PHI0 = val;}
  void set_phi(double val) {PHI = val;}
  void set_alpha(double val) {ALPHA = val;}
  void set_zed(double val) {ZED = val;}
  void set_beta(double val) {BETA = val;}
  void set_RecoEvalSingleList(RecoEvalSingleList *list) 
    {RecoList = list;}

  void add_RecoEvalSingleTrack(RecoEvalSingleTrack *track);

  int get_eventid() {return EVENTID;}
  int get_mctrackid() {return MCTRACKID;}
  int get_generation() {return GENERATION;}
  int get_particleid() {return PARTICLEID;}
  int get_parentid() {return PARENTID;}
  int get_primaryid() {return PRIMARYID;}
  float get_vertexx() {return VERTEXX;}
  float get_vertexy() {return VERTEXY;}
  float get_vertexz() {return VERTEXZ;}
  float get_parentvertexx() {return PARENTVERTEXX;}
  float get_parentvertexy() {return PARENTVERTEXY;}
  float get_parentvertexz() {return PARENTVERTEXZ;}
  float get_primaryvertexx() {return PRIMARYVERTEXX;}
  float get_primaryvertexy() {return PRIMARYVERTEXY;}
  float get_primaryvertexz() {return PRIMARYVERTEXZ;}
  float get_momentumx() {return MOMENTUMX;}
  float get_momentumy() {return MOMENTUMY;}
  float get_momentumz() {return MOMENTUMZ;}
  float get_parentmomentumx() {return PARENTMOMENTUMX;}
  float get_parentmomentumy() {return PARENTMOMENTUMY;}
  float get_parentmomentumz() {return PARENTMOMENTUMZ;}
  float get_primarymomentumx() {return PRIMARYMOMENTUMX;}
  float get_primarymomentumy() {return PRIMARYMOMENTUMY;}
  float get_primarymomentumz() {return PRIMARYMOMENTUMZ;}
  int get_quality() {return QUALITY;}
  double get_momentumr() {return MOMENTUMR;}
  double get_theta0() {return THETA0;}
  double get_phi0() {return PHI0;}
  double get_phi() {return PHI;}
  double get_alpha() {return ALPHA;}
  double get_zed() {return ZED;}
  double get_beta() {return BETA;}
  RecoEvalSingleList* get_RecoEvalSingleList() {return RecoList;}

protected:

  void Init();

  int EVENTID;
  int MCTRACKID;
  int GENERATION;
  int PARTICLEID, PARENTID, PRIMARYID;
  float VERTEXX, PARENTVERTEXX, PRIMARYVERTEXX;
  float VERTEXY, PARENTVERTEXY, PRIMARYVERTEXY;
  float VERTEXZ, PARENTVERTEXZ, PRIMARYVERTEXZ;
  float MOMENTUMX, PARENTMOMENTUMX, PRIMARYMOMENTUMX;
  float MOMENTUMY, PARENTMOMENTUMY, PRIMARYMOMENTUMY;
  float MOMENTUMZ, PARENTMOMENTUMZ, PRIMARYMOMENTUMZ;
  int QUALITY;
  double MOMENTUMR;
  double THETA0, PHI0;
  double PHI, ALPHA;
  double ZED, BETA;

  RecoEvalSingleList *RecoList;

  ClassDef(McEvalSingleTrack_v2,1)
};

#endif /*__MCEVALSINGLETRACK_v2_H*/
