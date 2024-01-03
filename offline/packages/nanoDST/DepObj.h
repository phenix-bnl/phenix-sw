#ifndef DEPOBJ_H__
#define DEPOBJ_H__

class DepObj
{

 protected:
  DepObj() {}
  virtual ~DepObj();


 public:
  virtual float get_dep(const int /*charge*/, const float /*mom*/, const float /*ecore*/, const short /*emcsector*/) const {Warning("get_dep()"); return 0;}

  virtual int get_N_meanpar() const {Warning("get_N_meanpar()"); return 0;} 
  virtual int get_N_sigpar() const {Warning("get_N_mompar()"); return 0;}

  virtual void set_meanpar(const int /*icharge*/, const int /*isect*/, const int /*ipar*/, const double /*val*/){Warning("set_meanpar()"); return;}
  virtual void set_sigpar(const int /*icharge*/, const int /*isect*/, const int /*ipar*/, const double /*val*/){Warning("set_sigpar()"); return;}

  virtual float get_meanpar(const int /*icharge*/, const int /*isect*/, const int /*ipar*/) const {Warning("get_meanpar()"); return 0;}
  virtual float get_sigpar(const int /*icharge*/, const int /*isect*/, const int /*ipar*/) const {Warning("get_sigpar()"); return 0;}

  static DepObj *instance();

protected:
  void Warning(const char* func) const;

  static  DepObj *__instance; 

};

#endif /*  DEPOBJ_H__ */
