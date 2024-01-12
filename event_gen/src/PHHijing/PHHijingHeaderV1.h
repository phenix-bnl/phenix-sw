#ifndef __PHHIJINGHEADERV1_H__
#define __PHHIJINGHEADERV1_H__

#include <phool.h>
#include <PHObject.h>
#include <PHHijingHeader.h>

class PHHijingHeaderV1 : public PHHijingHeader
{
public:

  PHHijingHeaderV1();
  virtual ~PHHijingHeaderV1() {}

  /// event number
  virtual Int_t GetEvt() const { return event; }

  /// number of particles in event
  virtual Int_t GetNpart() const { return npart; }

  // A of target
  virtual int GetATarg() const { return atarg; }

  // Z of target
  virtual int GetZTarg() const { return ztarg; }

  // Projectile A
  virtual int GetAProj() const { return aproj; }

  // Projectile Z
  virtual int GetZProj() const { return zproj; }

  // sqrt(s_nn) of collision
  virtual float GetCollisionE() const { return ecoll; }

  // Impact Parameter
  virtual float GetBimpact() const { return bimpact; }

  // Number of binary collisions
  virtual int GetNbinary() const { return nbin; }

  virtual void SetEvt(const Int_t Evt) { event = Evt; }
  virtual void SetNpart(const Int_t Npart) { npart = Npart; }
  virtual void SetATarg(const int val) { atarg = val; }
  virtual void SetZTarg(const int val) { ztarg = val; }
  virtual void SetAProj(const int val) { aproj = val; }
  virtual void SetZProj(const int val) { zproj = val; }
  virtual void SetCollisionE(const float val) { ecoll = val; }
  virtual void SetNbinary(const int val) { nbin = val; }
  virtual void SetBimpact(const float val) { bimpact = val; }

  // PHObject methods
  int isValid() const { return 1; }
  void Reset();
  void identify(std::ostream& os=std::cout) const;
  void print(std::ostream& os=std::cout) const;

private:

  int event;
  int npart;
  int atarg;
  int ztarg;
  int aproj;
  int zproj;
  float ecoll;
  int nbin;
  float bimpact;

  ClassDef (PHHijingHeaderV1,1)
};

#endif	// __PHPYTHIAHEADER_H__
