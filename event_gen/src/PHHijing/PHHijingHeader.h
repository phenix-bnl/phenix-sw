#ifndef __PHHIJINGHEADER_H__
#define __PHHIJINGHEADER_H__

#include <phool.h>
#include <PHObject.h>

class PHHijingHeader : public PHObject
{
public:

  PHHijingHeader();
  virtual ~PHHijingHeader() {}
  /// event number
  virtual Int_t GetEvt() const		{ PHOOL_VIRTUAL_WARNING; return -9999; }

  /// number of particles in event
  virtual Int_t GetNpart() const	{ PHOOL_VIRTUAL_WARNING; return -9999; }

  // A of target
  virtual int GetATarg() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  // Z of target
  virtual int GetZTarg() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  // Projectile A
  virtual int GetAProj() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  // Projectile Z
  virtual int GetZProj() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  // sqrt(s_nn) of collision
  virtual float GetCollisionE() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  // Impact Parameter
  virtual float GetBimpact() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  // Number of binary collisions
  virtual int GetNbinary() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  // Number of participating nucleons in target
  virtual int GetNt() const { PHOOL_VIRTUAL_WARNING; return -9999; }
  // Number of participating nucleons in projectile
  virtual int GetNp() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual float GetRP() const { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual void SetEvt(const Int_t Evt) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetNpart(const Int_t Npart) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetATarg(const int val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetZTarg(const int val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetAProj(const int val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetZProj(const int val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetCollisionE(const float val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetNbinary(const int val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetBimpact(const float val) { PHOOL_VIRTUAL_WARNING; }

  virtual void SetNp(const int val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetNt(const int val) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetRP(const float val) { PHOOL_VIRTUAL_WARNING; }

  //!@name primary vertex position
  //@{
  
  /*! 
  it is safe to return zero here
  since it is what pythia uses by defaul
  */
  virtual Float_t GetPrimaryVertexX() const	{ return 0; }
  virtual Float_t GetPrimaryVertexY() const	{ return 0; }
  virtual Float_t GetPrimaryVertexZ() const	{ return 0; }
  
  virtual void SetPrimaryVertexX( Float_t value ) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetPrimaryVertexY( Float_t value ) { PHOOL_VIRTUAL_WARNING; }
  virtual void SetPrimaryVertexZ( Float_t value ) { PHOOL_VIRTUAL_WARNING; }
  //@}

  // PHObject methods
  int isValid() const { PHOOL_VIRTUAL_WARNING; return 0; }
  void Reset() { PHOOL_VIRTUAL_WARNING; }
  void identify(std::ostream& os=std::cout) const { PHOOL_VIRTUAL_WARNING; }
  void print(std::ostream& os=std::cout) const { PHOOL_VIRTUAL_WARNING; }

private:

  ClassDef (PHHijingHeader,1)
};

#endif	// __PHPYTHIAHEADER_H__
