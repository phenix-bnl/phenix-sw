#ifndef __PHHijingHeaderV2_H__
#define __PHHijingHeaderV2_H__

#include <phool.h>
#include <PHObject.h>
#include <PHHijingHeader.h>

class PHHijingHeaderV2 : public PHHijingHeader
{
public:

  PHHijingHeaderV2();
  virtual ~PHHijingHeaderV2() {}

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

  //!@name primary vertex position
  //@{
  Float_t GetPrimaryVertexX() const	{return primary_vertex_x;}
  Float_t GetPrimaryVertexY() const	{return primary_vertex_y;}
  Float_t GetPrimaryVertexZ() const	{return primary_vertex_z;}
  
  void SetPrimaryVertexX( Float_t value ) {primary_vertex_x = value;}
  void SetPrimaryVertexY( Float_t value ) {primary_vertex_y = value;}
  void SetPrimaryVertexZ( Float_t value ) {primary_vertex_z = value;}
  //@}

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

  //!@name primary vertex
  //@{
  Float_t primary_vertex_x; 
  Float_t primary_vertex_y; 
  Float_t primary_vertex_z; 
  //@}

  ClassDef (PHHijingHeaderV2,1)
};

#endif	// __PHPYTHIAHEADER_H__
