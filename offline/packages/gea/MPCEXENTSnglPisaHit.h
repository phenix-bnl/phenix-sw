#ifndef __MPCEXENTSNGLPisaHIT_HH_
#define __MPCEXENTSNGLPisaHIT_HH_

#include <PHObject.h>
#include <phool.h>
#include <iostream>

class MPCEXENTSnglPisaHit : public PHObject
{

 public:
 virtual ~MPCEXENTSnglPisaHit() {}
 virtual   int GetMPCEXENTCount() {warning("MPCEXENTCount"); return -9999;}
   virtual void    SetMPCEXENTCount( const int val) {warning("MPCEXENTCount");}
 virtual   int         GetMctrack() const {warning("mctrack"); return -9999;}
   virtual void    SetMctrack(const int val) {warning("mctrack");}
 virtual   float       GetVx() const {warning("vx"); return -9999;}
   virtual void    SetVx(const float val) {warning("vx");}
 virtual   float       GetVy() const {warning("vy"); return -9999;}
   virtual void    SetVy(const float val) {warning("vy");}
 virtual   float       GetVz() const {warning("vz"); return -9999;}
   virtual void    SetVz(const float val) {warning("vz");}
 virtual   float       GetPx() const {warning("px"); return -9999;}
   virtual void    SetPx(const float val) {warning("px");}
 virtual   float       GetPy() const {warning("py"); return -9999;}
   virtual void    SetPy(const float val) {warning("py");}
 virtual   float       GetPz() const {warning("pz"); return -9999;}
   virtual void    SetPz(const float val) {warning("pz");}
 virtual   int         GetIsubevent() const {warning("id"); return -9999;}
   virtual void    SetIsubevent(const int val) {warning("id");}
 virtual   int         GetNfile() const {warning("id"); return -9999;}
   virtual void    SetNfile(const int val) {warning("id");}
 virtual   int         GetNtrack() const {warning("id"); return -9999;}
   virtual void    SetNtrack(const int val) {warning("id");}

  private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "Single MPCEXENT PisaHIT Offending field == " << field << std::endl;
 };

  ClassDef(MPCEXENTSnglPisaHit,1)
};

#endif
