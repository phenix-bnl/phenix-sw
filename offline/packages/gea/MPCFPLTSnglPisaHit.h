#ifndef __MPCFPLTSNGLPisaHIT_HH_
#define __MPCFPLTSNGLPisaHIT_HH_

#include <PHObject.h>
#include <phool.h>
#include <iostream>

class MPCFPLTSnglPisaHit : public PHObject
{

 public:
  virtual ~MPCFPLTSnglPisaHit() {}
 virtual   int GetMPCFPLTCount() {warning("MPCFPLTCount"); return -9999;}
   virtual void    SetMPCFPLTCount( const int val) {warning("MPCFPLTCount");}
 virtual   int GetMPCFPLT1Count() {warning("MPCFPLT1Count"); return -9999;}
   virtual void    SetMPCFPLT1Count( const int val) {warning("MPCFPLT1Count");}
 virtual   int GetMPCFPLT2Count() {warning("MPCFPLT2Count"); return -9999;}
   virtual void    SetMPCFPLT2Count( const int val) {warning("MPCFPLT2Count");}
 virtual   int         GetIarm() const {warning("arm"); return -9999;}
   virtual void    SetIarm(const int val) {warning("arm");}
 virtual   int         GetIsubevent() const {warning("isubevent"); return -9999;}
   virtual void    SetIsubevent(const int val) {warning("isubevent");}
 virtual   int         GetNtrack() const {warning("track"); return -9999;}
   virtual void    SetNtrack(const int val) {warning("track");}
 virtual   int         GetId() const {warning("id"); return -9999;}
   virtual void    SetId(const int val) {warning("id");}
 virtual   int         GetMctrack() const {warning("mctrack"); return -9999;}
   virtual void    SetMctrack(const int val) {warning("mctrack");}
 virtual   int         GetIpc() const {warning("ipc"); return -9999;}
   virtual void    SetIpc(const int val) {warning("ipc");}
 virtual   float       GetXin() const {warning("xx"); return -9999;}
   virtual void    SetXin(const float val) {warning("xx");}
 virtual   float       GetYin() const {warning("yy"); return -9999;}
   virtual void    SetYin(const float val) {warning("yy");}
 virtual   float       GetZin() const {warning("zz"); return -9999;}
   virtual void    SetZin(const float val) {warning("zz");}
 virtual   float       GetDedx() const {warning("dedx"); return -9999;}
   virtual void    SetDedx(const float val) {warning("dedx");}
 virtual   float       GetXe() const {warning("Xe"); return -9999;}
   virtual void    SetXe(const float val) {warning("Xe");}
 virtual   float       GetYe() const {warning("Ye"); return -9999;}
   virtual void    SetYe(const float val) {warning("Ye");}
 virtual   float       GetPmom() const {warning("Pmom"); return -9999;}
   virtual void    SetPmom(const float val) {warning("Pmom");}
 virtual   float       GetP_id() const {warning("P_id"); return -9999;}
   virtual void    SetP_id(const float val) {warning("P_id");}
 virtual   float       GetPNum() const {warning("PNum"); return -9999;}
   virtual void    SetPNum(const float val) {warning("PNum");}

   virtual   int         GetNfile() const {warning("nfile"); return -9999;}
   virtual void    SetNfile(const int val) {warning("nfile");}

 virtual   int         GetIncc() const {warning("incc"); return -9999;}
   virtual void    SetIncc(const int val) {warning("incc");}
 virtual   int         GetNEvent() const {warning("NEvent"); return -9999;}
   virtual void    SetNEvent(const int val) {warning("NEvent");}

  private:
  void warning(const char* field) const {
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "Single MPCFPLT PisaHIT Offending field == " << field << std::endl;
 };

  ClassDef(MPCFPLTSnglPisaHit,1)
};

#endif
