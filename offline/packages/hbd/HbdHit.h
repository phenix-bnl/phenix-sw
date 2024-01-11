#ifndef __HBDHIT_HH_
#define __HBDHIT_HH_

#include <iostream>
#include <PHObject.h>
#include <phool.h>

// **************************************************************
//
// Temporary implementation of HBD GEANT hits.
// This will be superceded by additions to offline/packages/gea.
//
// Created on 11/13/03 by Jeffery Mitchell.
//
// **************************************************************

class HbdHit : public PHObject
{

 public:
  virtual ~HbdHit() {}

  // Set the values in the HbdHit...

  virtual void set_track (const int val)   {warning("track   ");}
  virtual void set_idPart  (const int val)   {warning("idPart    ");}
  virtual void set_sector  (const int val)   {warning("sector    ");}
  virtual void set_npe   (const int val)   {warning("npe     ");}
  virtual void set_nentr   (const int val)   {warning("nentr     ");}
  virtual void set_detflag (const int val)   {warning("detflag   ");}
  virtual void set_xyzin  (const int ind, const float val) 
    {warning("xyzin      ");}
  virtual void set_pxyz   (const int ind, const float val) 
    {warning("pxyz       ");}
  virtual void set_ptot     (const float val) {warning("ptot       ");}
  virtual void set_theta     (const float val) {warning("theta       ");}
  virtual void set_eta     (const float val) {warning("eta       ");}
  virtual void set_phi     (const float val) {warning("phi       ");}
  virtual void set_xyzloc  (const int ind, const float val) 
    {warning("xyzloc      ");}
  virtual void set_xyze  (const int ind, const float val) 
    {warning("xyze      ");}
  virtual void set_pt     (const float val) {warning("pt       ");}
  virtual void set_tof     (const float val) {warning("tof       ");}
  virtual void set_dy     (const float val) {warning("dy       ");}
  virtual void set_dz     (const float val) {warning("dz       ");}
  virtual void set_rmsy     (const float val) {warning("rmsy       ");}
  virtual void set_rmsz     (const float val) {warning("rmsz       ");}
  virtual void set_charge     (const float val) {warning("charge       ");}
  virtual void set_time     (const float val) {warning("time       ");}
  virtual void set_size (const int val)   {warning("size   ");}
  virtual void set_nct (const int val)   {warning("nct   ");}
  virtual void set_part     (const float val) {warning("part       ");}
  virtual void set_id (const int val)   {warning("id   ");}
  virtual void set_pxyzini   (const int ind, const float val) 
    {warning("pxyzini       ");}
  virtual void set_idParent  (const int val)   {warning("idParent    ");}
  virtual void set_itOrigin  (const int val)   {warning("itOrigin    ");}
  virtual void set_idOrigin  (const int val)   {warning("idOrigin    ");}
  virtual void set_Ptheta     (const float val) {warning("Ptheta       ");}
  virtual void set_Peta     (const float val) {warning("Peta       ");}
  virtual void set_Pphi     (const float val) {warning("Pphi       ");}
  virtual void set_Rvertex     (const float val) {warning("Rvertex       ");}
  virtual void set_Zvertex     (const float val) {warning("Zvertex       ");}
  virtual void set_Thetavertex     (const float val) {warning("Thetavertex       ");}
  virtual void set_Phivertex     (const float val) {warning("Phivertex       ");}

  // Get the values from the HbdHit...
  virtual int get_track        () const {warning("track      "); return -9999;}
  virtual int get_idPart       () const {warning("idPart     "); return -9999;}
  virtual int get_sector       () const {warning("sector     "); return -9999;}
  virtual int get_npe        () const {warning("npe      "); return -9999;}
  virtual int get_nentr        () const {warning("nentr      "); return -9999;}
  virtual int get_detflag      () const {warning("detflag    "); return -9999;}
  virtual float get_xyzin  (const unsigned int ind)
    const {warning("xyzin      "); return -9999;}
  virtual float get_pxyz   (const unsigned int ind)
    const {warning("pxyz       "); return -9999;}
  virtual float get_ptot        () const {warning("ptot       "); return -9999;}
  virtual float get_theta        () const {warning("theta        "); return -9999;}
  virtual float get_eta        () const {warning("eta        "); return -9999;}
  virtual float get_phi        () const {warning("phi        "); return -9999;}
  virtual float get_xyzloc  (const unsigned int ind)
    const {warning("xyzloc      "); return -9999;}
  virtual float get_xyze  (const unsigned int ind)
    const {warning("xyze      "); return -9999;}
  virtual float get_pt        () const {warning("pt        "); return -9999;}
  virtual float get_tof        () const {warning("tof        "); return -9999;}
  virtual float get_dy        () const {warning("dy        "); return -9999;}
  virtual float get_dz        () const {warning("dz        "); return -9999;}
  virtual float get_rmsy        () const {warning("rmsy        "); return -9999;}
  virtual float get_rmsz        () const {warning("rmsz        "); return -9999;}
  virtual float get_charge        () const {warning("charge        "); return -9999;}
  virtual float get_time        () const {warning("time        "); return -9999;}
  virtual int get_size       () const {warning("size     "); return -9999;}
  virtual int get_nct        () const {warning("nct      "); return -9999;}
  virtual float get_part        () const {warning("part        "); return -9999;}
  virtual int get_id        () const {warning("id      "); return -9999;}
  virtual float get_pxyzini   (const unsigned int ind)
    const {warning("pxyzini       "); return -9999;}
  virtual int get_idParent       () const {warning("idParent     "); return -9999;}
  virtual int get_itOrigin       () const {warning("itOrigin     "); return -9999;}
  virtual int get_idOrigin       () const {warning("idOrigin     "); return -9999;}
  virtual float get_Ptheta        () const {warning("Ptheta       "); return -9999;}
  virtual float get_Peta        () const {warning("Peta       "); return -9999;}
  virtual float get_Pphi        () const {warning("Pphi       "); return -9999;}
  virtual float get_Rvertex        () const {warning("Rvertex       "); return -9999;}
  virtual float get_Zvertex        () const {warning("Zvertex       "); return -9999;}
  virtual float get_Thetavertex        () const {warning("Thetavertex       "); return -9999;}
  virtual float get_Phivertex        () const {warning("Phivertex       "); return -9999;}

  // Standard functions of all inheritors of PHObject classes...

  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual HbdHit object" << std::endl;
    return;
  }

 private:
  void warning(const char* field) const { 
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "HbdHit Offending field == " << field << std::endl;
  }

  ClassDef(HbdHit,1)

};
#endif

