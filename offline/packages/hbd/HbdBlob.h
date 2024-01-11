#ifndef __HBDBLOB_HH_
#define __HBDBLOB_HH_

#include <iostream>
#include <PHObject.h>
#include <phool.h>
#include <PHPoint.h>

// **************************************************************
//
// Implementation of TPC reconstructed clusters
//
// Created on 9/3/03 by Jeffery Mitchell.
//
// **************************************************************

class HbdBlob : public PHObject
{

 public:
  virtual ~HbdBlob() {}

  // Set the values in the HbdBlob...

  virtual void set_id (const int val)   {warning("id        ");}
  virtual void set_sector (const int val) {warning("sector   ");}
  virtual void set_charge (const float val)   {warning("charge     ");}
  virtual void set_blobx(const float val) {warning("blobx   ");}
  virtual void set_bloby(const float val) {warning("bloby   ");}
  virtual void set_blobz(const float val) {warning("blobz   ");}
  virtual void set_localmax(const int val) {warning("localmax   ");}
  virtual void set_nlocalmax(const int val) {warning("nlocalmax   ");}
  virtual void set_parentid(const int val) {warning("parentid   ");}
  virtual void set_size (const short val)   {warning("size      ");}
  virtual void set_bloby_local(const float val) {warning("bloby_local");}
  virtual void set_blobz_local(const float val) {warning("blobz_local");}
  virtual void set_charge_pad(const int ind, const float val) {warning("charge_pad");}
  virtual void set_pady_local(const int ind, const float val) {warning("pady_local");}
  virtual void set_padz_local(const int ind, const float val) {warning("padz_local");}
  virtual void set_pad_sector(const int ind, const int val) {warning("pad_sector");}

//----------------------------------------------------------------

  // Get the values from the HbdBlob...
  virtual int get_id () const {warning("id    "); return -9999;}
  virtual int get_sector () const {warning("sector   "); return -9999;}
  virtual float get_charge () const {warning("charge  "); return -9999.;}
  virtual float get_blobx() const {warning("blobx  "); return -9999.;}
  virtual float get_bloby() const {warning("bloby  "); return -9999.;}
  virtual float get_blobz() const {warning("blobz  "); return -9999.;}
  virtual float get_localmax() const {warning("localmax   "); return -9999;}
  virtual int get_nlocalmax() const {warning("nlocalmax   "); return -9999;}
  virtual int get_parentid() const {warning("parentid   "); return -9999;}
  virtual short get_size () const {warning("size  "); return -9999;}
  virtual float get_bloby_local() const {warning("bloby_local "); return -9999.;}
  virtual float get_blobz_local() const {warning("blobz_local "); return -9999.;}
  virtual float get_charge_pad(const unsigned int ind) const {warning("charge_pad "); return -9999.;}
  virtual float get_pady_local(const unsigned int ind) const {warning("pady_local "); return -9999.;}
  virtual float get_padz_local(const unsigned int ind) const {warning("padz_local "); return -9999.;}
  virtual int get_pad_sector(const unsigned int ind) const {warning("pad_sector "); return -9999;}

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
    os << "identify yourself: virtual HbdBlob object" << std::endl;
    return;
  }

 private:
  void warning(const char* field) const { 
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "HbdBlob Offending field == " << field << std::endl;
  }

  ClassDef(HbdBlob,1)

};

#endif /* __HBDBLOB_HH_ */
