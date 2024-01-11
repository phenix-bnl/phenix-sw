#ifndef __MPCRAWCONTENT_H__
#define __MPCRAWCONTENT_H__

#include <TObject.h>
#include <iostream>
#include "phool.h"

class mpcRawContent : public TObject
{
public:
  mpcRawContent();
  virtual ~mpcRawContent() {}

  virtual mpcRawContent& operator=(const mpcRawContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  virtual mpcRawContent& operator+(const mpcRawContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }
  virtual mpcRawContent& operator+=(const mpcRawContent &rhs)
  { PHOOL_VIRTUAL_WARNING; return *this; }

  virtual short get_ch() const     { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_tdc() const    { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_tdc1() const    { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_tdc2() const    { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_lopost() const { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_lopre() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_hipost() const { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual short get_hipre() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_adc() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_adc1() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_adc2() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_ZSM() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_sample() const    { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual float get_time() const    { PHOOL_VIRTUAL_WARNING; return -9999; }

  virtual Short_t get_quality() const  { PHOOL_VIRTUAL_WARNING; return -9999; }
  virtual Float_t get_fquality() const  { return -9999; }

  //virtual void Copy( const unsigned int iclus ) { PHOOL_VIRTUAL_WARNING; }
  virtual void print(std::ostream&) { PHOOL_VIRTUAL_WARNING; }

protected:
  //
  // this is the base class for single crystals
  //

private:

  ClassDef(mpcRawContent,1)
};

#endif /* __MPCRAWCONTENT_H__ */

