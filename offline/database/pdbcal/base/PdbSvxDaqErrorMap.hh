// Declaration of class PdbSvxDaqErrorMap
// Purpose: SVX DAQ error maps
//          Now the class is for a module.
//          Only with bad module
// Author: Takashi Hachiya

#ifndef __PDBSVXDAQERRORMAP_HH__
#define __PDBSVXDAQERRORMAP_HH__

//#include <string>

#include "PdbCalChan.hh"

class PdbSvxDaqErrorMap : public PdbCalChan
{
 public:
 
  enum SvxDaqErrorDetID {
    SVX_PIXEL=0,
    SVX_STRIP=1 
  };

 public:
   PdbSvxDaqErrorMap();
   virtual ~PdbSvxDaqErrorMap();
   
   virtual int           getDetector() const;
   virtual int           getModule()   const;
   virtual unsigned int  getStatus()   const;

   void    setStatus (const int detid, const int module, const unsigned int status);

   virtual void print() const;
	
 private:
   int          m_detector;
   int          m_module;
   unsigned int m_status;

   ClassDef (PdbSvxDaqErrorMap, 1);
};

#endif /* __PDBSVXDAQERRORMAP_HH__ */

