#ifndef PROCESSNONLINTREE_H__
#define PROCESSNONLINTREE_H__

#include "EmcAnaCommon.h"

class TFile;
class TH2;

class Pi0MassFitter;

class ProcessNonLinTree {
   TFile* m_ofile;
   TH2* m_h2_pi0mass[EmcAnaCommon::N_ARMSECT]; //!
   Pi0MassFitter* m_fitter;

 public:
   ProcessNonLinTree() {;}
   virtual ~ProcessNonLinTree() {;}

   void Process(const char* fn_iflist, const char* ofilename);

 protected:
   void ReadHistos(const char* fn_iflist);
   
};

#endif // PROCESSNONLINTREE_H__
