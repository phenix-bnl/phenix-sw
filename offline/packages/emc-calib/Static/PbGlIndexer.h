#ifndef  __PBGLINDEXER_H__
#define  __PBGLINDEXER_H__

#include "EmcIndexer.h"

/** Implementation of the EMCAL Indexer for the PbGl.
In this class all detector specific functions are defined.
All functions act within the scope of a single Sector only
*/
class PbGlIndexer : public EmcIndexer
{
 public:
	///
  static PbGlIndexer * buildPbGlIndexer();
	///
  static int deletePbGlIndexer();
	///
  void  iCHi24Packi24PackT(int &, int &, int &);
	///
  int   i24Packi24PackTiCH(int &,int &); 
	///
  int   iCHi144PackT(int &);
	///
  int   i144PackTiCH(int &);
	///
  void  SMxySM(int , int &, int &);  
	///
  int   xySTiST(int x, int y) {return y*96+x;}
	///
  int   SMxySMTiST(int , int , int );  
	///
  int   SMiSMTiST(int , int );
	///
  void  iSTxyST(int const , int &, int &); 
	///
  int   xySMTiSMT(int x, int y) {return y*6+x;}  
        ///
  int   xySMiSM(int x, int y);
	///
  void  iSM24TxySM24T(int , int &, int &);
	///
  void  iST_SMInd(int const , int &, int &, int &, int &);

  ///  Converts sequential number of the 24 channel wide supermodule (iSM24, 0-191) and a tower number defined on the scope of this supermodule (0-23) into Sector Tower number (0-4607 for PbGlass EMCal)
  int   SM24iSM24TiST(int iSM24, int iSM24T);
 protected:
 PbGlIndexer();
  virtual ~PbGlIndexer();

 private:
  static PbGlIndexer * single;
  static int access_count;
};

#endif
