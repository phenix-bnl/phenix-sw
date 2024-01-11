#ifndef  __PBSCINDEXER_H__
#define  __PBSCINDEXER_H__

#ifndef __EMCINDEXER_H__
#include "EmcIndexer.h"
#endif

// 
// //////////////////////////////////////////////////////////////////////////////// 
// E.Kistenev         6/10/99 
// send comments to kistenev@bnl.gov 
//                    6/11/99 - removed external at the end of this file ????
// ///////////////////////////////////////////////////////////////////////////////
/** Implementation of the EMCAL Indexer for the PbSc.
In this class all detector specific functions are defined.
All functions act within the scope of a single Sector only
*/
class PbScIndexer : public EmcIndexer
{
 public:
	///
  static PbScIndexer * buildPbScIndexer();
	///
  static int deletePbScIndexer();
        ///
  int   xySMiSM(int x, int y);
	///
  void  SMxySM(int , int &, int &);  
	///
  int   xySTiST(int x, int y) {return y*72+x;}
	///
  int   SMxySMTiST(int , int , int );  
	///
  int   SMiSMTiST(int , int );
	///
  void  iSTxyST(int const , int &, int &); 
	///
  int   xySMTiSMT(int x, int y) {return y*12+x;}  
	///
  void  iST_SMInd(int const , int &, int &, int &, int &);

 protected:
 PbScIndexer();
 virtual ~PbScIndexer();

 private:
  static PbScIndexer * single;
  static int access_count;
};
//extern PbScIndexer    * gPbSc;
#endif
