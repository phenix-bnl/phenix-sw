// $Id: mRxnpCalXang.h,v 1.8 2007/11/30 19:49:56 hpereira Exp $
#ifndef __mRxnpCalXang_h__
#define __mRxnpCalXang_h__

/*!
  \file    mRxnpCalXang.h
  \brief   unpack raw data hits. Generates interface class TRxnpRawScint_v1.
  \author  Chun Zhang
  \version $Revision: 1.8 $
  \date    $Date: 2007/11/30 19:49:56 $
*/

// STL include
//
#include <map>

#include <phool.h>
#include <PHTimeServer.h>

#include <fstream>
#include <TH1.h>
#include <TH2.h>
#include <TProfile.h>
#include <TFile.h>

// Rxnp includes
#include "mRxnpCalXangPar.h"

using std::cout;
using std::endl;

// forward declaration for interfaces
//
class PHCompositeNode;
class TRxnpRawXangMap;
class TRxnpScintMap;

//@{
/*! \ingroup modules */
//! rxnp raw prdf data unpacker.
/*! 
    rxnp raw data unpacker. Generates interface class TRxnpRawScint_v1 for each scintilator.
*/

class mRxnpCalXang
{
 public:
  //! constructor
  //
  mRxnpCalXang();
  //! destructor
  //
  virtual ~mRxnpCalXang();
  //! event method
  //
  virtual PHBoolean event(PHCompositeNode*);

  void closeHisto();

  //! functions   
  double getGlobal(int);              // opt
  // opt = 0 for south charge
  // opt = 1 for north charge
  // opt = 2 for south+north charge
  // opt = 3 for south time
  // opt = 4 for north time
  // opt = 5 for south,north average time
  // opt = 6 for z-vertex
  // opt = 7 for south nsegment
  // opt = 8 for north nsegment
  // opt = 9 for south+north nsegment
  double getRxnPlane(int,int,int);    // (id,ih,ig)
  double getXYWsum(int,int,int,int);  // (id,ih,ig,ii)
  // id = [0,8] for si,so,sc,ni,no,nc,ci,co,cc   where s[south],n[north],c[combined]
  //                                                   i[inner],o[outer],c[combined]
  // ih = [0,3] for harmonics 1-4
  // ig = [0,1] for high-low gain
  // ih = [0,2] for X_Sum, Y_Sum and Weight_Sum
  // init the components for flatting
  //
  void init_calcu(int run, int iflag)
    {
      nrun = run;
      flag = iflag;
      bookHisto();
      clearArray();	
    }

 private:
  //! get local pointers to needed nodes/maps
  //
  void set_interface_ptrs(PHCompositeNode* top_node);

  //! calculate raw reaction plane angles
  //
  void cal_raw_xang();
  void cal_global();
  void flatten_xang(int,int); // imul for centrality, izps for z-vertex
  void readTable();
  void writeTable();
  void readOffst();
  void writeOffst();
  void readInput();
  void clearArray();
  void bookHisto();
  void resetHisto();
  void dump_out();
  void init_tables();
  /* private data member */

  //! module timer
  //
  PHTimeServer::timer _timer;

  //! calibrated scint map
  //
  TRxnpScintMap* _scint_map;
  //! xang map
  //
  TRxnpRawXangMap* _xang_map;

  //! runtime parameter
  //
  mRxnpCalXangPar* _mod_par;

  //! private variables
  //
  int flag; // flag=0 for 1st correction, flag=1 for 2nd correction, flag=2 for flat plane
  int nrun; // run number
  int goodCounter; // good event counter
  int eventCounter; // total event counter
  static const int ndet=72; // 9(south/north/comb,inner/outer/comb)*4(harmonics)*2(high/low gain)
  static const int nmul=10; // centrality division
  static const int nzps=10; // z-vertex division
  static const int nord=8;  // flattening order
  double glb[10];           // 0-2 for charge,  3-5 for time, 6 for z-vertex, 7-9 for nsegments
  double sum[9][4][2][3];   // 9 for detector, 4 for harmonics, 2 for high/low gain, 3 for x/y/w
  double ang[9][4][2];      // 9 for detector, 4 for harmonics, 2 for high/low gain
  double ofsx[ndet][nmul][nzps];
  double ofsy[ndet][nmul][nzps];
  double widx[ndet][nmul][nzps];
  double widy[ndet][nmul][nzps];
  double aaa[ndet][nmul][nzps][nord];
  double bbb[ndet][nmul][nzps][nord];
  TFile *hfile;
  TH1F *Hphi[ndet][nmul][nzps];
  TH1F *Hphj[ndet][nmul][nzps];
  TH1F *Hsmx[ndet][nmul][nzps];
  TH1F *Hsmy[ndet][nmul][nzps];
  TProfile *Hsum[ndet][nmul][nzps];
  TProfile *Hprc[ndet][nmul][nzps];
  TProfile *Hprs[ndet][nmul][nzps];
};

//@}

#endif
