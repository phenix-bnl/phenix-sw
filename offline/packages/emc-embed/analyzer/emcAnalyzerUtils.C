// $Header
//-----------------------------------------------------------------------------
//
//  (c) PHENIX Collaboration 2002-2003
//
//  Author: D. d'Enterria
//
//  Nevis Labs. Columbia University
//
//-----------------------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <cassert>
#include <strstream>
#include <cstring>
#include <iomanip>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <climits>
#include <vector>

#include "TH1.h"
#include "TF1.h"
#include "TMath.h"
#include "TGraphErrors.h"
#include "TClonesArray.h"
#include "TString.h"
#include "TSystem.h"
#include "TBox.h"
#include "TPolyLine.h"

#include "emcAnalyzerUtils.h"

#include "star_200GeV_pi_K_p_AuAu_pp.hh"

// COMMENT OUT this next line if you want CentClass=17 to be 60-92%

//#define _CENTRALITY_CLASS_17_IS_70_92_

using namespace std;
using namespace TMath;

namespace emcAnalyzerUtils
{

static const int CentClasses = 26;
static const double crazyValue = -99999.;

//_____________________________________________________________________________
// Centrality class
int
getCentralityClass(const int Cent1, 
		   const int Cent2)

{
  // Mind that the centrality class order defined here (esp. above CC == 15) matches
  // the one in the correction factors file "emcEfficiency_7percent_counts.h" !!

  //CentClass =  0 :  0- 5%
  //CentClass =  1 :  5-10%
  //CentClass =  2 : 10-15%
  //CentClass =  3 : 15-20%
  //CentClass =  4 : 20-25%
  //CentClass =  5 : 25-30%
  //CentClass =  6 : 30-35%
  //CentClass =  7 : 35-40%
  //CentClass =  8 : 40-45%
  //CentClass =  9 : 45-50%
  //CentClass = 10 : 50-55%
  //CentClass = 11 : 55-60%
  //CentClass = 12 : 60-65%
  //CentClass = 13 : 65-70%
  //CentClass = 14 : 70-75%
  //CentClass = 15 : 75-80%

  //CentClass = 16 : 80-92%    <-- special case
  //CentClass = 17 : 70-92%    <-- special case (mind next one)
  //CentClass = 17 : 60-92%    <-- special case (mind former one)
  //CentClass = 18 : 60-80%    <-- special case
  //CentClass = 19 : min.bias  <-- special case

  //CentClass = 20 :  0-20%
  //CentClass = 21 : 20-60%

  //CentClass = 22 : 10-30%  <-- AuAu @ 62 GeV
  //CentClass = 23 : 30-60%  <-- AuAu @ 62 GeV
  //CentClass = 24 : 60-85%  <-- AuAu @ 62 GeV
  //CentClass = 25 :  0-85%  <-- AuAu @ 62 GeV

  int CC = Cent1/5;

  if ( (CC>25) || (CC<0) || (Cent1>Cent2) )
    {
      cout << " <E> emcAnalyzerUtils::getCentralityClass() : I don't know this Centrality Class :" 
	   << Cent1 << " - " << Cent2 << "%" << endl;
      return -1;
    }

  if ( (Cent1==0) && (Cent2==20) ) // 0-20%
    {
      return 20;
    }
  if ( (Cent1==20) && (Cent2==60) ) // 20-60%
    {
      return 21;
    }
  if ( (Cent1==10) && (Cent2==30) ) // 10-30%
    {
      return 22;
    }
  if ( (Cent1==30) && (Cent2==60) ) // 30-60%
    {
      return 23;
    }
  if ( (Cent1==60) && (Cent2==85) ) // 60-85%
    {
      return 24;
    }
  if ( (Cent1==0) && (Cent2==85) ) // 0-85%
    {
      return 25;
    }

  if ( (Cent1==100) || (Cent2==100) ) // min. bias (let's assume that's what the user want ...0-100, 100-100)
    {
      return 19;
    }
  if ( (Cent1==60) && (Cent2==80) ) // 60-80%
    {
      return 18;
    }

#ifdef _CENTRALITY_CLASS_17_IS_70_92_
  if ( (Cent1==70) && (Cent2==92) ) // 70-92%
#else
  if ( (Cent1==60) && (Cent2==92) ) // 60-92%
#endif
    {
      return 17;
    }
  if ( (Cent1==80) && (Cent2==92) ) // 80-92%
    {
      return 16;
    }

  if ( (Cent1!=Cent2-5) && (Cent1!=Cent2-10) )
    {
      cout << " <E> emcAnalyzerUtils::getCentralityClass() : I don't know this Centrality Class :" 
	   << Cent1 << " - " << Cent2 << "%" << endl;
      return -1;
    }

  if ( (Cent1==Cent2-5) && (Cent1>=80) )
    {
      cout << " <E> emcAnalyzerUtils::getCentralityClass() : I don't know this Centrality Class :" 
	   << Cent1 << " - " << Cent2 << "%" << endl;
      return -1;
    }

  return CC;

}

//_____________________________________________________________________________
void 
getCentralityClassLimits( const char* cent,    // outdated !
			  int& c1, int& c2)
{

  cout << "ACHTUNG !! this is an outdated function !!" << endl; // we keep in case one day we need to readapt it ...
  return ;

  c1 = c2 = -1;
  if ( strcmp(cent,"C0")==0  ) { c1=  0; c2= 10; }
  if ( strcmp(cent,"C1")==0  ) { c1= 10; c2= 20; }
  if ( strcmp(cent,"C2")==0  ) { c1= 20; c2= 30; }
  if ( strcmp(cent,"C3")==0  ) { c1= 30; c2= 40; }
  if ( strcmp(cent,"C4")==0  ) { c1= 40; c2= 50; }
  if ( strcmp(cent,"C5")==0  ) { c1= 50; c2= 60; }
  if ( strcmp(cent,"C6")==0  ) { c1= 60; c2= 70; }
  if ( strcmp(cent,"C7")==0  ) { c1= 70; c2= 80; }
  if ( strcmp(cent,"C8")==0  ) { c1= 80; c2= 92; }
  if ( strcmp(cent,"C10")==0 ) { c1=  0; c2=100; }
}

//_____________________________________________________________________________
void 
getCentralityClassLimits( const int CC, 
			  int& c1, int& c2)
{

  c1 = c2 = -1;
  if ( CC==0  ) { c1=  0; c2=  5; }
  if ( CC==1  ) { c1=  5; c2= 10; }
  if ( CC==2  ) { c1= 10; c2= 15; }
  if ( CC==3  ) { c1= 15; c2= 20; }
  if ( CC==4  ) { c1= 20; c2= 25; }
  if ( CC==5  ) { c1= 25; c2= 30; }
  if ( CC==6  ) { c1= 30; c2= 35; }
  if ( CC==7  ) { c1= 35; c2= 40; }
  if ( CC==8  ) { c1= 40; c2= 45; }
  if ( CC==9  ) { c1= 45; c2= 50; }
  if ( CC==10 ) { c1= 50; c2= 55; }
  if ( CC==11 ) { c1= 55; c2= 60; }
  if ( CC==12 ) { c1= 60; c2= 65; }
  if ( CC==13 ) { c1= 65; c2= 70; }
  if ( CC==14 ) { c1= 70; c2= 75; }
  if ( CC==15 ) { c1= 75; c2= 80; }
  if ( CC==16 ) { c1= 80; c2= 92; }   //<-- special case
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
  if ( CC==17 ) { c1= 70; c2= 92; }   //<-- special case
#else
  if ( CC==17 ) { c1= 60; c2= 92; }   //<-- special case
#endif
  if ( CC==18 ) { c1= 60; c2= 80; }   //<-- special case
  if ( CC==19 ) { c1=  0; c2=100; }   //<-- special case

  if ( CC==20  ) { c1=  0; c2= 20; }
  if ( CC==21  ) { c1= 20; c2= 60; }

  if ( CC==22  ) { c1= 10; c2= 30; }
  if ( CC==23  ) { c1= 30; c2= 60; }
  if ( CC==24  ) { c1= 30; c2= 85; }
  if ( CC==25  ) { c1=  0; c2= 85; }

}

//_____________________________________________________________________________
void 
getCentralityClassLimits2( const int CC, 
			   int& c1, int& c2)
{

  c1 = c2 = -1;
  if ( CC==0  ) { c1=  0; c2= 10; }
  if ( CC==2  ) { c1= 10; c2= 20; }
  if ( CC==4  ) { c1= 20; c2= 30; }
  if ( CC==6  ) { c1= 30; c2= 40; }
  if ( CC==8  ) { c1= 40; c2= 50; }
  if ( CC==10 ) { c1= 50; c2= 60; }
  if ( CC==12 ) { c1= 60; c2= 70; }
  if ( CC==14 ) { c1= 70; c2= 80; }
  if ( CC==16 ) { c1= 80; c2= 92; }   //<-- special case
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
  if ( CC==18 ) { c1= 70; c2= 92; }   //<-- special case
  //if ( CC==18 ) { c1= 60; c2= 80; }   //<-- special case
#else
  if ( CC==18 ) { c1= 60; c2= 92; }   //<-- special case
#endif
  if ( CC==20 ) { c1=  0; c2=100; }   //<-- special case // fixme: CC=20 is actually 0-20 in other functions

  if ( CC==22 ) { c1=  0; c2= 20; }   //<-- special case // 
  if ( CC==24 ) { c1= 20; c2= 60; }   //<-- special case
}

//_____________________________________________________________________________
// ---------------- total # of events for PPG014 pi0 analysis ----------------
double
getEvents( const int Cent1, 
	   const int Cent2,
	   const char *type)
{

//   const double Events[CentClasses] = 
//     { 
//       3148148./2., 3148148./2.,  //  0-10
//       3177042./2., 3177042./2.,  // 10-20
//       3129548./2., 3129548./2.,  // 20-30
//       3139715./2., 3139715./2.,  // 30-40
//       3181711./2., 3181711./2.,  // 40-50
//       3158935./2., 3158935./2.,  // 50-60
//       3181306./2., 3181306./2.,  // 60-70
//       3118054./2., 3118054./2.,  // 70-80
//       3547648.,                  // 80-92,
// #ifdef _CENTRALITY_CLASS_17_IS_70_92_
//       3118054.+3547648.,         // 70-92
// #else
//       3181306.+3118054.+3547648.,// 60-92
// #endif
//       3181306.+3118054.,         // 60-80
//       28782107.,                 // min.bias
//       //28781108.                  // min.bias
//       0.,0.                      // 0-20, 20-60 (not used for pi0 ...)
//     };
  
  const double Events[CentClasses] = 
    { 
      3.59334e06/2., 3.59334e06/2.,  //  0-10
      3.61426e06/2., 3.61426e06/2.,  // 10-20
      3.56102e06/2., 3.56102e06/2.,  // 20-30
      3.56730e06/2., 3.56730e06/2.,  // 30-40
      3.60950e06/2., 3.60950e06/2.,  // 40-50
      3.57730e06/2., 3.57730e06/2.,  // 50-60
      3.60203e06/2., 3.60203e06/2.,  // 60-70
      3.54297e06/2., 3.54297e06/2.,  // 70-80
      4.05828e06,                    // 80-92,
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      3.54297e06+4.05828e06,         // 70-92
#else
      3.60203e06+3.54297e06+4.05828e06,// 60-92
#endif
      3.60203e06+3.54297e06,         // 60-80
      3.27260e07,                    // min.bias
      0.,0.,                         // 0-20, 20-60 (not used for pi0 ...)
      0.,0.,0.,0.                    // 10-30, 30-60, 60-85, 0-85 (not used for 200 GeV ...)

    };
  
  int CC = getCentralityClass(Cent1, Cent2);
  if ( CC == -1) return -1;

  if (Events[CC]==0.)
    {
      cout << " <E> emcAnalyzerUtils::getEvents() : zero events !?!?! returning -1 ..." << endl;
      return -1;
    }

  TString type_str = type ;

  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_str.Contains("val")) return Events[CC];
      else if (type_str.Contains("err")) return Sqrt(Events[CC]);
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_str.Contains("val")) return Events[CC]+Events[CC+1];
      else if (type_str.Contains("err")) return Sqrt(Events[CC]+Events[CC+1]);
    }
  
  cout << " <E> emcAnalyzerUtils::getEvents() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;
}

//_____________________________________________________________________________
//---------- total # of events for REACTION-PLANE binned pi0 analysis ---------

double
getEventsReacPlane( const int Cent1, 
		    const int Cent2,
		    const char *type)
{

  const double Events[CentClasses] = 
    { 
      3875409./2., 3875409./2.,  //  0-10 (no reaction-plane binned data here)
      3899965./2., 3899965./2.,  // 10-20
      3841993./2., 3841993./2.,  // 20-30
      3850059./2., 3850059./2.,  // 30-40
      3894999./2., 3894999./2.,  // 40-50
      3861926./2., 3861926./2.,  // 50-60
      3885488./2., 3885488./2.,  // 60-70
      3821215./2., 3821215./2.,  // 70-80
      0.,0.,                     // 80-92, 70-92 or 60-92  (no reaction-plane binned data here)
      0.,//3885488.+3821215.,    // 60-80 (no reaction-plane binned data here actually)
      30931054.,                 // min.bias (is actually 10-80% !!)
      0.,0.,                     // 0-20, 20-60 (no reaction-plane binned data here)
      0.,0.,0.,0.                // 10-30, 30-60, 60-85, 0-85 (not used for 200 GeV ...)
    };  

  int CC = getCentralityClass(Cent1, Cent2);
  if ( CC == -1) return -1;

  if (Events[CC]==0.)
    {
      cout << " <E> emcAnalyzerUtils::getEventsReacPlane() : zero events !?!?! returning -1 ..." << endl;
      return -1;
    }

  TString type_str = type ;

  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_str.Contains("val")) return Events[CC];
      else if (type_str.Contains("err")) return Sqrt(Events[CC]);
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_str.Contains("val")) return Events[CC]+Events[CC+1];
      else if (type_str.Contains("err")) return Sqrt(Events[CC]+Events[CC+1]);
    }

  cout << " <E> emcAnalyzerUtils::getEventsReacPlane() : Something wrong with centrality class: "
       << Cent1 << "-" << Cent2 << "%" << endl;

  return crazyValue;
}

//_____________________________________________________________________________
//---------- total # of events for 62.4 GeV pi0 analysis ---------

double
getEvents62GeV( const int Cent1, 
		const int Cent2,
		const char *type)
{

  const double Events[CentClasses] = 
    { 
      4725057./2., 4725057./2.,  //  0-10
      0., 0.,   // 10-20
      0., 0.,   // 20-30
      0., 0.,   // 30-40
      0., 0.,   // 40-50
      0., 0.,   // 50-60
      0., 0.,   // 60-70
      0., 0.,   // 70-80
      0., 10558974.,   // 80-92, 70-92 or 60-92
      0.,       // 60-80
      40003938, // min.bias (is actually 0-85% !!)
      0.,0.,     // 0-20, 20-60
      9502793.,14348681.,11427407.,40003938. // 10-30, 30-60, 60-85, 0-85
    };  

  int CC = getCentralityClass(Cent1, Cent2);
  if ( CC == -1) return -1;

  if (Events[CC]==0.)
    {
      cout << " <E> emcAnalyzerUtils::getEvents62GeV() : zero events !?!?! returning -1 ..." << endl;
      return -1;
    }

  TString type_str = type ;

  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_str.Contains("val")) return Events[CC];
      else if (type_str.Contains("err")) return Sqrt(Events[CC]);
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_str.Contains("val")) return Events[CC]+Events[CC+1];
      else if (type_str.Contains("err")) return Sqrt(Events[CC]+Events[CC+1]);
    }

  cout << " <E> emcAnalyzerUtils::getEventsReacPlane() : Something wrong with centrality class: "
       << Cent1 << "-" << Cent2 << "%" << endl;

  return crazyValue;
}

//_____________________________________________________________________________
// Num. of Eta Events
double
getEtaEvents( const int Cent1, 
	      const int Cent2,
	      const char *type)
{

  // Yields as of July 31, 2003
  //   const double Events[CentClasses] = 
  //     { 
  //       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., // 0-50  not used for eta
  //       0.,0.,0.,0.,0.,0.,                // other not used for eta
  //       12079552,  // 60-92
  //       0.,        // 60-80 not used
  //       7775374+15448977+12079552, // min.bias
  //       7775374,   //  0-20
  //       15448977,  // 20-60
  //     };

//   // Yields as of Oct. 6, 2003
//   const double Events[CentClasses] = 
//     { 
//       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., // 0-50  not used for eta
//       0.,0.,0.,0.,0.,0.,                // other not used for eta
//       11043032,  // 60-92
//       0.,        // 60-80 not used
//       7102179+14117832+11043032, // min.bias
//       7102179,   //  0-20
//       14117832,  // 20-60
//     };

//   // Yields as of Dec. 2003
//   const double Events[CentClasses] = 
//     { 
//       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., // 0-50  not used for eta
//       0.,0.,0.,0.,0.,0.,                // other not used for eta
//       11203269,  // 60-92
//       0.,        // 60-80 not used
//       7207599+14315117+11203269, // min.bias
//       7207599,   //  0-20
//       14315117,  // 20-60
//     };

//   // Yields as of Apr. 21 2004
//   const double Events[CentClasses] = 
//     { 
//       0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., // 0-50  not used for eta
//       0.,0.,0.,0.,0.,0.,                // other not used for eta
//       10552075,  // 60-92
//       0.,        // 60-80 not used
//       30838963,  // min.bias
//       6793424,   //  0-20
//       13493464,  // 20-60
//       0.,0.,0.,0.// 10-30, 30-60, 60-85, 0-85 (not used for eta ...)
//     };

  // Yields Aug. 2004 (final pass)
  const double Events[CentClasses] = 
    { 
      0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0., // 0-50  not used for eta
      0.,0.,0.,0.,0.,0.,                // other not used for eta
      11497275.,  // 60-92
      0.,        // 60-80 not used
      33584023.,  // min.bias
      7395494.,   //  0-20
      14691254.,  // 20-60
      0.,0.,0.,0.// 10-30, 30-60, 60-85, 0-85 (not used for eta ...)
    };

  int CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  if (Events[CC]==0.)
    {
      cout << " <E> emcAnalyzerUtils::getEtaEvents() : zero events !?!?! returning -1 ..." << endl;
      return -1;
    }

  TString type_str = type ;

  if (type_str.Contains("val")) return Events[CC];
  else if (type_str.Contains("err")) return Sqrt(Events[CC]);
  
  cout << " <E> emcAnalyzerUtils::getEtaEvents() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;
}

//_____________________________________________________________________________
// Impact parameter
double
getImpactParam( const int Cent1, 
		const int Cent2, 
		const char *type_and_glauber )
{
  int CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  TString type_and_glauber_str = type_and_glauber ;

  const float b[CentClasses] = 
    { 
       2.3,  4.1, //  0- 5%,  5-10%
       5.2,  6.2, // 10-15%, 15-20%
       7.0,  7.8, // 20-25%, 25-30%
       8.4,  9.1, // 30-35%, 35-40%
       9.7, 10.2, // 40-45%, 45-50% 
      10.7, 11.2, // 50-55%, 55-60%
      11.7, 12.1, // 60-65%, 65-70%
      12.6, 13.0, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      14.1, 13.5, // 80-92%, 70-92%
#else
      14.1, 13.0, // 80-92%, 60-92%
#endif
      12.3,  9.5, // 60-80%, min.bias
       0.,   0.,  // 0-20%, 20-60% (not used for pi0 ...)
       0.,0.,0.,0.// 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };
  
  const float e_b[CentClasses] = 
    { 
      0.1, 0.2, //  0- 5%,  5-10%
      0.3, 0.2, // 10-15%, 15-20%
      0.4, 0.3, // 20-25%, 25-30%
      0.4, 0.4, // 30-35%, 35-40%
      0.4, 0.4, // 40-45%, 45-50%
      0.4, 0.4, // 50-55%, 55-60%
      0.5, 0.5, // 60-65%, 65-70%
      0.5, 0.6, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      0.6, 0.5, // 80-92%, 70-92%
#else
      0.6, 0.5, // 80-92%, 60-92%
#endif
      0.5, 0.4, // 60-80%, min.bias
      0.0, 0.0, // 0-20%, 20-60% (not used for pi0 ...)
      0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)
   };
  
  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_and_glauber_str.Contains("val") ) return b[CC];
      else if (type_and_glauber_str.Contains("err") ) return e_b[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_and_glauber_str.Contains("val") ) return (b[CC]+b[CC+1])/2;
      else if (type_and_glauber_str.Contains("err") ) return (e_b[CC]+e_b[CC+1])/2.;
    }
  
  cout << " <E> emcAnalyzerUtils::getImpactParam() : Something wrong with centrality class: "
       << Cent1 << "-" << Cent2 << "%" << endl;
  
  return crazyValue;

}

//_____________________________________________________________________________
// Num. of collisions
double
getNcoll( const int Cent1, 
	  const int Cent2, 
	  const char *type_and_glauber )
{

  int CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  TString type_and_glauber_str = type_and_glauber ;

  //_____________________________________________________________________________
  // ** OLD ** Glauber QM: Ncoll for sigma_inel = 43 mb

  if (type_and_glauber_str.Contains("old"))
    {
      const float Ncoll[CentClasses] = 
	{ 
	  1090.7, 858.9, //  0- 5%,  5-10%
	   680.2, 537.3, // 10-15%, 15-20%
	   421.6, 327.6, // 20-25%, 25-30%
	   250.4, 189.1, // 30-35%, 35-40%
	   140.2, 102.7, // 40-45%, 45-50%
	    73.5,  51.4, // 50-55%, 55-60%
	    35.0,  23.0, // 60-65%, 65-70%
	    14.9,   9.7, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      	     4.7,   8.2, // 80-92%, 70-92% --> 70-92% = (12.3*(10./22.)+4.7*(12./22.)) = 8.154
#else
	     4.7,  14.8, // 80-92%, 60-92%
#endif
	    20.7, 245.9, // 60-80%, min.bias

	     0.,    0.,  // 0-20%, 20-60% (not used in old glauber ...)
	  
	     0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)

	  //6.4,   4.2,// 80-90%
	  //2.8,   1.8 //90-100%
	};

      // percentual (%) relat errors for sigma_inel = 43 mb

      const float eNcoll_rel[CentClasses] = 
	{   
	  9.3,  9.9,  //  0- 5%,  5-10%
	  10.6, 11.3,  // 10-15%, 15-20%
	  12.5, 12.5,  // 20-25%, 25-30%
	  14.5, 14.5,  // 30-35%, 35-40%
	  17.2, 17.2,  // 40-45%, 45-50%
	  20.7, 20.7,  // 50-55%, 55-60%
	  25.7, 25.7,  // 60-65%, 65-70%
	  32.7, 32.7,  // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
	  42.8, 31.0,  // 80-92%, 70-92%  <------- FIXME: recompute 70-92% error !
#else
	  42.8, 31.0,  // 80-92%, 60-92%
#endif
	  29.2, 14.1,  // 60-80%, min.bias  <------- FIXME: recompute 60-80% error !

           0.,   0.,   // 0-20%, 20-60% (not used in old glauber ...)
	  
	   0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)

	  //42.8, 42.8,// 80-90%
	  //42.8, 42.8 //90-100%
	};

      // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
      if ( (Cent1 == Cent2-5) || (CC >= 16) )
	{
	  if (type_and_glauber_str.Contains("val")) return Ncoll[CC];
	  else if (type_and_glauber_str.Contains("err")) return Ncoll[CC]*eNcoll_rel[CC]/100.;
	}
      else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
	{
	  double mean = (Ncoll[CC]+Ncoll[CC+1])/2.;
	  if (type_and_glauber_str.Contains("val")) return mean;
	  else if (type_and_glauber_str.Contains("err")) return mean*(eNcoll_rel[CC]+eNcoll_rel[CC+1])/200.;
	}
    }

  //_____________________________________________________________________________
  // ** DEFAULT ** : type_and_glauber = "value_new"

  // Glauber new:  Ncoll for sigma_inel = 42 mb

  const float Ncoll[CentClasses] = 
    { 
      1065.4, 845.4, //  0- 5%,  5-10%
       672.4, 532.7, // 10-15%, 15-20%
       421.8, 325.6, // 20-25%, 25-30%
       251.0, 188.6, // 30-35%, 35-40%
       139.4, 101.3, // 40-45%, 45-50%
        72.1,  49.9, // 50-55%, 55-60%
        34.4,  22.6, // 60-65%, 65-70%
        14.8,   9.9, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
        4.9,   8.3, // 80-92%, 70-92%
#else
        4.9,  14.5, // 80-92%, 60-92%
#endif
        20.4, 257.8, // 60-80%, min.bias
       779.0, 193.7, //  0-20%, 20-60%
         0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };

  // absolute Ncoll errors for sigma_inel = 42 mb

  const float eNcoll[CentClasses] = 
    {   
     105.3, 82.1,  //  0- 5%,  5-10%
      66.8, 52.1,  // 10-15%, 15-20%
      46.8, 32.4,  // 20-25%, 25-30%
      25.9, 20.6,  // 30-35%, 35-40%
      15.4, 12.1,  // 40-45%, 45-50%
      10.5,  9.6,  // 50-55%, 55-60%
       8.7,  6.6,  // 60-65%, 65-70%
       5.1,  3.3,  // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
       1.2,  2.4,  // 80-92%, 70-92%
#else
       1.2,  4.0,  // 80-92%, 60-92%
#endif
       5.9, 25.4,  // 60-80%, min.bias
      75.2, 19.1,  //  0-20%, 20-60%
       0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };

  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_and_glauber_str.Contains("val")) return Ncoll[CC];
      else if (type_and_glauber_str.Contains("err")) return eNcoll[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      double mean = (Ncoll[CC]+Ncoll[CC+1])/2.;
      if (type_and_glauber_str.Contains("val")) return mean;
      else if (type_and_glauber_str.Contains("err")) return (eNcoll[CC]+eNcoll[CC+1])/2.;
    }

  cout << " <E> emcAnalyzerUtils::getNcoll() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;

}

//_____________________________________________________________________________
// Ncoll pp
double
getNcoll( const char *type )
{
  const double Ncoll_pp = 1;
  if (strcmp(type,"pp") == 0) return Ncoll_pp;
  else return 0;
}

//_____________________________________________________________________________
void
dumpCentDependParams( int step, 
		      const char *what_to_dump, // "Npart,Ncoll,TAB,ET,EBj,impactparam,Aoverlap,all"
		      const char *glauber,
		      const char *style ) // ascii, LaTeX
{

  int Cent1, Cent2;
  TString what = what_to_dump;

  //if (what.Contains("all",TString::kIgnoreCase)) what = "impactparam,Npart,Ncoll,Aoverlap,TAB,ET,EBj";
  if (what.Contains("all",TString::kIgnoreCase)) what = "impactparam,Npart,Ncoll,Aoverlap,ET,EBj";

  TString glauber_str = glauber;
  TString val = "value_" + glauber_str;
  TString err = "error_" + glauber_str;

  TString sty = style;

  TString title = "Table of " + what ;

  double ncoll = 0.;
  double encoll = 0.;
  double npart = 0.;
  double enpart = 0.;
  double T_AB = 0.;
  double eT_AB = 0.;
  double E_T = 0.;
  double eE_T = 0.;
  double E_bj = 0.;
  double eE_bj = 0.;
  double b = 0.;
  double e_b = 0.;
  double A_overlap = 0.;
  double eA_overlap = 0.;
  
  if (sty.Contains("ascii",TString::kIgnoreCase))
    {
      cout << "Cent.Class --> b +/- eb   Ncoll +/- eNcoll   Npart +/- eNpart   " << endl;
    }
  else if (sty.Contains("latex",TString::kIgnoreCase))
    {
      char *space2 = "\\hspace{2mm}";
      char *space4 = "\\hspace{4mm}";

      printf("\n\\begin{table}[H] \n");
      printf("\\begin{center} \n");
      printf("\\begin{tabular}{|c|c|c|c|c|c|c|c}\\hline\\hline \n");
      printf("%s Centrality %s & %s Impact param. $\\langle b \\rangle$ (fm) %s  & %s $\\langle N^{coll}\\rangle$ %s & %s $\\langle N^{part}\\rangle$ %s & %s $\\langle T_{AB} \\rangle$ (mb$^{-1}$) %s & %s $\\langle A_{overlap} \\rangle$ (fm$^{-2}$) %s & %s $\\langle E_T\\rangle$ (GeV) %s & %s $\\langle\\epsilon_{Bjorken}\\rangle$ (GeV/fm$^3$)  %s \\\\\\hline \n"
	     ,space2,space2,space2,space2,space4,space4,space4,space4,space2,space2,space2,space2,space2,space2,space2,space2);
    }

  for (int i=0; i<=CentClasses; i+=step)
    {
      if (step==1)
	{
	  getCentralityClassLimits( i, Cent1, Cent2);
	}
      else if (step==2)
	{
	  getCentralityClassLimits2( i, Cent1, Cent2);
	}

      if (what.Contains("coll",TString::kIgnoreCase))
	{
	  ncoll = getNcoll(Cent1,Cent2,val.Data());
	  encoll = getNcoll(Cent1,Cent2,err.Data());
	}
      if (what.Contains("part",TString::kIgnoreCase))
	{
	  npart = getNpart(Cent1,Cent2,val.Data());
	  enpart = getNpart(Cent1,Cent2,err.Data());
	}
      if (what.Contains("tab",TString::kIgnoreCase))
	{
	  T_AB = getTAB(Cent1,Cent2,val.Data());
	  eT_AB = getTAB(Cent1,Cent2,err.Data());
	}
      if (what.Contains("et",TString::kIgnoreCase))
	{
	  E_T = getET(Cent1,Cent2,val.Data());
	  eE_T = getET(Cent1,Cent2,err.Data());
	}
      if (what.Contains("ebj",TString::kIgnoreCase))
	{
	  E_bj = getEBjorken(Cent1,Cent2,val.Data());
	  eE_bj = getEBjorken(Cent1,Cent2,err.Data());
	}
      if (what.Contains("overlap",TString::kIgnoreCase))
	{
	  A_overlap = getNormOverlapArea(Cent1,Cent2,val.Data());
	  eA_overlap = getNormOverlapArea(Cent1,Cent2,err.Data());
	}
      if (what.Contains("imp",TString::kIgnoreCase))
	{
	  b = getImpactParam(Cent1,Cent2,val.Data());
	  e_b = getImpactParam(Cent1,Cent2,err.Data());
      }

      if (sty.Contains("ascii")) // only Ncoll & Npart
	{
	  printf("(%d,%d%%)  -->  %.1f +/- %.1f | %.1f +/- %.1f | %.1f +/- %.1f |  %.1f +/- %.1f | %.1f +/- %.1f | %.1f +/- %.1f | %.1f +/- %.1f \n",
		 Cent1,Cent2,b,e_b,ncoll,encoll,npart,enpart,T_AB,eT_AB,A_overlap,eA_overlap,E_T,eE_T,E_bj,eE_bj);
	}
      else if (sty.Contains("latex",TString::kIgnoreCase))
	{
	  printf(" %d - %d\\%% & %.1f $\\pm$ %.1f & %.1f $\\pm$ %.1f & %.1f $\\pm$ %.1f & %.1f $\\pm$ %.1f & %.1f $\\pm$ %.1f & %.1f $\\pm$ %.1f & %.1f $\\pm$ %.1f \\\\ \n",
		Cent1,Cent2,b,e_b,ncoll,encoll,npart,enpart,T_AB,eT_AB,A_overlap,eA_overlap,E_T,eE_T,E_bj,eE_bj);
	}
    }

  if (sty.Contains("latex",TString::kIgnoreCase))
    {
      printf("\\hline\\hline \n");
      //  printf("\\vspace{5mm} \n");
      printf("\\end{tabular} \n");
      printf("\\caption{%s .}\n", title.Data());
      printf("\\label{tab:} \n");
      printf("\\end{center} \n");
      printf("\\end{table} \n");
    }

}

//_____________________________________________________________________________
// Npart pp
double
getNpart( const char *type )
{
  const double Npart_pp = 2;
  if (strcmp(type,"pp") == 0) return Npart_pp;
  else return 0;
}

//_____________________________________________________________________________
// Num. of participants
double
getNpart( const int Cent1, 
	  const int Cent2, 
	  const char *type_and_glauber)
{

  int CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  TString type_and_glauber_str = type_and_glauber ;

  //_____________________________________________________________________________
  // Npart for sqrt(s) = 130 GeV

  float npart130[CentClasses] = { 347.7, 294.0, 
				  249.5, 211.0, 
				  178.6, 149.7,
				  124.8, 102.9, 
				  83.2,  66.3, 
				  52.1,  40.1,
				  30.1,  21.9,
				  0,0,
				  0,0,0,0,
				  0,0,
				  0,0,0};

  //_____________________________________________________________________________
  // Npart for sqrt(s) = 62 GeV

//   double npart62[CentClasses] = { 345.1 , 9.9 475.4 , 0.4 ,33.1
// 				  296.3 , 8.9 382.6 , 0.4 ,26.4
// 				  250.3 , 8.0 310.3 , 0.3 ,21.6
// 				  211.0 , 7.2 252.8 , 0.3 ,18.0
// 				  177.7 , 6.6 205.4 , 0.2 ,15.3
// 				  148.7 , 6.0 165.4 , 0.2 ,13.1
// 				  123.8 , 5.5 131.6 , 0.2 ,11.1
// 				  102.1 , 5.0 103.8 , 0.2 ,9.8
// 				  82.9 , 4.7 80.2 , 0.2 ,8.5
// 				  66.2 , 4.3 60.9 , 0.1 ,7.2};
  
  //_____________________________________________________________________________
  //
  // **** OLD **** Glauber QM: Npart for sigma_inel = 43 mb
  //

  if (type_and_glauber_str.Contains("old"))
    {
      const float Npart[CentClasses] = 
	{ 
	  353.4, 300.2, //  0-10%
	  254.3, 215.2, // 10-20%
	  181.0, 151.3, // 20-30% 
	  125.2, 102.7, // 30-40%
	   83.3,  66.7, // 40-50%
      	   52.5,  40.2, // 50-60%
	   30.2,  22.0, // 60-70%
	   15.7,  11.1, // 70-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
	    6.1,   9.7, // 80-92%, 70-92% <------- FIXME: recompute 70-92% error !
#else
	    6.1,  14.9, // 80-92%, 60-92% 
#endif
	   20.0, 102.7, // 60-80%, min.bias <------- FIXME: recompute 60-80% error !
	  
	  //7.9,   5.6,// 80-90%
	  //4.1,   2.9 //90-100%

	  0.,0.,       // 0-20, 20-60 (not used for pi0 ...)
	  0.,0.,0.,0.  // 10-30, 30-60, 60-85, 0-85 (to be implemented)
	};

      // percentual(%) relat. errors for sigma_inel = 43 mb
      const float eNpart_rel[CentClasses] = 
	{  
	  2.5,  2.8,   //  0-10%
	  3.1,  3.5,   // 10-20%
	  4.2,  4.2,   // 20-30% 
	  5.4,  5.4,   // 30-40%
	  7.2,  7.2,   // 40-50%
	  10.1, 10.1,  // 50-60%
	  15.0, 15.0,  // 60-70%
	  23.3, 23.3,  // 70-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
	  41.2, 22.2,  // 80-92%, 70-92% <------- FIXME: recompute 70-92% error !
#else
	  41.2, 22.2,  // 80-92%, 60-92%
#endif
	  19.1,  5.8,  // 60-80%, min.bias <------- FIXME: recompute 60-80% error !
	  0.,0.,       // 0-20, 20-60 (not used for pi0 ...)
	  0.,0.,0.,0.  // 10-30, 30-60, 60-85, 0-85 (to be implemented)
	};

      // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
      if ( (Cent1 == Cent2-5) || (CC >= 16) )
	{
	  if (type_and_glauber_str.Contains("val")) return Npart[CC];
	  else if (type_and_glauber_str.Contains("err")) return Npart[CC]*eNpart_rel[CC]/100.;
	}
      else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
	{
	  double mean = (Npart[CC]+Npart[CC+1])/2.;
	  if (type_and_glauber_str.Contains("val")) return mean;
	  else if (type_and_glauber_str.Contains("err")) return mean*(eNpart_rel[CC]+eNpart_rel[CC+1])/200.;
	}
    }

  //_____________________________________________________________________________
  // ** DEFAULT ** : type_and_glauber = "value_new"

  // Glauber new:  Npart for sigma_inel = 42 mb

  const float Npart[CentClasses] = 
    { 
      351.4, 299.0, //  0-10%
      253.9, 215.3, // 10-20%
      181.6, 151.5, // 20-30% 
      125.7, 102.7, // 30-40%
       82.9,  65.9, // 40-50%
       51.6,  39.4, // 50-60%
       29.8,  21.5, // 60-70%
       15.5,  11.3, // 70-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
        6.3,   9.5, // 80-92%, 70-92%
#else
        6.3,  14.5, // 80-92%, 60-92% 
#endif
       19.5, 109.1, // 60-80%, min.bias
      279.9, 100.2, //  0-20%, 20-60%
        0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };

  // absolute Npart errors for sigma_inel = 42 mb

  const float eNpart[CentClasses] = 
    {  
      2.9,  3.8,  //  0-10%
      4.3,  5.3,  // 10-20%
      5.6,  4.9,  // 20-30% 
      4.9,  4.3,  // 30-40%
      4.3,  3.4,  // 40-50%
      3.2,  3.5,  // 50-60%
      4.1,  3.8,  // 60-70%
      3.4,  2.6,  // 70-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      1.2,  1.9,  // 80-92%, 70-92%
#else
      1.2,  2.5,  // 80-92%, 60-92%
#endif
      3.3,  4.1,  // 60-80%, min.bias
      4.0,  3.4,  //  0-20%, 20-60%
      0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };

  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_and_glauber_str.Contains("val")) return Npart[CC];
      else if (type_and_glauber_str.Contains("err")) return eNpart[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      double mean = (Npart[CC]+Npart[CC+1])/2.;
      if (type_and_glauber_str.Contains("val")) return mean;
      else if (type_and_glauber_str.Contains("err")) return (eNpart[CC]+eNpart[CC+1])/2.;
    }

  cout << " <E> emcAnalyzerUtils::getNpart() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;

}

//_____________________________________________________________________________
// Nuclear overlap function

double
getTAB( const int Cent1, 
	const int Cent2, 
	const char *type_and_glauber)
{

  int CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  TString type_and_glauber_str = type_and_glauber ;

  const float TAB[CentClasses] = 
    { 
      25.37, 20.13, //  0- 5%,  5-10%
      16.01, 12.68, // 10-15%, 15-20%
      10.04,  7.75, // 20-25%, 25-30%
      5.98,  4.49, // 30-35%, 35-40%
      3.32,  2.41, // 40-45%, 45-50% 
      1.72,  1.19, // 50-55%, 55-60%
      0.82,  0.54, // 60-65%, 65-70%
      0.35,  0.24, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      0.12,  0.20, // 80-92%, 70-92%
#else
      0.12,  0.35, // 80-92%, 60-92%
#endif
      0.49,  6.14, // 60-80%, min.bias
     18.55,  4.61, //  0-20%, 20-60%
      0.,0.,0.,0.  // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };
  
  const float eTAB[CentClasses] = 
    { 
      1.77, 1.36, //  0- 5%,  5-10%
      1.15, 0.86, // 10-15%, 15-20%
      0.85, 0.58, // 20-25%, 25-30%
      0.48, 0.43, // 30-35%, 35-40%
      0.31, 0.25, // 40-45%, 45-50%
      0.23, 0.23, // 50-55%, 55-60%
      0.21, 0.16, // 60-65%, 65-70%
      0.12, 0.08, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      0.03, 0.06, // 80-92%, 70-92%
#else
      0.03, 0.10, // 80-92%, 60-92%
#endif
      0.14, 0.45, // 60-80%, min.bias
      1.27, 0.36, //  0-20%, 20-60%
      0.,0.,0.,0. // 10-30, 30-60, 60-85, 0-85 (to be implemented)

    };
  
  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_and_glauber_str.Contains("val") ) return TAB[CC];
      else if (type_and_glauber_str.Contains("err") ) return eTAB[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_and_glauber_str.Contains("val") ) return (TAB[CC]+TAB[CC+1])/2;
      else if (type_and_glauber_str.Contains("err") ) return (eTAB[CC]+eTAB[CC+1])/2.;
    }

  cout << " <E> emcAnalyzerUtils::getTAB() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;  
  
}

//_____________________________________________________________________________
// A_overlap (fm^2)

double
getOverlapArea( const int Cent1, 
		const int Cent2, 
		const char *type)
{

  int CC = getCentralityClass(Cent1, Cent2);

  if ( CC == -1) return -1;

  TString type_str = type ;
  
  const double Aoverlap[CentClasses] = 
    { 
      26.40, 23.68, //  0- 5%,  5-10%
      21.20, 19.06, // 10-15%, 15-20%
      17.16, 15.48, // 20-25%, 25-30%
      14.00, 12.60, // 30-35%, 35-40%
      11.36, 10.32, // 40-45%, 45-50% 
       9.30,  8.50, // 50-55%, 55-60%
       7.70,  7.03, // 60-65%, 65-70%
       6.42,  5.91, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
       4.92,  5.48, // 80-92%, 70-92%
#else
       4.92,  6.07, // 80-92%, 60-92%
#endif
       6.76, 12.36, // 60-80%, min.bias
       crazyValue, crazyValue,  //  0-20%, 20-60% (to be defined later ...)
       0.,0.,0.,0.  // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };
  

  // FIXME: Those errors are too large !
  const double eAoverlap[CentClasses] = 
    { 
      1.63, 1.77, //  0- 5%,  5-10%
      1.77, 1.79, // 10-15%, 15-20%
      1.80, 1.88, // 20-25%, 25-30%
      1.91, 1.98, // 30-35%, 35-40%
      2.04, 2.19, // 40-45%, 45-50%
      2.30, 2.49, // 50-55%, 55-60%
      2.73, 2.97, // 60-65%, 65-70%
      3.29, 3.77, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      4.44, 4.11, // 80-92%, 70-92%
#else
      4.44, 3.87, // 80-92%, 60-92%
#endif
      3.28, 7.03,  // 60-80%, min.bias
      crazyValue, crazyValue,  //  0-20%, 20-60% (to be defined later ...)
      0.,0.,0.,0.  // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };

   // FIXME: Errors in eAoverlap taken as (%) eT_AB for the time being ...

  double error = Aoverlap[CC]*getTAB(Cent1,Cent2,"err")/getTAB(Cent1,Cent2,"val");
 
  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if ( type_str.Contains("val") )
	{
	  return Aoverlap[CC];
	}
      else if ( type_str.Contains("err") )
	{
	  //return eAoverlap[CC];
	  return error;
	}
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if ( type_str.Contains("val") )
	{
	  return (Aoverlap[CC]+Aoverlap[CC+1])/2;
	}
      else if ( type_str.Contains("err") )
	{
	  //return (eAoverlap[CC]+eAoverlap[CC+1])/2.;
	  return error;
	}
    }

  cout << " <E> emcAnalyzerUtils::getOverlapArea() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;

}

//_____________________________________________________________________________
// A_overlap normalized to Au full area

double
getNormOverlapArea( const int Cent1, 
		    const int Cent2, 
		    const char *type )
{

  double R_Au = 6.38; // Au radius: R = 6.38 fm
  double eR_Au = 0.27;
  double a_Au = 0.53; // Au skin: a = 0.53 fm
  double ea_Au = 0.01;

  double max_Au_area  = Pi()*(R_Au+a_Au)*(R_Au+a_Au); // Area ~ 150.0 fm^2
  double emax_Au_area = max_Au_area*quadRelatError(eR_Au,R_Au,ea_Au,a_Au);

  double max_Aoverlap = 27.0; // Glauber MC overlap "area" for 0-1% (max ~ 28.0 from inspection of glauber_AuAu200.root)
  double emax_Aoverlap = 0.0; // the error will be propagated from the overlap area (no error over-counting)
  
  double Aoverlap = getOverlapArea(Cent1,Cent2,"val");
  double eAoverlap = getOverlapArea(Cent1,Cent2,"err");
  
  double eff_Au_area = max_Au_area*(Aoverlap/max_Aoverlap); // effective nuclear area for the centrality class
  double e_eff_Au_area = quadRelatError(emax_Au_area,max_Au_area,
					emax_Aoverlap,max_Aoverlap,
					eAoverlap,Aoverlap);

  TString type_str = type ;

  if ( type_str.Contains("val") ) return eff_Au_area;
  else if ( type_str.Contains("err") ) return eff_Au_area*e_eff_Au_area;

  cout << " <E> emcAnalyzerUtils::getNormOverlapArea() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;

}

//_____________________________________________________________________________
// 

double
getEccentricity( const int Cent1, 
		 const int Cent2, 
		 const char *type )
{


  int CC = getCentralityClass(Cent1, Cent2);
  
  if ( CC == -1) return -1;

  const float ecc[CentClasses] = 
    { 
      0.027, 0.086, //  0- 5%,  5-10%
      0.140, 0.182, // 10-15%, 15-20%
      0.224, 0.257, // 20-25%, 25-30%
      0.287, 0.315, // 30-35%, 35-40%
      0.337, 0.360, // 40-45%, 45-50% 
      0.372, 0.383, // 50-55%, 55-60%
      0.397, 0.399, // 60-65%, 65-70%
      0.392, 0.381, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      0.261, 0.317, // 80-92%, 70-92% 
#else
      0.261, 0.342, // 80-92%, 60-92%
#endif
      0.392, 0.281, // 60-80%, min.bias
      0.109, 0.317, //  0-20%, 20-60%
      0.,0.,0.,0.   // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };
  
  const float e_ecc[CentClasses] = 
    { 
      0.007, 0.011, //  0- 5%,  5-10%
      0.023, 0.020, // 10-15%, 15-20%
      0.031, 0.022, // 20-25%, 25-30%
      0.025, 0.032, // 30-35%, 35-40%
      0.039, 0.046, // 40-45%, 45-50%
      0.048, 0.069, // 50-55%, 55-60%
      0.052, 0.054, // 60-65%, 65-70%
      0.075, 0.115, // 70-75%, 75-80%
#ifdef _CENTRALITY_CLASS_17_IS_70_92_
      0.082, 0.084, // 80-92%, 70-92%
#else
      0.082, 0.074, // 80-92%, 60-92%
#endif
      0.072, 0.045, // 60-80%, min.bias
      0.014, 0.038, //  0-20%, 20-60%
      0.,0.,0.,0.   // 10-30, 30-60, 60-85, 0-85 (to be implemented)
    };

  TString type_str = type ;

  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_str.Contains("val") ) return ecc[CC];
      else if (type_str.Contains("err") ) return e_ecc[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_str.Contains("val") ) return (ecc[CC]+ecc[CC+1])/2;
      else if (type_str.Contains("err") ) return (e_ecc[CC]+e_ecc[CC+1])/2.;
    }

  cout << " <E> emcAnalyzerUtils::getEccentricity() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;  

}

//_____________________________________________________________________________
// Transverse energies

double
getET( const int Cent1, 
       const int Cent2, 
       const char *type)
{

  // Data from AN168: /phenix/WWW/p/draft/shura/AN_etnch_feb03/

  const double ET[CentClasses]     = { 611.7, 497.2,  //   0-10%
				       405.1, 329.4,  //  10-20%
				       267.4, 216.4,  //  20-30%
				       172.5, 136.4,  //  30-40%
				       106.4,  81.1,  //  40-50%
				        59.7,  43.2,  //  50-60%
				        30.5,  20.5,  //  60-70%
				         0,0,
				         0,0,0,0,
				         0,0,
				         0,0,0};

  const double eET[CentClasses]    = { 27.0, 22.5,  //   0-10%
				       19.2, 16.4,  //  10-20%
				       14.4, 12.8,  //  20-30%
				       11.3, 10.1,  //  30-40%
				        8.9,  7.8,  //  40-50%
				        6.9,  5.8,  //  50-60%
				        4.8,  3.9,  //  60-70%
				        0,0,
				        0,0,0,0,
				        0,0,
				        0,0,0};

  int CC = getCentralityClass(Cent1, Cent2);
  
  if ( CC == -1) return -1;

  TString type_str = type ;
  
  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_str.Contains("val")) return ET[CC];
      else if (type_str.Contains("err")) return eET[CC];
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_str.Contains("val")) return (ET[CC]+ET[CC+1])/2;
      else if (type_str.Contains("err")) return (eET[CC]+eET[CC+1])/2.;
    }

  cout << " <E> emcAnalyzerUtils::getET() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;
}

//_____________________________________________________________________________
// returns (e)EBjorken for a given centrality class

double 
getEBjorken( const int Cent1, 
	     const int Cent2, 
	     const char *type )
{

  double EBjorken = 0.;
  double eEBjorken = 0.;

  int CC = getCentralityClass(Cent1, Cent2);
  
  if ( CC == -1) return EBjorken;
    
  double eff_Au_area = getNormOverlapArea(Cent1,Cent2,"val"); // effective nuclear area for the centrality class
  double e_eff_Au_area = getNormOverlapArea(Cent1,Cent2,"err"); // effective nuclear area for the centrality class

  double eta_to_y = 1.2 ;
  double e_eta_to_y = 0.01 ;

  double tau0 = 1.; // fm/c

  double Et  = getET(Cent1,Cent2,"val");
  double eEt = getET(Cent1,Cent2,"err");
  
  EBjorken = eta_to_y*Et/(eff_Au_area*tau0);

  eEBjorken = quadRelatError(e_eff_Au_area,eff_Au_area,
			     e_eta_to_y,eta_to_y,
			     eEt,Et);
  eEBjorken *= EBjorken;
  
  TString type_str = type ;

  if (type_str.Contains("val")) return EBjorken;
  else if (type_str.Contains("err")) return eEBjorken;

  cout << " <E> emcAnalyzerUtils::getEBjorken() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;

}

//_____________________________________________________________________________
// returns dNch/deta (sqrt(s) = 130 GeV, 200 GeV) for a given centrality class

double 
getNch( const int Cent1, 
	const int Cent2, 
	const int sqrts, 
	const char *type )
{

  // default is sqrt(s) = 200

  double Nch200[CentClasses] = { 687.4, 560.4, //  0-10%
				 456.8, 371.5, // 10-20%
				 302.5, 245.6, // 20-30% 
				 197.2, 156.4, // 30-40%
				 123.5,  95.3, // 40-50%
				  70.9,  52.2, // 50-60%
				  37.5,  25.6, // 60-70%
				   0.,    0.,
				   0.,0.,0.,0.,
				   0.,0.,
				   0.,0.,0.};
  
  double eNch200[CentClasses]  = { 36.6, 27.9,
				   22.3, 18.2,
				   15.8, 13.8,
				   12.2, 10.9,
				   9.6,  8.6,
				   7.6,  6.5,
				   5.4,  4.5, // 60-70%
				   0.,   0.,
				   0.,0.,0.,0.,
				   0.,0.,
				   0.,0.,0.};

  double Nch130[CentClasses] = { 601.8, 488.5,  //   0-10%
				 402.7, 328.8,  //  10-20%
				 270.5, 219.3,  //  20-30%
				 175.8, 139.0,  //  30-40%
				 109.4,  84.1,  //  40-50%
				 64.3,  48.4,  //  50-60%
				 35.2,  25.3,  //  60-70%
				 0.,    0.,
				 0.,0.,0.,0.,
				 0.,0.,
				 0.,0.,0.};
  
  double eNch130[CentClasses] = { 28.4, 21.7,
				  17.4, 15.0,
				  12.8, 11.5,
				  10.4,  9.2,
				  8.4,  7.1,
				  6.3,  5.5,
				  4.5,  4.2,  //  60-70%
				  0.,   0.,
				  0.,0.,0.,0.,
				  0.,0.,
				  0.,0.,0.};

//   double Nch62[CentClasses] = { 345.1 , 9.9 475.4 , 0.4 ,33.1
// 				296.3 , 8.9 382.6 , 0.4 ,26.4
// 				250.3 , 8.0 310.3 , 0.3 ,21.6
// 				211.0 , 7.2 252.8 , 0.3 ,18.0
// 				177.7 , 6.6 205.4 , 0.2 ,15.3
// 				148.7 , 6.0 165.4 , 0.2 ,13.1
// 				123.8 , 5.5 131.6 , 0.2 ,11.1
// 				102.1 , 5.0 103.8 , 0.2 ,9.8
// 				82.9 , 4.7 80.2 , 0.2 ,8.5
// 				66.2 , 4.3 60.9 , 0.1 ,7.2};
  
//   double eNch62[CentClasses] = { 345.1 , 9.9 475.4 , 0.4 ,33.1
// 				 296.3 , 8.9 382.6 , 0.4 ,26.4
// 				 250.3 , 8.0 310.3 , 0.3 ,21.6
// 				 211.0 , 7.2 252.8 , 0.3 ,18.0
// 				 177.7 , 6.6 205.4 , 0.2 ,15.3
// 				 148.7 , 6.0 165.4 , 0.2 ,13.1
// 				 123.8 , 5.5 131.6 , 0.2 ,11.1
// 				 102.1 , 5.0 103.8 , 0.2 ,9.8
// 				 82.9 , 4.7 80.2 , 0.2 ,8.5
// 				 66.2 , 4.3 60.9 , 0.1 ,7.2};

  int CC = getCentralityClass(Cent1, Cent2);
  
  if ( CC == -1) return -1;

  TString type_str = type ;

  if (sqrts != 130 && sqrts != 200 && sqrts != 62)
    {
      cout << " <E> emcAnalyzerUtils::getNch() : I don't know Nch for sqrt(s) = " 
	   << sqrts << endl;
      return crazyValue;
    }  
  
  // case 0-5, 15-20 ... or special cases: 60-80, min. bias ...
  if ( (Cent1 == Cent2-5) || (CC >= 16) )
    {
      if (type_str.Contains("val"))
	{
	  if (sqrts == 200) return Nch200[CC];
	  if (sqrts == 130) return Nch130[CC];
	  //if (sqrts ==  62) return Nch62[CC];
	}
      else if (type_str.Contains("err"))
	{
	  if (sqrts == 200) return eNch200[CC];
	  if (sqrts == 130) return eNch130[CC];
	  //if (sqrts ==  62) return eNch62[CC];
	}
    }
  else if ( (Cent1 == Cent2-10) ) // case 0-10, 30-40 ...
    {
      if (type_str.Contains("val"))
	{
	  if (sqrts == 200) return (Nch200[CC]+Nch200[CC+1])/2;
	  if (sqrts == 130) return (Nch130[CC]+Nch130[CC+1])/2;
	  //if (sqrts ==  62) return (Nch62[CC] + Nch62[CC+1])/2;
	}
      else if (type_str.Contains("err"))
	{
	  if (sqrts == 200) return (eNch200[CC]+eNch200[CC+1])/2.;
	  if (sqrts == 130) return (eNch130[CC]+eNch130[CC+1])/2.;
	  //if (sqrts ==  62) return (eNch62[CC]+eNch62[CC+1])/2.;
	}
    }

  cout << " <E> emcAnalyzerUtils::getNch() : Something wrong with centrality class: "
       << Cent1 << " - " << Cent2 << "%" << endl;

  return crazyValue;

}

//_____________________________________________________________________________
// dNch/deta ppbar (UA5 data)

double
getNch_ppbar( const double sqrts, 
	      const char *type )
{
  // so far only sqrt(s) = 200 GeV

  TString type_str = type ;

  if (type_str.Contains("val")) return 2.48;
  else if (type_str.Contains("err")) return 0.1;

  return -1;
}


//_____________________________________________________________________________
// returns val[line][column]
// e.g.:   val[0][1] = 2nd value of 1st column

double**
readAscii( const char* datafile, 
	   int &ncols,
	   int &nlines,
	   int skiplines,
	   const int verbose )
{

  // accessing ncols and nlines directly not recommended for now: set to zero ...
  ncols = 0;
  nlines = 0;

  // default values
  int const maxcols  =  50;
  int const maxlines = 1000;
  int const maxlinewidth = 256;

  int emptylines = 0;

  char file[400];
  sprintf(file,"%s",datafile);

  strstream strline;

  char line[maxlinewidth];
  int linesize = sizeof(line);

  int totlinecounter = 0, totcolcounter = 0;
  int k = 0;

  double firstline[maxcols];

  double **val = 0;

  ifstream ifiledata(file);
  if (!ifiledata)
    {
	  cout << " <E> emcAnalyzerUtils::readAscii : Can't open file: " << file << endl;
	  return val;
    }
  ifiledata.close();
  if (verbose) 
    {
      cout << " <I> emcAnalyzerUtils::readAscii : Trying to read " << file  
	   << " (skipping the first " << skiplines << " lines) ..." << endl;
    }

  // input ncols and/or nlines given
  if ((!(ncols > 0)) || (!(nlines > 0))) 
    {
      ifiledata.open(file, ios::in);

      // Reading first line and determining the number of columns
      if (!(ncols > 0)) 
	{
	  while (k++<=skiplines) // skip first line(s)
	    {
	      ifiledata.getline(line,linesize);
	      totlinecounter++;
	    }
	  strline << line;

	  do{
	    if  (totcolcounter>maxcols) 
	      {
		cout << " <E> First line unreadable : " << file 
		     << " (Try skipping a few lines ? Too long rows/columns ?)" << endl;
		return val; // strange files
	      }
	    strline >> firstline[totcolcounter++];
	  } while (!strline.eof());

	  ncols = totcolcounter;
	  //ncols = totcolcounter-1;
	  strline.clear();

	}

      if (verbose) 
	{
	  cout << " <I> emcAnalyzerUtils::readAscii : Found " << ncols  
	       << " columns in file " << file << " ..." << endl;
	}
      
      // Reading the rest of the file and determining the number of lines
      if (!(nlines > 0)) 
	{
	  do {
	    if  (totlinecounter>maxlines)
	      {
		cout << " <E> Unreadable file: " << file 
		     << " (Try skipping a few lines ?)" << endl;
		return val; // strange files
	      }
	    ifiledata.getline(line,linesize);
	    //if (verbose > 1) cout << ": " << line << endl;
	    //printf("line length: %d\n",strlen(line));
	    if ( (!ifiledata.eof()) && (strlen(line) <= 2) ) emptylines++; // empty line (= line with <= 2 characters)
	    totlinecounter++;
	  } while ( (!ifiledata.eof()) && (emptylines<2) ); // we stop reading if we find >=2 empty lines in file

	  if (ifiledata.eof()) nlines = totlinecounter-1;
	  else nlines = totlinecounter;

	  strline.clear();
	}
      
      ifiledata.close();
    }
  
  if (nlines<skiplines) skiplines = nlines;

  // reopening the file for real read
  ifiledata.open(file, ios::in);

  // allocating memory
  val = new double*[ncols];
  for (int j=0;j<ncols;j++)
    val[j] = new double[nlines-skiplines-emptylines];

  // reading file
  for (int i=0; i<nlines; i++) 
    {
      ifiledata.getline(line,linesize);

      if ( strlen(line) > 2 ) // skip empty line(s)
	{
	  if (i>=skiplines)  // skip the first line(s)
	    {
	      strline << line;
	      for (int j=0;j<ncols;j++) 
		{
		  strline >> val[j][i-skiplines];
		  
		  if (verbose>1) 
		    { 
		      printf("[%d][%d]=%.1e ",j,i-skiplines,val[j][i-skiplines]);
			     //cout << "[" << j << "][" << i-skiplines << "]=" 
			     //	   << val[j][i-skiplines] << "  " ;
		    }
		}
	      if (verbose>1) { cout << endl; }
	    }
	}
      strline.clear();
	  
      if ( i && (i%1000)==0) { cout << i << endl; }
    }

  ifiledata.close();

  if (verbose)
    {
      cout << " <I> emcAnalyzerUtils::readAscii: " << file << endl;
      cout << "  -- ncols: " << ncols << endl;
      cout << "  -- nlines (total): " << nlines << endl;
      cout << "  -- skipped lines (on top): " << skiplines << endl;
      cout << "  -- empty lines (on bottom): " << emptylines ;
      if (emptylines>1) cout << " (didn't actually read the file beyond the 2 last empty lines).";
      cout << endl;
    }

  nlines-=(skiplines+emptylines);

  return val;

}

//_____________________________________________________________________________
//
// function to read data from an ascii file and create an histogram
// the file can have 2-3-4 columns of float data.
// it assumes that the values in the file are ordered increasingly
// IMPORTANT: only works if the data values have fixed binwidth !

TH1F* readAsciiToHisto( const char *filename, 
			const int ncols,
			const char *format,
			const int verbose )
{

#include "Riostream.h"

   ifstream in;
   in.open(filename, ios::in);

   float x,y,ex;
   float x1,x2;
   float ey = 0;
   int nbins = 0;
   double xmin = 0.;
   double xmax = 0.;
   double halfbinwidth = 0.;

   int rows = -1;

   // first read to determine number of bins
   // we don't care about format (as long as 1st column is x ...)
   while (1) {

     if ( ncols==2 )
       {
	 in >> x >> y;
	 if (verbose) printf(" x=%8f, x2=%8f\n",x,y);
       }
     else if (ncols==3)
       {
	 in >> x >> y >> ey;
	 if (verbose) printf(" x=%8f, x2=%8f, x3=%8f\n",x,y,ey);
       }
     else if (ncols==4)
       {
	 in >> x >> y >> ex >> ey;
	 if (verbose) printf(" x=%8f, x2=%8f, x3=%8f, x4=%8f\n",x,y,ex,ey);
       }

     if (nbins==0) xmin = x; // assuming ascii file is ordered increasingly
     if (nbins==1) halfbinwidth = 0.5*(x-xmin); // assuming ascii file has fixed binwidth

      if (!in.good()) break;
      nbins++;
   }
   printf(" found %d points\n",nbins);
   in.close();

   xmax = x; // last value read

   char *hname = (char*)gSystem->BaseName(filename);

   TH1F *hascii = new TH1F(hname,hname,nbins,xmin-halfbinwidth,xmax+halfbinwidth);
   printf(" booking histo %s with %d bins: xmin=%8f, xmax=%8f (binwidth=%8f) \n",
	  hname,nbins,xmin,xmax,halfbinwidth*2.);

   TString format_str = format;

   // second read to fill histo
   in.open(filename, ios::in);
   while (1) {

     if ( format_str.Contains("x-y-ex-ey") )
       {
	 if (ncols==2)
	   in >> x >> y;
	 else if (ncols==3)
	   in >> x >> y >> ex;
	 else if (ncols==4)
	   in >> x >> y >> ex >> ey;
       }
     else if ( format_str.Contains("x-ex-y-ey") )
       {
	 if (ncols==2)
	   in >> x >> ex;
	 else if (ncols==3)
	   in >> x >> ex >> y;
	 else if (ncols==4)
	   in >> x >> ex >> y >> ey;
       }
     else
       {
	 cout << " <E> unknown format: " << format << endl;
	 break;
       }

      if (!in.good()) break;

      int bin = hascii->GetXaxis()->FindBin(x);
      hascii->SetBinContent(bin,y);
      if (ey==0) ey=1.e-20; // set a minimum error
      hascii->SetBinError(bin,ey);
   }

   in.close();

   if (verbose) hascii->Print("all");

   float xmax2 = hascii->GetBinCenter(nbins); // last valid value of the histo (0=underflow, (nbins+1)=overflow)
   float epslon = xmax*10e-5;
   if (abs(xmax-xmax2)>epslon) // checking quasi-equality with floats
     {
       cout << endl << " <W> Problems filling the histo. "
	    << "probably: (a) some bin missing, or (b) some bin with different width ..." << endl ;
     }

   return hascii;
}

//_____________________________________________________________________________
// 
void dumpHisto( const TH1 *h, const char *format )
{
  if (!h) return;
  
  const size_t N = h->GetNbinsX();
  TString format_str = format;

  for (size_t i=1;i<=N;i++) // we start at i=1 since i=0 is for underflows
    {
      double x = h->GetBinCenter(i); 
      double y = h->GetBinContent(i);
      double ex = h->GetBinWidth(i)/2.;
      double ey = h->GetBinError(i);

      if ( format_str.Contains("x-y-ex-ey") )
	{
	  printf("%4.2f   %g   %g   %g\n",x,y,ex,ey);
	}
      else if ( format_str.Contains("x-ex-y-ey") )
	{
	  printf("%4.2f   %g   %g   %g\n",x,ex,y,ey);
	}
      else if ( format_str.Contains("x-y-ey") )
	{
	  printf("%4.2f   %g   %g\n",x,y,ey);
	}
    }

}

//_____________________________________________________________________________
//  dumps Graph
//  format options: "x-y-ex-ey" (and combinations), "array",

void dumpGraph( TGraph *gr, const char *format )
{
  if (!gr) return;
  
  size_t N = gr->GetN();
  TString format_str = format;

  double X[N],Y[N],eX[N],eY[N];
  double x,y,ex,ey;

  for (size_t i=0;i<N;i++) 
    {
      gr->GetPoint(i,x,y);
      if (gr->InheritsFrom("TGraphErrors")) // attempt to do the same as next function ...
	{
	  ex = gr->GetErrorX(i);
	  ey = gr->GetErrorY(i);
	}
      else
	{
	  ex = 0.;
	  ey = 0.;
	}

      if ( format_str.Contains("x-y-ex-ey") )
	{
	  printf("%4.2f   %g   %g   %g \n",x,y,ex,ey);
	}
      else if ( format_str.Contains("x-ex-y-ey") )
	{
	  printf("%4.2f   %g   %g   %g \n",x,ex,y,ey);
	}
      else if ( format_str.Contains("x-y-ey") )
	{
	  printf("%4.2f   %g   %g \n",x,y,ey);
	}
      else if ( format_str.Contains("x") )
	{
	  printf("%4.2f \n",x);
	}
      else if ( format_str.Contains("array") )
	{
	  X[i] = x;
	  Y[i] = y;
	  eX[i] = ex;
	  eY[i] = ey;
	}
      else if ( format_str.Contains("y") )
	{
	  printf("%g \n",y);
	}
    }

  if ( format_str.Contains("arraynon0") )
    {
      size_t i = 0;
      cout << "const int N = " << N << " ;" << endl;
      printf("double x[N]={"); 
      for (i=0;i<N-1;i++)
	{
	  if (Y[i]>0.) printf("%4.3f, ",X[i]); 
	}
      if (Y[i]>0.) printf("%4.3f};\n",X[N-1]);
      printf("double y[N]={"); 
      for (i=0;i<N-1;i++)
	{
	  if (Y[i]>0.) printf("%.2e, ",Y[i]); 
	}
      if (Y[i]>0.) printf("%.2e};\n",Y[N-1]);
      printf("double ex[N]={"); 
      for (i=0;i<N-1;i++) 
	{
	  if (Y[i]>0.) printf("%4.2f, ",eX[i]);
	}
      if (Y[i]>0.) printf("%4.2f};\n",eX[N-1]);
      printf("double ey[N]={"); 
      for (i=0;i<N-1;i++)
	{ 
	  if (Y[i]>0.) printf("%.2e, ",eY[i]);
	}
      if (Y[i]>0.) printf("%.2e};\n",eY[N-1]);
      cout << "TGraphErrors *" << gr->GetName() << " = new TGraphErrors(N,x,y,ex,ey);" << endl;
    }
  else if ( format_str.Contains("array") )
    {
      size_t i = 0;
      cout << "const int N = " << N << " ;" << endl;
      printf("double x[N]={"); for (i=0;i<N-1;i++) printf("%4.3f, ",X[i]); printf("%4.3f};\n",X[N-1]);
      printf("double y[N]={"); for (i=0;i<N-1;i++) printf("%.2e, ",Y[i]); printf("%.2e};\n",Y[N-1]);
      printf("double ex[N]={"); for (i=0;i<N-1;i++) printf("%4.2f, ",eX[i]); printf("%4.2f};\n",eX[N-1]);
      printf("double ey[N]={"); for (i=0;i<N-1;i++) printf("%.2e, ",eY[i]); printf("%.2e};\n",eY[N-1]);
      cout << "TGraphErrors *" << gr->GetName() << " = new TGraphErrors(N,x,y,ex,ey);" << endl;
    }

}

//_____________________________________________________________________________
// returns % value of normalization errors from Ncoll/TAB uncertainties for C1-C2% 
// or quadratically combined for C1-C2% and C3-C4% (e.g. ratios) 
// plus any other "extra" % normalization error passed as argument
//

double
getPropagNormRelatError( const int Cent1, 
			 const int Cent2,
			 const int Cent3, 
			 const int Cent4,
			 const double extra_erelat,
			 const bool bin_scaling,
			 const bool useTAB,
			 const int verbose )
{

  double eRelatNorm = 0;
  double  Norm1 = 0;
  double eNorm1 = 0;
  double  Norm2 = 0;
  double eNorm2 = 0;


  //_________________________________________________________________
  // get Normalization error from TAB or Ncoll for each Centrality class

  if ( bin_scaling ) // Binary scaling
    {

      char normtype[100];

      if ( !useTAB ) // Ncoll
	{
	  Norm1  = getNcoll(Cent1,Cent2,"val");
	  eNorm1 = getNcoll(Cent1,Cent2,"err");
	  sprintf(normtype,"Ncoll");
	}
      else // TAB
	{
	  Norm1  = getTAB(Cent1,Cent2,"val");
	  eNorm1 = getTAB(Cent1,Cent2,"err");
	  sprintf(normtype,"TAB");
	}

      if (verbose)
	{
	  cout << " <I> AuAu (" << Cent1 << "-" << Cent2 ;
	  cout <<"%) - " << normtype << " = " << Norm1 << " +/- " << eNorm1 << endl;
	}

      if (Cent3 == 0 && Cent4 == 0) // pp (assume Cent3 == Cent4 == 0)
	{
	  Norm2 = getNcoll("pp");
	  eNorm2 = 0;
	  if (verbose) cout << " <I> pp ref.: ";
	}
      else
	{
	  Norm2  = getNcoll(Cent3,Cent4,"val");
	  eNorm2 = getNcoll(Cent3,Cent4,"err");
	  if (verbose) cout << " <I> AuAu (" << Cent3 << "-" << Cent4 <<"%) ";
	}
      if (verbose) cout << "- " << normtype << " = " << Norm2 << " +/- " <<  eNorm2 << endl;

    }
  else // Npart scaling
    {
      Norm1  = getNpart(Cent1,Cent2,"val");
      eNorm1 = getNpart(Cent1,Cent2,"err");

      if (verbose)
	{
	  cout << " <I> AuAu (" << Cent1 << "-" << Cent2 <<"%) -  Npart = " 
	       << Norm1 << " err: " << eNorm1 << endl;
	}

      if ( Cent3 == 0 && Cent4 == 0) // pp 
	{
	  Norm2 = getNpart("pp");
	  eNorm2 = 0;
	  if (verbose) cout << " <I> pp ref.: ";
	}
      else 
	{
	  Norm2  = getNpart(Cent3,Cent4,"val");
	  eNorm2 = getNpart(Cent3,Cent4,"err");
	  if (verbose) cout << " <I> AuAu (" << Cent3 << "-" << Cent4 <<"%) ";
	}
      if (verbose) cout << "-  Npart = " << Norm2 << " +/- " <<  eNorm2 << endl;
    }

  // Norm + extra_erelat (e.g. pT correlated, luminosity) error
  if (extra_erelat)
    {
      eRelatNorm = quadRelatError(eNorm1,Norm1,eNorm2,Norm2,extra_erelat,1.);
    }
  else
    {
      eRelatNorm = quadRelatError(eNorm1,Norm1,eNorm2,Norm2,0.,1.);
    }
  
  if (verbose>1) printf(" <I> Propagated normalization (TAB+others) %% error = %.3f \n",eRelatNorm);

  return eRelatNorm;

}

//_____________________________________________________________________________
//
//
double*
readErrorNcollAscii(const char *file)
{

  double *eNcoll = 0;

  char ffile[300];
  //sprintf(ffile,"%s%s",datapath,file);
  sprintf(ffile,"%s",file);
  
  ifstream ifiledata(ffile);
  if (!ifiledata)
    {
      cout << " <E> Can't open file: " << ffile << endl;
      return eNcoll;
    }
  
  int verbose = 0;
  if (verbose) cout << " <I> emcAnalyzerUtils::readErrorNcollAscii reading " << ffile  << " ..." << endl;
  
  double bidon;
  // Read pT, R_AA from the file
  while ( ifiledata >> 
	  bidon >> 
	  bidon >> 
	  bidon >>
	  *eNcoll
	  ){
    if (verbose) cout << "  " << *eNcoll << endl;
    eNcoll++;
  }

  return eNcoll;

}
//_____________________________________________________________________________
// Returns a histo with the computed Ncoll-error band for Centrality C1-C2% 
// or quadratically combined for C1-C2% and C3-C4% (e.g. for ratios)
// (passing the Graph just for the x-bin position of the band points)
//
TH1F*
getPropagErrorNcollBand( TGraphErrors *g,
			 const int Cent1, 
			 const int Cent2,
			 const int Cent3, 
			 const int Cent4,
			 const double *extra_erelat,
			 const bool bin_scaling,
			 const bool useTABinsteadOfNcoll,
			 const int verbose)
{

  if (!g) return 0;

  double *eNcoll = 0;

  if (!useTABinsteadOfNcoll)
    {
      eNcoll = getPropagErrorNcoll(g,Cent1,Cent2,Cent3,Cent4,extra_erelat,bin_scaling,verbose);
    }
  else
    {
      eNcoll = getPropagErrorTAB(g,Cent1,Cent2,Cent3,Cent4,extra_erelat,verbose);
    }

  int N = g->GetN(); 

//   int Narray = sizeof(eNcoll)/sizeof(double);

//   if (N != Narray)
//     {
//       N = Narray;
//     }

  TGraphErrors *eNcollband = new TGraphErrors(N);

  for (int i=0; i<N; i++) 
    {
      double x,y;
      g->GetPoint(i,x,y);
      double ex = 2.0*g->GetErrorX(i); // let's make the x-error slightly larger for plotting purposes

      eNcollband->SetPoint(i,x,y);
      eNcollband->SetPointError(i,ex,eNcoll[i]); // set the graph error value to the error in Ncoll

      if (verbose) printf("(%.2f,%.2f) --> eNcoll=%.3f \n",x,y,eNcoll[i]);
    }

  return graph2histo(eNcollband);
}

//_____________________________________________________________________________
// returns array of Ncoll errors for C1-C2% or quadratically combined for 
// C1-C2% and C3-C4% (e.g. ratios).
// the array is indexed according to the x-bins of graph
// (passing the Graph just for the x-bin position of the band points)
//

double*
getPropagErrorNcoll( TGraphErrors *g,
		     const int Cent1, 
		     const int Cent2,
		     const int Cent3, 
		     const int Cent4,
		     const double *extra_erelat,
		     const bool bin_scaling,
		     const int verbose )
{

  if (!g) return 0;

  int N = g->GetN();
  double *eNcoll = new double[N];

  double  Ncoll1 = 0;
  double eNcoll1 = 0;
  double  Ncoll2 = 0;
  double eNcoll2 = 0;

  //_____________________________________
  // get Ncoll for each Centrality class

  if ( bin_scaling ) // Binary scaling
    {
      Ncoll1  = getNcoll(Cent1,Cent2,"val");
      eNcoll1 = getNcoll(Cent1,Cent2,"err");

      if (verbose)
	{
	  cout << " <I> AuAu (" << Cent1 << "-" << Cent2 ;
	  cout <<"%) - Ncoll = " << Ncoll1 << " +/- " << eNcoll1 << endl;
	}

      if (Cent3 == 0 && Cent4 == 0) // pp (assume Cent3 == Cent4 == 0)
	{
	  Ncoll2 = getNcoll("pp");
	  eNcoll2 = 0;
	  if (verbose) cout << " <I> pp ref.: ";
	}
      else
	{
	  Ncoll2  = getNcoll(Cent3,Cent4,"val");
	  eNcoll2 = getNcoll(Cent3,Cent4,"err");
	  if (verbose) cout << " <I> AuAu (" << Cent3 << "-" << Cent4 <<"%) ";
	}
      if (verbose) cout << "- Ncoll = " << Ncoll2 << " +/- " <<  eNcoll2 << endl;
    }
  else // Npart scaling
    {
      Ncoll1  = getNpart(Cent1,Cent2,"val");
      eNcoll1 = getNpart(Cent1,Cent2,"err");

      if (verbose)
	{
	  cout << " <I> AuAu (" << Cent1 << "-" << Cent2 <<"%) -  Npart = " 
	       << Ncoll1 << " err: " << eNcoll1 << endl;
	}

      if ( Cent3 == 0 && Cent4 == 0) // pp 
	{
	  Ncoll2 = getNpart("pp");
	  eNcoll2 = 0;
	  if (verbose) cout << " <I> pp ref.: ";
	}
      else 
	{
	  Ncoll2  = getNpart(Cent3,Cent4,"val");
	  eNcoll2 = getNpart(Cent3,Cent4,"err");
	  if (verbose) cout << " <I> AuAu (" << Cent3 << "-" << Cent4 <<"%) ";
	}
      if (verbose) cout << "-  Npart = " << Ncoll2 << " +/- " <<  eNcoll2 << endl;
    }

  //_____________________________________
  // Fill the array with the errors

  //int Nunused = 0;

  for(int i=0; i<N; i++) 
    {
      double x,y;
      g->GetPoint(i,x,y);

//       if (y==0)
// 	{
// 	  cout << " <W> Found point =0. Skipping it !" << endl;
// 	  Nunused++;
// 	  continue;
// 	}

      double ex,ey;
      ex = g->GetErrorX(i); 
      ey = g->GetErrorY(i);

      // Ncoll + extra_erelat (e.g. pT correlated, luminosity) error
      if (extra_erelat)
	{
	  eNcoll[i] = quadRelatError(eNcoll1,Ncoll1,eNcoll2,Ncoll2,extra_erelat[i],1.);
	}
      else
	{
	  eNcoll[i] = quadRelatError(eNcoll1,Ncoll1,eNcoll2,Ncoll2,0.,1.);
	}
      eNcoll[i] *= y; // transform in absolute errors

      if (verbose>1) printf("%.2f+-%.2f   ratio=%.3f+-%.3f   eNcoll=%.3f \n",x,ex,y,ey,eNcoll[i]);
    }

//   if (Nunused)
//     {

//       double *eNcoll_chopped = new double[Nunused];

//       for(int i=0; i<Nunused; i++) 
// 	{
// 	  eNcoll_chopped[i] = eNcoll[i];
// 	}

//       return eNcoll_chopped;

//     }

  return eNcoll;

}

//_____________________________________________________________________________
// returns array of TAB errors for C1-C2% or quadratically combined for C1-C2% and C3-C4% (e.g. ratios)
// the array is indexed according to the x-bins of graph
// (passing the Graph just for the x-bin position of the band points)
//

double*
getPropagErrorTAB( TGraphErrors *g,
		   const int Cent1, 
		   const int Cent2,
		   const int Cent3, 
		   const int Cent4,
		   const double *extra_erelat,
		   const int verbose )
{

  if (!g) return 0;

  int N = g->GetN();
  double *eTAB = new double[N];

  double  TAB1 = 0;
  double eTAB1 = 0;
  double  TAB2 = 0;
  double eTAB2 = 0;

  //_____________________________________
  // get TAB for each Centrality class

  TAB1  = getTAB(Cent1,Cent2,"val");
  eTAB1 = getTAB(Cent1,Cent2,"err");
  
  if (verbose)
    {
      cout << " <I> AuAu (" << Cent1 << "-" << Cent2 ;
      cout <<"%) - TAB = " << TAB1 << " +/- " << eTAB1 << endl;
    }
  
  if (Cent3 == 0 && Cent4 == 0) // pp (assume Cent3 == Cent4 == 0)
    {
      TAB2 = 1.;
      eTAB2 = 0;
      if (verbose) cout << " <I> pp ref.: ";
    }
  else
    {
      TAB2  = getTAB(Cent3,Cent4,"val");
      eTAB2 = getTAB(Cent3,Cent4,"err");
      if (verbose) cout << " <I> AuAu (" << Cent3 << "-" << Cent4 <<"%) ";
    }
  if (verbose) cout << "- TAB = " << TAB2 << " +/- " <<  eTAB2 << endl;

  //_____________________________________
  // Fill the array with the errors

  for(int i=0; i<N; i++) 
    {
      double x,y;
      g->GetPoint(i,x,y);

      double ex,ey;
      ex = g->GetErrorX(i); 
      ey = g->GetErrorY(i);

      // TAB + extra_erelat (e.g. pT correlated, luminosity) error
      if (extra_erelat)
	{
	  eTAB[i] = quadRelatError(eTAB1,TAB1,eTAB2,TAB2,extra_erelat[i],1.);
	}
      else
	{
	  eTAB[i] = quadRelatError(eTAB1,TAB1,eTAB2,TAB2,0.,1.);
	}

      eTAB[i] *= y; // transform into absolute errors
      
      if (verbose>1) printf("%.2f+-%.2f   ratio=%.3f+-%.3f   eTAB=%.3f \n",x,ex,y,ey,eTAB[i]);
    }

  return eTAB;

}

// //_____________________________________________________________________________
// // returns percentual TAB errors for C1-C2% or quadratically combined 
// // for C1-C2% and C3-C4% (e.g. ratios)
// //

// double
// getPropagErrorTAB( const int Cent1, 
// 		   const int Cent2,
// 		   const int Cent3, 
// 		   const int Cent4,
// 		   const double extra_erelat,
// 		   const int verbose )
// {

//   double eTAB = 0;

//   //_____________________________________
//   // get TAB for each Centrality class

//   double  TAB1 = getTAB(Cent1,Cent2,"val");
//   double eTAB1 = getTAB(Cent1,Cent2,"err");

//   if (verbose)
//     {
//       cout << " <I> AuAu (" << Cent1 << "-" << Cent2 ;
//       cout <<"%) - TAB = " << TAB1 << " +/- " << eTAB1 << endl;
//     }

//   double  TAB2 = 0;
//   double eTAB2 = 0;
    
//   if (Cent3 == 0 && Cent4 == 0) // pp (assume Cent3 == Cent4 == 0)
//     {
//       TAB2 = 1.;
//       eTAB2 = 0;
//       if (verbose) cout << " <I> pp ref.: ";
//     }
//   else
//     {
//       TAB2  = getTAB(Cent3,Cent4,"val");
//       eTAB2 = getTAB(Cent3,Cent4,"err");
//       if (verbose) cout << " <I> AuAu (" << Cent3 << "-" << Cent4 <<"%) ";
//     }
//   if (verbose) cout << "- TAB = " << TAB2 << " +/- " <<  eTAB2 << endl;

//   //_____________________________________
//   // TAB + extra_erelat (e.g. pT correlated, luminosity, ...) error

//   if (extra_erelat)
//     {
//       eTAB = quadRelatError(eTAB1,TAB1,eTAB2,TAB2,extra_erelat,1.);
//     }
//   else
//     {
//       eTAB = quadRelatError(eTAB1,TAB1,eTAB2,TAB2,0.,1.);
//     }
  
//   if (verbose>1) printf(" <I> Propagated TAB error = %.3f \n",eTAB);

//   return eTAB;

// }

//_____________________________________________________________________________
//
double
quadRelatError( const double ea, const double a,
		const double eb, const double b,
		const double ec, const double c,
		const double ed, const double d,
		const double ee, const double e)
{

  double qrelaterror = 0.;

  if (a!=0)
    {
      qrelaterror = (ea/a)*(ea/a);
      if (b!=0) qrelaterror += (eb/b)*(eb/b);
      if (c!=0) qrelaterror += (ec/c)*(ec/c);
      if (d!=0) qrelaterror += (ed/d)*(ed/d);
      if (e!=0) qrelaterror += (ee/e)*(ee/e);
    }

  return Sqrt(qrelaterror);

}

//_____________________________________________________________________________
//
TF1*
hagedornInit( const char* funcname,
	      const double ptmin, 
	      const double ptmax,
	      const int verbose,
	      const double AHag,
	      const double p0, 
	      const double n_exp)
{

  if ( verbose )
    {
      cout << " <I> (unconstrained) hagedornInit: ptmin=" << ptmin << " ptmax=" << ptmax
	   << " A_input=" << AHag << " p0_fixed=" << p0 << " n_input=" << n_exp << endl;
    }

  TF1 *hagedorn = new TF1(funcname,"[0]/(1+x/[1])^[2]", ptmin, ptmax);
  //hagedorn = new TF1(funcname,"[0]/(x+[1])^[2])", ptmin, ptmax); // fit more sensitive to initial pars. values
  
  hagedorn->SetParameters(AHag, p0, n_exp);
  hagedorn->SetParNames("A_{Hag}", "p_{0}", "n");
  
  hagedorn->SetParLimits(0, 0.0001, 200000000);
  hagedorn->SetParLimits(1, p0, p0);
  hagedorn->SetParLimits(2, n_exp-6., n_exp+6.);

  hagedorn->SetLineWidth(1);

  //hagedorn->Print();

  return hagedorn;

}
//_____________________________________________________________________________
//
TF1*
hagedornConstrainedInit( const char* funcname,
			 const double ptmin, 
			 const double ptmax,
			 const int verbose,
			 const double AHag,
			 const double ptmean, 
			 const double n_exp){

  //cout << " <I> <p_T> for the fit ?" << endl; 
  //cin >> ptmean;

  if ( verbose )
    {
      cout << " <I> hagedornConstrainedInit: ptmin=" << ptmin << " ptmax=" << ptmax
	   << " A_input=" << AHag << ", <pT>_fixed=" << ptmean << " [p0 = <pT>/2*(n-3)], n_input=" << n_exp << endl;
    }

  TF1 *hagedorn = new TF1(funcname,"[0]/(1+x/([1]/2*([2]-3)))^[2]", ptmin, ptmax);

  hagedorn->SetParameters(AHag, ptmean, n_exp);
  hagedorn->SetParNames("A_{Hag}", "p_{T}", "n");
  
  hagedorn->SetParLimits(0, 0.0001, 200000000);
  hagedorn->SetParLimits(1, ptmean, ptmean);
  hagedorn->SetParLimits(2, n_exp-6., n_exp+6.);

  hagedorn->SetLineWidth(1);

  //hagedorn->Print();

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornConstrainedFit( TH1* h, 
			const char* funcname,
			const double ptmin, const double ptmax,
			const int verbose,
			const double AHag,
			const double ptmean, 
			const double n_exp)
{

  if (!h) return 0;

  TF1 *hagedorn = (TF1*)hagedornConstrainedInit(funcname, ptmin, ptmax, verbose, AHag, ptmean, n_exp);

  if (verbose>1) h->Fit(hagedorn,"REIM0+","",ptmin, ptmax);
  else h->Fit(hagedorn,"QREIM0+","",ptmin, ptmax); // Quiet

  h->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  // constrained fit: p0 = <pT>/2*(n-3) with <pT>=0.4 GeV/c
  p_0  = p_0/2*(n-3); 
  ep_0 = ep_0/2*(n-3);

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2):",ptmin,ptmax);
      printf(" A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornFit( TH1* h, 
	     const char* funcname,
	     const double ptmin, const double ptmax,
	     const int verbose,
	     const double AHag,
	     const double p0, 
	     const double n_exp)
{

  if (!h) return 0;

  TF1 *hagedorn = (TF1*)hagedornInit(funcname, ptmin, ptmax, verbose, AHag, p0, n_exp);

  if (verbose>1) h->Fit(hagedorn,"REIM0+","",ptmin, ptmax);
  else h->Fit(hagedorn,"QREIM0+","",ptmin, ptmax); // Quiet

  h->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2):",ptmin,ptmax);
      printf(" A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornConstrainedFit( TGraphErrors* g, 
			const char* funcname,
			const double ptmin, const double ptmax,
			const int verbose,
			const double AHag,
			const double ptmean, 
			const double n_exp)
{

  if (!g) return 0;

  TF1 *hagedorn = (TF1*)hagedornConstrainedInit(funcname, ptmin, ptmax, verbose, AHag, ptmean, n_exp);
 
  if (verbose>1) g->Fit(hagedorn,"0+","",ptmin, ptmax); // Option "R" or ptmin-ptmax needed even deleting previous functions ...
  else g->Fit(hagedorn,"Q0+","",ptmin, ptmax); // Quiet

  g->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  // constrained fit: p0 = <pT>/2*(n-3) with <pT>=0.4 GeV/c
  p_0  = p_0/2*(n-3); 
  ep_0 = ep_0/2*(n-3);

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2): A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     ptmin,ptmax,A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
//
TF1*
hagedornFit( TGraphErrors* g, 
	     const char* funcname,
	     const double ptmin, const double ptmax,
	     const int verbose,
	     const double AHag,
	     const double p0, 
	     const double n_exp)
{

  if (!g) return 0;

  //g->GetListOfFunctions()->Delete(); // the option "+" in Fit deletes any previous function

  TF1 *hagedorn = (TF1*)hagedornInit(funcname, ptmin, ptmax, verbose, AHag, p0, n_exp);
  
  if (verbose>1) g->Fit(hagedorn,"0+","",ptmin, ptmax); // Option "R" or ptmin-ptmax needed even deleting previous functions ...
  else g->Fit(hagedorn,"Q0+","",ptmin, ptmax); // Quiet

  g->GetListOfFunctions()->Add(hagedorn);

  double A  = hagedorn->GetParameter(0);
  double eA = hagedorn->GetParError(0);
  double p_0  = hagedorn->GetParameter(1);
  double ep_0 = hagedorn->GetParError(1);
  double n  = hagedorn->GetParameter(2);
  double en = hagedorn->GetParError(2);
  double chi2 = hagedorn->GetChisquare()/hagedorn->GetNDF();

  if ( verbose ) 
    {
      printf(" <I> hagedornFit (pT=%.2f-%.2f GeV/c2): A = %e +- %e p0 = %7.4f +- %7.4f n = %7.4f +- %7.4f Chi2/ndf = %7.4f\n",
	     ptmin,ptmax,A,eA,p_0,ep_0,n,en,chi2);
    }

  return hagedorn;

}

//_____________________________________________________________________________
void
getBinShift( const double ptini, 
	     const double ptbinwidth, 
	     TF1* hagedorn,
	     TGraphErrors *g,
	     std::vector<double> &xyshift)
{

  if (!hagedorn) return;
  if (!ptbinwidth) return;

  double ptmean = 0.;
  double yup = 0.;
  double s = ptbinwidth/2.0;

  int verbose = 0;

  double *hagedPars = (double*)hagedorn->GetParameters();

  double hagedptmin = hagedorn->GetXmin();
  double hagedptmax = hagedorn->GetXmax();

  // ================================================================================
  // ---- 1st method (principle in MJT's note) ----
  // ================================================================================

  // hagedorn integral (analytical)
  TF1 integhagedorn("integhagedorn","([0]/(1-[2]))*(x+[1])^(1-[2])",hagedptmin,hagedptmax);
  integhagedorn.SetParameters(hagedPars);

  yup = integhagedorn.Eval(ptini+s)-integhagedorn.Eval(ptini-s);
  yup /= ptbinwidth;

  // inverse of hagedorn
  TF1 invhagedorn("invhagedorn", "([0]/x)^(1/[2])-[1]",0.,100.);
  invhagedorn.SetParameters(hagedPars);

  ptmean = invhagedorn.Eval(yup);

  if (verbose) printf(" (anal. integ.) = (%.2f,%.3e) ",ptmean,yup);

  // ================================================================================
  // ---- 2nd method (as above but simpler) ----
  // ================================================================================

  // local Hagedorn fit
  yup = hagedorn->Integral(ptini-s,ptini+s);
  yup /= ptbinwidth;

  ptmean = invhagedorn.Eval(yup);

  if (verbose) printf(" (glob. fit) = (%.2f,%.3e) ",ptmean,yup);

  // ---- Shift of low pT bins (pT < 1.5 GeV/c) using an exponential fit ----

//   if ( ptini < 1.5)
//     {
//       double ptmin = 0.7;
//       double ptmax = 1.7;
//       TF1 *expo = new TF1("expo","[0]*exp([1]*x)",ptmin,ptmax);
//       expo->SetParameters(100.,-5.);
//       g->Fit(expo,"QR0+","",ptmin, ptmax);
//       //g->GetListOfFunctions()->Add(expo);

//       yup = expo->Integral(ptini-s,ptini+s);
//       yup /= ptbinwidth;

//       TF1 invexpo("invexpo","log(x/[0])*(1/[1])",0.,100.);
//       invexpo.SetParameters(expo->GetParameters());
//       ptmean = invexpo.Eval(yup);

//       yup = (expo->Eval(ptini)/yup); // if low pT we send directly the ratio instead of yup !!

//       delete expo;
//     }


  // ---- Shift of high pT bins (pT > 7.0 GeV/c) using a local hagedorn fit ----

//   bool hiptspectrum = false;
//   double hipt = g->GetX()[g->GetN()-1];
//   if ( hipt > 9. ) hiptspectrum = true;

//   if ( ptini > 7.0 && hiptspectrum )
//     {

//       // --- 3th method (as 2nd method but local fit) --- 
//       // Warning: This method seems to give a higher yup (i.e. lower ptmean) 
//       // for some points compared to the other methods ...

//       double ptmin = 0.7;
//       double ptmax = 1.7;
      
//       //TF1 *localhagedorn = new TF1("localhagedorn","[0]/(x+[1])^[2]", ptmin, ptmax);
//       // this guy segs. fault (getlistoffunc()->delete() in hagedornFit ?
//       TF1 *localhagedorn = (TF1*)hagedornFit(g,"localhag",ptmin,ptmax,0); 

//       yup = localhagedorn->Integral(ptini-s,ptini+s);
//       yup /= ptbinwidth;
      
//       invhagedorn.SetParameters(localhagedorn->GetParameters());
//       ptmean = invhagedorn.Eval(yup);
      
//       if (verbose) printf(" (hi-pT local fit) = (%.2f,%.3e) ",ptmean,yup);

//       yup = (localhagedorn->Eval(ptini)/yup); // if high pT we send directly the ratio instead of yup !!

//     }

  // ================================================================================
  // THOSE ARE THE FINAL SHIFTED VALUES
  // ================================================================================
  // FIXME: There is a slight difference between method 2(=1) and methods 3. and 4.
  // We take the (ptmean,yup) of method 2(=1)

  xyshift.push_back(ptmean);
  xyshift.push_back(yup);


//  // ================================================================================
//   // --- 3th method (as 2nd method but local fit) --- 
//  // ================================================================================
//   // Warning: This method seems to give a higher yup (i.e. lower ptmean) 
//   // for some points compared to the other methods ...

//   //this guy segs. fault (getlistoffunc()->delete() in hagedornFit ?
//   TF1 *localhagedorn = (TF1*)hagedornFit(g,"localhag",ptini-ptbinwidth,ptini+ptbinwidth,0); 
//   yup = localhagedorn->Integral(ptini-s,ptini+s);
//   yup /= ptbinwidth;

//   invhagedorn.SetParameters(hagedPars);
//   ptmean = invhagedorn.Eval(yup);
  
//   if (verbose) printf(" (loc. fit) = (%.2f,%.3e) ",ptmean,yup);
  

  // ================================================================================
  // ---- 4th method (Gines') ----
  //
  // this method provides the "gravity center" of the bin which is *not* correct
  // see 
  //
  // ================================================================================

  // x times hagedorn
  TF1 xhagedorn("xhagedorn", "[0]*x/(x+[1])^[2]",hagedptmin,hagedptmax);
  xhagedorn.SetParameters(hagedPars);

  ptmean = xhagedorn.Integral(ptini-s,ptini+s)/hagedorn->Integral(ptini-s,ptini+s);
  yup = hagedorn->Eval(ptmean);

  if (verbose) printf(" (Gines) = (%.2f,%.3e) \n",ptmean,yup);

  // QM02 values:
  //const double mean_pt_corr[] = { 1.18, 1.69, 2.20, 2.70, 3.21, 3.71, 4.22, 4.72, 5.22, 
  //				  5.72, 6.22, 6.73, 7.23, 7.73, 8.23, 8.73, 9.23, 9.73,
  //				  10.23, 10.73, 11.23, 11.73};

  //delete hagedPars;

  return;
}

//_____________________________________________________________________________
// Simplified version of the function above. Changes:
// (1) Keep one single bin-shift method
// (2) USE the new Hagedorn function: "[0]/(1+x/[1])^[2]" instead of "[0]/(x+[1])^[2])"

void
getBinShiftSimple( const double ptini, 
		   const double ptbinwidth, 
		   TF1* hagedorn,
		   TGraphErrors *g,
		   std::vector<double> &xyshift)
{

  if (!hagedorn) return;
  if (!ptbinwidth) return;

  double ptmean = 0.;
  double yup = 0.;

  double s = ptbinwidth/2.0;

  double *hagedPars = (double*)hagedorn->GetParameters();
  double p_0  = hagedorn->GetParameter(1);
  double n  = hagedorn->GetParameter(2);

  // inverse of hagedorn

  //TF1 invhagedorn("invhagedorn", "([0]/x)^(1/[2])-[1]",0.,100.); // for Hagedorn defined as "[0]/(x+[1])^[2])"

  TF1 invhagedorn("invhagedorn", "[1]*([0]/x)^(1/[2])-[1]",0.,100.); // for Hagedorn defined as "[0]/(1+x/[1])^[2]"
  invhagedorn.SetParameters(hagedPars);

  // For "constrained fit" where: p0 = <pT>/2*(n-3) with <pT>~0.3 GeV/c

  TString hag_type = hagedorn->GetName();
  if (hag_type.Contains("constr"))
    {
      invhagedorn.SetParameter(1,p_0/2*(n-3));
    }

  yup = hagedorn->Integral(ptini-s,ptini+s);
  yup /= ptbinwidth;

  ptmean = invhagedorn.Eval(yup);

  double yini = hagedorn->Eval(ptini);
  double relatshift = yini/yup*100.;
  printf(" (pTmean,yieldshift) = ( %.2f,%.1f%% ) \n",ptmean,relatshift);

  xyshift.push_back(ptmean);
  xyshift.push_back(yup);

  return;
}

//_____________________________________________________________________________

void
getBinShiftPowLaw( const double ptini, 
		   const double ptbinwidth, 
		   TF1* pow,
		   TGraphErrors *g,
		   std::vector<double> &xyshift)
{

  if (!pow) return;
  if (!ptbinwidth) return;

  double ptmean = 0.;
  double yup = 0.;

  double s = ptbinwidth/2.0;

  double *hagedPars = (double*)pow->GetParameters();

  // inverse of power-law

  TF1 invpow("invpow", "([0]/x)^(1/[1])",0.,100.);
  invpow.SetParameters(hagedPars);

  yup = pow->Integral(ptini-s,ptini+s);
  yup /= ptbinwidth;

  ptmean = invpow.Eval(yup);

  double yini = pow->Eval(ptini);
  double relatshift = yini/yup*100.;
  printf(" (pTmean,yieldshift) = ( %.2f,%.1f%% ) \n",ptmean,relatshift);

  xyshift.push_back(ptmean);
  xyshift.push_back(yup);

  return;
}

//_____________________________________________________________________________
double
shiftPt( TGraphErrors *g, 
	 TF1* hagedorn,
	 const double ptbinwidth,
	 const bool undoInvYieldNormFirst )
{
  double xin = 0.;
  double yin = 0.;
  std::vector<double> xyshift(2);
  double xout = 0.;

  // WARNING: This function as it is now does *NOT* change the x-errors when shifting along the x-axis (It's OK like this)

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	}

      xyshift.clear();
      //getBinShift(xin,ptbinwidth,hagedorn,g,xyshift);
      getBinShiftSimple(xin,ptbinwidth,hagedorn,g,xyshift);
      xout = xyshift[0];

      if (undoInvYieldNormFirst)
	{
	  yin /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      g->SetPoint(i,xout,yin);
    }

  return xin;
}

//_____________________________________________________________________________
double
shiftPtPowLaw( TGraphErrors *g, 
	       TF1* pow,
	       const double ptbinwidth,
	       const bool undoInvYieldNormFirst )
{
  double xin = 0.;
  double yin = 0.;
  std::vector<double> xyshift(2);
  double xout = 0.;

  // WARNING: This function as it is now does *NOT* change the x-errors when shifting along the x-axis (It's OK like this)

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	}

      xyshift.clear();
      getBinShiftPowLaw(xin,ptbinwidth,pow,g,xyshift);
      xout = xyshift[0];

      if (undoInvYieldNormFirst)
	{
	  yin /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      g->SetPoint(i,xout,yin);
    }

  return xin;
}

//_____________________________________________________________________________
double
shiftYield( TGraphErrors *g, 
	    TF1* hagedorn,
	    const double ptbinwidth,
	    const bool undoInvYieldNormFirst )
{

  double xin = 0.;
  double yin = 0.;
  double exin = 0.;
  double eyin = 0.;
  std::vector<double> xyshift(2);
  double yout = 0.;
  double eyout = 0.;

  if (!g) return 0.;

//   // for local hagedorn fit correction at very high-pT
//   bool hiptspectrum = false;
//   double hipt = g->GetX()[g->GetN()-1];
//   if ( hipt > 9. ) hiptspectrum = true;

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);

      // check binwidth argument coincides with true binwidth
      if (i>0) 
	{
	  double xtmp = 0, ytmp = 0;
	  g->GetPoint(i-1,xtmp,ytmp);
	  if ((xin-xtmp)!=ptbinwidth) 
	    cout << "<W> shiftYield(...): pT binwidth is not " << ptbinwidth 
		 << " ! Check your bin-shift correction for bin pT=" << xin << endl;
	}

      exin = g->GetErrorX(i);
      eyin = g->GetErrorY(i);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	  eyin *= xin; // idem for the error

	  g->SetPoint(i,xin,yin);
	  g->SetPointError(i,exin,eyin);
	}

      xyshift.clear();
      //getBinShift(xin,ptbinwidth,hagedorn,g,xyshift);
      getBinShiftSimple(xin,ptbinwidth,hagedorn,g,xyshift);
      double yup = xyshift[1];
      
      //if ( xin>1.5 && yup)
      //{
	  yout = yin*(hagedorn->Eval(xin)/yup); // standard method
	  //printf(" yout = %.3e*(%.3e/%.3e) = %.3e  ---- ",yin,hagedorn->Eval(xin),xyshift[1],yout);
      //}

//       // WARNING: yup *is* already the ratio expo->Eval(xin)/yup in this case !!
//       if ( xin<1.5 ) // || (hiptspectrum && xin >= 7.0) )
// 	{
// 	  yout = yin*yup;      
// 	  //printf(" yout  = %.3e*%.3e =  %.3e  ---- ",yin,yup,yout);
// 	}

      if (undoInvYieldNormFirst)
	{
	  yout /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      if (yin) eyout = eyin*(yout/yin);
      //printf(" eyout = %.3e*(%.3e/%.3e) =  %.3e\n",eyin,yout,yin,eyout);

      g->SetPoint(i,xin,yout);
      g->SetPointError(i,exin,eyout);
    }

  return yout;

}
//_____________________________________________________________________________
double
shiftYieldPowLaw( TGraphErrors *g, 
		  TF1* pow,
		  const double ptbinwidth,
		  const bool undoInvYieldNormFirst )
{

  double xin = 0.;
  double yin = 0.;
  double exin = 0.;
  double eyin = 0.;
  std::vector<double> xyshift(2);
  double yout = 0.;
  double eyout = 0.;

  if (!g) return 0.;

  for ( int i = 0; i < g->GetN(); i++ )
    {
      g->GetPoint(i,xin,yin);
      exin = g->GetErrorX(i);
      eyin = g->GetErrorY(i);

      if (undoInvYieldNormFirst)
	{
	  yin *= xin; // we multiply by pT the inv. yield (1/pT dN/dpT) first
	  eyin *= xin; // idem for the error

	  g->SetPoint(i,xin,yin);
	  g->SetPointError(i,exin,eyin);
	}

      xyshift.clear();
      getBinShiftPowLaw(xin,ptbinwidth,pow,g,xyshift);
      double yup = xyshift[1];
      
      yout = yin*(pow->Eval(xin)/yup); // standard method

      if (undoInvYieldNormFirst)
	{
	  yout /= xin; // we divide back by pT to get the inv. yield (1/pT dN/dpT)
	}

      if (yin) eyout = eyin*(yout/yin);

      g->SetPoint(i,xin,yout);
      g->SetPointError(i,exin,eyout);
    }

  return yout;

}

//_____________________________________________________________________________
void
scale( TGraphErrors& g, 
       const double a,
       const double ea, 
       const bool verbose )
{

  if (verbose)
    {
      cout << " <I> emcAnalyzerUtils::scale : Scaling graph " 
	   << g.GetName() << " by factor: " ;
      if (a<1.) cout << " 1/" << 1/a << endl ;
      else cout << a << endl ;
     }

  if (a==1) return; // no need to scale

  for ( int i = 0; i < g.GetN(); ++i ) 
    {
      double xi,yi,exi,eyi;
      g.GetPoint(i,xi,yi);
      exi = g.GetErrorX(i);
      eyi = g.GetErrorY(i);

      eyi = quadRelatError(eyi,yi,ea,a);

      g.SetPoint(i,xi,yi*a);
      g.SetPointError(i,exi,yi*a*eyi);
    }

  // refit the scaled TGraph

  if ( g.GetFunction("hagedorn") || g.GetFunction("haged") ||
       g.GetFunction("hagedornpp") || g.GetFunction("hagedornascii") ||
       g.GetFunction("constrhagedorn") || g.GetFunction("constrhagedornascii") ||
       g.GetFunction("scaledhagedorn") ) // contains a fit
    {

      g.GetListOfFunctions()->Delete();
      double Xmin,Xmax;
      getXminmax((TGraphErrors*)(&g),Xmin,Xmax);
      int verbose = 0;
      hagedornFit((TGraphErrors*)(&g),"scaledhagedorn",Xmin,Xmax,verbose);
    }

}

//_____________________________________________________________________________
TGraphErrors *
rebin( TGraphErrors* g, 
       const int rebin )
{

  if (!g) return 0;

  int N = g->GetN() ;

  int Nrebin = (int)(N/rebin);
  TGraphErrors *grebin = new TGraphErrors(Nrebin);

  for (int i=0; i<Nrebin; i++ ) // i loops over rebinned indices
    {
      double x=0;//g->GetX()[i];
      double y=0;//g->GetY()[i];
      double ex=0;//g->GetErrorX(i);
      double ey=0;//g->GetErrorY(i)*g->GetErrorY(i)/(y*y);

      for ( int j = i*rebin; j < rebin*(i+1); j++ ) // j loops over original indices
	{
	  //cout << " going from j= " << j << " up to " << rebin*(i+1) << endl;
	  if ((i+1) < N)
	    {
	      x+=g->GetX()[j]; // FIXME: correct x for steep spectrum
	      y+=g->GetY()[j];
	      ex+=g->GetErrorX(j);
	      ey+=g->GetErrorY(j)*g->GetErrorY(j)/(y*y);
	    }
	}
      cout << "setting the sum of points " << i*rebin << "-" << rebin*(i+1)-1 << " to new point " << i << " : ";
      grebin->SetPoint(i,x/rebin,y/rebin);
      grebin->SetPointError(i,ex/rebin,y/rebin*Sqrt(ey));
      //cout << "(" << x << "" << << "" << << "" << << 
    }

//   // IF rebin only in subrange then loop over the original graph and fill only the rebinned bins

//   TGraphErrors *grebinsubrange = 0;

//   if (ibegin<iend)
//     {
//       N =(g->GetN()-(iend-ibegin));
//       grebinsubrange = new TGraphErrors(N);

//       for (int i=iend; i<N; i++)
// 	{
// 	  if (i>=ibegin && i<=(iend/rebin) && iend!=0 )
// 	    {
// 	    }
// 	}
//     }

  grebin->SetMarkerColor(g->GetMarkerColor());
  grebin->SetMarkerSize(g->GetMarkerSize());
  grebin->SetMarkerStyle(g->GetMarkerStyle());
  
  return grebin;
}

//_____________________________________________________________________________
TGraphErrors *
normalizeYieldbyPt( TGraphErrors* g, 
		    const bool verbose)
{

  if (!g) return 0;

  int N = g->GetN();

  TGraphErrors *gnorm = new TGraphErrors(N);

  for ( int i=0; i<N; i++ ) // i loops over rebinned indices
    {
      double x=g->GetX()[i]; // FIXME: correct x for steep spectrum
      double y=g->GetY()[i];

      if (verbose) cout << "normalizeYieldbyPt(): pT=" << "" << x << " --> " << y/x ;
      gnorm->SetPoint(i,x,y/x);

      if (g->InheritsFrom("TGraphErrors")) 
	{
	  double ex=g->GetErrorX(i);
	  double ey=g->GetErrorY(i)/y; //*g->GetErrorY(j)/(y*y);
	  gnorm->SetPointError(i,ex,(y/x)*ey);
	  if (verbose) cout << "+/-" << (y/x)*ey ;
	}
      if (verbose) cout << endl;
    }

  gnorm->SetMarkerColor(g->GetMarkerColor());
  gnorm->SetMarkerSize(g->GetMarkerSize());
  gnorm->SetMarkerStyle(g->GetMarkerStyle());
  
  return gnorm;
}

//_____________________________________________________________________________
// goes from Ed3sigma/d3p --> dsigma/dpT = (2*pi*pT)*Ed3sigma/d3p 
//
TGraphErrors *
Ed3sigmad3p_to_dsigmadpT( TGraphErrors* g, 
			    const bool verbose)
{

  if (!g) return 0;

  int N = g->GetN();

  double twopi = 2.*Pi();

  TGraphErrors *gnorm = new TGraphErrors(N);

  for ( int i=0; i<N; i++ ) // i loops over rebinned indices
    {
      double x=g->GetX()[i]; // FIXME: correct x for steep spectrum
      double y=g->GetY()[i];

      if (verbose) cout << "Ed3sigmad3p_to_d2sigma2dpT(): pT=" << "" << x << " --> " << twopi*y*x ;
      gnorm->SetPoint(i,x,twopi*y*x);

      if (g->InheritsFrom("TGraphErrors")) 
	{
	  double ex=g->GetErrorX(i);
	  double ey=g->GetErrorY(i)/y; //*g->GetErrorY(j)/(y*y);
	  gnorm->SetPointError(i,ex,twopi*(y*x)*ey);
	  if (verbose) cout <<  "+/-" << twopi*(y*x)*ey ;
	}
      if (verbose) cout << endl;
    }

  gnorm->SetMarkerColor(g->GetMarkerColor());
  gnorm->SetMarkerSize(g->GetMarkerSize());
  gnorm->SetMarkerStyle(g->GetMarkerStyle());
  
  return gnorm;
}

//_____________________________________________________________________________
// TGraphErrors sum:
// sum = g1 + a*g2

TGraphErrors*
add( TGraphErrors *g,
     TF1 *f, 
     const double a,
     const int verbose ) 

{

  if (!g) return 0;
  if (!f) return g;

  if ( abs(a)<1e-05 )
    {
      cout << "<W> emcAnalyzerUtils::add : scale-factor==0. Nothing changed ... " << endl;
      return g;
    }

  int N = g->GetN();

  TGraphErrors *gnew = new TGraphErrors(N);

  for ( int i=0; i<N; i++ )
    {
      double x=g->GetX()[i];
      double y=g->GetY()[i];
      double ex=g->GetErrorX(i);
      double ey=g->GetErrorY(i)*g->GetErrorY(i)/(y*y);
      
      double fval = a*f->Eval(x);

      if (verbose) cout << "point " << i << " changed : " << y << " --> " << y+fval << endl;
      gnew->SetPoint(i,x,y+fval);
      gnew->SetPointError(i,ex,(y+fval)*Sqrt(ey));
    }

  gnew->SetMarkerColor(g->GetMarkerColor());
  gnew->SetMarkerSize(g->GetMarkerSize());
  gnew->SetMarkerStyle(g->GetMarkerStyle());
  
  return gnew;
}

//_____________________________________________________________________________
// TGraphErrors sum:
// sum = g1 + a*g2

TGraphErrors*
add( TGraphErrors *g1,
     TGraphErrors *g2, 
     const double a,
     const int verbose ) 

{

  if (!g1) return g2;
  if (!g2) return g1;

  if (abs(a)<1e-05)
    {
      cout << "<W> emcAnalyzerUtils::add : scale-factor==0. Nothing changed ... " << endl;
      return g1;
    }

  int n1 = g1->GetN();
  int n2 = g2->GetN();
  int N = Max(n1,n2);;
  int nmin = 0;

  TGraphErrors *min = 0;
  TGraphErrors *max = 0;
  TGraphErrors *sum = 0;
  
  if (n1 >= n2)
    {
      sum = (TGraphErrors*)g1->Clone();
      max = (TGraphErrors*)g1->Clone();

      min = (TGraphErrors*)g2->Clone();
      nmin = n2;
    }
  else
    {
      sum = (TGraphErrors*)g2->Clone();
      max = (TGraphErrors*)g2->Clone();

      min = (TGraphErrors*)g1->Clone();
      nmin = n1;
    }

  for ( int i = 0; i < N ; ++i ) 
    {
      double xmax = 0, ymax = 0, exmax = 0, eymax = 0;
      double xmin = 0, ymin = 0, exmin = 0, eymin = 0;

      max->GetPoint(i,xmax,ymax);
      exmax = max->GetErrorX(i);
      eymax = max->GetErrorY(i);

      if (i<nmin)
	{
	  min->GetPoint(i,xmin,ymin);
	  exmin = min->GetErrorX(i);
	  eymin = min->GetErrorY(i);
	}

      if ( ((xmax-xmin)<(exmax/2.)) || ((xmax-xmin)==0.) ) // fixme: binwidth ??
	{
	  double ysum = ymax+a*ymin;
	  double eysum = ysum*quadRelatError(eymax,ymax,eymin,ymin);

	  sum->SetPoint(i,xmax,ysum);
	  sum->SetPointError(i,exmax,eysum); // fixme: x-axis error fixed 

	  cout << "<I> emcAnalyzerUtils::add : Summing contents at (x1,x2)=(" 
	       << xmax << "," << xmin << ") --> (" << ymax << "+" << a << "*" << ymin << ")=" 
	       << ysum << endl;
	}
      else // fixme: sum should be g1 & g2 in each independent non-coincident bin ...
	{
	  sum->SetPoint(i,xmax,ymax);
	  sum->SetPointError(i,exmax,eymax);
	  cout << "<I> emcAnalyzerUtils::add : Non coincident binning (binwidth=" << exmax/2.
	       << ") at (x1,x2)=(" << xmax << "," << xmin << "). Nothing added ..." << endl;
	}
    }

  return sum;

}

//_____________________________________________________________________________
// reverse the order of entries in a TGraphErrors
void
reverse( TGraphErrors& g )
{

  const int N = g.GetN();
  double xi[N],yi[N],exi[N],eyi[N];
  int j = N-1;

  // fill the temporary arrays
  for ( int i = 0; i < N; ++i ) 
    {
      g.GetPoint(i,xi[j],yi[j]);
      exi[j] = g.GetErrorX(i);
      eyi[j] = g.GetErrorY(i);
      j--;
    }

  // refill the TGraphErrors with them
  for ( int i = 0; i < N; ++i ) 
    {
      g.SetPoint(i,xi[i],yi[i]);
      g.SetPointError(i,exi[i],eyi[i]);
    }

}

//_____________________________________________________________________________
// displace (slightly) the x points of a TGraphErrors (for clarity purposes)
void
displaceXvalues( TGraphErrors& g, const double dx )
{

  const int N = g.GetN();
  double x_in[N],ex_in[N];
  double y_in[N],ey_in[N];

  // fill the temporary arrays
  for ( int i = 0; i < N; ++i ) 
    {
      g.GetPoint(i, x_in[i], y_in[i]);
      ex_in[i] = g.GetErrorX(i);
      ey_in[i] = g.GetErrorY(i);
    }

  // refill the TGraphErrors with them
  for ( int i = 0; i < N; ++i ) 
    {
      g.SetPoint(i,x_in[i]+dx,y_in[i]);
      g.SetPointError(i,ex_in[i]+dx,ey_in[i]);
    }

}


//_____________________________________________________________________________
// add/substract a given % to the TGraphErrors
void
AddOrSubstractRelatError( TGraphErrors &g,
			  const double eAddOrSubRelat,
			  const int verbose )
{

  if (!eAddOrSubRelat)
    {
      cout << " <W> AddOrSubstractRelatError: zero relative error added. Nothing changed " << endl;
      return;
    }

  if (verbose) cout << endl;
  if (verbose>1) g.Print();

  double x,y;
  double ex,ey;
  double eyrelat = 0.;
  int N = g.GetN();

  double control = 0.;

  for ( int i = 0; i < N; i++ ) 
    {
      g.GetPoint(i,x,y);
      ex = g.GetErrorX(i); 
      ey = g.GetErrorY(i);
      eyrelat = ey/y;

      double e0 = eyrelat;

      control += abs(eAddOrSubRelat);

      if ( eAddOrSubRelat<0 ) // we substract the error
	{
	  eyrelat = eyrelat*eyrelat - eAddOrSubRelat*eAddOrSubRelat;
	  if (eyrelat<0)
	    {
	      cout << " <E>  AddOrSubstractRelatError: New error negative ?!?!" << endl;
	    }
	  else
	    {
	      eyrelat = Sqrt(eyrelat);
	    }
	}
      else // we add the error
	{
	  eyrelat = quadRelatError(eyrelat,1.,eAddOrSubRelat,1.);
	}
      
      if (verbose)
	{
	  printf(" <I> pT = %.2f Error modified from: ey = %.2f%%  to ---> %.2f%%\n",
		 x,e0*100.,eyrelat*100.);
	}

      eyrelat *= y;

      g.SetPointError(i,ex,eyrelat);
    }

  if (verbose>1) g.Print();

  if (verbose)
    {
      cout << " <I> AddOrSubstractRelatErrors for graph: " << g.GetName()
	   << " -- Average added/substracted " << 100.*control/N << "%" << endl;
    }

}

//_____________________________________________________________________________
double*
integral( TGraphErrors *g, 
	  const double xinf, 
	  const double xsup)
{

  double *graphint = new double[2];
  graphint[0] = 0.;
  graphint[1] = 0.;

  if ( !g ) return graphint;

  g->Print();
  int N = g->GetN();

  double binwidth = 0;  //g->GetXaxis()->GetBinWidth(0); // doesn't seem to give the good bin width

  for ( int i = 0; i < N; ++i ) 
    {
      double x = g->GetX()[i];
      if (!binwidth)
	{
	  double xnext = g->GetX()[i+1];
	  binwidth = Abs(x-xnext);//2.;  
	  printf(" <I> emcAnalyzerUtils::integral(g,xinf,xsup): xinf = %.3f, xsup = %.3f Bin width = %.4e\n"
		 ,xinf,xsup,binwidth);

	}
      if ( x > (xinf-binwidth/2.) && x < (xsup+binwidth/2.) ) 
	{
	  double y = g->GetY()[i];
	  double yerr = g->GetErrorY(i);

	  graphint[0] += y;
	  graphint[1] += yerr*yerr;
	  printf(" (x,y) = (%.4f,%.4e) -->  integ = %.4e +/- %.4e\n",x,y,graphint[0],graphint[1]);
	}
    }

  graphint[0]*= binwidth;
  graphint[1] = Sqrt(graphint[1])*binwidth;

  //c5->Close();
  //delete c5;

  return graphint;
}

//_____________________________________________________________________________
// 
double*
getHagedornYieldIntegral( TF1* hagedorn,
			  const double xinf, 
			  const double xsup)
{

  double *hagint = new double[2];
  hagint[0] = 0.;
  hagint[1] = 0.;

  if (!hagedorn) return hagint;

  double *hagedPars  = (double*)hagedorn->GetParameters();
  double *ehagedPars = (double*)hagedorn->GetParErrors();

  // hagedorn integral (analytical)
  TF1 integhagedorn("integhagedorn","([0]/(1-[2]))*(x+[1])^(1-[2])",xinf,xsup);
  integhagedorn.SetParameters(hagedPars);
  for (int i=0;i<3;i++)
    {
      integhagedorn.SetParError(i,ehagedPars[i]);
    }

  hagint[0] = integhagedorn.Integral(xinf,xsup);
  hagint[1] = 0;

  delete hagedPars;
  delete ehagedPars;

  return hagint;
}

//_____________________________________________________________________________
void
getXYminmax( TGraphErrors* g1,
	     TGraphErrors* g2,
	     double& Xmin, double& Xmax,
	     double& Ymin, double& Ymax)
{

  if (!g1 || !g2) return;

  double xmin1 = 0.5; 
  double xmax1 = 12.5;
  getXminmax( g1, xmin1, xmax1);

  double xmin2 = 0.5;
  double xmax2 = 12.5;
  getXminmax( g2, xmin2, xmax2);

  Xmin = Min(xmin1,xmin2);
  Xmax = Max(xmax1,xmax2);

  //cout << "getXYminmax: (Xmin,Xmax)=" << Xmin << "," << Xmax << endl; 

  double ymin1 = 0.; 
  double ymax1 = 30.;
  getYminmax( g1, ymin1, ymax1);//, xmin1, xmax1 );

  double ymin2 = 0.;
  double ymax2 = 30.;
  getYminmax( g2, ymin2, ymax2);//, xmin2, xmax2);

  Ymin = Min(ymin1,ymin2);
  Ymax = Max(ymax1,ymax2);

  //cout << "getXYminmax: (Ymin,Ymax)=" << Ymin << "," << Ymax << endl; 

}

//_____________________________________________________________________________
void
getXYminmax( TGraphErrors* g,
	     double& Xmin, double& Xmax,
	     double& Ymin, double& Ymax)
{

  if (!g) return;

  getXminmax( g, Xmin, Xmax);

  //cout << "getXYminmax: (Xmin,Xmax)=" << Xmin << "," << Xmax << endl; 

  getYminmax( g, Ymin, Ymax);//, xmin1, xmax1 );

  //cout << "getXYminmax: (Ymin,Ymax)=" << Ymin << "," << Ymax << endl; 

}

//_____________________________________________________________________________
void
getXminmax( TGraphErrors* g, 
	    double& Xmin, double& Xmax,
	    const bool consider_errors )
{

  // alternatively:
  // double xmin = TMath::MinElement(g->GetN(),g->GetX());

  Xmin=1.0e+30;
  Xmax=-1.0e+30;

  if (!g) return;

  for ( int i = 0; i < g->GetN(); ++i ) 
    { 
      double x,y;
      g->GetPoint(i, x, y);
      double ex = g->GetErrorX(i);
      if (consider_errors)
	{
	  if ( x+ex > Xmax ) Xmax = x+ex;
	  if ( x-ex < Xmin && x > 0 ) Xmin = x-ex;
	  else if ( x < Xmin ) Xmin = x;
	}
      else
	{
	  if ( x > Xmax ) Xmax = x;
	  if ( x < Xmin ) Xmin = x;
	}
    }

  //cout << "getXminmax of " << g->GetTitle() <<  " : (Xmin,Xmax)=" << Xmin << "," << Xmax << endl; 
}

//_____________________________________________________________________________
void
getYminmax( TGraphErrors* g, 
	    double& Ymin, double& Ymax )
  //const double Xmin, const double Xmax)
{
  Ymin=1.0e+30;
  Ymax=-1.0e+30;

  if (!g) return;

  int b1 = 0;
  int b2 = g->GetN()-1;

  for ( int i = b1; i <= b2 ; ++i ) 
    {
      double x,y;
      double ey = g->GetErrorY(i);
      g->GetPoint(i, x, y);

      if ( y+ey > Ymax ) Ymax = y+ey;
      if ( y-ey < Ymin && y-ey > 0 ) Ymin = y-ey;
      else if ( y < Ymin ) Ymin = y;
    }

  //c5->Close();

  //cout << "getYminmax of " << g->GetTitle() <<  " : (Ymin,Ymax)=" << Ymin << "," << Ymax << endl; 

}

//_____________________________________________________________________________
double 
getYatX( TGraphErrors* g, 
	 const double X,
	 const char *type )
{

  if (!g) return crazyValue;
 
  const int N = g->GetN();
  double x = 0., y = 0.;
  double prevx = 0., prevy = 0.;

  TString type_str = type ;

  for ( int i = 0; i < N; ++i ) 
    { 
      g->GetPoint(i, x, y);
      double ey = g->GetErrorY(i);
      if (i>0) g->GetPoint(i-1, prevx, prevy);
      //cout << " Looking for Yvalue at X = " << X 
      //	   <<  " ... (x,y)=(" << x << "," << y << "+/-" << ey << ")" << endl;

      double xbinwidth = (x-prevx)/2;

      if ( (x-xbinwidth <= X) && (x+xbinwidth > X) )
	{
	  cout << " Found Yvalue at X = " << X 
	       <<  " --> (x,y)=(" << x << "," << y << "+/-" << ey << ")" << endl;
	  if (type_str.Contains("val")) return y; 
	  else if (type_str.Contains("err")) return ey;
	}
    }

  cout << " <E> emcAnalyzerUtils::getYAtX: No y value found for x = " << X 
       << " of " << g->GetName() << ". Returning " << crazyValue << endl;

  return crazyValue;
}

//_____________________________________________________________________________
TGraphErrors*
ratio( TGraphErrors *g0, 
       TGraphErrors *g1,
       const int verbose,
       const double coincidentBinningWidth ) //to force the ratio of g0 and g1 to be done only for coincident bins set it to zero
{

  TGraphErrors *ratio_g0_g1 = 0;

  if (!g0 || !g1) return ratio_g0_g1;

  char name[300];
  sprintf(name,"ratio_%s_over_%s",g0->GetName(),g1->GetName());

  const int Npoints0 = g0->GetN();
  const int Npoints1 = g1->GetN();

  // we take the max. in case of (some) non-coincident binning and total same size
  int Npoints = Max(Npoints0,Npoints1);
  int unusedNpoints = 0;

  double *pT  = new double[Npoints];
  double *epT = new double[Npoints];

  double *ratio_graphs = new double[Npoints];
  double *eratio_graphs = new double[Npoints];

  double xbinwidth = coincidentBinningWidth; // default is 0.25 (half 0.5 GeV/c)

  xbinwidth = (xbinwidth == 0.) ? 0.001 : xbinwidth; // if 0. set it to ~0.

  if (verbose)
    {
      cout << " <I> emcAnalyzerUtils::ratio of " << g0->GetName()
	   << " over " << g1->GetName() << endl;
      cout << "    pT                pT_err   Ratio    errRatio " << endl; 
    }

  double pT0;
  double pT1;
  double epT0;
  double epT1;
  double yield0;
  double yield1;
  double e_y0;
  double e_y1;

  int i0 = -1;
  int i1 = -1;
  
  for( int i=0; i<Npoints; i++)  // i runs over ratio
    {
      i0++; // i0 runs over 1st file
      i1++; // i1 runs over 2nd file

      yield0 = 0.;
      yield1 = 0;;
      e_y0 = 0.;
      e_y1 = 0.;
      epT0 = 0.;
      epT1 = 0.;
      
      bool notEnd0 = (i0<Npoints0);
      bool notEnd1 = (i1<Npoints1);

      if (notEnd0)
	{
	  g0->GetPoint(i0,pT0,yield0);
	  epT0 = g0->GetErrorX(i0);
	  if (yield0) 
	    e_y0 = g0->GetErrorY(i0)/yield0;
	  else
	    e_y0 = g0->GetErrorY(i0); // propagate something at least ...
	}
      if (notEnd1)
	{
	  g1->GetPoint(i1,pT1,yield1);
	  epT1 = g1->GetErrorX(i1);
	  if (yield1) 
	    e_y1 = g1->GetErrorY(i1)/yield1;
	  else
	    e_y1 = g1->GetErrorY(i1); // propagate something at least ...
	}
      
      // Not end of file 0 nor 1: check coincident binning
      if (notEnd0 && notEnd1)
	{
	  
	  if ( (pT0-pT1)<=-xbinwidth ) // pT0 bin value lower than pT1 bin (within xbinwidth)
	    {
	      if (verbose)
		{
		  cout << " <W> pt0=" << pT0 << " < pt1=" << pT1 
		       << " --> Skipping to next pT bin of graph 0 ... " << endl;
		}
	      while( ((pT0-pT1)<=-xbinwidth) && i0<Npoints0 ) // let's try to find the first equal x bin 
		{
		  i0++;
		  g0->GetPoint(i0,pT0,yield0);
		  epT0 = g0->GetErrorX(i0);
		  if (yield0) 
		    e_y0 = g0->GetErrorY(i0)/yield0;
		  else
		    e_y0 = g0->GetErrorY(i0); // propagate something at least ...
		}
	      if (i0==Npoints0) // coincident binning not found
		{
		  if (verbose)
		    {
		      cout << " <W> TGraphs with different x-axis values at pT=" 
			   << pT0 << " GeV/c. Cannot do further the ratio !" << endl;
		    }
		  while( i<=Npoints0 ) // fill the rest with zeros
		    {
		      //pT[i] = xbinwidth*2.*i;
		      //epT[i] = 0.;
		      //ratio_graphs[i] = crazyValue;//0.;
		      //eratio_graphs[i] = crazyValue;//0.;
		      cout << " <W> Skipping point " << xbinwidth*2.*i << " from graph " << g0->GetName() << endl;
		      unusedNpoints++;
		      i++;
		    }
		  break;
		}
	    }
	  else if ( (pT0-pT1)>=xbinwidth ) //  pT0 bin larger than pT1 bin (within xbinwidth)
	    {
	      if (verbose)
		{
		  cout << " <W> pt0=" << pT0 << " > pt1=" << pT1 
		       << " --> Skipping to next pT bin of graph 1 ... " << endl;
		}
	      while( ((pT0-pT1)>=xbinwidth) && i1<Npoints1 ) // let's try to find the first equal x bin 
		{
		  i1++;
		  g1->GetPoint(i1,pT1,yield1);
		  epT1 = g1->GetErrorX(i1);
		  if (yield1) 
		    e_y1 = g1->GetErrorY(i1)/yield1;
		  else
		    e_y1 = g1->GetErrorY(i1); // propagate something at least ...
		}
	      if (i1==Npoints1) // coincident binning not found
		{
		  if (verbose)
		    {
		      cout << " <W> TGraphs with different x-axis values at pT=" 
			   << pT1 << " GeV/c. Cannot do further the ratio !" << endl;
		    }
		  while( i<=Npoints1 ) // fill the rest with zeros
		    {
		      //pT[i] = xbinwidth*2.*i;
		      //epT[i] = 0.;
		      //ratio_graphs[i] = crazyValue;//0.;
		      //eratio_graphs[i] = crazyValue;//0.;
		      cout << " <W> Skipping point " << xbinwidth*2.*i << " from graph " << g1->GetName() << endl;
		      unusedNpoints++;
		      i++;
		    }
		  break;
		}
	    }
	  
	  // Now pT0 and pT1 should be equal (within binwidth)
	  
	  pT[i]  = pT0;  
	  epT[i] = epT0; 
	  
	  // let's take the ratio
	  
	  if (yield1) ratio_graphs[i] = yield0/yield1;
	  else ratio_graphs[i] = 0.;

	  eratio_graphs[i] = quadRelatError(e_y0,1.,e_y1,1.);
	  eratio_graphs[i] *= ratio_graphs[i];
	  
	  if (verbose || (pT0 =! pT1) ) // output when verbose or x's are different
	    {
	      printf("    %.2f (%.2f,%.2f)   %.2f    %.3f    %.3f\n",
		     pT[i],pT0,pT1,epT[i],ratio_graphs[i],eratio_graphs[i]);
	    }
	}
      else // end of one or both files
	{
	  unusedNpoints++;
	}
      
    } // loop on ratio bins

  // Fill the TGraph

  const int Npointsfinal = Npoints-unusedNpoints;
  
  double *pTfinal = pT; // pT0 and pT1 should be equal (within binwidth)
  double *epTfinal = epT;
  double *ratio_final = ratio_graphs;
  double *eratio_final = eratio_graphs; 

  ratio_g0_g1 = new TGraphErrors(Npointsfinal,pTfinal,ratio_final,epTfinal,eratio_final);
  ratio_g0_g1->SetName(name);
  ratio_g0_g1->SetTitle(name);
  setMarkerLineType(ratio_g0_g1, 20,9,1.2);

//   for ( int i = 0; i < Npointsfinal; i++ )
//     {
//       double x, y;
//       ratio_g0_g1->GetPoint(i,x,y);
//       if ((int)y==(int)crazyValue) ratio_g0_g1->RemovePoint(i);
//     }

  if (verbose)
    {
      cout << "<I> Dumping " << name << endl;
      ratio_g0_g1->Print();
    }

  delete pT;
  delete epT;

  delete ratio_graphs;
  delete eratio_graphs;

  return ratio_g0_g1;

}

//_____________________________________________________________________________
TGraphErrors*
ratio( TGraphErrors *g, 
       TF1 *f,
       const int option,
       const int verbose )
{

  if (!g || !f) return 0;

  int N = g->GetN();

  TGraphErrors *ratio = new TGraphErrors(N);

  char name[300];
  sprintf(name,"ratio_%s_over_%s",g->GetName(),f->GetName());
  if (option==1) sprintf(name,"product_%s_times_%s",g->GetName(),f->GetName());
  if (option==2) sprintf(name,"ratio_%s_over_%s",f->GetName(),g->GetName());

  ratio->SetName(name);
  ratio->SetTitle(name);

  for ( int i = 0; i < N; i++ )
    {
      double x, y;
      double ex, ey;

      g->GetPoint(i,x,y);
      ex = g->GetErrorX(i);
      ey = g->GetErrorY(i);

      if (!option)
	{
	  y/=f->Eval(x);
	  ey/=f->Eval(x);
	}
      else if (option == 1)
	{
	  y*=f->Eval(x);
	  ey*=f->Eval(x);
	}
      else if (option == 2)
	{
	  double eyrel = ey/y;
	  y=f->Eval(x)/y;
	  ey=eyrel*y;
	}

      ratio->SetPoint(i,x,y);
      ratio->SetPointError(i,ex,ey); // FIXME: We should propagate fit (f->GetParErrors()) errors !
    }

  setMarkerLineType(ratio,20,9,1.2);

  if (verbose)
    {
      cout << "<I> Dumping " << name << endl;
      ratio->Print();
    }

  return ratio;

}

//_____________________________________________________________________________
TGraphErrors* 
data_minus_fit_over_fit( TGraphErrors *g, 
			 TF1 *f,
			 const int verbose )
{

  // TO BE IMPLEMENTED !!

  if (!g || !f) return 0;

  int N = g->GetN();

  TGraphErrors *ratio = new TGraphErrors(N);

  for ( int i = 0; i < N; i++ )
    {
      double x, y;
      double ex, ey;

      g->GetPoint(i,x,y);
      ex = g->GetErrorX(i);
      ey = g->GetErrorY(i);

      y/=f->Eval(x);
      ey/=f->Eval(x);

      ratio->SetPoint(i,x,y);
      ratio->SetPointError(i,ex,ey);
    }

  setMarkerLineType(ratio,20,9,1.2);

  if (verbose) ratio->Print();

  return ratio;

}

//_____________________________________________________________________________
TGraphErrors*
ratioFits( TGraphErrors *g0, 
	   TGraphErrors *g1,
	   const int verbose )
{

  TGraphErrors *ratio_g0_g1 = 0;

  if (!g0 || !g1) return ratio_g0_g1;

  TF1 *f0 = 0;
  TF1 *f1 = 0;
  f0 = (TF1*)g0->GetFunction("hagedorn");
  if (!f0) f0 = (TF1*)g0->GetFunction("hagedornascii"); 

  f1 = (TF1*)g1->GetFunction("hagedorn");
  if (!f1) f1 = (TF1*)g1->GetFunction("hagedornascii");
  if (!f0 || !f1) return ratio_g0_g1;

  double xmin0, xmax0;
  f0->GetRange(xmin0, xmax0);
  double xmin1, xmax1;
  f1->GetRange(xmin1, xmax1);

  double xmin = Min(xmin0,xmin1);
  double xmax = Max(xmax0,xmax1);
  f0->SetRange(xmin,xmax);
  f1->SetRange(xmin,xmax);

  TH1F *h0 = (TH1F*)f0->GetHistogram();
  TH1F *h1 = (TH1F*)f1->GetHistogram();
  if (!h0 || !h1) return ratio_g0_g1;

  TH1F *hratio = (TH1F*)h0->Clone();
  hratio->Divide(h1);
  if (verbose)
    {
      hratio->Print("all");
    }

  ratio_g0_g1 = histo2graph(hratio,g0);

  setMarkerLineType(ratio_g0_g1, 20,9,1.2);

  //ratio_g0_g1->Print();

  return ratio_g0_g1;

}
//_____________________________________________________________________________
// Make a TGraphErrors out of h0 (if g0 is there, use its errors)

TGraphErrors*
histo2graph( TH1* h0, 
	     TGraphErrors* g0)
{

  if (!h0) return 0;

  int N = 0;

  if (g0)
    {
      N = g0->GetN();
    }
  else
    {
      N = h0->GetNbinsX();
    }

  double* x = new double[N];
  double* ex = new double[N];
  double* y = new double[N];
  double* ey = new double[N];

  for ( int i = 0; i < N; i++ )
    {
      double xi = 0; double yi = 0;
      double exi = 0; double eyi = 0;

      if (g0)
	{
	  xi = g0->GetX()[i];
	  int bin = h0->GetXaxis()->FindBin(xi);
	  yi = h0->GetBinContent(bin);
	  exi = g0->GetErrorX(i);
	  eyi = g0->GetErrorY(i);
	}
      else
	{
	  xi = h0->GetBinCenter(i); 
	  yi = h0->GetBinContent(i);
	  exi = h0->GetBinWidth(i)/2.;
	  eyi = h0->GetBinError(i);
	}

      x[i] = xi;
      y[i] = yi;
      ey[i] = y[i] * (eyi/yi);
      ex[i] = exi;

    }

  TGraphErrors* g2 = new TGraphErrors(N,x,y,ex,ey);

  delete x ;
  delete ex ;
  delete y ;
  delete ey ;

  return g2;
}

//_____________________________________________________________________________
// Make a TH1F out of TGraphErrors (for plotting purposes only !)

TH1F*
graph2histo( TGraphErrors *g0 )
{

  if (!g0) return 0;

  int N = g0->GetN();

  double Xmin = 0.;
  double Xmax = 0.;
  //Xmax = g0->GetXaxis()->GetXmax();
  //Xmin = g0->GetXaxis()->GetXmin();
  //cout << "graph2histo: xmin=" << Xmin << " xmax=" << Xmax << endl;
  getXminmax( g0, Xmin, Xmax);
  //cout << "graph2histo: xmin=" << Xmin << " xmax=" << Xmax << endl;

  double halfbinwidth = 0.5*(Xmax-Xmin)/N;

  TString name = (TString)g0->GetTitle();

  TH1F *h1 = new TH1F(name,name,N,Xmin-halfbinwidth,Xmax+halfbinwidth); // fixme: does not take into account variable bin TGraphs

  for ( int i = 0; i < N; i++ )
    {
      double xi,yi,eyi;

      g0->GetPoint(i,xi,yi);
      eyi = g0->GetErrorY(i);

      int bin = h1->GetXaxis()->FindBin(xi);
      h1->SetBinContent(bin,yi);
      h1->SetBinError(bin,eyi);
    }

  return h1;
}

//_____________________________________________________________________________
// Make a new TGraphErrors from an input one with Nunused points removed

TGraphErrors*
chopPointsFromGraph( TGraphErrors* gin, 
		     int Nunused)
{

  TGraphErrors* gout = 0;
  if (!gin) return gout;

  int N = gin->GetN();
  const int Nfinal = N - Nunused;

  double x[Nfinal];
  double ex[Nfinal];
  double y[Nfinal];
  double ey[Nfinal];

  for ( int i = 0; i < Nfinal; i++ )
    {
      gin->GetPoint(i,x[i],y[i]);
      ex[i] = gin->GetErrorX(i);
      ey[i] = gin->GetErrorY(i);
    }

  gout = new TGraphErrors(Nfinal,x,y,ex,ey);

  return gout;
}

//_____________________________________________________________________________
// Marker Type: [0] = style, [1] = color, [2] = size

void 
setMarker( TGraph *Spectrum,
	   const int CC )
{

  if (!Spectrum) return;

  if ( CC == -1) return ;

  double myMarker[3];

  if ( CC < 2 ) // 0-10% 
    {
      myMarker[0] = 20;
      myMarker[1] = 2;
      myMarker[2] = 1.4;
    }
  else if (  CC < 4 ) // 10-20% 
    {
      myMarker[0] = 21;
      myMarker[1] = 5;
      myMarker[2] = 1.4;
    }
  else if (  CC < 6 ) // 20-30% 
    {
      myMarker[0] = 22;
      myMarker[1] = 3;
      myMarker[2] = 1.7;
    }
  else if ( CC <  8 ) // 30-40% 
    {
      myMarker[0] = 20;
      myMarker[1] = 50;//39;
      myMarker[2] = 1.4;
    }
  else if ( CC < 10 ) // 40-50% 
    {
      myMarker[0] = 21;
      myMarker[1] = 4;
      myMarker[2] = 1.4;
    }
  else if ( CC < 12 ) // 50-60% 
    {
      myMarker[0] = 22;
      myMarker[1] = 6;
      myMarker[2] = 1.7;
    }
  else if ( CC < 14 ) // 60-70% 
    {
      myMarker[0] = 29;
      myMarker[1] = 1;
      myMarker[2] = 1.9;
    }
  else if ( CC < 16 ) // 70-80% 
    {
      myMarker[0] = 28;
      myMarker[1] = 9;
      myMarker[2] = 1.7;
    }
  else if ( CC < 18 ) // 80-92%, 60-92%
    {
      myMarker[0] = 21;
      myMarker[1] = 28;
      myMarker[2] = 1.6;
    }
  else if ( CC == 18 ) // 60-80%
    {
      myMarker[0] = 20;
      myMarker[1] = 39;
      myMarker[2] = 1.4;
    }
  else // min.bias
    {
      myMarker[0] = 20;
      myMarker[1] = 1 ;
      myMarker[2] = 1.4;
    }

  Spectrum->SetMarkerStyle((int)myMarker[0]);
  Spectrum->SetMarkerColor((int)myMarker[1]);
  Spectrum->SetMarkerSize(myMarker[2]);

}

//_____________________________________________________________________________
// Marker Type: [0] = style, [1] = color, [2] = size

void 
setMarker( TGraph *Spectrum,
	   const int Cent1, const int Cent2 )
{

  if (!Spectrum) return;

  int CC = getCentralityClass(Cent1, Cent2);

  setMarker(Spectrum,CC);

}

//_____________________________________________________________________________
//
void 
setMarkerLineType( TGraph *Spectrum,
		   const int style, 
		   const int color,
		   const double size,
		   const int linstyle, 
		   const int lincolor,
		   const int linwidth)
{

  if (!Spectrum) return;

  if (style) Spectrum->SetMarkerStyle(style);
  if (color) Spectrum->SetMarkerColor(color);
  if (size)  Spectrum->SetMarkerSize(size);

  if (linstyle) Spectrum->SetLineStyle(linstyle);
  if (lincolor) Spectrum->SetLineColor(lincolor);
  if (linwidth) Spectrum->SetLineWidth(linwidth);

}

//_____________________________________________________________________________
//
// "Last" attempt to get some easy way of plotting "shading" bands:
// (i)  fixed % error per bin (if NormRelatErr!=0)
// (ii) as given by TGraphErrors itself (if NormRelatErr==0)
//
// Usage:
//   TClonesArray* errBand = (TClonesArray*)emcAnalyzerUtils::errorBand( graph, eNorm );
//   TGraph *errshade = (TGraph*)(*errBand)[0];
//   TGraph *errmin = (TGraph*)(*errBand)[1];
//   TGraph *errmax = (TGraph*)(*errBand)[2];
//   errshade->Draw("f");
//   errmin->Draw("l");
//   errmax->Draw("l");


TClonesArray* 
errorBand( TGraph *g,
	   const double NormRelatErr )
{

  if (!g) return 0;

  TClonesArray *array = new TClonesArray("TGraph",3);

  int n = g->GetN();
  double x[n];
  double y[n];
  double ymax[n];
  double ymin[n];

   for (int i=0;i<n;i++) 
     {
       x[i] = g->GetX()[i];
       y[i] = g->GetY()[i];

       if (NormRelatErr!=0.)
	 {
	   ymax[i] = y[i]*(1+NormRelatErr);
	   ymin[i] = y[i]*(1-NormRelatErr);
	 }
       else if (g->InheritsFrom("TGraphErrors")) // attempt to do the same as next function ...
	 {
	   ymax[i] = y[i]+g->GetErrorY(i);
	   ymin[i] = y[i]-1.*g->GetErrorY(i);
	 }
     }

  TGraph *gshade = new TGraph(2*n);
  TGraph *gmin = new TGraph(n,x,ymin);
  TGraph *gmax = new TGraph(n,x,ymax);

  for (int i=0;i<n;i++) 
    {
      gshade->SetPoint(i,x[i],ymax[i]);
      gshade->SetPoint(n+i,x[n-i-1],ymin[n-i-1]);
    }

  //gshade->SetFillStyle(3002);
  gshade->SetFillColor(90);
  gmin->SetLineColor(94);
  gmax->SetLineColor(94);

  new((*array)[0]) TGraph(*gshade);
  new((*array)[1]) TGraph(*gmin);
  new((*array)[2]) TGraph(*gmax);

  return array;

}

//_____________________________________________________________________________
//
// plot error band between 2 tgraphs ...
// Usage:
//  TClonesArray* errBandpp = (TClonesArray*)errorBand( pp1, pp2 );
//  TGraph *ppshade = (TGraph*)(*errBandpp)[0];
//  TGraph *ppmin = (TGraph*)(*errBandpp)[1];
//  TGraph *ppmax = (TGraph*)(*errBandpp)[2];
//  ppshade->Draw("f");
//  ppmin->Draw("l");
//  ppmax->Draw("l");

TClonesArray* 
errorBand( TGraph *g0, TGraph *g1 )
{

  if (!g0 || !g1) return 0;

  TClonesArray *array = new TClonesArray("TGraph",3);
  
  int n = g0->GetN();
  double x0[n];
  double y0[n];
  double x1[n];
  double y1[n];

  double ymax[n];
  double ymin[n];

   for (int i=0;i<n;i++) 
     {
       x0[i] = g0->GetX()[i];
       y0[i] = g0->GetY()[i];
       x1[i] = g1->GetX()[i];
       y1[i] = g1->GetY()[i];

       if (x0[i]==x1[i])
	 {
	   ymax[i] = max(y0[i],y1[i]);
	   ymin[i] = min(y0[i],y1[i]);
	 }
       else
	 {
	   cout << " <W> TClonesArray* errorBand(g0,g1): non-coincident binning ..." << endl;
	 }
     }

  TGraph *gshade = new TGraph(2*n);
  TGraph *gmin = new TGraph(n,x0,ymin);
  TGraph *gmax = new TGraph(n,x0,ymax);

  for (int i=0;i<n;i++) 
    {
      gshade->SetPoint(i,x0[i],ymax[i]);
      gshade->SetPoint(n+i,x0[n-i-1],ymin[n-i-1]);
    }

  //gshade->SetFillStyle(3002);
  gshade->SetFillColor(90);
  gmin->SetLineColor(94);
  gmax->SetLineColor(94);

  new((*array)[0]) TGraph(*gshade);
  new((*array)[1]) TGraph(*gmin);
  new((*array)[2]) TGraph(*gmax);

  return array;

}

// //_____________________________________________________________________________
// //
 
// TClonesArray* 
// errorBoxes( TGraphErrors *g,
// 	    const double NormRelatErr,
// 	    const double width )
// {

//   if (!g) return 0;

//   int n = g->GetN();
//   TClonesArray *array = new TClonesArray("TBox",n);

//   double x[n];
//   double y[n];
//   double ex[n];
//   double ymax[n];
//   double ymin[n];
//   double gwidth = 0.;

//   for (int i=0;i<n;i++) 
//     {
//       x[i] = g->GetX()[i];
//       y[i] = g->GetY()[i];
//       ex[i] = g->GetErrorX(i)*width;

//       // let's set a fixed minimum width of the error boxes in case the TGraph has no x-errors
//       if (ex[i]==0.)
// 	{
// 	  gwidth = abs(g->GetX()[1]-g->GetX()[0])*width;
// 	  ex[i] = gwidth;
// 	}

//       if (NormRelatErr!=0.)
// 	{
// 	  ymax[i] = y[i]*(1+NormRelatErr);
// 	  ymin[i] = y[i]*(1-NormRelatErr);
// 	}
//       else
// 	{
// 	  ymax[i] = y[i]+g->GetErrorY(i);
// 	  ymin[i] = y[i]-1.*g->GetErrorY(i);
// 	}

//       //printf("x=%4.2f   y=%g   ex=%g   ymin=%g   ymax=%g\n",x[i],y[i],ex[i],ymin[i],ymax[i]);
//     }
  
//   TBox *box;
  
//   for (int i=0;i<n;i++) 
//     {
//       box = new TBox(x[i]-ex[i],ymin[i],
// 		     x[i]+ex[i],ymax[i]);
//       box->SetFillColor(kYellow);

//       new((*array)[i]) TBox(*box);
//     }

//   //array->Print();

//   return array;

// }

//_____________________________________________________________________________
// Typical use of error-boxes:
/* 
   TGraphErrors *eband = ... ;
   TClonesArray* eboxes = (TClonesArray*)analyzerUtils::errorBoxes( eband, 0.);  // error boxes
   TBox *box = 0;
   for (int j = 0; j < eboxes->GetEntries();j++)
   {
   box = ((TBox*)(*eboxes)[j]);
   box->SetFillColor(7);
   box->SetFillStyle(3001);
   c->cd();
   box->Draw();
   }
*/
TClonesArray* 
errorBoxes( TGraphErrors *g,
	    const double NormRelatErr ,
	    const double width ,
	    const bool logy )
{

  if (!g) return 0;

  int n = g->GetN();
  TClonesArray *array = new TClonesArray("TBox",n);

  double x[n];
  double y[n];
  double ex[n];
  double ymax[n];
  double ymin[n];
  double gwidth = 0.;

  for (int i=0;i<n;i++) 
    {
      x[i] = g->GetX()[i];
      y[i] = g->GetY()[i];
      ex[i] = g->GetErrorX(i)*width;

      // let's set a fixed minimum width of the error boxes in case the TGraph has no x-errors
      if (ex[i]==0.)
	{
	  gwidth = abs(g->GetX()[1]-g->GetX()[0])*width;
	  ex[i] = gwidth;
	}

      if (NormRelatErr!=0.)
	{
	  ymax[i] = y[i]*(1+NormRelatErr);
	  ymin[i] = y[i]*(1-NormRelatErr);
	}
      else
	{
	  ymax[i] = y[i]+g->GetErrorY(i);
	  ymin[i] = y[i]-1.*g->GetErrorY(i);
	}

      if (logy)
	{
	  ymax[i] = log10(ymax[i]);
	  ymin[i] = log10(ymin[i]);
	}

      //printf("x=%4.2f   y=%g   ex=%g   ymin=%g   ymax=%g\n",x[i],y[i],ex[i],ymin[i],ymax[i]);
    }
  
  TBox *box;
  
  for (int i=0;i<n;i++) 
    {
      box = new TBox(x[i]-ex[i],ymin[i],
		     x[i]+ex[i],ymax[i]);
      box->SetFillColor(kYellow);

      new((*array)[i]) TBox(*box);
    }

  //array->Print();

  return array;

}

//_____________________________________________________________________________
//

TGraph* plot_shaded_area( TF1 *f1, double xmin, double xmax)
{

  if (!f1) return 0;

  const int np = 10;
  
  //first fill area below a sub-range of f2
  double x[np+3], y[np+3];
  double dx = (xmax-xmin)/(np-1);
  for (int i=0;i<np;i++) 
    {
      x[i] = xmin + dx*i;
      y[i] = f1->Eval(x[i]); //f2->Eval(x[i]);
    }
  x[np]   = x[np-1]; y[np]   = 0.; // y[np]   = c1->GetUymin();
  x[np+1] = x[0];    y[np+1] = y[np];
  x[np+2] = x[0];    y[np+2] = y[0];

  TGraph *shadedgr = new TGraph(np+3,x,y);
  shadedgr->SetFillStyle(3001);
  shadedgr->SetFillColor(2);

  return shadedgr;
  //shadedgr->Draw("lf");
}

//_____________________________________________________________________________
//

TGraph* plot_shaded_area( TF1 *f1, TF1 *f2,
			  double xmin, double xmax)
{

  if (!f1 || !f2) return 0;

  const int np = 20;
    
  //second fill area between f1 and f2
  double x[2*np+1], y[2*np+1];
  double dx = (xmax-xmin)/(np-1);
  for (int i=0;i<np;i++) 
    {
      x[i] = xmin + dx*i;
      y[i] = f2->Eval(x[i]);
      x[np+i] = xmax - dx*i;
      y[np+i] = f1->Eval(x[np+i]);
    }
  x[2*np] = x[0]; y[2*np] = y[0];

  TGraph *shadedgr = new TGraph(2*np+1,x,y);
  shadedgr->SetFillStyle(3001);
  shadedgr->SetFillColor(2);

  return shadedgr;
  //shadedgr->Draw("lf");

}

//_____________________________________________________________________________
//

TPolyLine *plot_contour_area(TF1 *f, double plusminus)
{

  if (!f) return 0;

  TF1 *f_up = (TF1*)f->Clone();
  TF1 *f_dn = (TF1*)f->Clone();

  f_up ->SetParameter(0,f ->GetParameter(0)*(1.+plusminus));
  f_dn ->SetParameter(0,f ->GetParameter(0)*(1-plusminus));

  const int Np=4;
  float contour_x[Np], contour_y[Np];

  contour_x[0] = (float)f->GetXmin();  
  contour_x[1] = contour_x[0];  
  contour_x[2] = (float)f->GetXmax();  
  contour_x[3] = contour_x[2];
      
  contour_y[0] = f_dn ->Eval(contour_x[0]);
  contour_y[1] = f_up ->Eval(contour_x[0]);
  contour_y[2] = f_up ->Eval(contour_x[2]);
  contour_y[3] = f_dn ->Eval(contour_x[2]);

//    for (int i = 0; i < Np; i++)
//     {
//       float x = (float)(xmin +i);
//       if (i>N) x=xmax;
//       contour_x[i] =x;
//       contour_x[Np-i] = contour_x[i];
      
//       contour_y[i]     = f_up ->Eval(contour_x[i]);
//       contour_y[Np-i] = f_dn ->Eval(contour_x[Np-i]);
//     }
 
  TPolyLine *contour = new TPolyLine(Np,contour_x,contour_y);
  contour->SetFillColor(17);
  //contour->Draw("f");

  return contour;

}

// //_____________________________________________________________________________
// //

// TPolyLine *plot_contour_area( TGraph *g1, TGraph *g2 )
// {

//   if (!g1 || !g2) return 0;

//   const int Np=20;
//   float contour_x[Np], contour_y[Np];

// //   contour_x[0] = (float)f->GetXmin();  
// //   contour_x[1] = contour_x[0];  
// //   contour_x[2] = (float)f->GetXmax();  
// //   contour_x[3] = contour_x[2];
      
// //   contour_y[0] = f_dn ->Eval(contour_x[0]);
// //   contour_y[1] = f_up ->Eval(contour_x[0]);
// //   contour_y[2] = f_up ->Eval(contour_x[2]);
// //   contour_y[3] = f_dn ->Eval(contour_x[2]);

//    for (int i = 0; i < Np; i++)
//     {
//       float x = (float)(xmin +i);
//       if (i>N) x=xmax;
//       contour_x[i] =x;
//       contour_x[Np-i] = contour_x[i];
      
//       contour_y[i]     = f_up ->Eval(contour_x[i]);
//       contour_y[Np-i] = f_dn ->Eval(contour_x[Np-i]);
//     }
 
//   TPolyLine *contour = new TPolyLine(Np,contour_x,contour_y);
//   contour->SetFillColor(17);
//   //contour->Draw("f");

//   return contour;

// }

//_____________________________________________________________________________
//

void dump_function( TF1 *f1 )
{
  if (!f1) return;

  int N = f1->GetNpar();
  double *params = (double*)f1->GetParameters();

  cout << "TF1 *" << f1->GetName() << " = new TF1(\"" << f1->GetName() << "\",\""
       << f1->GetExpFormula() << "\","
       << f1->GetXmin() << "," << f1->GetXmax() << ");" << endl;

  cout << f1->GetName() << "->SetParameters(";
  for(int i=0;i<N;i++)
    {
      cout << params[i];
      if (i<(N-1))cout << ",";
    }
  cout << ");" << endl;

}

}// end of namespace


