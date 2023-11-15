// $Id: encodeRootEvntKin.cc,v 1.7 2012/01/06 15:50:46 bbannier Exp $

/*!
\file  encodeRootEvntKin.cc
\brief store fortran output into KinPISAHit array
\version $Revision: 1.7 $
\date    $Date: 2012/01/06 15:50:46 $
*/

#include <iostream>
#include "PISAEvent.h"
#include "KinPISAHit.h"
#include <cstdlib>

using namespace std;

//________________________________________________________________________________________
void encodeRootEvntKin(int kentries, int i[], float f[], int isubevent, PISAEvent *pisaevent)
{

  // kentries is the number of KIN hits in this subevent (contains one extra entry)
  // for the last subevent, it is possible that kentries = 0

  // Save the PISAEvent startflag
  // startflag = 0  means subevent output of ROOT clone objects
  // startflag = 1  means full event output of ROOT clone objects
  // startflag = 2  means full event output of PHOOL objects (method not yet defined)
  Int_t startflag = pisaevent->GetStartFlag();

  // Save the KIN status flag
  // 0 indicates start of new event
  static Int_t KinStatus = 0;

  // KinCount is initialized when KinStatus = 0
  static Int_t KinCount = 0;

  // Following branch only needed for PHOOL output
  // kentries is the number of KIN hits in this subevent
  // for the last subevent, it is possible that kentries = 0
  if(startflag == 2 && kentries == 0 && KinStatus == 0)
  {

    // We should have no KinCount in this state
    if(KinCount != 0)
    {
      // This is an error condition
      std::cerr << "\n\n Error state in encodeRootEvntKin";
      std::cerr << "\n KinStatus = 0 and KinCount = " << KinCount << "\n";
      std::exit(1);
      
    } // check on whether there is any KinCount
    return;  // nothing to do since there is no KinCount in this event
  }

  Int_t argtrue_track = -1;  // Will change in Off-Line
  Int_t argnfile = -1;       // Will change in Off-line
  Int_t argisubevent = isubevent;
  
  // kentries will contain one more than actual number of tracks
  Int_t p = 0;
  for (int k=0; k<kentries-1; k++)
  {
    Int_t argntrack = i[p++];
    Int_t argidpart = i[p++];
    Float_t argptot = f[p++];
    Float_t argpthet = f[p++];
    Float_t argpphi = f[p++];
    Float_t argr_vertex = f[p++];
    Float_t argz_vertex = f[p++];
    Float_t argth_vertx = f[p++];
    Float_t argph_vertx = f[p++];
    Int_t argitparent = i[p++];
    Int_t argidparent = i[p++];

//     // print vertex
//     cout << "encodeRootEvntKin - vertex:"
//       << " r=" << argr_vertex 
//       << " z=" << argz_vertex 
//       << " theta=" << argth_vertx 
//       << " phi=" << argph_vertx 
//       << endl;
    
    // Original subevent output, kept for future use
    if(startflag == 0 || startflag == 1) 
    {
      
      pisaevent->AddKinHit(
        argtrue_track, argisubevent, argntrack, argidpart,
        argptot, argpthet, argpphi,
        argr_vertex, argz_vertex, argth_vertx,
        argph_vertx, argitparent, argidparent,
        argnfile);
    }
    
  }  
  
  return;
}
