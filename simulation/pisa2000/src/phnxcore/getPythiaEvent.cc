// $Id: getPythiaEvent.cc,v 1.11 2014/02/06 21:00:58 bbannier Exp $

/*!
   \file getPythiaEvent.cc
   \brief phPythia events stored in root files
   \author Mickey Chiu
   \version $Revision: 1.11 $
   \date $Date: 2014/02/06 21:00:58 $
*/

#include <PHHijingHeader.h>
#include <PHPythiaHeader.h>
#include <PHPythiaContainer.h>

// depending on the root version,
// the TMCParticle definition is not found
// in the same header file
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8)
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <TROOT.h>
#include <TFile.h>
#include <TNtuple.h>
#include <TTree.h>

#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <string>

using namespace std;

//________________________________________________________________________
// forward declaration
extern "C" void getpythiaevent_(
  int *mxtot, int idtot[], float pptot[], float xyzmv[],
  float xyz[], float* bimpact,
  int *istart, int *nClone, int *clength,  char *phpythiaFileName);

//_______________________________________________________________
void getpythiaevent_(
  int *mxtot, int idtot[], float pptot[], float xyzmv[],
  float xyz[],
  float* bimpact,
  int *istart, int *nClone,
  int *clength,
  char *phpythiaFileName)
{

  const float MMTOCM = 0.1; // convert mm to cm
  static int iFirst = 0;

  // mxtot is the number of particles in the event
  // idtot is the PDG particle ID  (will be converted to GEANT ID in FORTRAN)
  // pptot is the momentum 4-vector particle in GeV
  // xyzmv is the vertex of the particle in cm (pythia is in mm)

  static TTree *T = 0;
  static Int_t nentries = 0;
  static Int_t iCall = 0;
  static Int_t iStore = 0;
  static TFile *f1 = 0;

  // phpythia header
  static PHPythiaHeader *pyhdr = 0;
  static PHHijingHeader *hihdr = 0;

  // phpythia container
  static PHPythiaContainer *phpythia = 0;

  if (iFirst == 0) {

    cout << "getpythiaevent_ - initialization" << endl;

    // Open the PHPythia input file
    const std::string rootFile(phpythiaFileName, *clength);
    if (f1) delete f1;
    f1 = new TFile(rootFile.c_str(), "READ");
    if (!f1) {
      cerr << "getpythiaevent_ - Unable to find PHPythia input file "
           << rootFile << endl;
      exit(1);
    }

    T = (TTree*)f1->Get("T");
    if (!T) {
      cout << "getpythiaevent_ - Cannot find TTree T " << endl;
      exit(1);
    }

    // Set branch addresses
    if ( T->GetBranch("DST/PHPythia") )
    {

      T->SetBranchAddress("DST/PHPythiaHeader",&pyhdr);
      T->SetBranchAddress("DST/PHPythia",&phpythia);

    } else if ( T->GetBranch("DST/PHHijingHeader") ) {

      T->SetBranchAddress("DST/PHHijingHeader",&hihdr);
      T->SetBranchAddress("DST/PHHijing",&phpythia);

    } else {

      cout << "getpythiaevent_ - No PHPythia or PHHijing branch in file" << endl;
      throw std::runtime_error("getpythiaevent_: No PHPythia or PHHijing branch in file");
    }

    nentries = (Int_t)T->GetEntries();
    cout << "getpythiaevent_ - PHPythia input file " << rootFile << " has "
         << nentries << " entries " << endl;
    if ( *istart>0 )
    {
      cout << " Skipping first " << *istart << " events" << endl;
      iStore = *istart;
    }

    iFirst = 1;
  }

  // check on overflow beyond number of events
  if (iStore >= nentries)
  {
    cerr << "getpythiaevent_ - Trying to get non-existent entry in ROOT file " << endl;
    cerr << "getpythiaevent_ - iCall = " << iCall << ", iStore = " << iStore;
    cerr << " , nentries = " << nentries << endl;

    // set total number of particles to -1 and return
    // this tell the calling method that this is the end of the file

    *mxtot = -1;
    return;

  }

  // Now Retrieve the particles in this event
  T->GetEvent(iStore++);

  unsigned int npart = phpythia->size();
  if ( npart==0 )
  {
    *mxtot = 0;
    return;
  }

  int nstable_part = 0;
  for (unsigned int ipart=0; ipart<npart; ipart++)
  {

    TMCParticle *part = phpythia->getParticle(ipart);
    Int_t ks = part->GetKS();         // particle status
    if ( ks<1 || ks>10 ) continue;    // skip unstable particles

    idtot[nstable_part] = part->GetKF();
    pptot[nstable_part*4] = part->GetEnergy();
    pptot[nstable_part*4+1] = part->GetPx();
    pptot[nstable_part*4+2] = part->GetPy();
    pptot[nstable_part*4+3] = part->GetPz();
    xyzmv[nstable_part*3] = MMTOCM*part->GetVx();     // pythia pos. are in mm
    xyzmv[nstable_part*3+1] = MMTOCM*part->GetVy();
    xyzmv[nstable_part*3+2] = MMTOCM*part->GetVz();

    nstable_part++; // found a stable particle
  }

  *mxtot = nstable_part;
  *bimpact = 0;

  // store primary vertex
  if( pyhdr && string(pyhdr->ClassName()) != "PHPythiaHeaderV1")
  {

    xyz[0] = MMTOCM*pyhdr->GetPrimaryVertexX();
    xyz[1] = MMTOCM*pyhdr->GetPrimaryVertexY();
    xyz[2] = MMTOCM*pyhdr->GetPrimaryVertexZ();

  } else if( hihdr ) {

    xyz[0] = MMTOCM*hihdr->GetPrimaryVertexX();
    xyz[1] = MMTOCM*hihdr->GetPrimaryVertexY();
    xyz[2] = MMTOCM*hihdr->GetPrimaryVertexZ();

    // should also retrieve impact parameter
    *bimpact = hihdr->GetBimpact();

  } else if( nstable_part > 0 ) {

    // no primary vertex information available from header
    // use vertex from first stable particle
    xyz[0] = xyzmv[0];
    xyz[1] = xyzmv[1];
    xyz[2] = xyzmv[2];

  }

  iCall++;
  return;

}
