#include <PHPySelectEfromOHF.h>
#include <PHPythiaContainer.h>
#include <PHPythiaHeader.h>
#include <PHPyCommon.h>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>

#include <TLorentzVector.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8)
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <TRandom3.h>

#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

PHPySelectEfromOHF::PHPySelectEfromOHF(const std::string &name): SubsysReco(name)
{
  phpythia = 0;		// array of pythia particles
  phpythiaheader = 0;		// array of pythia particles
  zvtx_width = 0;
}

PHPySelectEfromOHF::~PHPySelectEfromOHF()
{
}

int PHPySelectEfromOHF::Init(PHCompositeNode *topNode)
{

  //set the random number generator
  rnd = new TRandom3();
  rnd->SetSeed(0);

  return EVENT_OK;
}

int PHPySelectEfromOHF::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int PHPySelectEfromOHF::process_event(PHCompositeNode *topNode)
{

  // Get PHPythia header
  phpythiaheader = findNode::getClass<PHPythiaHeader>(topNode,"PHPythiaHeader");
  if (!phpythiaheader)
    {
      cout << PHWHERE << "Unable to get PHPythiaHeader, is Node missing?" << endl;
      return ABORTEVENT;
    }

  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if ( !phpythia ) {
    cout << PHWHERE << "Unable to get PHPythia, is Node missing?" << endl;
    return ABORTEVENT;
  }



  int npart = phpythia->size();
  int new_count = 1;
  vector<int> new_line_number(npart);
  vector<int> remove_tag(npart);

  if ( verbosity )
    {
      cout << PHWHERE << " Selecting desired particles" << endl;
    }

  for ( int ipart=0; ipart<npart; ipart++ )
    {
      TMCParticle *part = phpythia->getParticle(ipart);
      if ( ParticleCut(part)!=0 )
	{
	  remove_tag[ipart] = 1;

	  if ( verbosity )
	    {
	      cout << phpythia->getLineNumber(part) << "\t" << part->GetName() << "\t"
		   << part->GetParent() << "\t" << part->GetFirstChild() << "\t"
		   << part->GetLastChild() << endl;
	    }

	  //we found a particle we want, loop through the ancestry and save the parents
	  TMCParticle *parent = phpythia->getParent(part);
	  while (parent)
	    {
	      unsigned int pnum = phpythia->getLineNumber(parent);
	      remove_tag[pnum-1] = 1;

	      if ( verbosity )
		{
		  cout << phpythia->getLineNumber(parent) << "\t" << parent->GetName() << "\t"
		       << parent->GetParent() << "\t" << parent->GetFirstChild() << "\t"
		       << parent->GetLastChild() << endl;
		}

	      parent = phpythia->getParent(parent);
	    }

	  continue;
	}
      remove_tag[ipart] = 0;
    }

  if ( verbosity )
    {
      cout << PHWHERE << " Setting new line number" << endl;
    }

  for ( int ipart=0; ipart<npart; ipart++ ) {
    TMCParticle *part = phpythia->getParticle(ipart);
    if ( remove_tag[ipart]!=0 ) {
      new_line_number[ipart] = new_count++;
      if ( verbosity ) {
	cout << phpythia->getLineNumber(part) << "\t" << part->GetName() << "\t"
	     << part->GetParent() << "\t" << part->GetFirstChild() << "\t"
	     << part->GetLastChild() << endl;
      }
      continue;
    }

    new_line_number[ipart] = 0;
    phpythia->RemoveAt(ipart);
  }

  phpythia->Compress();

  if ( verbosity )
    {
      cout << PHWHERE << " Resetting parent/child dependencies" << endl;
    }


  // Now go through and reset parent and firstchild,lastchild line numbers
  npart = phpythia->size();
  for ( int ipart=0; ipart<npart; ipart++ ) {
    TMCParticle *part = phpythia->getParticle(ipart);
    int parent = part->GetParent();
    if ( parent!=0 ) {
      part->SetParent(new_line_number[parent-1]);
    }

    int firstchild = part->GetFirstChild();
    int lastchild = part->GetLastChild();
    if ( firstchild!=0 && lastchild!=0 ) {
      // find new entries for first and last child
      int new_firstchild = 0;
      int new_lastchild = 0;
      for ( int i=firstchild-1; i<=lastchild-1; i++ ) {
	if ( new_line_number[i]!=0 ) {
	  new_firstchild = new_line_number[i];
	  break;
	}
      }
      for ( int i=lastchild-1; i>=firstchild-1; i-- ) {
	if ( new_line_number[i]!=0 ) {
	  new_lastchild = new_line_number[i];
	  break;
	}
      }

      part->SetFirstChild(new_firstchild);
      part->SetLastChild(new_lastchild);
    } else {
      part->SetFirstChild(0);
      part->SetLastChild(0);
    }

    if ( verbosity ) {
      cout << phpythia->getLineNumber(part) << "\t" << part->GetName() << "\t"
	   << part->GetParent() << "\t" << part->GetFirstChild() << "\t"
	   << part->GetLastChild() << endl;
    }
  }

  //shift the vertex in z
  //Get a random flat vertex between -zvtx_width & zvtx_width
  double zvtx = rnd->Uniform(-1*zvtx_width, zvtx_width);

  if ( verbosity )
    {
      cout << PHWHERE << " Shifting z vertex to " << zvtx << " cm" << endl;
    }

  phpythiaheader->moveVertex(0,0,zvtx);
  phpythia->moveVertex(0,0,zvtx);



  return EVENT_OK;	// trigger condition not found, don't write out event
}

/// return 1 if we want to keep the particle, 0 if not
/// This example is for W bosons and any descendants of W's
int PHPySelectEfromOHF::ParticleCut( TMCParticle *part )
{
  //list of open heavy flavor bottom hadrons
  int KFlist_b[25] = {511, 521, 531, 541,         // B's
		      513, 523, 533, 543,         //B*'s
		      10511, 10521, 10531, 10541, //B*_0's
		      30513, 20523, 20533, 20543, //B*_1's
		      515, 525, 535, 545,         //B*_2's
		      5122,                       //Lambda_b
		      5342,                       //Omega_b
		      5112, 5212, 5222            //Sigma_b's
  };

  //list of open heavy flavor charm hadrons
  int KFlist_c[13] = {411, 421, 431, //D's
		      413, 423, 433, //D*'s
		      4122, //Lambda_c
		      4114, 4214, 4224, //Sigma*_c's
		      4334, 4434, 4444, //Omega*_c's
  };



  int kf = part->GetKF();
  int ks = part->GetKS();
  if ( ks>=0 && ks<=10 && abs(kf)==11)
    {
      if (phpythia->hasAncestorAbs(part, 5)) //bottom
	{
	  for (int iKF = 0; iKF < 25; iKF++)
	    {
	      if (phpythia->hasAncestorAbs(part,KFlist_b[iKF]))
		return 1;
	    }
	}
      else if (phpythia->hasAncestorAbs(part, 4)) //charm
	{
	  for (int iKF = 0; iKF < 13; iKF++)
	    {
	      if (phpythia->hasAncestorAbs(part,KFlist_c[iKF]))
		return 1;
	    }
	}
      else
	{
	  return 0;
	}
    }

  return 0;
}

