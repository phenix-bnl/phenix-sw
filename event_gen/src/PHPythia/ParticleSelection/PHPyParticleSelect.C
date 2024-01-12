#include <PHPyParticleSelect.h>
#include <PHPythiaContainer.h>
#include <PHPyCommon.h>

#include <getClass.h>
#include <Fun4AllReturnCodes.h>

#include <TLorentzVector.h>

#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

PHPyParticleSelect::PHPyParticleSelect(const std::string &name): SubsysReco(name)
{
  phpythia = 0;		// array of pythia particles
}

PHPyParticleSelect::~PHPyParticleSelect()
{
}

int PHPyParticleSelect::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int PHPyParticleSelect::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int PHPyParticleSelect::process_event(PHCompositeNode *topNode)
{
  // Get PYTHIA Particles
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if (!phpythia)
    {
      cout << PHWHERE << "Unable to get PHPythia, is Node missing?" << endl;
      return ABORTEVENT;
    }


  int npart = phpythia->size();
  if ( verbosity ) cout << "Old List " << npart << endl;

  int new_count = 1;
  vector<int> new_line_number(npart);
  vector<int> remove_tag(npart); 
 
  for (int ipart=0; ipart<npart; ipart++)
    {
      TMCParticle *part = phpythia->getParticle(ipart);

      if ( ParticleCut(part) != 0 )
        {
          remove_tag[ipart] = 1;

          if ( verbosity )
            {
              cout << phpythia->getLineNumber(part) << "\t" << part->GetName()
                   << "\t" << part->GetParent() << "\t" << part->GetFirstChild() << "\t" << part->GetLastChild() << endl;
            }

          continue;
        }

      remove_tag[ipart] = 0;
    }

  for (int ipart=0; ipart<npart; ipart++)
    {
      TMCParticle *part = phpythia->getParticle(ipart);

      if ( remove_tag[ipart] != 0 )
        {
          new_line_number[ipart] = new_count++;

          if ( verbosity )
            {
              cout << phpythia->getLineNumber(part) << "\t" << part->GetName()
                   << "\t" << part->GetParent() << "\t" << part->GetFirstChild() << "\t" << part->GetLastChild() << endl;
            }

          continue;
        }

      new_line_number[ipart] = 0;
      phpythia->RemoveAt(ipart);
    }

  phpythia->Compress();

  // Now go through and reset parent and firstchild,lastchild line numbers
  npart = phpythia->size();
  if ( verbosity ) cout << "New List " << npart << endl;
  for (int ipart=0; ipart<npart; ipart++)
    {
      TMCParticle *part = phpythia->getParticle(ipart);

      Int_t parent = part->GetParent();
      if ( parent!=0 )
        {
          part->SetParent( new_line_number[ parent-1 ] );
        }

      Int_t firstchild = part->GetFirstChild();
      Int_t lastchild = part->GetLastChild();
      if ( firstchild!=0 && lastchild!=0 )
        {
          // find new entries for first and last child
          int new_firstchild = 0;
          int new_lastchild = 0;
          for ( int i=firstchild-1; i<=lastchild-1; i++)
            {
              if ( new_line_number[i] != 0 )
                {
                  new_firstchild = new_line_number[i];
                  break;
                }
            }
          for ( int i=lastchild-1; i>=firstchild-1; i--)
            {
              if ( new_line_number[i] != 0 )
                {
                  new_lastchild = new_line_number[i];
                  break;
                }
            }

          part->SetFirstChild( new_firstchild );
          part->SetLastChild( new_lastchild );
        }
      else
        {
          part->SetFirstChild( 0 );
          part->SetLastChild( 0 );
        }

      if ( verbosity )
        {
          cout << phpythia->getLineNumber(part) << "\t" << part->GetName()
               << "\t" << part->GetParent() << "\t" << part->GetFirstChild() << "\t" << part->GetLastChild() << endl;
        }
    }

  return EVENT_OK;	// trigger condition not found, don't write out event
}

/// return 1 if we want to keep the particle, 0 if not
/// This example is for W bosons and any descendants of W's
int PHPyParticleSelect::ParticleCut( TMCParticle *part )
{
  if ( part->GetKF() == abs(PY_W) ) return 1;
  if ( phpythia->hasAncestor(part,PY_W) != 0 ) return 1;
  if ( phpythia->hasAncestor(part,-PY_W) != 0 ) return 1;

  return 0;
}

