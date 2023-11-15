// $Id: PriPISAHitHelper.cc,v 1.5 2015/03/04 16:26:11 snowball Exp $

/*!
\file  PriPISAHitHelper.cc
\brief Helper class used to associate primary hit index to KinHit objects
\author  H. Pereira
\version $Revision: 1.5 $
\date    $Date: 2015/03/04 16:26:11 $
*/

#include "KinPISAHit.h"
#include "PriPISAHit.h"
#include "PriPISAHitHelper.h"

//_________________________________________
void PriPISAHitHelper::associate()
{
  // Set evttrack number in Kin objects
  // These should be primary track objects
  PriPISAHit *PriHitEvt = PriPISAHit::GetPriHitEvt();
  for( int pri=0; pri<PriPISAHit::GetPriCount(); pri++ )
  {

    PriPISAHit& local( PriHitEvt[pri] );

    Int_t evttrack = local.GetEvttrack();
    Int_t true_track = local.GetTrue_track();

    if( true_track > 0 )
    { KinPISAHit::Find( true_track )->SetEvttrack(evttrack); }

  }

  // Loop over all Kin objects for which the Primary evttrack has not been set
  // These could be secondaries in subsystems for which we can determine the primary ancestor
  // Secondaries which are not in the subsystems do not have their primary ancestor determined
  KinPISAHit *KinHitEvt = KinPISAHit::GetKinHitEvt();
  for( int kin=0; kin < KinPISAHit::GetKinCount(); kin++ )
  {

    // check if EvtTrack already set
    if(KinHitEvt[kin].GetEvttrack() >= 0) continue;

    // retrieve original hit (from primary particle) associated to this hit
    KinPISAHit &orig( *KinPISAHit::FindOrigin( KinHitEvt[kin].GetTrue_track() ) );

    // retrieve its priHit index
    Int_t evttrack = orig.GetEvttrack();
    //Int_t ttrack = orig.GetTrue_track();
    //Int_t tid = orig.GetIdpart();
    //Int_t ntrack = orig.GetNtrack();
    //Int_t tparnt = orig.GetItparent();
    //Int_t iparnt = orig.GetIdparent();

    // check result
    //assert( evttrack <= PriPISAHit::GetPriCount() );
    if(evttrack > PriPISAHit::GetPriCount())
      {
	if(warningCounter < 10)	std::cout << "PriPISAHitHelper::associate - WARNING: evttrack > PriPISAHit::GetPriCount()" << std::endl; 
	if(warningCounter == 10) std::cout << "PriPISAHitHelper::associate - WARNING: evttrack > PriPISAHit::GetPriCount() - silencing warning message from now on..." << std::endl;
	warningCounter++;
      }
    //if( KinHitEvt[kin].GetIdpart() > 0 ) { assert( evttrack > 0 ); }
    //std::cout << kin << "  ("<< KinHitEvt[kin].GetIdpart() << " - " << KinHitEvt[kin].GetNtrack() << "," << KinHitEvt[kin].GetEvttrack() << "," << KinHitEvt[kin].GetTrue_track() << "," << KinHitEvt[kin].GetItparent() << "," << KinHitEvt[kin].GetIdparent() 
    //	      << ")  ("<< tid << " - " << ntrack << "," << evttrack <<  "," << ttrack << "," << tparnt << "," << iparnt << ") " << std::endl;
    
    // assign track
    KinHitEvt[kin].SetEvttrack(evttrack);

  }

  return;

}
