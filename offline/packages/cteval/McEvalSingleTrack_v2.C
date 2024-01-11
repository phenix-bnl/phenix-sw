#include "McEvalSingleTrack_v2.h"
#include "RecoEvalSingleList_v2.h"
//INCLUDECHECKER: Removed this line: #include "RecoEvalSingleTrack.h"

ClassImp(McEvalSingleTrack_v2)

void McEvalSingleTrack_v2::Init()
{
  EVENTID    = -999;
  MCTRACKID  = -999;
  GENERATION = -999;
  PARTICLEID = -999;
  PARENTID   = -999;
  PRIMARYID  = -999;
  VERTEXX    = -999;
  VERTEXY    = -999;
  VERTEXZ    = -999;
  PARENTVERTEXX = -999;
  PARENTVERTEXY = -999;
  PARENTVERTEXZ = -999;
  PRIMARYVERTEXX = -999;
  PRIMARYVERTEXY = -999;
  PRIMARYVERTEXZ = -999;
  MOMENTUMX = -999;
  MOMENTUMY = -999;
  MOMENTUMZ = -999;
  PARENTMOMENTUMX = -999;
  PARENTMOMENTUMY = -999;
  PARENTMOMENTUMZ = -999;
  PRIMARYMOMENTUMX = -999;
  PRIMARYMOMENTUMY = -999;
  PRIMARYMOMENTUMZ = -999;
  QUALITY   = -999;
  MOMENTUMR = -999;
  THETA0    = -999;
  PHI0      = -999;
  PHI       = -999;
  ALPHA     = -999;
  ZED       = -999;
  BETA      = -999;

  RecoList  = new RecoEvalSingleList_v2();
  RecoList->Reset();
}

McEvalSingleTrack_v2::McEvalSingleTrack_v2()
{
  Init();
}

McEvalSingleTrack_v2::~McEvalSingleTrack_v2()
{
  if (RecoList)
    {
      RecoList->Clear();
      delete RecoList;
    }
}

McEvalSingleTrack_v2::McEvalSingleTrack_v2(McEvalSingleTrack *track)
{
  Init();
  if(!track) return;

  EVENTID             = track->get_eventid();
  MCTRACKID           = track->get_mctrackid();
  GENERATION          = track->get_generation();
  PARTICLEID          = track->get_particleid();
  PARENTID            = track->get_parentid();
  PRIMARYID           = track->get_primaryid();
  VERTEXX             = track->get_vertexx();
  VERTEXY             = track->get_vertexy();
  VERTEXZ             = track->get_vertexz();
  PARENTVERTEXX       = track->get_parentvertexx();
  PARENTVERTEXY       = track->get_parentvertexy();
  PARENTVERTEXZ       = track->get_parentvertexz();
  PRIMARYVERTEXX      = track->get_primaryvertexx();
  PRIMARYVERTEXY      = track->get_primaryvertexy();
  PRIMARYVERTEXZ      = track->get_primaryvertexz();
  MOMENTUMX           = track->get_momentumx();
  MOMENTUMY           = track->get_momentumy();
  MOMENTUMZ           = track->get_momentumz();
  PARENTMOMENTUMX     = track->get_parentmomentumx();
  PARENTMOMENTUMY     = track->get_parentmomentumy();
  PARENTMOMENTUMZ     = track->get_parentmomentumz();
  PRIMARYMOMENTUMX    = track->get_primarymomentumx();
  PRIMARYMOMENTUMY    = track->get_primarymomentumy();
  PRIMARYMOMENTUMZ    = track->get_primarymomentumz();
  QUALITY             = track->get_quality();
  MOMENTUMR           = track->get_momentumr();
  THETA0              = track->get_theta0();
  PHI0                = track->get_phi0();
  PHI                 = track->get_phi();
  ALPHA               = track->get_alpha();
  ZED                 = track->get_zed();
  BETA                = track->get_beta();
  RecoList            = track->get_RecoEvalSingleList();
}

void McEvalSingleTrack_v2::add_RecoEvalSingleTrack(RecoEvalSingleTrack 
						   *recotrack) 
{
  RecoList->AddRecoEvalSingleTrack(recotrack,
				   RecoList->get_RecoEvalSingleTrackN());
  RecoList->set_RecoEvalSingleTrackN(RecoList->get_RecoEvalSingleTrackN()+1);
}






