#ifndef __PADDOMINANTDCH__
#define __PADDOMINANTDCH__

// #include <dDchTracksWrapper.h>
#include <dDchGhitHitsWrapper.h>
#include <dDchHitWrapper.h>
#include <DchTrackv1.h>
#include <dcghitWrapper.h>

int PadDominantDch(int dchTrackID, dDchGhitHitsWrapper *dDchGhitHits, DchTrackv1 *DchTracks,
		   dcghitWrapper *dcghit, int &hitPlanes, int &domTotSum, int &hitUVPlanes,
		   dDchHitWrapper *dDchHit, int iEvent);

// int PadDominantDch(int dchTrackID, dDchGhitHitsWrapper *dDchGhitHits, dDchTracksWrapper *dDchTracks,
//                    dcghitWrapper *dcghit, int &hitPlanes, int &domTotSum, int &hitUVPlanes,
//                    dDchHitWrapper *dDchHit);

#endif
