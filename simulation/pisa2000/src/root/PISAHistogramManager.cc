#include "TDirectory.h"
#include "PISAEvent.h"
#include "PISAHistogramManager.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// PISAHistogramManager.cc                                              //
//                                                                      //
// Implementation of ROOT histograms in PISA                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(PISAHistogramManager)


PISAHistogramManager::PISAHistogramManager(TDirectory *dir)
{
  // Create PISA Histogram object in the "dir" directory

  // Save current directory and cd to "dir"
  TDirectory *saved = gDirectory;
  dir->cd();

  fBbcNhit = new TH1F("hNhit", "BBC Nhit", 100,0,100);
  fBbcdel  = new TH1F("hDel", "BBC Energy loss", 100, 0, 100);

  // cd back to original directory
  saved->cd();
}

PISAHistogramManager::~PISAHistogramManager()
{
   // Clean up all histograms.

   // Nothing to do. Histograms will be deleted when the directory
   // in which they are stored is closed.
}


void PISAHistogramManager::PISAHfill(PISAEvent *PISAevent)
{
   // Fill histograms.

   fBbcNhit->Fill(PISAevent->GetBbcNhit());
   /*
   for (Int_t ihit=0; ihit<PISAevent->GetBbcNhit(); ihit++) {
     PISAHit *hit = (PISAHit*)PISAevent->GetHits()->UncheckedAt(ihit);
     fdel->Fill(hit->GetDel());
   } // loop over number of hits in this PISA event
   */
}
