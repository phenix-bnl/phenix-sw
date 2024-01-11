// sort the vtxp events, and make the bbc vs vtxp chip correlation
//#include <CorrDataV1.h>
#include <TSystem.h>
#include <TFile.h>
#include <TTree.h>
#include <TGraph.h>
#include <TString.h>
#include <iostream>
#include <fstream>
#include <queue>

#ifndef __CINT__
#include <CorrDataV1.h>
#endif

const int NCHIPS = 24;
const int MAXFILES = 500;
const int packetid[] = {
  24016, 24017, 24018
};

void sortvtxp_tmoon(const char *flist = "corr.list")
{
  gSystem->Load("libsvxjumpchip.so");

  queue<int> a;
  // New TFile that has events in order
  Int_t     fEvent;
  UInt_t    fStrig;
  UInt_t    fSegment;
  Int_t     fEntry;
  Int_t     fNchips = NCHIPS;
  Int_t     fVtxpSum;
  Int_t     fVtxsSum;
  Int_t     fFvtxSum;
  CorrData  *fCorrdata = new CorrDataV1();
  Float_t   fOrigCorr[NCHIPS];
  Float_t   fFixCorr[NCHIPS];
  TFile *savefile = new TFile("sorted.root","RECREATE");
  TTree *ttree = new TTree("t","JumpChip Study, Events in Order");
  ttree->Branch("evt", &fEvent);
  ttree->Branch("strig", &fStrig);
  ttree->Branch("seg", &fSegment);
  ttree->Branch("entry", &fEntry);
  ttree->Branch("vtxpsum", &fVtxpSum);
  ttree->Branch("vtxssum", &fVtxsSum);
  ttree->Branch("fvtxsum", &fFvtxSum);
  //ttree->Branch("nchips", &fNchips);
  ttree->Branch("chipdata", &fCorrdata);
  ttree->Branch("origcorr", &fOrigCorr,"origcorr[24]/F");
  ttree->Branch("fixcorr", &fFixCorr,"fixcorr[24]/F");
  TGraph  *g_bbcq_vs_chiphits[NCHIPS];
  TGraph  *g_corr_vs_event[NCHIPS]; // correlation strength between bbcq and hits in a chip
  TString gname;
  for (int ichip=0; ichip<NCHIPS; ichip++)
  {
    g_bbcq_vs_chiphits[ichip] = new TGraph();
    gname = "bbcq_vs_chiphits_p"; gname += ichip/8; gname += "c"; gname += ichip%8;
    g_bbcq_vs_chiphits[ichip]->SetName(gname);

    g_corr_vs_event[ichip] = new TGraph();
    gname = "corr_vs_event_p"; gname += ichip/8; gname += "c"; gname += ichip%8;
    g_corr_vs_event[ichip]->SetName(gname);
  }

  // Buffer for storing events in regular order
  const int BUFFER_SIZE = 32000000; // Maximum number of events in a run?
  Int_t       eventnumBuf[BUFFER_SIZE] = {0};
  UInt_t      strigBuf[BUFFER_SIZE] = {0};
  Short_t     segBuf[BUFFER_SIZE] = {0};
  UInt_t      entryBuf[BUFFER_SIZE] = {0};
  Int_t     vtxpsumBuf[BUFFER_SIZE] = {0};
  Int_t     vtxssumBuf[BUFFER_SIZE] = {0};
  Int_t     fvtxsumBuf[BUFFER_SIZE] = {0};
  CorrDataV1  CorrDataBuf[BUFFER_SIZE];
  for (int i=0; i<BUFFER_SIZE; i++)
  {
    eventnumBuf[i] = 0;
  }

  // Get data from segments (not sorted yet)
  ifstream inlist(flist);
  TFile *tfile[MAXFILES];

  Int_t     evt      = 0;
  UInt_t    strig    = 0;
  Int_t     vtxpsum  = 0;
  Int_t     vtxssum  = 0;
  Int_t     fvtxsum  = 0;
  ULong64_t cross    = 0;
  ULong64_t clock    = 0;
  CorrData *corrdata = 0;
  TTree *t;

  Int_t max_evtnum = -1;

  int nfiles = 0;
  TString fname;
  TString name;
  while ( inlist >> fname )
  {
    cout << fname << endl;
    // get the segment number (needs to be done)
    TString segname = fname;
    segname.ReplaceAll(".root","");
    int len = segname.Length();
    segname.Remove(0,len-4);
    int segment = segname.Atoi();
    //cout << segname << "\t" << segment << endl;

    tfile[nfiles] = new TFile(fname,"READ");
    t = (TTree*)tfile[nfiles]->Get("t");
    t->SetBranchAddress("evt",&evt);
    t->SetBranchAddress("strig",&strig);
    t->SetBranchAddress("vtxpsum",&vtxpsum);
    t->SetBranchAddress("vtxssum",&vtxssum);
    t->SetBranchAddress("fvtxsum",&fvtxsum);
    t->SetBranchAddress("cross",&cross);
    t->SetBranchAddress("corr",&corrdata);
    
    Long64_t nentries = t->GetEntries();
    for (Long64_t ientry=0; ientry<nentries; ientry++)
    {
      t->GetEntry(ientry);
      if ( (ientry%10000)==0 )
      {
        cout << ientry << "\t" << evt << "\t" << evt%BUFFER_SIZE 
          << "\t" << strig << "\t" << corrdata->get_bbcq() << endl;
      }

      // BBC charge cut for Run15pAu as Akiba-san suggested
      //if ( corrdata->get_bbcq()<100. ) continue;

      eventnumBuf[evt%BUFFER_SIZE] = evt;
      strigBuf[evt%BUFFER_SIZE] = strig;
      segBuf[evt%BUFFER_SIZE] = segment;
      entryBuf[evt%BUFFER_SIZE] = ientry;
      vtxpsumBuf[evt%BUFFER_SIZE] = vtxpsum;
      vtxssumBuf[evt%BUFFER_SIZE] = vtxssum;
      fvtxsumBuf[evt%BUFFER_SIZE] = fvtxsum;

      //cout << "aaa " << corrdata->get_bbcq() << endl;
      //CorrDataBuf[evt%BUFFER_SIZE] = corrdata;
      CorrDataBuf[evt%BUFFER_SIZE].set_bbcq(corrdata->get_bbcq());
      for (int ichip=0; ichip<NCHIPS; ichip++)
      {
        CorrDataBuf[evt%BUFFER_SIZE].set_nhits(ichip,corrdata->get_nhits(ichip));
      }

      if ( evt > max_evtnum ) max_evtnum = evt;

    }

    tfile[nfiles]->Close();
    nfiles++;
  }

  cout << "Found " << max_evtnum << " events" << endl;

  // End of run, process the stored data
  const Int_t NEVENTS_IN_CORR = 40;
  Int_t corr_index[NCHIPS] = {0};
  Int_t num_badgood[NCHIPS] = {0};
  Int_t event_misalign[NCHIPS] = {0};
  Int_t discontinuity[NCHIPS] = {0};

  savefile->cd();
  for (Int_t ievt=1; ievt<=max_evtnum; ievt++)
  {
    //if ( (ievt%10000)==0 ) cout << "evt " << ievt << endl;
    // missing event!
    if ( eventnumBuf[ievt] == 0 )
    {
      //cout << "Found missing event " << ievt << endl;
      continue;
    }

    fEvent = eventnumBuf[ievt];
    fStrig = strigBuf[ievt];
    fSegment = segBuf[ievt];
    fEntry   = entryBuf[ievt];
    fVtxpSum =  vtxpsumBuf[ievt];
    fVtxsSum =  vtxssumBuf[ievt];
    fFvtxSum =  fvtxsumBuf[ievt];
    *fCorrdata = CorrDataBuf[ievt];

    // don't calculate correlation for non-mbias events
    // (note this means we store value from previous event)
    if ( (fStrig&0x10) == 0 )
    {
      ttree->Fill();
      continue;
    }

    // Compute Correlation with BBC for BBC minbias events
    Float_t bbcq_to_use;
    Int_t index;
    for (int ichip=0; ichip<NCHIPS; ichip++)
    {
      bbcq_to_use = fCorrdata->get_bbcq();
      index = corr_index[ichip]%NEVENTS_IN_CORR;

      g_bbcq_vs_chiphits[ichip]->SetPoint(index,bbcq_to_use,fCorrdata->get_nhits(ichip));
      Double_t corr = g_bbcq_vs_chiphits[ichip]->GetCorrelationFactor();
      fOrigCorr[ichip] = corr;

      Int_t n = g_corr_vs_event[ichip]->GetN();
      g_corr_vs_event[ichip]->SetPoint(n,fEvent,corr);

      // Look for a bad event
      // Here we look for N consecutive bad or good events
      int bad_or_good = 0;
      if ( corr<0.4 ) bad_or_good = 1;

      if ( event_misalign[ichip]==0 )
      {
        // in good state, look for consecutive bad
        if ( bad_or_good == 1 )
        {
          num_badgood[ichip]++;
          // store first event where correlation starts to drop
          if ( num_badgood[ichip] == 1 ) discontinuity[ichip] = fEvent;
        }
        else
        {
          num_badgood[ichip] = 0;
        }
      }
      else if ( event_misalign[ichip]!=0 )
      { 
        // in bad state, look for consecutive good
        if ( bad_or_good == 0 )
        {
          num_badgood[ichip]++;
          // store first event where correlation starts to rise
          if ( num_badgood[ichip] == 1 ) discontinuity[ichip] = fEvent;
        }
        else
        {
          num_badgood[ichip] = 0;
        }
      }

      // testing purposes for now
      fFixCorr[ichip] = num_badgood[ichip];

      if ( event_misalign[ichip]==0 && num_badgood[ichip]>=80 )
      {
        cout << "Found good2bad transition, pkt/chip/event/seg "
          << packetid[ichip/8] << "\t"
          << ichip%8 << "\t" << discontinuity[ichip]
          << "\t" << segBuf[ discontinuity[ichip] ] << endl;
        event_misalign[ichip] = -1;
        num_badgood[ichip] = 0;
      }
      if ( event_misalign[ichip]!=0 && num_badgood[ichip]>=80 )
      {
        cout << "Found bad2good transition, pkt/chip/event/seg "
          << packetid[ichip/8] << "\t"
          << ichip%8 << "\t" << discontinuity[ichip]
          << "\t" << segBuf[ discontinuity[ichip] ] << endl;
        event_misalign[ichip] = 0;
        num_badgood[ichip] = 0;
      }

      corr_index[ichip]++;
    }

    if (fNchips!=24 ) cout << "fnchips = " << fNchips << endl;
    ttree->Fill();
  }

  for (int ichip=0; ichip<NCHIPS; ichip++)
  {
    g_bbcq_vs_chiphits[ichip]->Write();
    g_corr_vs_event[ichip]->Write();
  }

  savefile->Write();
  savefile->Close();

}

