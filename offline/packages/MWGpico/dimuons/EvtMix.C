#include <TChain.h>
#include <TNtuple.h>      // needed by root
#include <TH1.h>
#include <vector>
#include <string>
#include <PHGlobal.h> // event information
#include <PHMuoTracksOut.h> // muon and dimuon information
#include <Tools.h>
#include <MWGConsts.h>
#include "../MWGpico.h"

using namespace std;

//_________________________________________________________________
int MWGpico::BookEvtMixNtuple( TNtuple*& evtMix, TString name, TString title )
{
  // define variable list
  
  if( _runtype == RUN3 ) BookEvtMixNtupleRun3( evtMix, name, title );
  else if( _runtype == RUN4 ) BookEvtMixNtupleRun4( evtMix, name, title );
  else {
    cout << "unrecognized run type" << endl;
    return 1;
  }
  return 0;
}

int MWGpico::StoreEvtMixMuons (PHMuoTracksOut* &muo)
{
  if (_runtype == RUN3) StoreEvtMixMuonsRun3(muo);
  else if (_runtype == RUN4) StoreEvtMixMuonsRun4(muo);
  else {
    cout << "unrecognized run type" << endl;
    return 1;
  }
  return 0;
}

int MWGpico::FillEvtMix(TNtuple* evtMix)
{
  if( _runtype == RUN3 )  FillEvtMixRun3(evtMix);
  else if( _runtype == RUN4 ) FillEvtMixRun4(evtMix);
  else {
    cout << "unrecognized run type" << endl;
    return 1;
  }
  return 0;
}
