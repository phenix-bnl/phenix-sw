// $Id: Sngmuons.C,v 1.64 2014/03/10 05:45:12 mwysocki Exp $

/*!
  \file Sngmuons.C
  \brief single muons ntuples and histograms
  \author Frederic Fleuret/Hugo Pereira
  \version $Revision: 1.64 $
  \date $Date: 2014/03/10 05:45:12 $
*/

#include <iostream>
#include <MWGVertex.h>
#include "../MWGpico.h"

using namespace std;

//__________________________________________________
void MWGpico::BookSngmuonsNtuple(TNtuple*& sngmuons, TString name, TString title)
{
  switch( _runtype )
    {
    case RUN3: BookSngmuonsNtupleRun3( sngmuons, name, title ); break;
    case RUN4: BookSngmuonsNtupleRun4( sngmuons, name, title ); break;
    case RUN5: BookSngmuonsNtupleRun5( sngmuons, name, title ); break;
    case RUN6: BookSngmuonsNtupleRun6( sngmuons, name, title ); break; 
    case RUN7: BookSngmuonsNtupleRun7( sngmuons, name, title ); break; 
    case RUN8: BookSngmuonsNtupleRun8( sngmuons, name, title ); break; 
    case RUN8pp: BookSngmuonsNtupleRun8pp( sngmuons, name, title ); break; 
    case RUN9: BookSngmuonsNtupleRun9( sngmuons, name, title ); break; 
    case RUN12pp200: BookSngmuonsNtupleRun12pp200( sngmuons, name, title ); break; 
    case RUN12CuAu: BookSngmuonsNtupleRun12CuAu( sngmuons, name, title ); break;
    case RUN13: BookSngmuonsNtupleRun13( sngmuons, name, title ); break; 
    default:
      cout << "MWGpico::FillSngmuonsNtuple  -> _runtype: " << _runtype << " not implemented, use RUN6!" << endl;
      BookSngmuonsNtupleRun6( sngmuons, name, title ); break; 
    }
  return;
}

//__________________________________________________________________
void MWGpico::BookSngmuonsEvtNtuple(TNtuple*& sngvtx, TString name, TString title )
{
  switch( _runtype )
    {
    case RUN3: BookSngmuonsEvtNtupleRun3( sngvtx, name, title ); break;
    case RUN4: BookSngmuonsEvtNtupleRun4( sngvtx, name, title ); break;
    case RUN5: BookSngmuonsEvtNtupleRun5( sngvtx, name, title ); break;
    case RUN6: BookSngmuonsEvtNtupleRun6( sngvtx, name, title ); break; 
    case RUN7: BookSngmuonsEvtNtupleRun7( sngvtx, name, title ); break; 
    case RUN8: BookSngmuonsEvtNtupleRun8( sngvtx, name, title ); break; 
    case RUN8pp: BookSngmuonsEvtNtupleRun8pp( sngvtx, name, title ); break; 
    case RUN9: BookSngmuonsEvtNtupleRun9( sngvtx, name, title ); break; 
    case RUN12pp200: BookSngmuonsEvtNtupleRun12pp200( sngvtx, name, title ); break; 
    case RUN12CuAu: BookSngmuonsEvtNtupleRun12CuAu( sngvtx, name, title ); break;
    case RUN13: BookSngmuonsEvtNtupleRun13( sngvtx, name, title ); break; 
    default:
      cout << "MWGpico::FillSngmuonsNtuple  -> _runtype: " << _runtype << " not implemented, use RUN6!" << endl;
      BookSngmuonsEvtNtupleRun6( sngvtx, name, title ); 
      break;
    }
  return;
}

//_________________________________________________________________
int MWGpico::FillSngmuons(PHMuoTracksOut* &muo, TNtuple*  sngmuons,  TNtuple* sngvtx, bool golden)
{
  switch(_runtype)
    {
    case RUN3:  FillSngmuonsNtpRun3(muo,sngmuons,sngvtx); break; 
    case RUN4:  FillSngmuonsNtpRun4(muo,sngmuons,sngvtx); break; 
    case RUN5:  FillSngmuonsNtpRun5(muo,sngmuons,sngvtx); break; 
    case RUN6:  FillSngmuonsNtpRun6(muo,sngmuons,sngvtx); break; 
    case RUN7:  FillSngmuonsNtpRun7(muo,sngmuons,sngvtx); break; 
    case RUN8:  FillSngmuonsNtpRun8(muo,sngmuons,sngvtx); break; 
    case RUN8pp:  FillSngmuonsNtpRun8pp(muo,sngmuons,sngvtx); break; 
    case RUN9:  FillSngmuonsNtpRun9(muo,sngmuons,sngvtx); break; 
    case RUN12pp200:  FillSngmuonsNtpRun12pp200(muo,sngmuons,sngvtx,golden); break; 
    case RUN12CuAu: FillSngmuonsNtpRun12CuAu(muo,sngmuons,sngvtx); break;
    case RUN13:  FillSngmuonsNtpRun13(muo,sngmuons,sngvtx); break; 
    default:
      cout << "unrecognized run type:  ---> fill with RUN6 format!" << endl;
      FillSngmuonsNtpRun6(muo,sngmuons,sngvtx); break; 
    }
  return 0;
}
