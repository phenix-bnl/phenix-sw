// $Id: Dimuons.C,v 1.59 2012/02/03 04:45:32 rseidl Exp $

/*!
   \file Dimuons.C
   \brief wrapper for dimuons ntuple booking and filling
   \author Frederic Fleuret/Hugo Pereira
   \version $Revision: 1.59 $
   \date $Date: 2012/02/03 04:45:32 $
*/

#include <iostream>
#include "../MWGpico.h"

using namespace std;

//_________________________________________________________________
void MWGpico::BookDimuonsNtuple( TNtuple*& dimuons, TString name, TString title )
{
  switch( _runtype )
    {
    case RUN3: BookDimuonsNtupleRun3( dimuons, name, title ); break;
    case RUN4: BookDimuonsNtupleRun4( dimuons, name, title ); break;
    case RUN5: BookDimuonsNtupleRun5( dimuons, name, title ); break;
    case RUN6: BookDimuonsNtupleRun6( dimuons, name, title ); break;
    case RUN7: BookDimuonsNtupleRun7( dimuons, name, title ); break;
    case RUN8: BookDimuonsNtupleRun8( dimuons, name, title ); break;
    case RUN8pp: BookDimuonsNtupleRun8pp( dimuons, name, title ); break;
    case RUN9: BookDimuonsNtupleRun9( dimuons, name, title ); break;
    case RUN10: BookDimuonsNtupleRun10( dimuons, name, title ); break;
    case RUN11: BookDimuonsNtupleRun11( dimuons, name, title ); break;
    case RUN11pp: BookDimuonsNtupleRun11pp( dimuons, name, title ); break;
    case RUN12: BookDimuonsNtupleRun12( dimuons, name, title ); break;
    case RUN12pp: BookDimuonsNtupleRun12pp( dimuons, name, title ); break;

    default:
      cout << "MWGpico::BookDimuonsNtuple - _runtype: " << get_run_type_name() << " not implemented" << endl;
      break;
    }
  return;
}

//_________________________________________________________________
void MWGpico::BookDimuonsNtupleBackToBack( TNtuple*& dimuonsb2b, TString name, TString title )
{
  switch( _runtype )
  {
    case RUN5: BookDimuonsNtupleRun6BackToBack( dimuonsb2b, name, title ); break;
    case RUN6: BookDimuonsNtupleRun6BackToBack( dimuonsb2b, name, title ); break;
//    case RUN12: BookDimuonsNtupleRun12ppBackToBack( dimuonsb2b, name, title ); break;

    default:
//    cout << "MWGpico::BookDimuonsNtupleBackToBack - _runtype: " << get_run_type_name() << " not implemented" << endl;
    break;
  }

  return;
}

//_________________________________________________________________
int MWGpico::FillDimuons(PHMuoTracksOut* &muo, TNtuple* dimuons)
{
  switch( _runtype )
  {

    case RUN3: FillDimuonsRun3( muo, dimuons ); break;
    case RUN4: FillDimuonsRun4( muo, dimuons ); break;
    case RUN5: FillDimuonsRun5( muo, dimuons ); break;
    case RUN6: FillDimuonsRun6( muo, dimuons ); break;
    case RUN7: FillDimuonsRun7( muo, dimuons ); break;
    case RUN8: FillDimuonsRun8( muo, dimuons ); break;
    case RUN8pp: FillDimuonsRun8pp( muo, dimuons ); break;
    case RUN9: FillDimuonsRun9( muo, dimuons ); break;
    case RUN10: FillDimuonsRun10( muo, dimuons ); break;
    case RUN11: FillDimuonsRun11( muo, dimuons ); break;
    case RUN11pp: FillDimuonsRun11pp( muo, dimuons ); break;
    case RUN12: FillDimuonsRun12( muo, dimuons ); break;
    case RUN12pp: FillDimuonsRun12pp( muo, dimuons ); break;

    default:
    cout << "MWGpico::FillDimuons - _runtype: " << _runtype << " not implemented" << endl;
    break;
  }

  return 0;
}
//----------------------------------------------------------------
int MWGpico::FillDimuonsBackToBack(PHMuoTracksOut* &muo, TNtuple* dimuonsb2b)
{
  switch( _runtype )
  {
    case RUN5: FillDimuonsRun6BackToBack( muo, dimuonsb2b ); break;
    case RUN6: FillDimuonsRun6BackToBack( muo, dimuonsb2b ); break;
//    case RUN12: FillDimuonsRun12ppBackToBack( muo, dimuonsb2b ); break;

    default:
    cout << "MWGpico::FillDimuonsBackToBack - _runtype: " << _runtype << " not implemented" << endl;
    break;
  }

  return 0;
}

