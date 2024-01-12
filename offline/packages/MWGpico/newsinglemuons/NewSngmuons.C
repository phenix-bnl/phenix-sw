// $Id: NewSngmuons.C,v 1.2 2012/01/23 09:01:46 rseidl Exp $

/*!
  \file Sngmuons.C
  \brief single muons ntuples and histograms
  \author Frederic Fleuret/Hugo Pereira
  \version $Revision: 1.2 $
  \date $Date: 2012/01/23 09:01:46 $
*/

#include <iostream>
#include <MWGVertex.h>
#include "../MWGpico.h"

using namespace std;

//__________________________________________________
void MWGpico::BookNewSngmuonsNtuple(TTree*& newsngmuons, TString name, TString title)
{
  //  cout <<  "in booknewsngmuonstnuple, name " << name << " title " << title << endl;
  switch( _runtype )
    {
    case RUN9: BookNewSngmuonsNtupleRun9( newsngmuons, name, title ); break; 
    case RUN11: BookNewSngmuonsNtupleRun11( newsngmuons, name, title ); break; 
    case RUN11pp: BookNewSngmuonsNtupleRun11( newsngmuons, name, title ); break; 
    case RUN12: BookNewSngmuonsNtupleRun12( newsngmuons, name, title ); break; 
    default:
      cout << "MWGpico::BookNewSngmuonsNtuple  -> _runtype: " << _runtype << " not implemented, use RUN11!" << endl;
      BookNewSngmuonsNtupleRun12( newsngmuons, name, title ); break; 
    }
  //  cout << " after booking...." << endl;
  return;
}

//__________________________________________________________________
void MWGpico::BookNewSngmuonsEvtNtuple(TNtuple*& newsngvtx, TString name, TString title )
{
  //cout <<  "in booknewsngmuonsEvtNtuple" << endl;
  switch( _runtype )
    {

    case RUN9: BookNewSngmuonsEvtNtupleRun9( newsngvtx, name, title ); break; 
    case RUN11: BookNewSngmuonsEvtNtupleRun11( newsngvtx, name, title ); break;
       case RUN11pp: BookNewSngmuonsEvtNtupleRun11( newsngvtx, name, title ); break; 
    case RUN12: BookNewSngmuonsEvtNtupleRun12( newsngvtx, name, title ); break; 
    default:
      cout << "MWGpico::FillNewSngmuonsNtuple  -> _runtype: " << _runtype << " not implemented, use RUN11!" << endl;
      BookNewSngmuonsEvtNtupleRun12( newsngvtx, name, title ); 
      break;
    }
  return;
}

//_________________________________________________________________
int MWGpico::FillNewSngmuons(PHMuoTracksOut* &muo, TTree*  newsngmuons,  TNtuple* newsngvtx)
{

  //cout <<  "in fillnewsngmuons" << endl;
  //  cout << " in Sngmuons FillSngmuons " << endl;
  switch(_runtype)
    {
    case RUN9:  FillNewSngmuonsNtpRun9(muo,newsngmuons,newsngvtx); break; 
    case RUN11:  FillNewSngmuonsNtpRun11(muo,newsngmuons,newsngvtx); break; 
       case RUN11pp:  FillNewSngmuonsNtpRun11(muo,newsngmuons,newsngvtx); break; 
case RUN12:  FillNewSngmuonsNtpRun12(muo,newsngmuons,newsngvtx); break; 
    default:
      cout << "unrecognized run type:  ---> fill with RUN11 format!" << endl;
      FillNewSngmuonsNtpRun12(muo,newsngmuons,newsngvtx); break; 
    }
  return 0;
}
