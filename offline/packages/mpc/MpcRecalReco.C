#include <iostream>
#include <fstream>

#include <MpcRecalReco.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <getClass.h>
#include <MpcMap.h>
//#include <recoConsts.h>

#include <iostream>

using namespace std;

MpcRecalReco::MpcRecalReco(const std::string &name): SubsysReco(name)
{
  fVerbose = 1;
}

MpcRecalReco::~MpcRecalReco()
{
}

int MpcRecalReco::Init(PHCompositeNode *topNode)
{
  InitCorrections();

  // Read in Recal Gain Coefficients
  if ( correction_file.size()==0 )
    {
      correction_file = "MpcCal.recal_gains";
    }
 
  if ( fVerbose )
    {
      cout << PHWHERE << " Reading " << correction_file << endl;
    }
 
  ifstream infile(correction_file.c_str());

  if ( infile.good() )
    {
      Float_t mean, deviation;
      Int_t   ifeech, status;
      while ( infile >> ifeech >> mean >> deviation >> status )
        {
          if ( ifeech<0 || ifeech>575 )
            {
              continue;
            }
    
          correction[ifeech] = mean;
        }
    
      infile.close();
    }
  else
    {
      cout << PHWHERE << " ERROR, failed to open correction_file" << endl;
    }

  mpcmap = MpcMap::instance();

  return 0;
}

int MpcRecalReco::EndRun(const int runnumber)
{
  return 0;
}

int MpcRecalReco::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

int MpcRecalReco::process_event(PHCompositeNode *topNode)
{
  mpcTowerContainer *mpctow = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if (!mpctow)
    {
      cout << PHWHERE << "Unable to get mpcTowerContainer, is Node missing?" << endl;
      return False;
    }

  // Correct the tower gains by the new values
  unsigned int ntow = mpctow->size();
  for (unsigned int itow=0; itow<ntow; itow++)
    {
      mpcTowerContent *tower = mpctow->getTower(itow);

      Int_t tow_ch = tower->get_ch();               // tower channel (FEM numbering)
      int gridx = mpcmap->getGridX( tow_ch );
      //int gridy = mpcmap->getGridY( tow_ch );

      if ( gridx == -1 )
        {
          cout << "ERROR, Non-existent channel " << tow_ch << endl;
          continue;
        }

      Float_t e = tower->get_energy();
      Float_t new_e = e*correction[tow_ch];
      tower->set_energy( new_e );
    }

  return 0;
}

void MpcRecalReco::InitCorrections()
{
  for (int ich=0; ich<576; ich++)
    {
      correction[ich] = 0.;
    }
}

void MpcRecalReco::Print(const std::string&) const
{
  cout << "MpcRecalReco::Print(), Corrections:" << endl;
  for (int ich=0; ich<576; ich++)
    {
      if ( correction[ich]==0. ) continue;
      cout << ich << "\t" << correction[ich] << endl;
    }
}

