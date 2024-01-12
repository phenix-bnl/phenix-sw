#include "EmcGenericDeadRecalReco.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "EmcIndexer.h"
#include "emcRejectList.h"
#include "emcDefines.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"

#include "Fun4AllServer.h"
#include "getClass.h"

#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbBankID.hh"
#include "PdbCalBank.hh"
#include "PdbEmcTowerReject.hh"

#include "PHCompositeNode.h"
#include "PHTimeStamp.h"
#include "recoConsts.h"

#include "TH3.h"

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;
static size_t NFEMS = 172;
static size_t NCHANNELS = NFEMS*144;
string
EmcGenericDeadRecalReco::histoName(const int is, const char* suffix)
{
  std::ostringstream name;

  name << Name() << "_" << EmcIndexer::EmcSectorId(is) << suffix;

  return name.str();
}

//_____________________________________________________________________________
EmcGenericDeadRecalReco::EmcGenericDeadRecalReco(const string &name): Recalibrator(name)
{
  baseclasses.insert("emcClusterContainer");
  baseclasses.insert("PHCentralTrack");

  memset(histo, 0, sizeof(histo));
  nevents = 0;
}

int
EmcGenericDeadRecalReco::isValidRun(const int runno) const
{
  recoConsts* rc = recoConsts::instance();

  if ((runno >= 107445 && runno <= 122223)  // Run4 Au+Au
    ||(runno >= 189579 && runno <= 204639 && rc->FlagExist("RUN6DEADRECAL"))  // Run6 p+p
    ||(runno >= 227016 && runno <= 240121)) // Run7 Au+Au
    {
      return 1;
    }
  return 0;
}

//_____________________________________________________________________________
int
EmcGenericDeadRecalReco::Init(PHCompositeNode* topNode)
{
  recoConsts* rc = recoConsts::instance();
  Fun4AllServer* se = Fun4AllServer::instance();

  DataFromFile = 0;
  if ( rc->FlagExist("EMCDEADRECALDATASOURCE") )
    {
      if ( strcmp(rc->get_CharFlag("EMCDEADRECALDATASOURCE"), "DB"))
        {
          std::cout << "EmcGenericDeadRecalReco::EmcGenericDeadRecalReco : will use" << std::endl;
          std::cout << "Ascii file of " << rc->get_CharFlag("EMCDEADRECALDATASOURCE") << " as Dead/Warn Map Recal." << std::endl;
          DataFromFile = 1;
        }
      else
        {
          std::cout << "EmcGenericDeadRecalReco::EmcGenericDeadRecalReco : will read" << std::endl;
          std::cout << "Recal Dead/warn map from DB" << std::endl;
          DataFromFile = 0;
        }
    }
  if (fillhistos)
    {
      for ( size_t is = 0; is < 8; ++is )
        {
          int nx = 72; // for PbSc
          int ny = 36;

          if ( is > 5 )
            {
              nx = 96;
              ny = 48;
            } // for PbGl

          const char* names[] =
            { "_before", "_afterdead" , "_aftercold", "_afterwarm" , "_afterhot",
              "_afterdeadhot", "_afterall"
            };

          for ( int i = 0; i < 7; ++i )
            {

              if (!histo[is][i])
                {
                  Float_t ranges[6] = {0.0, 0.2, 0.5, 1.0, 3.0, 10.0};
                  std::string name = histoName(is, names[i]);
		  if ( topNode ) name += topNode->getName().getString();
                  histo[is][i] = new TH3F(name.c_str(), name.c_str(), nx, -0.5, nx - 0.5, ny, -0.5, ny - 0.5, 6, 0.0, 10.0);
                  histo[is][i]->GetZaxis()->Set(5, ranges);
                  se->registerHisto(histo[is][i]);
                }
            }
        }
    }
  return 0;
}

//_____________________________________________________________________________
int
EmcGenericDeadRecalReco::InitRun(PHCompositeNode* topNode)
{
  recoConsts* rc = recoConsts::instance();

  PHTimeStamp t2(rc->get_TimeStamp());

  Allocate();

  if ( DataFromFile == 1 )
    {
      CollectPhysicsQAFromFile(t2);
    }
  else
    {
      CollectPhysicsQAFromDB(t2);
    }

  ComputeMaps("emcal");
  recalemc = 0;
  recalcnt = 0;
  if (mybaseclass == "emcClusterContainer")
    {
      recalemc = 1;
    }
  else if (mybaseclass == "PHCentralTrack")
    {
      recalcnt = 1;
    }
  return 0;
}

//_____________________________________________________________________________
int
EmcGenericDeadRecalReco::process_event(PHCompositeNode* topNode)
{
  //  if(nevents%200==0) std::cout<<"Event: " << nevents<< std::endl;
  nevents++;

  if (recalcnt)
    {
      // Recalibration for CNT
      PHCentralTrack* phtrack =
        findNode::getClass<PHCentralTrack>(topNode, inputnodename.c_str());

      if (phtrack)
        {
          unsigned int ntracks = phtrack->get_npart();
          for (size_t i = 0;i < ntracks; i++)
            {
              PHSnglCentralTrack *sngltrk = phtrack->get_track(i);

              int emcarm;
              int sector = sngltrk->get_sect();
              if (!sngltrk->isValid(sngltrk->get_sect()))
                {
                  continue;
                }
              int arm = sngltrk->get_dcarm();
              if (arm == 1)
                {
                  emcarm = 0;
                }
              else
                {
                  emcarm = 1;
                }
              unsigned int cdead, cwarn;
              int towerid;

              // For Normal
              if (sngltrk->isImplemented(sngltrk->get_ysect()) && sngltrk->isImplemented(sngltrk->get_zsect()))
                {
                  int ysect = sngltrk->get_ysect();
                  int zsect = sngltrk->get_zsect();
                  towerid = EmcIndexer::TowerID(emcarm, sector, ysect, zsect);

                  cdead = (sngltrk->get_deadmap() & 0x0201ce70) | DeadmapFast(towerid);
                  cwarn = (sngltrk->get_warnmap() & 0x0201ce70) | WarnmapFast(towerid);
                  sngltrk->set_deadmap(cdead);
                  sngltrk->set_warnmap(cwarn);
                }

              // For Swapped
              sngltrk->ShutUp();
              if (sngltrk->isImplemented(sngltrk->get_sysect()) &&
                  sngltrk->isImplemented(sngltrk->get_szsect()) &&
                  sngltrk->isValid(sngltrk->get_sysect()) &&
                  sngltrk->isValid(sngltrk->get_szsect()))
                {
                  int sysect = sngltrk->get_sysect();
                  int szsect = sngltrk->get_szsect();
                  towerid = EmcIndexer::TowerID(emcarm, sector, sysect, szsect);
                  cdead = (sngltrk->get_sdeadmap() & 0x0201ce70) | DeadmapFast(towerid);
                  sngltrk->set_sdeadmap(cdead);
                  cwarn = (sngltrk->get_swarnmap() & 0x0201ce70) | WarnmapFast(towerid);
                  sngltrk->set_swarnmap(cwarn);
                }
              sngltrk->ShutUp(0);

            }
        }
    }
  if (recalemc)
    {
      emcClusterContainer* clusters =
        findNode::getClass<emcClusterContainer>(topNode, inputnodename.c_str());


      if (!clusters)
        {
          //std::cerr << PHWHERE << " No emcClusterContainer object !" << std::endl;
          return 0;
        }

      //  const unsigned int cut3x3map[] = { 0x0201ce70, 0x1ce70 }; // PbSc then PbGl
      static const unsigned int cut3x3map[] =
        {
          0x0001ce70, 0x1ce70
        }
      ; // PbSc then PbGl
      static const unsigned int cut3x3mapwarm[] =
        {
          0x7de00000, 0x7de00000
        }
      ; // PbSc then PbGl

      for ( size_t i = 0; i < clusters->size(); ++i )
        {
          emcClusterContent* clu = clusters->getCluster(i);
          if (clu == 0)
            {
              cout << PHWHERE << " SEVERE ERROR: Got NULL pointer for cluster, contact the emc group with this"
		   << endl;
              exit(1);
            }

          int towerid = clu->towerid(0);

          unsigned int cdead = (clu->deadmap() & 0x0201ce70) | DeadmapFast(towerid);
          unsigned int cwarn = (clu->warnmap() & 0x0201ce70) | WarnmapFast(towerid);
          //      unsigned int cwarn = (clu->warnmap()&0x02000000) | WarnmapFast(towerid);

          //
          // If additional warnmap does not indicate whether it is hot or not for
          //  the middle tower, mark it as hot!  It is the safest way.
          //
          //      if((clu->warnmap()&0x0400 != 0)&&(WarnmapFast(towerid)&0x0400 == 0)){
          //         cwarn |= (1 << 30);
          //      }

          unsigned int cutmap = 0;
          unsigned int cutmapwarm = 0;

          clu->ShutUp();
          if (clu->isValid(clu->type()))
            {
              cutmap = cut3x3map[clu->type() - 1];
              cutmapwarm = cut3x3mapwarm[clu->type() - 1];
            }
          clu->ShutUp(0);

          int is, iz, iy;

          EmcIndexer::decodeTowerId(towerid, is, iz, iy);
          //      std::cout << "is: " << is << ", iz: " << iz << ", iy: " << iy << std::endl;

          if (fillhistos)
            {
              if ( (clu->deadmap() & cutmap) == 0 &&
                   (clu->warnmap() & cutmap) == 0 )
                {
                  histo[is][0]->Fill(iz, iy, clu->e());
                }
            }
          /*
            if ( verbosity>0 && 
            ( cdead != clu->deadmap() || cwarn != clu->warnmap() ) ) {  
            std::ostream::fmtflags oldflags = std::cout.flags();
            std::cout << PHWHERE << " Tower " << towerid << " has been updated "
            << "from (dead,warn)=(" << std::hex
            << clu->deadmap() << "," << std::hex
            << clu->warnmap() << ") to (" << std::hex
            << cdead << "," << std::hex
            << cwarn << ")." 
            << std::endl;
            std::cout.setf(oldflags);
            }
             
          */
          clu->set_maps(cdead, cwarn);

          /*
            int flag1 =0;
            int flag2 =0;
            int flag3 =0;
            int flag4 =0;
          */

          if (fillhistos)
            {
              if ( (clu->deadmap() & cutmap) == 0 )
                {
                  //                        std::cout << "came 1" << std::endl;
                  histo[is][1]->Fill(iz, iy, clu->e());
                  //              flag1=1;
                }
              if ( (clu->deadmap() & cutmapwarm) == 0 )
                {
                  //                        std::cout << "came 2" << std::endl;
                  histo[is][2]->Fill(iz, iy, clu->e());
                  //              flag2=1;
                }
              if ( (clu->warnmap() & cutmapwarm) == 0 )
                {
                  //                        std::cout << "came 3" << std::endl;

                  histo[is][3]->Fill(iz, iy, clu->e());
                  //              flag3=1;
                }
              if ( (clu->warnmap() & cutmap) == 0 )
                {
                  //                        std::cout << "came 4" << std::endl;
                  histo[is][4]->Fill(iz, iy, clu->e());
                  //              flag4=1;
                }
              if ( (clu->warnmap() & cutmap) == 0 &&
                   (clu->deadmap() & cutmap) == 0 )
                {
                  //                        std::cout << "came 5" << std::endl;
                  histo[is][5]->Fill(iz, iy, clu->e());
                  //              if(!(flag1==1&&flag4==1)) std::cout<<"ERROR::5!!!" << std::endl;
                  //              else std::cout<<"5 is OK!" << std::endl;
                }
              if ( (clu->warnmap() & cutmap) == 0 &&
                   (clu->deadmap() & cutmap) == 0 &&
                   (clu->warnmap() & cutmapwarm) == 0 &&
                   (clu->deadmap() & cutmapwarm) == 0 )
                {
                  //                        std::cout << "came 6" << std::endl;
                  histo[is][6]->Fill(iz, iy, clu->e());
                  //              if(!(flag1==1&&flag4==1)) std::cout<<"ERROR::6.1!!!" << std::endl;
                  //              if(!(flag2==1&&flag3==1)) std::cout<<"ERROR::6.2!!!" << std::endl;
                  //              else std::cout<<"6 is OK!" << std::endl;
                }
            }
        }
    }

  return 0;
}

//_____________________________________________________________________________
void
EmcGenericDeadRecalReco::Allocate(void)
{
  fErrorRaw.resize(NCHANNELS, 0);
  fWarnRaw.resize(NCHANNELS, 0);

  fErrorRaw.resize(NCHANNELS, 0);
  fWarnRaw.resize(NCHANNELS, 0);

  fErrorMap.resize(fErrorRaw.size() , 0 );
  fWarnMap.resize(fErrorRaw.size() , 0 );

  fCollectedFEMs.resize(NFEMS, false);
  fComputedFEMs.resize(NFEMS, false);
}

//_____________________________________________________________________________
std::string
EmcGenericDeadRecalReco::CollectPhysicsQAFromDB(PHTimeStamp time_stamp)
{
  recoConsts* rc = recoConsts::instance();

  emcRejectList rl;

  rl.Reset();

  int version_all = rc->get_IntFlag("EMCDEADMAPVER",1); // If the variable is not set, default will be 1

  int version_pbsc = rc->get_IntFlag("PBSCDEADMAPVER",version_all); // If the variable is not set, default will be version_all
  int version_pbgl = rc->get_IntFlag("PBGLDEADMAPVER",version_all); // If the variable is not set, default will be version_all

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (application->startRead())
    {
      PdbBankID bankID;

      //
      // First, PbSc part
      //
      std::cout << "EMC GenericDeadRecal PbSc::Read" << std::endl;

      bankID.setInternalValue(version_pbsc);

      PdbCalBank* emcBank1 = bankManager->fetchBank("PdbEmcTowerRejectBank", bankID, "calib.emc.rejectlist.recal", time_stamp);

      if (!emcBank1)
        {
          std::cerr << "EMC GenericDeadRecal Error::Read : "
		    << "Cannot fetch reject list bank for PbSc "
		    << "at time " << time_stamp << std::endl;
          return "";
        }

      emcBank1->printHeader();

      for ( size_t i = 0; i < emcBank1->getLength(); ++i )
        {
          PdbEmcTowerReject *tr = (PdbEmcTowerReject *) & (emcBank1->getEntry(i));
          rl.set_or( tr->TowerId(), tr->AmplitudeError(), tr->AmplitudeWarning(),
                     tr->TimingError(), tr->TimingWarning());

          if (verbosity > 0)
            {
              std::cout << "TowerID: " << tr->TowerId();
              std::cout << ", AmpError: " << tr->AmplitudeError();
              std::cout << ", AmpWarn: " << tr->AmplitudeWarning();
              std::cout << ", TofError: " << tr->TimingError();
              std::cout << ", TofWarn: " << tr->TimingWarning() << std::endl;
            }
        }

      delete emcBank1;

      for ( size_t towerid = 0; towerid < rl.maxsize(); ++towerid )
        {
          if(towerid>=15552) continue;
          fErrorRaw[towerid] = rl.Error(towerid);
          fWarnRaw[towerid] = rl.Warning(towerid);

          /*
	    if(fErrorRaw[towerid]) std::cout << "TowerID: " << towerid << ", ErrRaw: " << fErrorRaw[towerid]<< std::endl;
	    if(fWarnRaw[towerid]) std::cout << "TowerID: " << towerid << ", WarnRaw: " << fWarnRaw[towerid]<< std::endl;
          */

        }

      //
      // Next, PbGl part
      //
      std::cout << "EMC GenericDeadRecal PbGl::Read" << std::endl;

      bankID.setInternalValue(version_pbgl);

      PdbCalBank* emcBank2 = bankManager->fetchBank("PdbEmcTowerRejectBank", bankID, "calib.emc.rejectlist.recal", time_stamp);

      if (!emcBank2)
        {
          std::cerr << "EMC GenericDeadRecal Error::Read : "
		    << "Cannot fetch reject list bank for PbGl "
		    << "at time " << time_stamp << std::endl;
          return "";
        }

      emcBank2->printHeader();

      for ( size_t i = 0; i < emcBank2->getLength(); ++i )
        {
          PdbEmcTowerReject *tr = (PdbEmcTowerReject *) & (emcBank2->getEntry(i));
          rl.set_or( tr->TowerId(), tr->AmplitudeError(), tr->AmplitudeWarning(),
                     tr->TimingError(), tr->TimingWarning());

          if (verbosity > 0)
            {
              std::cout << "TowerID: " << tr->TowerId();
              std::cout << ", AmpError: " << tr->AmplitudeError();
              std::cout << ", AmpWarn: " << tr->AmplitudeWarning();
              std::cout << ", TofError: " << tr->TimingError();
              std::cout << ", TofWarn: " << tr->TimingWarning() << std::endl;
            }
        }

      delete emcBank2;

      for ( size_t towerid = 0; towerid < rl.maxsize(); ++towerid )
        {
          if(towerid<15552) continue;
          fErrorRaw[towerid] = rl.Error(towerid);
          fWarnRaw[towerid] = rl.Warning(towerid);

          /*
	    if(fErrorRaw[towerid]) std::cout << "TowerID: " << towerid << ", ErrRaw: " << fErrorRaw[towerid]<< std::endl;
	    if(fWarnRaw[towerid]) std::cout << "TowerID: " << towerid << ", WarnRaw: " << fWarnRaw[towerid]<< std::endl;
          */

        }
      return "";
    }
  else
    return "NG";
}

//_____________________________________________________________________________
std::string
EmcGenericDeadRecalReco::CollectPhysicsQAFromFile(PHTimeStamp time_stamp)
{
  recoConsts *rc = recoConsts::instance();

  unsigned int err, warn;
  unsigned int towerid;

  std::ifstream fin(rc->get_CharFlag("EMCDEADRECALDATASOURCE"));
  while (fin >> towerid >> err >> warn)
    {
      fErrorRaw[towerid] = err;
      fWarnRaw[towerid] = warn;
    }
  fin.close();

  return "";
}


//_____________________________________________________________________________
void
EmcGenericDeadRecalReco::ComputeMaps(const char* sectors)
{
  for (size_t i = 0;i < NFEMS;i++)
    ComputeMaps(i);
}

//_____________________________________________________________________________
void
EmcGenericDeadRecalReco::ComputeMaps(size_t ifem)
{
  // Compute the neighbour error and warning flags for each tower
  // in the fem ifem.
  //
  // For amplitude bits are:
  // ---------------------
  // |   | 18| 19| 20|   |
  // ---------------------
  // | 13| 14| 15| 16| 17|
  // ---------------------  ^ y
  // | 8 | 9 | 10| 11| 12|  |
  // ---------------------  |
  // | 3 | 4 | 5 | 6 | 7 |  |
  // ---------------------  ------> z(x)
  // |   | 0 | 1 | 2 |   |
  // ---------------------
  // as viewed from the back of the central tower (which has bit 10 set
  // to 1 if it's itself a bad module); corner towers are excluded
  //
  // For ToF bits are :
  // -------------
  // | 27| 28| 29|  ^ y
  // -------------  |
  // | 24| 25| 26|  |
  // -------------  |
  // | 21| 22| 23|  ------> z(x)
  // -------------
  // as viewed from the back of the central tower (which has bit 25 set
  // to 1 if it's itself a bad module)
  //
  // So, a channel has a problem with amplitude measurements if its neighbor
  // error bit map  satisfies this mask:
  //            0x400
  // Actually, this mask is returned by the IamDeadMask() method
  // so that the amplitude for bad modules can be set to 0 at the calibration
  // stage.
  //
  // Some other useful masks.
  // The mask to look for amplitude errors or warnings in the 3x3 region
  // around a tower is:
  //          0x1ce70
  // In the 5x5 region:
  //         0x1fffff
  // To see if there are ToF problems for this tower:
  //        0x2000000
  //
  //
  //   From Here, added by T. Sakaguchi on 04/20/2005
  //     The 30th bit shows if the middle tower is is warm or hot tower
  //              in terms of charge
  //
  //    warm(high frequency in 1-2GeV) - 0, hot(high frequency in >2GeV) - 1
  //
  //    In WarnRaw Map, hot is implemented as 0x08.
  //       (This is just for the case of recalibration database)
  //
  // ----------------------------------
  // Now, ToF bits are utilized as warm map definition:
  // -------------
  // | 27| 28| 29|  ^ y
  // -------------  |
  // | 24| 30| 26|  |
  // -------------  |
  // | 21| 22| 23|  ------> z(x)
  // -------------
  // as viewed from the back of the central tower (which has bit 25 set
  // to 1 if it's itself a bad module)
  //

  if ( fComputedFEMs[ifem] )
    return ;

  //  std::cout << "Computing!" << std::endl;

  // Mind your steps. In the loop below, only *Fast methods are allowed
  // as the non-Fast method use update() which in turn use this ComputeMaps
  // method (i.e. you would end up in infinite recursion trying to use
  // non-Fast methods here).

  for ( size_t i = 0; i < 144; i++ )
    {
      int towerID = EmcIndexer::PXSM144iCH_iPX(ifem, i);
      int sector, x, y;
      EmcIndexer::decodeTowerId(towerID, sector, x, y);

      unsigned int neighbourError = 0;
      unsigned int neighbourWarn = 0;
      unsigned int bitOffset = 0;
      unsigned int bitOffsetWarm = 21;

      //
      // Amplitude tests first (in a 5x5 area).
      //

      for ( int yoff = -2; yoff <= 2; yoff++ )
        {
          for ( int xoff = -2; xoff <= 2; xoff++ )
            {
              //=====> check if this is a corner tower
              bool corner =
                (xoff == -2 && yoff == -2) ||
                (xoff == -2 && yoff == 2) ||
                (xoff == 2 && yoff == -2) ||
                (xoff == 2 && yoff == 2);

              if (corner)
                continue;

              if (!EmcIndexer::IsValid(sector, x + xoff, y + yoff))
                {
                  // physical boundaries (edges)
                  neighbourError |= (1 << bitOffset);
                  neighbourWarn |= (1 << bitOffset);
                  if (xoff >= -1 && xoff <= 1 && yoff >= -1 && yoff <= 1)
                    {
                      if (xoff == 0 && yoff == 0)
                        neighbourWarn |= ( 1 << 30 );
                      else
                        neighbourWarn |= ( 1 << bitOffsetWarm );

                      if (xoff == 0 && yoff == 0)
                        neighbourError |= ( 1 << 30 );
                      else
                        neighbourError |= ( 1 << bitOffsetWarm );

                      bitOffsetWarm++;
                    }
                }
              else
                {
                  int neighbourTower =
                    EmcIndexer::getTowerId(sector, x + xoff, y + yoff);

                  // Errors (Dead)
                  if ( ErrorFast(neighbourTower) & fMASK_Ampl_Hot_Physics )
                    {
                      neighbourError |= ( 1 << bitOffset );
                    }

                  // Warnings (Hot)
                  if ( WarningFast(neighbourTower) & fMASK_Ampl_Hot_Physics )
                    {
                      neighbourWarn |= ( 1 << bitOffset );
                    }

                  if (xoff >= -1 && xoff <= 1 && yoff >= -1 && yoff <= 1)
                    {

                      // Errors (Cold)
                      if ( ErrorFast(neighbourTower) & fMASK_Ampl_Warm_Physics )
                        {
                          if (xoff == 0 && yoff == 0)
                            neighbourError |= ( 1 << 30 );
                          else
                            neighbourError |= ( 1 << bitOffsetWarm );
                        }

                      // Warnings (Warm)
                      if ( WarningFast(neighbourTower) & fMASK_Ampl_Warm_Physics )
                        {
                          if (xoff == 0 && yoff == 0)
                            neighbourWarn |= ( 1 << 30 );
                          else
                            neighbourWarn |= ( 1 << bitOffsetWarm );
                        }
                      bitOffsetWarm++;
                    }
                }
              bitOffset++;
            }
        } // end of ampl tests.

      //
      // timing tests then. (previously in a 3x3 area. Now, just the given one).
      //
      bitOffset++;
      if (!EmcIndexer::IsValid(sector, x, y))
        {
          // physical boundaries (edges)
          neighbourError |= (1 << 25);
          neighbourWarn |= (1 << 25);
        }
      else
        {
          int neighbourTower = EmcIndexer::getTowerId(sector, x, y);

          // Errors
          if ( ErrorFast(neighbourTower) & fMASK_TOF_Physics )
            {
              neighbourError |= ( 1 << 25 );
            }

          // Warnings
          if ( WarningFast(neighbourTower) & fMASK_TOF_Physics )
            {
              neighbourWarn |= ( 1 << 25 );
            }
        } // end of timing tests.

      fErrorMap[towerID] = neighbourError;
      fWarnMap[towerID] = neighbourWarn;

      //      if(neighbourWarn) std::cout <<"towerID: "<< towerID <<", Warn: " << neighbourWarn << std::endl;

    } // end of loop over towers.

  fComputedFEMs[ifem] = true;
}

//_____________________________________________________________________________
unsigned int
EmcGenericDeadRecalReco::ErrorFast(int towerID) const
{
  return fErrorRaw[towerID];
}

//_____________________________________________________________________________
unsigned int
EmcGenericDeadRecalReco::DeadmapFast(int towerID) const
{
  return fErrorMap[towerID];
}

//_____________________________________________________________________________
unsigned int
EmcGenericDeadRecalReco::WarningFast(int towerID) const
{
  return fWarnRaw[towerID];
}

//_____________________________________________________________________________
unsigned int
EmcGenericDeadRecalReco::WarnmapFast(int towerID) const
{
  return fWarnMap[towerID];
}

//_____________________________________________________________________________
ostream&
EmcGenericDeadRecalReco::print(int towerid, ostream& out) const
{
  std::string head = " TOWID FEM  CH SEC  Z  Y   ERRonl  WARNonl  ERRphys  WARNphys  ERRMAP  WARNMAP";

  size_t rc = 20;
  size_t count = rc;

  if ( towerid == -1 )
    {
      for ( size_t i = 0; i < fErrorMap.size(); i++ )
        {
          if ( count == rc )
            {
              out << head << std::endl;
              count = 0;
            }
          PrintOne(i, out);
          ++count;
        }
    }
  else
    {
      out << head << std::endl;
      PrintOne(towerid, out);
    }

  return out;
}

//_____________________________________________________________________________
ostream&
EmcGenericDeadRecalReco::PrintOne(int towerID, ostream& out) const
{
  ostream::fmtflags oldflags = out.flags();

  int femAbsolutePosition, femChannel;
  int sector, z, y;

  EmcIndexer::PXPXSM144CH(towerID, femAbsolutePosition, femChannel);

  EmcIndexer::decodeTowerId(towerID, sector, z, y );

  // We put those into local variables, just in case
  // some data collection occurs in the methods in the r.h.s. of
  // the expressions below (because in collection some messages can
  // be output on the screen and would screw up this PrintOne method output).
  int errPhy = fErrorRaw[towerID];
  int warnPhy = fWarnRaw[towerID];
  unsigned int dead = DeadmapFast(towerID);
  unsigned int warn = WarnmapFast(towerID);

  out << std::setw(6) << towerID << " "
  << std::setw(3) << femAbsolutePosition << " "
  << std::setw(3) << femChannel << " "
  << std::setw(3) << sector << " "
  << std::setw(2) << z << " "
  << std::setw(2) << y << " ";

  out.setf(ostream::showbase);
  out.setf(ostream::hex, ostream::basefield);

  out << std::setw(8) << errPhy << " "
  << std::setw(8) << warnPhy << " "
  << std::setw(8) << dead << " "
  << std::setw(8) << warn << std::endl;

  out.setf(oldflags);

  return out;
}
