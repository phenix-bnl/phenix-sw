#include <EmcRejectAfterBurner.h>
#include <RunHeader.h>
#include <PHEmcConstants.h>
#include <EmcClusterLocalExt.h>
#include <PHIODataNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbBankID.hh>
#include <PdbCalBank.hh>
#include <PdbADCChan.hh>

#include <cassert>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>

// These typedefs should cover most user's needs...
typedef PHIODataNode<RunHeader> RunHeaderNode_t;
typedef PHIODataNode<EmcClusterLocalExt> EmcClusterLocalExtNode_t;

ClassImp(EmcRejectAfterBurner);

using namespace std;
  
EmcRejectAfterBurner::EmcRejectAfterBurner()
{
  rejectdir = "/afs/rhic.bnl.gov/phenix/users/jfrantz/reject";
  EmcNumReject = 0;
  Clear();
}

int EmcRejectAfterBurner::isValid() const
{
  if (EmcNumReject != 0) return 1;
  return 0;
}

void EmcRejectAfterBurner::identify (ostream &os) const
{
  os << "identify yourself: EmcRejectAfterBurner Object" << endl;
}

void EmcRejectAfterBurner::Clear(Option_t *) {
  EmcNumReject = 0;
  for (int isect=0; isect<8; isect++)
    for (int iz=0; iz<96; iz++)
      for (int iy=0; iy<48; iy++)
        RejectList[isect][iz][iy] = 0;
}

void EmcRejectAfterBurner::set_rejectdir(const char *dir)
{
  rejectdir = dir;
}

int EmcRejectAfterBurner::set_run(const int run)
{
  TString rejectfile = rejectdir;
  rejectfile += "/";
  rejectfile += run;
  rejectfile += ".rej";

  return set_rejectfile(rejectfile.Data());
}

int EmcRejectAfterBurner::set_rejectfile(const char *f)
{
  Clear();

  ifstream rejfile(f);
  if (!rejfile.is_open())
    {
      cout << "EmcRejectAfterBurner::set_RejectFile, error opening file "
           << f << endl;
      return -1;
    }
  else
    {
      cout << "EmcRejectAfterBurner:: opened file " << f << endl;
    }

  int s, z, y, a, b, c, d;
  char buffer[1000];
  while ( !rejfile.eof() )
    {
      rejfile.getline(buffer,1000);
      if ( buffer[0] == '#' ) continue;
      if ( buffer[0] == '/' ) continue;
      if ( !isdigit(buffer[0]) ) continue;
      istringstream tempbuf;
      tempbuf.str(buffer);
      tempbuf >> s >> z >> y >> a >> b >> c >> d;
      RejectList[s][z][y] = 1;
      EmcNumReject++;
    }

  return EmcNumReject;
}

void EmcRejectAfterBurner::dump() const
{
  if (EmcNumReject == 0)
    {
      cout << "EmcRejectAfterBurner: no bad towers" << endl;
      return;
    }

  int nprinted = 0;
  cout << "EmcRejectAfterBurner bad towers:" << endl;
  for (int isect=0; isect<8; isect++)
    for (int iz=0; iz<96; iz++)
      for (int iy=0; iy<48; iy++)
        {
          if ( RejectList[isect][iz][iy] == 0 ) continue;
          cout << isect << "\t" << iz << "\t" << iy << endl;
          nprinted++;
          if ( nprinted%8 == 0 ) cout << endl;
        }
  if ( nprinted%8 != 0 ) cout << endl;
}

void EmcRejectAfterBurner::initialize(PHCompositeNode *topNode)
{
  // First find the run header and get the run number...
  RunHeader *d_runhdr;
  PHTypedNodeIterator<RunHeader> iRUN(topNode);
  RunHeaderNode_t *RUN = iRUN.find("RunHeader");
  if (!RUN)
    {
      cout << PHWHERE << "no RUN Header Node, canceling corrections" << endl;
      return;
    }
  d_runhdr = RUN->getData();
  if ( !d_runhdr )
    {
      cout << PHWHERE << "no RUN Header Object, canceling corrections" << endl;
      return;
    }

  int RunNumber = d_runhdr->get_RunNumber();
  cout << "EmcAfterBurner::Calibration adjustments for run #" << RunNumber << endl;

//  set_rejectfile("/afs/rhic/phenix/users/jfrantz/reject/reject.rej");
//  //set_run( RunNumber );
  
  // Read list of hot towers from the database.  SL 5-22-2002
     PHBoolean status = FetchFromDatabase(RunNumber);
       if(!status) {
         cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
         cout << PHWHERE << "ERROR: Failed to fetch list of hot emcal towers from the database." << endl;
         cout << PHWHERE << "       Afterburning terminated." << endl;
	 exit(1);
       }
  
}

void EmcRejectAfterBurner::apply(PHCompositeNode *udstNode)
{
  EmcClusterLocalExt *d_emc = 0;
  PHTypedNodeIterator<EmcClusterLocalExt> iEMC(udstNode);
  EmcClusterLocalExtNode_t *EMC = iEMC.find("EmcClusterLocalExt");
  if (!EMC)
    {
      cout << PHWHERE << "no Emc Track in udst" << endl;
      return;
    }
  d_emc = EMC->getData();
  if (!d_emc)
    {
      cout << PHWHERE << "EmcClusterLocalExt object not found" << endl;
      return;
    }

  //-****************************************
  //-* Loop over all the EMC hits and update the deadmap
  //-****************************************
  static const int sw2hwsector[] = { 0, 1, 2, 3, 6, 7, 4, 5 };
  for (unsigned int iclus=0; iclus<d_emc->get_EmcNCluster(); iclus++)
    {
      unsigned int hwsector = sw2hwsector[d_emc->get_arm(iclus)*4 + d_emc->get_sector(iclus)];
      unsigned int zpos = d_emc->get_ind(iclus,0);
      unsigned int ypos = d_emc->get_ind(iclus,1);
      assert( hwsector<8 && zpos<96 && ypos<48 );

      unsigned int deadmap = 0;
      int ztemp, ytemp;

      ytemp = ypos - 2;
      if ( ytemp>=0 )
        {
          for (int iz=0; iz<3; iz++)
            {
              ztemp = zpos + iz - 1;
              if ( ztemp>=0 && ztemp<EmcZtowers[hwsector] &&
                   (RejectList[hwsector][ztemp][ytemp] != 0) )
                {
                  unsigned int map = 1 << iz;
                  deadmap |= map;
                }
            }
        }
      for (int iy=0; iy<3; iy++)
        {
          ytemp = ypos + iy - 1;
          if ( ytemp>=0 && ytemp<EmcYtowers[hwsector] )
            {
              for (int iz=0; iz<5; iz++)
                {
                  ztemp = zpos + iz - 2;
                  if ( ztemp>=0 && ztemp<EmcZtowers[hwsector] &&
                       (RejectList[hwsector][ztemp][ytemp] != 0) )
                    {
                      unsigned int map = 1 << (iy*5 + iz + 3);
                      deadmap |= map;
                    }
                }
            }
        }
     ytemp = ypos + 2;
     if ( ytemp>=0 && ytemp<EmcYtowers[hwsector] )
        {
          for (int iz=0; iz<3; iz++)
            {
              ztemp = zpos + iz - 1;
              if ( ztemp>=0 && ztemp<EmcZtowers[hwsector] &&
                   (RejectList[hwsector][ztemp][ytemp] != 0) )
                {
                  unsigned int map = 1 << (iz + 18);
                  deadmap |= map;
                }
            }
        }

          //
     // we update the DEADMAP for PbGl 
     //
     if (hwsector > 5)
       {
	 deadmap |= d_emc->get_deadmap(iclus);
	 d_emc->set_deadmap(iclus, deadmap);
       }
     else 
       {
	 //and clear and reset the warnmap for PbSc
	 d_emc->set_warnmap(iclus,0);
	 d_emc->set_warnmap(iclus, deadmap);	      
       }
    }
  
}


// SL 5-22-2002
PHBoolean
EmcRejectAfterBurner::FetchFromDatabase(int RunNumber) 
{

  PdbADCChan* achan = 0;
  const char* calibname = "afterburner.emc.hotlist";

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);

  if(application->startRead()) {

    PdbCalBank *Bank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, RunNumber);

    if(Bank) {
      int banklength = Bank->getLength();
      for(int i=0; i<banklength; i++) {
        achan = (PdbADCChan*)&(Bank->getEntry(i));
        int tmp1 = (int)achan->getParameter(0);
        int tmp2 = (int)achan->getParameter(1);
        int tmp3 = (int)achan->getParameter(2);
	RejectList[tmp1][tmp2][tmp3]=1;
	EmcNumReject++;
      }
      delete Bank;
    }
    else {
      cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
      cout << PHWHERE << "ERROR: Can not get list of hot emcal towers from the database." << endl;
      return False;
    }

    application->commit();

  }
  else {
    cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
    cout << PHWHERE << "ERROR: Can not access the database." << endl;
    return False;
  }

  return True;
}

// SL 5-22-2002
PHBoolean EmcRejectAfterBurner::FetchFromDatabase(PHTimeStamp* Tsearch) {

  PdbADCChan* achan = 0;
  const char* calibname = "afterburner.emc.hotlist";

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);
  PHTimeStamp tSearch = *Tsearch;

  if(application->startRead()) {

    PdbCalBank *Bank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);

    if(Bank) {
      int banklength = Bank->getLength();
      for(int i=0; i<banklength; i++) {
        achan = (PdbADCChan*)&(Bank->getEntry(i));
        int tmp1 = (int)achan->getParameter(0);
        int tmp2 = (int)achan->getParameter(1);
        int tmp3 = (int)achan->getParameter(2);
	RejectList[tmp1][tmp2][tmp3]=1;
	EmcNumReject++;
      }

    }
    else {
      cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
      cout << PHWHERE << "ERROR: Can not get list of hot emcal towers from the database." << endl;
      return False;
    }

      application->commit();

  }
  else {
    cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
    cout << PHWHERE << "ERROR: Can not access the database." << endl;
    return False;
  }

  return True;
}

// SL 5-22-2002
PHBoolean EmcRejectAfterBurner::UpdateDatabase(PHTimeStamp* Tbeg, PHTimeStamp* Tend) {

// make a list of hot towers
  float sect[EmcNumReject], zpos[EmcNumReject], ypos[EmcNumReject];
  int nnn=0;
  for(int i=0; i<8; i++) {
    for(int j=0; j<96; j++) {
      for(int k=0; k<48; k++) {
        if(RejectList[i][j][k]!=0) {
	  sect[nnn]=(float)i;
	  zpos[nnn]=(float)j;
	  ypos[nnn]=(float)k;
	  nnn++;
	}
      }
    }
  }
  
  PdbADCChan *achan = 0;
  const char* calibname = "afterburner.emc.hotlist";
  const char* description = "list of hot emcal towers for afterburner";

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHTimeStamp tStart = *Tbeg;
  PHTimeStamp tStop  = *Tend;
  PdbBankID bankID(0);

    if(application->startUpdate()) {

      PdbCalBank *Bank = bankManager->createBank("PdbADCChanBank",
                                                  bankID,
                                                  description,
                                                  tStart, tStop,
                                                  calibname);

      Bank->setLength(EmcNumReject);
      for(int i=0; i<EmcNumReject; i++) {
	float tmp1 = sect[i];
	float tmp2 = zpos[i];
	float tmp3 = ypos[i];
	achan = (PdbADCChan*)&(Bank->getEntry(i));
        achan->setParameter(0, tmp1);
        achan->setParameter(1, tmp2);
        achan->setParameter(2, tmp3);
      }
      application->commit();
    }
    else {
      cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
      cout << PHWHERE << "ERROR: Failed to update list of hot emcal towers in the database." << endl;
      return False;
    }

  return True;
}

