#include <MvdRpParams.h>

// database
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <RunToTime.hh>

#include <iostream>
#include <memory> // for auto_ptr

using namespace std;

MvdRpParams *MvdRpParams::__instance=0;

MvdRpParams &MvdRpParams::instance(int run)
{
  if (!__instance)
    {
      __instance = new MvdRpParams;
      __instance->init();
      __instance->fetch(run);
      return *__instance;
    }
  if (__instance->get_runNumber() != run)
    {
      __instance->init();
      __instance->fetch(run);
    }

  return *__instance;
}

MvdRpParams::MvdRpParams()
{
  dcName = "calib.mvd.deadchannel_run";
  rpName = "calib.mvd.rp_run";
}

MvdRpParams::~MvdRpParams()
{
}

void
MvdRpParams::calcDead()
{
	m_nDeadS = 0;
	m_nDeadN = 0;
	for(int iPacket = 0; iPacket<12; iPacket++){
		for(int iHard = 2; iHard<254; iHard++){
			if(m_aDc[iPacket].get_dspare0(iHard) == 1) m_nDeadS++;
			if(m_aDc[iPacket+12].get_dspare0(iHard) == 1) m_nDeadN++;
		}
	}
}

void
MvdRpParams::init()
{
  cout << "MvdRpParams::init Setting all channels as not dead..." << endl;

  // initialize proper geometry for MVD pads
  for(int i=0; i<24; i++){
    PdbMvdDeadchannel *dc = getDeadchannel(i);
    dc->set_installed(1);
    dc->set_packetid(2097 + i);
    dc->set_type(1); // pads
    if(i<12){
      dc->set_phi((17 - i)%12);
    }else{
      dc->set_phi((29 - i)%12);
    }
    dc->set_z(i/12);
    
    for(int j=0; j<PdbMvdDeadchannel::size; j++){
      dc->set_dspare0(0, j);
    }
  }

  m_nDeadS = -1;
  m_nDeadN = -1;
}

void 
MvdRpParams::fetch(int run)
{
  cout << "MvdRpParams::fetch Fetching Dead Channels for run " << run << endl;

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startRead())
    {
      PHMessage("MvdRpParams::", PHError, 
								"Aborting ... Database not readable");
      application->abort();
			init();  // leave state healthy (all channels good)
    }

  //  Make a bank ID
  PdbBankID dcBankID(31);
	PdbBankID rpBankID(3141);

  // Use auto_ptr so deltaBank will be deleted automatically.
  std::auto_ptr<PdbCalBank> dcBank
    (bankManager->fetchBank("PdbMvdDeadchannelBank", 
			      dcBankID, 
			      dcName.c_str(), 
			      run));
  std::auto_ptr<PdbCalBank> rpBank
    (bankManager->fetchBank("PdbMvdRpBank", 
			      rpBankID, 
			      rpName.c_str(), 
			      run));

	// sanity check

  unsigned int nPackets = dcBank->getLength();
	if(nPackets < 24){
		cout << "MvdRpParams: error, only found " << nPackets
				 << " in calib.mvd.deadchannels_test." << endl;
		init();  // leave state healthy (all channels good)
		return;
	}
	if(rpBank->getLength() != 1){
		cout << "MvdRpParams: error, calib.mvd.rp_run should have 1 entry, not "
				 << rpBank->getLength() << "." << endl;
		init();  // leave state healthy (all channels good)
		return;
	}

  cout << "MvdRpParams::READING from Database..." << endl;

	// copy dead channel map for each packet
	for(unsigned int i = 0; i<nPackets; i++){
		PdbMvdDeadchannel dc = (PdbMvdDeadchannel &) dcBank->getEntry(i);
		int iPacket = dc.get_packetid();
		if(2097>iPacket || iPacket>2120) continue;
		setDeadchannel(iPacket - 2097, dc);
	}

	// copy rp parameters
	setRp((PdbMvdRp &) rpBank->getEntry(0));

	m_iRun = run;
}

void 
MvdRpParams::update(int beginrun, int endrun, bool bMap, bool bRp)
{
  RunToTime* runTime = RunToTime::instance();
  PHTimeStamp *ts(runTime->getBeginTime(beginrun));
  PHTimeStamp Tstart = *ts;
  delete ts;
  PHTimeStamp Tstop;

  // The runnumber is encoded into PHTimeStamp.
  if (endrun > 0)
    {
      //ts = runTime->getEndTime(endrun);
      //Tstop = *ts;
			ts = runTime->getBeginTime(endrun + 1);
			Tstop = *ts - 1;
      delete ts;
    }
  else
    {
      Tstop.setToFarFuture();
    }

  //  Make the managers...
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if (!application->startUpdate()){
		PHMessage("MvdRpParams::", PHError, 
							"Aborting ... Database not writable");
		application->abort();
	}
  //  Make a bank ID...
  PdbBankID dcBankID(31);
  PdbBankID rpBankID(3141);
	char dcDesc[256];
	sprintf(dcDesc, "BEN: Dead channel map (spare0) updated with offline "
		"pad selection based on population cut (run %d)", beginrun);
	char rpDesc[256];
	sprintf(rpDesc, "BEN: Q vector component mean/rms updated (run %d)",
					beginrun);

	if(bMap){
		// Grap a pointer to the bank...
		// Use auto_ptr so deltaBank will be deleted automatically
		// when going out of scope (i.e. when leaving this method).
		std::auto_ptr<PdbCalBank> 
			dcBank(bankManager->createBank("PdbMvdDeadchannelBank", 
																		 dcBankID, 
																		 dcDesc, 
																		 Tstart, 
																		 Tstop, 
																		 dcName.c_str()));

		dcBank->setLength(24);

		for(unsigned int i = 0; i<24; i++){
			PdbMvdDeadchannel &dc = (PdbMvdDeadchannel &) dcBank->getEntry(i);
			dc = *(getDeadchannel(i));
		}

		application->commit(dcBank.get());
	}
	if(bRp){
	// Grap a pointer to the bank...
  // Use auto_ptr so deltaBank will be deleted automatically
  // when going out of scope (i.e. when leaving this method).
		std::auto_ptr<PdbCalBank> 
			rpBank(bankManager->createBank("PdbMvdRpBank", 
																		 rpBankID, 
																		 rpDesc, 
																		 Tstart, 
																		 Tstop, 
																		 rpName.c_str()));
		
		rpBank->setLength(1);

		PdbMvdRp &rp = (PdbMvdRp &) rpBank->getEntry(0);
		rp = *(getRp());
		
		application->commit(rpBank.get());
	}

}

void
MvdRpParams::set_channelDead(int iCol, int iRow, int iZ, bool bDead)
{
	int iHard, iWedge = iCol/12;
	int iPacket = 5 - iWedge  + 12*(iZ + iWedge/6);  // packet # - 2097
	m_aDc[iPacket].Soft2Hard(iCol%12, iRow, &iHard);	
	bool bWasDead = (m_aDc[iPacket].get_dspare0(iHard) == 1);
	m_aDc[iPacket].set_dspare0(bDead ? 1 : 0, iHard);

	// update # dead
	if(!bWasDead && bDead){
		if(iZ>0) m_nDeadN++;
		else     m_nDeadS++;
	}else if(bWasDead && !bDead){
		if(iZ>0) m_nDeadN--;
		else     m_nDeadS--;
	}
}

bool
MvdRpParams::get_channelDead(int iCol, int iRow, int iZ)
{
	int iHard, iWedge = iCol/12;
	int iPacket = 5 - iWedge  + 12*(iZ + iWedge/6);  // packet # - 2097
	m_aDc[iPacket].Soft2Hard(iCol%12, iRow, &iHard);	
	return (m_aDc[iPacket].get_dspare0(iHard) == 1);
}
