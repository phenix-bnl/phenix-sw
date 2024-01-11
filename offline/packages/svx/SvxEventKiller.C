#include "SvxEventKiller.h"

#include <EventHeader.h>
#include <EventTypes.h>
#include <TrigLvl1.h>

#include <recoConsts.h>
#include <RunNumberRanges.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllHistoManager.h>
#include <getClass.h>
#include <svxAddress.hh>

#include <SvxDaqErrorMap.h>

#include <PHCompositeNode.h>
#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbBankID.hh>
#include <PdbInt.hh>
#include <PdbSvxBias.hh>
#include <RunToTime.hh>


#include <PHString.h>
#include <TH1.h>

#include <cstdlib>
#include <iostream>
#include <memory>
#include <string>
#include <sstream>

using namespace std;

SvxEventKiller::SvxEventKiller(const string &name) :
  SubsysReco(name),
  _mode(7),
  _runnumber(0),
  _strip_last_good_event(-1),
  _RetCode(ABORTEVENT),
  _calibration_status(0),
  _nGood(0),
  _nConsider(0),
  _verbose(0),
  m_activeladderDB(true),
  _event_status(0),
  h1nevent(0x0),
  h1packet(0x0),
  h1packet_mask(0x0),
  h1strip_last_good_event(0x0),
  h1biastime(0x0),
  m_addr(0x0),
  m_daqerror(0x0)
{
  h1status[0] = 0;
  h1status[1] = 0;
}

int SvxEventKiller::SetLadderAllActive()
{
  ostringstream buffer;
  const string arm_label[2]={"S","N"};
  const int nladder[4]={10,20,16,24};
  for(int barrel=0; barrel<2; barrel++)
    {
      for(int ladder=0; ladder<nladder[barrel]; ladder++)
	{
	  for(int arm=0; arm<2; arm++)
	    {
	      buffer.str("");
	      buffer << "B" << barrel << "-L" << ladder << "-" << arm_label[arm];
	      _reqlist[buffer.str()]=1;
	    }
	}
    }
  for(int barrel=2; barrel<4; barrel++)
    {
      for(int ladder=0; ladder<nladder[barrel]; ladder++)
	{
	  buffer.str("");
	  buffer << "B" << barrel << "-L" << ladder;
	  _reqlist[buffer.str()]=1;
	}
    }
  return 0;
}

int SvxEventKiller::Init(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  PHString nodename =  topNode->getName().getString();

  PHString histname = "h1status_0_";
  histname += nodename;
  h1status[0]=new TH1D(histname.getString(),"raw status;bitword;count",7,-0.5,7-0.5);

  histname="h1status_1_";
  histname += nodename;
  h1status[1]=new TH1D(histname.getString(),"raw status & mode;bitword;count",7,-0.5,7-0.5);

  histname="h1nevent_";
  histname += nodename;
  h1nevent=new TH1D(histname.getString(),"",2,-0.5,1.5);
  h1nevent->GetXaxis()->SetBinLabel(1,"NConsider");
  h1nevent->GetXaxis()->SetBinLabel(2,"NAccept");

  histname="h1biastime";
  histname+=nodename;
  h1biastime=new TH1I(histname.getString(),"channel;time",100,-0.5,99.5);

  histname="h1strip_last_good_event";
  histname+=nodename;
  h1strip_last_good_event=new TH1I(histname.getString(),"",1,-10,+10);

  histname="h1packet_";
  histname += nodename;
  h1packet=new TH1D(histname.getString(),"bad packets;packetid;count",200,24000-0.5,24200-0.5);

  histname="h1packet_mask_";
  histname += nodename;
  h1packet_mask=new TH1D(histname.getString(),"bad packets;packetid;count",200,24000-0.5,24200-0.5);

  se->registerHisto(h1status[0]);
  se->registerHisto(h1status[1]);
  se->registerHisto(h1nevent);
  se->registerHisto(h1biastime);
  se->registerHisto(h1packet);
  se->registerHisto(h1packet_mask);
  
  return 0;
}

int SvxEventKiller::SetLadderTag(const string& tag)
{
  SetLadderAllActive();
  if(tag.compare("Run12")==0)
    {
      _reqlist.erase("B1-L10-N");
      _reqlist.erase("B1-L10-S");
      _reqlist.erase("B1-L18-N");
      _reqlist.erase("B1-L18-S");
      _reqlist.erase("B1-L19-N");
      _reqlist.erase("B1-L19-S");
    }
  else if(tag.compare("Run11AuAu200-A4")==0)
    {
      const string badladder[40]={"B0-L5-S","B0-L6-N","B0-L6-S","B0-L7-S","B0-L8-S","B0-L9-N","B0-L9-S","B1-L0-S","B1-L2-N","B1-L5-S","B1-L7-S","B1-L8-N","B1-L9-N","B1-L9-S","B1-L10-N","B1-L10-S","B1-L11-N","B1-L12-N","B1-L13-N","B1-L14-N","B1-L14-S","B1-L15-N","B1-L16-N","B1-L16-S","B1-L17-N","B1-L17-S","B2-L1","B2-L5","B2-L7","B2-L8","B2-L9","B2-L13","B2-L14","B2-L15","B3-L0","B3-L7","B3-L11","B3-L13","B3-L22","B3-L23"};
      for(int ii=0; ii<40; ii++)
	{
	  _reqlist.erase(badladder[ii]);
	}
    }
  else if(tag.compare("Run11AuAu200-B4")==0)
    {
      const string badladder[31]={"B0-L5-S","B0-L6-S","B0-L7-S","B0-L8-S","B0-L9-N","B0-L9-S","B1-L0-S","B1-L5-S","B1-L7-S","B1-L8-N","B1-L9-N","B1-L9-S","B1-L10-S","B1-L11-N","B1-L12-N","B1-L13-N","B1-L14-N","B1-L16-N","B1-L17-N","B1-L17-S","B2-L5","B2-L7","B2-L8","B2-L9","B2-L13","B2-L14","B2-L15","B3-L0","B3-L11","B3-L13","B3-L23"};
      for(int ii=0; ii<31; ii++)
	{
	  _reqlist.erase(badladder[ii]);
	}
    }
  else if(tag.compare("Run11AuAu200-C4")==0)
    {
      const string badladder[21]={"B0-L6-S","B0-L8-S","B0-L9-N","B1-L0-S","B1-L7-S","B1-L10-S","B1-L11-N","B1-L12-N","B1-L13-N","B1-L14-N","B1-L16-N","B1-L17-S","B2-L5","B2-L7","B2-L8","B2-L9","B2-L13","B2-L15","B3-L0","B3-L13","B3-L23"};
      for(int ii=0; ii<21; ii++)
	{
	  _reqlist.erase(badladder[ii]);
	}
    }
  else if(tag.compare("Run12pp200-A")==0)
    {
      const string badladder[50]={"B0-L0-N","B0-L0-S","B0-L2-N","B0-L3-N","B0-L4-S","B0-L5-S","B0-L6-S","B0-L8-S","B0-L9-SB1-L0-N","B1-L0-S","B1-L1-N","B1-L3-N","B1-L4-N","B1-L5-N","B1-L6-N","B1-L7-N","B1-L10-N","B1-L10-S","B1-L11-S","B1-L12-S","B1-L13-S","B1-L14-N","B1-L15-N","B1-L15-S","B1-L16-N","B1-L16-S","B1-L17-S","B1-L18-N","B1-L18-S","B1-L19-N","B1-L19-S","B2-L0","B2-L1","B2-L2","B2-L12","B2-L14","B2-L15","B3-L6","B3-L7","B3-L9","B3-L10","B3-L11","B3-L12","B3-L16","B3-L17","B3-L19","B3-L21","B3-L22","B3-L23"};
      for(int ii=0; ii<50; ii++)
	{
	  _reqlist.erase(badladder[ii]);
	}
    }
  else if(tag.compare("Run12pp200-B")==0)
    {
      const string badladder[43]={"B0-L0-N","B0-L0-S","B0-L2-N","B0-L3-N","B0-L4-S","B0-L5-S","B0-L9-S","B1-L4-N","B1-L5-N","B1-L6-N","B1-L7-N","B1-L10-N","B1-L10-S","B1-L11-S","B1-L12-S","B1-L13-S","B1-L15-N","B1-L15-S","B1-L16-N","B1-L16-S","B1-L17-S","B1-L18-N","B1-L18-S","B1-L19-N","B1-L19-S","B2-L0","B2-L1","B2-L2","B2-L12","B2-L14","B2-L15","B3-L6","B3-L7","B3-L9","B3-L10","B3-L11","B3-L12","B3-L16","B3-L17","B3-L19","B3-L21","B3-L22","B3-L23"};
      for(int ii=0; ii<43; ii++)
	{
	  _reqlist.erase(badladder[ii]);
	}
    }
  else if(tag.compare("Run12pp200-C")==0)
    {
      const string badladder[29]={"B0-L4-S","B0-L9-S","B1-L4-N","B1-L6-N","B1-L10-N","B1-L10-S","B1-L11-S","B1-L12-S","B1-L15-S","B1-L16-N","B1-L18-N","B1-L18-S","B1-L19-N","B1-L19-S","B2-L0","B2-L1","B2-L14","B2-L15","B3-L6","B3-L7","B3-L9","B3-L10","B3-L11","B3-L12","B3-L16","B3-L17","B3-L19","B3-L21","B3-L22"};
      for(int ii=0; ii<29; ii++) 
	{
	  _reqlist.erase(badladder[ii]);
	}
    }
  else if(tag.compare("Run12pp200-D")==0)
    {
      const string badladder[13]={"B0-L4-S","B1-L10-N","B1-L10-S","B1-L18-N","B1-L18-S","B1-L19-N","B1-L19-S","B2-L15","B3-L7","B3-L16","B3-L17","B3-L19","B3-L22"};
      for(int ii=0; ii<13; ii++) 
	{
	  _reqlist.erase(badladder[ii]);
	}
    }
  return 0;
}

int SvxEventKiller::PrintLadder() const
{
  cout << "Required Ladder List:\n";
  for( map<string, int>::const_iterator ii=_reqlist.begin(); ii!=_reqlist.end(); ++ii)
    {
      cout << (*ii).first << endl;
    }
  return 0;
}

int SvxEventKiller::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  _runnumber = rc->get_IntFlag("RUNNUMBER");
  if(verbosity > 0) 
    {
      cout << "SvxEventKiller, fetching data for run number: " << _runnumber << endl;
    }

  _calibration_status=fetch(topNode,_runnumber);
  //cout<<"calib status : "<<_calibration_status<<" "<<_mode<<endl;

  // get pointer to SvxDaqErrorMap
  m_daqerror = findNode::getClass<SvxDaqErrorMap>(topNode,"SvxDaqErrorMap");
  if(!m_daqerror)
    {
      cout << PHWHERE << " SvxDaqErrorMap node not found " << endl;
    }

  if(m_activeladderDB){
    SetActiveLadderByDatabase();
    if(verbosity>1) PrintLadder();
  }
  

  // fetched at _least_ the calibrations needed for requested checks
  if( (_calibration_status & _mode) != _mode )
    {
      if( ! (_calibration_status & FLAG_CELLID) )
	{
	  cout << PHWHERE << "No SvxEventKiller cellid calibration for run " << _runnumber << endl;
	}
      if( ! (_calibration_status & FLAG_BIASQA) )
	{
	  cout << PHWHERE << "No SvxEventKiller biasqa calibration for run " << _runnumber << endl;
	}
      if( ! (_calibration_status & FLAG_DAQERR) )
	{
	  cout << PHWHERE << "No SvxEventKiller daqerr calibration for run " << _runnumber << endl;
	}
    }

  return 0;
}

int SvxEventKiller::process_event(PHCompositeNode *topNode)
{
  ladder_status.clear();
  _event_status=0;

  if( (_calibration_status & _mode ) != _mode )
    {
      _event_status=-1;
      return _RetCode;
    }
  _nConsider++;

  EventHeader *evthead = findNode::getClass<EventHeader>(topNode,"EventHeader");
  if(!evthead)
    {
      cout << PHWHERE << " EventHeader node not found " << endl;
      _event_status=-2;
      return _RetCode;
    }

  // cellid qa
  if(_strip_last_good_event>=0)
    {
      if(_strip_last_good_event < evthead->get_EvtSequence())
	{
	  for( map<string,int>::iterator ii=_lad_pid.begin(); ii!=_lad_pid.end(); ++ii)
	    {
	      string ladder=(*ii).first;
	      ladder_status[ladder]+=FLAG_CELLID;
	    }
	}
    }

  // bias qa
  int timestamp=evthead->get_TimeStamp();
  for( map<string,int>::iterator ii=_lad_pid.begin(); ii!=_lad_pid.end(); ++ii)
    {
      string ladder=(*ii).first;
      int badtime=0;
      if(_bad_bias_time.find(ladder)==_bad_bias_time.end())
	{
	  // no data, assumed to be good
	}
      else
	{
	  badtime=_bad_bias_time[ladder];
	}

      // determine if ladder is good or bad for this event
      if(badtime==0)
	{
	  // ladder is fine for the whole run
	}
      else if(badtime==-1)
	{
	  // ladder is bad for the whole run
	  ladder_status[ladder]+=FLAG_BIASQA;
	}
      else if( badtime<timestamp && badtime>0 )
	{
	  // ladder went bad before this event
	  ladder_status[ladder]+=FLAG_BIASQA;
	}
    }

  // parity error
  for( map<string,int>::iterator ii=_lad_pid.begin(); ii!=_lad_pid.end(); ++ii)
    {
      string ladder=(*ii).first;
      int packetid=(*ii).second;

      if(evthead->isBadPacket(packetid))
	{
	  ladder_status[ladder]+=FLAG_DAQERR;
	  h1packet->Fill(packetid);
	}
    }

  checkMask(evthead);

  // make a determination for the entire event
  for( map<string,int>::iterator ii=_reqlist.begin(); ii!=_reqlist.end(); ++ii)
    {
      string ladder=(*ii).first;
      if(ladder_status.find(ladder)==ladder_status.end())
	{
	  if(verbosity>2) cerr << "Error coding up SvxEventKiller" << endl;
	  continue;
	}
      _event_status|=ladder_status[ladder];
    }

  h1status[0]->Fill(_event_status);
  h1status[1]->Fill(_event_status & _mode );
  h1nevent->Fill(0);

  if( (_event_status & _mode) == 0 )
    {
      _nGood++;
      h1nevent->Fill(1);
      return EVENT_OK;
    }

  return _RetCode;
}

vector<string> SvxEventKiller::GetBadLadderList() const
{
  vector<string> bad_ladders;

  std::map<std::string,int>ladder_status;
  for( map<string,int>::iterator ii=ladder_status.begin(); ii!=ladder_status.end(); ++ii)
    {
      string ladder=(*ii).first;
      int status=(*ii).second;
      if(status!=0) 
	{
	  bad_ladders.push_back(ladder);
	}
    }
  return bad_ladders;
}

int SvxEventKiller::GetLadderStatus(std::string& ladder)
{
  if(ladder_status.find(ladder)==ladder_status.end()) return -1;
  return ladder_status[ladder];
}

int SvxEventKiller::isValidRun(const int runno) const
{
  if(runno > BEGIN_OF_RUN11) 
    {
      return 1;
    }
  return 0;
}


int SvxEventKiller::fetch(PHCompositeNode* topNode,const int run)
{
  int returncode=0;
  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  if(!application->startRead()) 
    {
      PHMessage("SvxEventKiller::", PHError, "Aborting ... Database not readable");
      application->abort();
    }
  
  //  Make a bank ID
  PdbBankID bankID(0);
  PdbCalBank* Bank = bankManager->fetchBank("PdbIntBank",
                                            bankID,
					    "svxstripstuckcellid",
                                            run);

  // the cellid logic is: Matt only enters a calibration if the run is
  // bad. If there is no calibration for a given run, then it is a run
  // with no problems.

  // there is a calibration and it has information in it
  if(Bank && Bank->getLength()>0)
    {
      if(_verbose) { cout << "Found cellid valid calibration" << endl; }
      PdbInt* stripevent = (PdbInt *) & Bank->getEntry(0);
      _strip_last_good_event = stripevent->getValue();
      delete Bank;
      h1strip_last_good_event->SetBinContent(1,_strip_last_good_event);
      returncode+=FLAG_CELLID;
    }
  else if(Bank==0) // there is no bank, then it is good
    {
      if(_verbose) { cout << "No cellid bank, means run has no problems." << endl; }
      returncode+=FLAG_CELLID;
    }
  else
    {
      cout << PHWHERE << "Failed to get stripixel cell ID info from DB" << endl;
    }

  // biasqa
  //PdbBankID bankID(0);
  Bank = bankManager->fetchBank("PdbSvxBias",
				bankID,
				"svxbiasqa",
				run);
  if(Bank && Bank->getLength()>0)
    {
      PdbSvxBias* biasqa = (PdbSvxBias *) & Bank->getEntry(0);
      std::map<std::string,std::pair<int,int> >badlist = biasqa->get();
      delete Bank;
      returncode+=FLAG_BIASQA;

      // copy the last good bias timestamps into a data holder
      for( map<string, pair<int,int> >::const_iterator ii=badlist.begin(); ii!=badlist.end(); ++ii)
	{
	  string ladder=(*ii).first;
	  pair<int,int> data=(*ii).second;
	  int fail_time=data.first;
	  //int flag=data.second;
	  _bad_bias_time[ladder]=fail_time;
	}
    }
  else
    {
      cout << PHWHERE << "Failed to get svx bias qa info from DB" << endl;
    }

  // module id mapping taken from:
  // https://www.phenix.bnl.gov/WWW/p/draft/hachiya/VTX/hard_soft_channel/
  // VTX_sensor_index_revised_v1.2.pptx
  // on advice from Takashi

  // make a list of packetid's
  svxAddress* adr = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( adr != NULL) 
    {
      m_addr = adr;
      // make a map giving the string_ladder --> packetid
      _lad_pid["B0-L0-N"]=adr->getPixelPacketID(0);
      _lad_pid["B0-L1-N"]=adr->getPixelPacketID(1);
      _lad_pid["B0-L2-N"]=adr->getPixelPacketID(2);
      _lad_pid["B0-L3-N"]=adr->getPixelPacketID(3);
      _lad_pid["B0-L4-N"]=adr->getPixelPacketID(4);
      _lad_pid["B0-L5-N"]=adr->getPixelPacketID(10);
      _lad_pid["B0-L6-N"]=adr->getPixelPacketID(11);
      _lad_pid["B0-L7-N"]=adr->getPixelPacketID(12);
      _lad_pid["B0-L8-N"]=adr->getPixelPacketID(13);
      _lad_pid["B0-L9-N"]=adr->getPixelPacketID(14);
      _lad_pid["B0-L0-S"]=adr->getPixelPacketID(5 );
      _lad_pid["B0-L1-S"]=adr->getPixelPacketID(6 );
      _lad_pid["B0-L2-S"]=adr->getPixelPacketID(7 );
      _lad_pid["B0-L3-S"]=adr->getPixelPacketID(8 );
      _lad_pid["B0-L4-S"]=adr->getPixelPacketID(9 );
      _lad_pid["B0-L5-S"]=adr->getPixelPacketID(15);
      _lad_pid["B0-L6-S"]=adr->getPixelPacketID(16);
      _lad_pid["B0-L7-S"]=adr->getPixelPacketID(17);
      _lad_pid["B0-L8-S"]=adr->getPixelPacketID(18);
      _lad_pid["B0-L9-S"]=adr->getPixelPacketID(19);
      
      _lad_pid["B1-L0-N"]=adr->getPixelPacketID(20);
      _lad_pid["B1-L1-N"]=adr->getPixelPacketID(21);
      _lad_pid["B1-L2-N"]=adr->getPixelPacketID(22);
      _lad_pid["B1-L3-N"]=adr->getPixelPacketID(23);
      _lad_pid["B1-L4-N"]=adr->getPixelPacketID(24);
      _lad_pid["B1-L5-N"]=adr->getPixelPacketID(25);
      _lad_pid["B1-L6-N"]=adr->getPixelPacketID(26);
      _lad_pid["B1-L7-N"]=adr->getPixelPacketID(27);
      _lad_pid["B1-L8-N"]=adr->getPixelPacketID(28);
      _lad_pid["B1-L9-N"]=adr->getPixelPacketID(29);
      _lad_pid["B1-L0-S"]=adr->getPixelPacketID(30);
      _lad_pid["B1-L1-S"]=adr->getPixelPacketID(31);
      _lad_pid["B1-L2-S"]=adr->getPixelPacketID(32);
      _lad_pid["B1-L3-S"]=adr->getPixelPacketID(33);
      _lad_pid["B1-L4-S"]=adr->getPixelPacketID(34);
      _lad_pid["B1-L5-S"]=adr->getPixelPacketID(35);
      _lad_pid["B1-L6-S"]=adr->getPixelPacketID(36);
      _lad_pid["B1-L7-S"]=adr->getPixelPacketID(37);
      _lad_pid["B1-L8-S"]=adr->getPixelPacketID(38);
      _lad_pid["B1-L9-S"]=adr->getPixelPacketID(39);

      _lad_pid["B1-L10-N"]=adr->getPixelPacketID(40);
      _lad_pid["B1-L11-N"]=adr->getPixelPacketID(41);
      _lad_pid["B1-L12-N"]=adr->getPixelPacketID(42);
      _lad_pid["B1-L13-N"]=adr->getPixelPacketID(43);
      _lad_pid["B1-L14-N"]=adr->getPixelPacketID(44);
      _lad_pid["B1-L15-N"]=adr->getPixelPacketID(45);
      _lad_pid["B1-L16-N"]=adr->getPixelPacketID(46);
      _lad_pid["B1-L17-N"]=adr->getPixelPacketID(47);
      _lad_pid["B1-L18-N"]=adr->getPixelPacketID(48);
      _lad_pid["B1-L19-N"]=adr->getPixelPacketID(49);
      _lad_pid["B1-L10-S"]=adr->getPixelPacketID(50);
      _lad_pid["B1-L11-S"]=adr->getPixelPacketID(51);
      _lad_pid["B1-L12-S"]=adr->getPixelPacketID(52);
      _lad_pid["B1-L13-S"]=adr->getPixelPacketID(53);
      _lad_pid["B1-L14-S"]=adr->getPixelPacketID(54);
      _lad_pid["B1-L15-S"]=adr->getPixelPacketID(55);
      _lad_pid["B1-L16-S"]=adr->getPixelPacketID(56);
      _lad_pid["B1-L17-S"]=adr->getPixelPacketID(57);
      _lad_pid["B1-L18-S"]=adr->getPixelPacketID(58);
      _lad_pid["B1-L19-S"]=adr->getPixelPacketID(59);

      _lad_pid["B2-L0"]   =adr->getStripPacketID(0);
      _lad_pid["B2-L1"]   =adr->getStripPacketID(1);
      _lad_pid["B2-L2"]   =adr->getStripPacketID(2);
      _lad_pid["B2-L3"]   =adr->getStripPacketID(3);
      _lad_pid["B2-L4"]   =adr->getStripPacketID(4);
      _lad_pid["B2-L5"]   =adr->getStripPacketID(5);
      _lad_pid["B2-L6"]   =adr->getStripPacketID(6);
      _lad_pid["B2-L7"]   =adr->getStripPacketID(7);
      _lad_pid["B2-L8"]   =adr->getStripPacketID(8);
      _lad_pid["B2-L9"]   =adr->getStripPacketID(9);
      _lad_pid["B2-L10"]   =adr->getStripPacketID(10);
      _lad_pid["B2-L11"]   =adr->getStripPacketID(11);
      _lad_pid["B2-L12"]   =adr->getStripPacketID(12);
      _lad_pid["B2-L13"]   =adr->getStripPacketID(13);
      _lad_pid["B2-L14"]   =adr->getStripPacketID(14);
      _lad_pid["B2-L15"]   =adr->getStripPacketID(15);

      _lad_pid["B3-L0"]   =adr->getStripPacketID(16);
      _lad_pid["B3-L1"]   =adr->getStripPacketID(17);
      _lad_pid["B3-L2"]   =adr->getStripPacketID(18);
      _lad_pid["B3-L3"]   =adr->getStripPacketID(19);
      _lad_pid["B3-L4"]   =adr->getStripPacketID(20);
      _lad_pid["B3-L5"]   =adr->getStripPacketID(21);
      _lad_pid["B3-L6"]   =adr->getStripPacketID(22);
      _lad_pid["B3-L7"]   =adr->getStripPacketID(23);
      _lad_pid["B3-L8"]   =adr->getStripPacketID(24);
      _lad_pid["B3-L9"]   =adr->getStripPacketID(25);
      _lad_pid["B3-L10"]   =adr->getStripPacketID(26);
      _lad_pid["B3-L11"]   =adr->getStripPacketID(27);
      _lad_pid["B3-L12"]   =adr->getStripPacketID(28);
      _lad_pid["B3-L13"]   =adr->getStripPacketID(29);
      _lad_pid["B3-L14"]   =adr->getStripPacketID(30);
      _lad_pid["B3-L15"]   =adr->getStripPacketID(31);
      _lad_pid["B3-L16"]   =adr->getStripPacketID(32);
      _lad_pid["B3-L17"]   =adr->getStripPacketID(33);
      _lad_pid["B3-L18"]   =adr->getStripPacketID(34);
      _lad_pid["B3-L19"]   =adr->getStripPacketID(35);
      _lad_pid["B3-L20"]   =adr->getStripPacketID(36);
      _lad_pid["B3-L21"]   =adr->getStripPacketID(37);
      _lad_pid["B3-L22"]   =adr->getStripPacketID(38);
      _lad_pid["B3-L23"]   =adr->getStripPacketID(39);

      returncode+=FLAG_DAQERR; // added Takashi
    }
  else
    {
      cout << PHWHERE<< "Failed to get svxAddress from topNode" << endl; 
      returncode+=FLAG_DAQERR;
    }
  return returncode;
}

int SvxEventKiller::SetReturnCode(const char *action)
{
  if(!strcmp(action, "DISCARD")) 
    {
      cout << "SvxEventKiller action: Discard Event for specific IOManager" << endl;
      _RetCode = DISCARDEVENT;
    } 
  else if(!strcmp(action, "ABORT")) 
    {
      cout << "SvxEventKiller action: Abort Event in reconstruction" << endl;
      _RetCode = ABORTEVENT;
    }
  else if(!strcmp(action, "OKAY")) 
    {
      cout << "SvxEventKiller action: Allways write Event (kind of pointless)" << endl;
      _RetCode = EVENT_OK;
    } 
  else 
    {
      cout << PHWHERE << " Unknown option: " << action << endl;
      return -1;
    }
  return 0;
}

int SvxEventKiller::RemoveLadder(const std::string& ladder)
{
  if(_reqlist.find(ladder)==_reqlist.end())
    {
      cout << "Ladder: " << ladder << " is not active.  Why are you trying to remove it?\n";
      return -1;
    }
  else
    {
      _reqlist.erase(ladder);
    }
  return 0;
}

int
SvxEventKiller::End(PHCompositeNode *topNode)
{
  cout << "SvxEventKiller Summary: " << endl
       << "Number events considered: " << _nConsider << endl
       << "Number events accepted: " << _nGood << endl;

  return 0;
}

void SvxEventKiller::checkMask(EventHeader *evthead)
{
  // parity error
  //cout<<__FUNCTION__<<" "<<_lad_pid.size()<<endl;
  for( map<string,int>::iterator ii=_lad_pid.begin(); ii!=_lad_pid.end(); ++ii)
    {
      string ladder=(*ii).first;
      int packetid=(*ii).second;

      if(evthead->isBadPacket(packetid))
	{
          //////////
          // test
          int det = (packetid<24100) ? 0 : 1; // pixel or strip
          int module = (det==0) ? m_addr->getPixelModuleID(packetid)
                                : m_addr->getStripModuleID(packetid);

          if(m_daqerror!=NULL && m_daqerror->getStatus(det, module)==0 )
            {
              h1packet_mask->Fill(packetid);
            }
          //////////
	}
    }
}

void SvxEventKiller::SetActiveLadderByDatabase()
{
  if(m_daqerror==NULL){
    cerr<<PHWHERE<<" no DaqErrorMap object"<<endl;
    return;
  }
  if(m_addr==NULL){
    cerr<<PHWHERE<<" no SvxAddress object"<<endl;
    return;
  }

  // 
  SetLadderAllActive();

  // remove problematic ladders
  //  search ladder by moduleid -> packetid -> ladder_name
  //  remove by ladder_name

  static const int NMODULE[2] = {SVXNMODULEPIXEL, SVXNMODULESTRIP};
  for(int idet=0; idet<2; idet++)
    {
      for(int imod=0; imod<NMODULE[idet]; imod++)
        {
          unsigned int status = m_daqerror->getStatus(idet, imod);
          if(verbosity > 1) cout<<PHWHERE<<" ladder "<<idet<<" "<<imod<<" 0x"<<hex<<status<<dec<<endl;

          if(status>0)
            {
              int packetid = (idet==0) ? m_addr->getPixelPacketID(imod)
                                       : m_addr->getStripPacketID(imod);

              for( map<string, int>::iterator ii=_lad_pid.begin(); ii!=_lad_pid.end(); ++ii)
                {
                   string lad_name = ii->first; 
                   int    lad_pid  = ii->second; 
                   if(lad_pid == packetid) 
                     {
                       _reqlist.erase(lad_name.c_str());
                       if(verbosity > 0) cout<<PHWHERE<<" ladder removed : "<<lad_name.c_str()<<endl;
                       break;
                     }
                }
            }
        }
    }
} 
