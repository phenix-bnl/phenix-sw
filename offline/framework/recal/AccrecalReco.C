
#include "AccrecalReco.h"
#include "MasterRecalibrator.h"
#include "RunHeader.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "getClass.h"

#include "PHCompositeNode.h"
#include <iostream>

#include "Acc.h"
#include "AccCluster.h"
#include "AccSnglCluster.h"
#include "AccCalib.h"

using namespace std;

AccrecalReco::AccrecalReco():Recalibrator("AccrecalReco")
{
  baseclasses.insert("AccCluster");
  acccalib = 0;
}

AccrecalReco::~AccrecalReco()
{
  if(acccalib) delete acccalib;
}

int AccrecalReco::isValidRun(const int runno) const
{

  // Run4 (Au+Au)
  if (runno >= 107001 && runno <= 123564)
    {
      return 1;
    }

  // Run5 (Cu+Cu)
  if (runno >= 149001 && runno <= 163700)
    {
      return 1;
    }

  // Run5 (p+p)
  if (runno >= 166001 && runno <= 180000)
    {
      return 1;
    }

  // Run6 (p+p 200GeV)
  if (runno >= 188216 && runno <= 204639) 
    {
      return 1;
    }

  // Run6 (p+p 62.4GeV)
  if (runno >= 205153 && runno <= 206495)
    {
      return 1;
    }

  // Run7 (Au+Au 200GeV)
  if (runno >= 227016 && runno <= 240121)
    {
      return 1;
    }

  return 0;
}

int
AccrecalReco::Init(PHCompositeNode *topNode)
{
  acccalib = new AccCalib();
  return 0;
}

int AccrecalReco::InitRun(PHCompositeNode *topNode)
{
  RunHeader * d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");

  if (!d_runhdr)
    {
      cout << PHWHERE << "AccrecalReco:: runhdr not in Node Tree" << endl;
      cout << PHWHERE << "You get zeroes for your Acc Recalibrations" << endl;
      return -1;
    }

  //  Use the run header to fetch the run number...
  int runnumber = d_runhdr->get_RunNumber();
  if (verbosity > 0) 
    {
      cout << "Fetching for run number: " << runnumber << endl;
    }

  // run-by-run fetch 
  acccalib->fetchAdcPedestal(runnumber);
  acccalib->fetchAdcGain(runnumber);
  acccalib->fetchTdcPedestal(runnumber);
  acccalib->fetchTdcGain(runnumber);
  cntnodes.clear();

  MasterRecalibrator *mr = GetMasterRecalibrator();
  mr->searchNodeTree(topNode, "PHCentralTrack",cntnodes);
  vector<string>::const_iterator it;
  if (verbosity > 0)
    {
      for (it = cntnodes.begin(); it != cntnodes.end(); it++)
	{
	  cout << "found Node with PHCentralTrack Object: " << *it << endl;
	}
    }

  return 0;
}

int
AccrecalReco::process_event(PHCompositeNode *topNode)
{

  AccCluster *d_acccls = findNode::getClass<AccCluster>(topNode, inputnodename.c_str());

  PHCentralTrack *d_cnt = 0;
  vector<string>::const_iterator it;
  int haveaero = 0;

  for (it = cntnodes.begin(); it != cntnodes.end(); it++)
    {
      d_cnt = findNode::getClass<PHCentralTrack>(topNode, (*it).c_str());
      if (d_cnt->get_npart() > 0)
	{
          PHSnglCentralTrack *sngltrk = d_cnt->get_track(0);
          sngltrk->ShutUp();
	  if (sngltrk->isImplemented(sngltrk->get_aerindex()) && 
              sngltrk->isImplemented(sngltrk->get_aersindex()))
	    {
              haveaero = 1;
              sngltrk->ShutUp(0);
              break;
	    }
          sngltrk->ShutUp(0);
        }
    }
  if (!haveaero)
    {
      return 0;
    }

  // Run7 (Au+Au 200GeV)
  RunHeader * d_runhdr = findNode::getClass<RunHeader>(topNode, "RunHeader");

  if (!d_runhdr)
    {
      cout << PHWHERE << "AccrecalReco:: runhdr not in Node Tree" << endl;
      cout << PHWHERE << "You get zeroes for your Acc Recalibrations" << endl;
      return 0;
    }

  //  Use the run header to fetch the run number...
  int runnumber = d_runhdr->get_RunNumber();

  if (runnumber >= 227016 && runnumber <= 240121)
    {
      d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
    }


  const int NBOXES = 4;

  //  Then "correct" (FAKE & MEANINGLESS!) the tracks you have found...
  if (d_cnt && d_acccls)
    {
      for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
        {

          int aerindex = d_cnt->get_aerindex(i);
          int aersindex = d_cnt->get_aersindex(i);

          // aerindex part --------------
          if (aerindex != -9999)
            {

              int aerhitid = d_acccls->get_cluster(aerindex)->get_aerhitid();
              int aerhitconfig = d_acccls->get_cluster(aerindex)->get_aerhitconfig();

              for (int ibox = 0; ibox < NBOXES; ibox++)
                {

                  int boxid = get_BoxID(aerhitid, aerhitconfig, ibox);

                  if ( -1 < boxid && boxid < ACC::ACC_NBOX)
                    {

                      float aerph1 = d_acccls->get_cluster(aerindex)->get_aerph1(ibox);
                      float aerph2 = d_acccls->get_cluster(aerindex)->get_aerph2(ibox);
                      float aert1 = d_acccls->get_cluster(aerindex)->get_aert1(ibox);
                      float aert2 = d_acccls->get_cluster(aerindex)->get_aert2(ibox);

                      int pmt_1, pmt_2;	 // 1:N,2:S - PMT of the same cell
                      get_PmtFromBox(boxid, &pmt_1, &pmt_2);

                      float adcped1 = acccalib->get_adcpedvalue(pmt_1, 0);
                      float adcped2 = acccalib->get_adcpedvalue(pmt_2, 0);
                      float adcgain1 = acccalib->get_adcgainvalue(pmt_1, 0);
                      float adcgain2 = acccalib->get_adcgainvalue(pmt_2, 0);
                      float tdcped1 = acccalib->get_tdcpedvalue(pmt_1, 0);
                      float tdcped2 = acccalib->get_tdcpedvalue(pmt_2, 0);
                      float tdcgain1 = acccalib->get_tdcgainvalue(pmt_1, 0);
                      float tdcgain2 = acccalib->get_tdcgainvalue(pmt_2, 0);


                      if (adcgain1 > 0 && aerph1 != -9999 && aerph1 != 0 && aerph1 != + 9999)
                        d_acccls->get_cluster(aerindex)->set_aerph1(ibox, (aerph1 - adcped1) / adcgain1 );
                      else if (aerph1 == 0)
                        d_acccls->get_cluster(aerindex)->set_aerph1(ibox, 0);
                      else if (aerph1 == + 9999)
                        d_acccls->get_cluster(aerindex)->set_aerph1(ibox, + 9999);
                      else
                        d_acccls->get_cluster(aerindex)->set_aerph1(ibox, -9999);

                      if (adcgain2 > 0 && aerph2 != -9999 && aerph2 != 0 && aerph2 != + 9999)
                        d_acccls->get_cluster(aerindex)->set_aerph2(ibox, (aerph2 - adcped2) / adcgain2 );
                      else if (aerph2 == 0)
                        d_acccls->get_cluster(aerindex)->set_aerph2(ibox, 0);
                      else if (aerph2 == + 9999)
                        d_acccls->get_cluster(aerindex)->set_aerph2(ibox, + 9999);
                      else
                        d_acccls->get_cluster(aerindex)->set_aerph2(ibox, -9999);

                      if (aert1 != -9999 && aert1 != 0)
                        d_acccls->get_cluster(aerindex)->set_aert1(ibox, (aert1 - tdcped1) * tdcgain1 );
                      else if (aert1 == 0)
                        d_acccls->get_cluster(aerindex)->set_aert1(ibox, 0);
                      else
                        d_acccls->get_cluster(aerindex)->set_aert1(ibox, -9999);

                      if (aert2 != -9999 && aert2 != 0)
                        d_acccls->get_cluster(aerindex)->set_aert2(ibox, (aert2 - tdcped2) * tdcgain2 );
                      else if (aert2 == 0)
                        d_acccls->get_cluster(aerindex)->set_aert2(ibox, 0);
                      else
                        d_acccls->get_cluster(aerindex)->set_aert2(ibox, -9999);

                    }
                  else
                    {
                      d_acccls->get_cluster(aerindex)->set_aerph1(ibox, -9999);
                      d_acccls->get_cluster(aerindex)->set_aerph2(ibox, -9999);
                      d_acccls->get_cluster(aerindex)->set_aert1 (ibox, -9999);
                      d_acccls->get_cluster(aerindex)->set_aert2 (ibox, -9999);
                    }
                }
            }
          // end of aerindex part --------------

          // aersindex part --------------
          if (aersindex != -9999)
            {

              int aerhitid = d_acccls->get_cluster(aersindex)->get_aerhitid();
              int aerhitconfig = d_acccls->get_cluster(aersindex)->get_aerhitconfig();

              for (int ibox = 0; ibox < NBOXES; ibox++)
                {

                  int boxid = get_BoxID(aerhitid, aerhitconfig, ibox);

                  if ( -1 < boxid && boxid < ACC::ACC_NBOX)
                    {

                      float aerph1 = d_acccls->get_cluster(aersindex)->get_aerph1(ibox);
                      float aerph2 = d_acccls->get_cluster(aersindex)->get_aerph2(ibox);
                      float aert1 = d_acccls->get_cluster(aersindex)->get_aert1(ibox);
                      float aert2 = d_acccls->get_cluster(aersindex)->get_aert2(ibox);

                      int pmt_1, pmt_2;	 // 1:N,2:S - PMT of the same cell
                      get_PmtFromBox(boxid, &pmt_1, &pmt_2);

                      float adcped1 = acccalib->get_adcpedvalue(pmt_1, 0);
                      float adcped2 = acccalib->get_adcpedvalue(pmt_2, 0);
                      float adcgain1 = acccalib->get_adcgainvalue(pmt_1, 0);
                      float adcgain2 = acccalib->get_adcgainvalue(pmt_2, 0);
                      float tdcped1 = acccalib->get_tdcpedvalue(pmt_1, 0);
                      float tdcped2 = acccalib->get_tdcpedvalue(pmt_2, 0);
                      float tdcgain1 = acccalib->get_tdcgainvalue(pmt_1, 0);
                      float tdcgain2 = acccalib->get_tdcgainvalue(pmt_2, 0);


                      if (adcgain1 > 0 && aerph1 != -9999 && aerph1 != 0 && aerph1 != + 9999)
                        d_acccls->get_cluster(aersindex)->set_aerph1(ibox, (aerph1 - adcped1) / adcgain1 );
                      else if (aerph1 == 0)
                        d_acccls->get_cluster(aersindex)->set_aerph1(ibox, 0);
                      else if (aerph1 == + 9999)
                        d_acccls->get_cluster(aersindex)->set_aerph1(ibox, + 9999);
                      else
                        d_acccls->get_cluster(aersindex)->set_aerph1(ibox, -9999);

                      if (adcgain2 > 0 && aerph2 != -9999 && aerph2 != 0 && aerph2 != + 9999)
                        d_acccls->get_cluster(aersindex)->set_aerph2(ibox, (aerph2 - adcped2) / adcgain2 );
                      else if (aerph2 == 0)
                        d_acccls->get_cluster(aersindex)->set_aerph2(ibox, 0);
                      else if (aerph2 == + 9999)
                        d_acccls->get_cluster(aersindex)->set_aerph2(ibox, + 9999);
                      else
                        d_acccls->get_cluster(aersindex)->set_aerph2(ibox, -9999);

                      if (aert1 != -9999 && aert1 != 0)
                        d_acccls->get_cluster(aersindex)->set_aert1(ibox, (aert1 - tdcped1) * tdcgain1 );
                      else if (aert1 == 0)
                        d_acccls->get_cluster(aersindex)->set_aert1(ibox, 0);
                      else
                        d_acccls->get_cluster(aersindex)->set_aert1(ibox, -9999);

                      if (aert2 != -9999 && aert2 != 0)
                        d_acccls->get_cluster(aersindex)->set_aert2(ibox, (aert2 - tdcped2) * tdcgain2 );
                      else if (aert2 == 0)
                        d_acccls->get_cluster(aersindex)->set_aert2(ibox, 0);
                      else
                        d_acccls->get_cluster(aersindex)->set_aert2(ibox, -9999);

                    }
                  else
                    {
                      d_acccls->get_cluster(aersindex)->set_aerph1(ibox, -9999);
                      d_acccls->get_cluster(aersindex)->set_aerph2(ibox, -9999);
                      d_acccls->get_cluster(aersindex)->set_aert1 (ibox, -9999);
                      d_acccls->get_cluster(aersindex)->set_aert2 (ibox, -9999);
                    }
                }
            }
          // end of aersindex part --------------

        }
    }

  return 0;

}

int AccrecalReco::get_BoxID(int hitid, int hitconfig, int ibox)
{

  int boxid;

  if(hitconfig==0 && ibox==0)      boxid = hitid;
  else if(hitconfig==0 && ibox==1) boxid = hitid +1;
  else if(hitconfig==0 && ibox==2) boxid = hitid +10;
  else if(hitconfig==0 && ibox==3) boxid = hitid +11;

  else if(hitconfig==1 && ibox==0) boxid = hitid -1;
  else if(hitconfig==1 && ibox==1) boxid = hitid;
  else if(hitconfig==1 && ibox==2) boxid = hitid +9;
  else if(hitconfig==1 && ibox==3) boxid = hitid +10;

  else if(hitconfig==2 && ibox==0) boxid = hitid -10;
  else if(hitconfig==2 && ibox==1) boxid = hitid -9;
  else if(hitconfig==2 && ibox==2) boxid = hitid;
  else if(hitconfig==2 && ibox==3) boxid = hitid +1;

  else if(hitconfig==3 && ibox==0) boxid = hitid -11;
  else if(hitconfig==3 && ibox==1) boxid = hitid -10;
  else if(hitconfig==3 && ibox==2) boxid = hitid -1;
  else if(hitconfig==3 && ibox==3) boxid = hitid;
  else boxid = -9999;

  return boxid;

}

void AccrecalReco::get_PmtFromBox(int box_id, int *pmt_1, int *pmt_2)
{

  // Conversion (New -> Old)
  int boxindex = box_id / 10; // 0 - 7
  int offset = 10;
  if( boxindex%2 == 0 ) box_id = box_id + offset;
  else                  box_id = box_id - offset;

  // The following function works for old-convention box number !

  // Geometry
  int box_raw = box_id % 10;
  int box_col = box_id / 20;
  int isEast  = 0;

  if(box_id % 20 >= 10){
    isEast = 1;
  }

  // Preamp
  int preamp_id            = box_col * 5 + box_raw/2;
  int preamp_channel_north = box_id % 2 + isEast * 2;
  int preamp_channel_south = preamp_channel_north + 4;
  
  // PMT
  int pmtid_north = preamp_id * 8 + preamp_channel_north;
  int pmtid_south = preamp_id * 8 + preamp_channel_south;
  
  // Return
  *pmt_1 = pmtid_north;
  *pmt_2 = pmtid_south;

}


