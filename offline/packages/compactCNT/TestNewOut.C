#include <TestNewOut.h>

#include <TrigRunLvl1.h>
#include <TrigLvl1.h>
#include <PHTypedNodeIterator.h>

#include <PHCentralTrackv24.h>
#include <PHSnglCentralTrackv24.h>
#include <AccHitMap.h>
#include <AccHitMapEntry.h>
#include "AccCluster.h"
#include "AccSnglCluster.h"
#include <HbdHitMap.h>
#include <HbdHitMapEntry.h>
#include <Fun4AllReturnCodes.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <RunHeader.h>
#include <getClass.h>
#include <PHGlobal.h>
#include <PHGlobal_Central.h>
#include <PadHitMap.h>
#include <PadHitMapEntry.h>
#include <CrkAssocHitsEntry.h>
#include <CrkAssocHits.h>
#include <vararray/VariableArray.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <CrkHitMapEntry.h>
#include <CrkHitMap.h>

#include <fstream>
#include <set>
#include <sstream>

#include <TFile.h>
#include <TH1F.h>

#include <iomanip>
#include <cstdlib>

using namespace std;

TestNewOut::TestNewOut(const std::string &name): SubsysReco(name)
{
  // Default output path for the histogram, override the default with SetHistPath()
  histoutpath.assign("/phenix/scratch/frawley/");

#ifdef DUMP
  dumpfile.open("/phenix/scratch/frawley/testnewout.dump");
#endif

  nevts = 0;
  got_lvl1_names=0;

  return;
}

int
TestNewOut::InitRun(PHCompositeNode *topNode)
{
  for(int ilvl1=0;ilvl1<32;ilvl1++)
    {
      Lvl1Scaled[ilvl1]=0;
    }

  // Make some histograms for the RICH

  hn0 = new TH1F("hn0","hn0",100,0,15);
  hn1 = new TH1F("hn1","hn1",100,0,15);
  hn2 = new TH1F("hn2","hn2",100,0,15);
  hn3 = new TH1F("hn3","hn3",100,0,15);
  hnpe0 = new TH1F("hnpe0","hnpe0",100,0,20);
  hnpe1 = new TH1F("hnpe1","hnpe1",100,0,20);
  hnpe2 = new TH1F("hnpe2","hnpe2",100,0,20);
  hnpe3 = new TH1F("hnpe3","hnpe3",100,0,20);
  hchi2 = new TH1F("hchi2","hchi2",100,0,120);
  hdisp = new TH1F("hdisp","hdisp",100,0,20);
  hcross_phi = new TH1F("hcross_phi","hcross_phi",200,-1,4);
  hcenter_phi = new TH1F("hcenter_phi","hcenter_phi",200,-1,4);
  hcross_z = new TH1F("hcross_z","hcross_z",600,-300,-300);
  hcenter_z = new TH1F("hcenter_z","hcenter_z",600,-300,300);

  hsn0 = new TH1F("hsn0","hsn0",100,0,15);
  hsn1 = new TH1F("hsn1","hsn1",100,0,15);
  hsn2 = new TH1F("hsn2","hsn2",100,0,15);
  hsn3 = new TH1F("hsn3","hsn3",100,0,15);
  hsnpe0 = new TH1F("hsnpe0","hsnpe0",100,0,20);
  hsnpe1 = new TH1F("hsnpe1","hsnpe1",100,0,20);
  hsnpe2 = new TH1F("hsnpe2","hsnpe2",100,0,20);
  hsnpe3 = new TH1F("hsnpe3","hsnpe3",100,0,20);
  hschi2 = new TH1F("hschi2","hschi2",100,0,120);
  hsdisp = new TH1F("hsdisp","hsdisp",100,0,20);

  // Make some histograms for the Aerogel
  float amax = 400.0;
  hid = new TH1F("hid","hid",160,0,160);
  ha1 = new TH1F("ha1","ha1",410,-10,amax);
  ha2 = new TH1F("ha2","ha2",410,-10,amax);

  hsid = new TH1F("hsid","hsid",160,0,160);
  hsa1 = new TH1F("hsa1","hsa1",410,-10,amax);
  hsa2 = new TH1F("hsa2","hsa2",410,-10,amax);

  // Make some histograms for the TEC
  htc = new TH1F("htc","htc",500,0,1000.0);
  htnt = new TH1F("htnt","htnt",120,0,119);
  htavgt = new TH1F("htavgt","htavgt",120,0,119);
  htdphi = new TH1F("htdphi","htdphi",200,-0.025,0.025);
  htnplane = new TH1F("htnplane","htnplane",7,0,7);

  hstc = new TH1F("hstc","hstc",500,0,1000.0);
  hstnt = new TH1F("hstnt","hstnt",120,0,119);
  hstavgt = new TH1F("hstavgt","hstavgt",120,0,119);
  hstdphi = new TH1F("hstdphi","hstdphi",200,-0.025,0.025);
  hstnplane = new TH1F("hstnplane","hstnplane",7,0,7);

  // For the HBD

  hhbdadcch = new TH1F("hhbdadcch","hhbdadcch",2000,0,1999);
  hhbdcharge = new TH1F("hhbdcharge","hhbdcharge",2000,0,1999);
  hhbdentries = new TH1F("hhbdentries","hhbdentries",2000,0,1999);

  // For the EMCal

  hecore = new TH1F("hecore","hecore",200,0,10.0);
  hmom = new TH1F("hmom","hmom",200,0,10.0);
  heoverp = new TH1F("heoverp","heoverp",100,0,4.0);

  // For the DCH

  hnx1 = new TH1F("hnx1","hnx1",12,-0.5,11.5);
  hnx2 = new TH1F("hnx2","hnx2",12,-0.5,11.5);
  hqual = new TH1F("hqual","hqual",80,0,80);
  hzed = new TH1F("hzed","hzed",201,-100,100);
  hphi = new TH1F("hphi","hphi",200,-1.0,4.5);
  hbeta = new TH1F("hbeta","hbeta",200,0.5,2.5);

  // For charged veto

  hemcpc3dphi = new TH1F("hemcpc3dphi","hemcpc3dz",200,-2.0,2.0);
  hemcpc3dz = new TH1F("hemcpc3dz","hemcpc3dz",200,-300,300);
  hemcpc3neartrk = new TH1F("hemcpc3neartrk","hemcpc3neartrk",100,0,99);
  hemctrk = new TH1F("hemctrk","hemctrk",100,0,99);
  hemctrkdz = new TH1F("hemctrkdz","hemctrkdz",200,-300,300);
  hemctrkdphi = new TH1F("hemctrkdphi","",200,-2.0,2.0);
  hpemctrk = new TH1F("hpemctrk","hpemctrk",100,0.0,20.0);
  hemctrkquality = new TH1F("hemctrkquality","hemctrkquality",100,0,99);

  // Global

  hzvert = new TH1F("hzvert","hzvert",300,-150,150);
  hpc1hits = new TH1F("hpc1hits","hpc1hits",200,0,800);

  hpc1assoc = new TH1F("hpc1assoc","hpc1assoc",200,0,800);

  return EVENT_OK;
}

int
TestNewOut::process_event(PHCompositeNode *topNode)
{
  if(nevts%1000 < 1)
    cout << "Event " << nevts << endl;

  nevts++;

  // Get a list of the level 1 triggers
  
  ///////////////////////////////////////////////////////////////////////
  // find the runNode
  //////////////////////////////////////////////////////////////////////

  PHNodeIterator iter(topNode);
 
  PHCompositeNode *runNode = NULL;
  runNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "RUN"));
  if(!runNode)
    {
      cout << "TestNewout::process_event: No RUN node, do nothing and return!"
           << endl;
      return 1;
    }
  
  
  //////////////////////////////////////////////////////////////////////////
  // Trigger selection
  //////////////////////////////////////////////////////////////////////////
  
  // These guys should all have been placed on the run node already by head/trig
  
  TrigRunLvl1 *trRunLvl1 = findNode::getClass<TrigRunLvl1>(runNode,"TrigRunLvl1");
  TrigLvl1 *trLvl1 = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  if (!trLvl1)
    {
      cout <<PHWHERE << " No TrigLvl1 object, return and do nothing"
           << endl;
      return 0;
    }
  
  // get the Lvl1 and Lvl2 names only once
  if(got_lvl1_names==0)
    {
      got_lvl1_names=1;
      
      if(verbosity>1)
        cout << "TestNewOut: Version " << ThisName << " Lvl1 algorithm names are:" << endl;

      for(int ilvl1=0;ilvl1<32;ilvl1++)
        {
          Lvl1Name[ilvl1]=trRunLvl1->get_lvl1_trig_name_bybit(ilvl1);
	  
          if(verbosity>1)
            cout << "  ilvl1 = " << ilvl1
                 << " Lvl1 name = " <<  Lvl1Name[ilvl1] << endl;
        }
    }

  //const char *minbias = "BBCLL1(>0 tubes)";
      
  for (int ilvl1 = 0; ilvl1 < 32; ilvl1++)
    {
      // Get the Lvl1 trigger decision - we want scaled
      
      if( trLvl1->get_lvl1_trigscaled_bit(ilvl1) )
	{
	  if(verbosity>1)
	    cout << " Lvl1:  bit " << ilvl1 << " name "
		 << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1)
		 << " fired " << endl;
	  
	  Lvl1Scaled[ilvl1]++;

	  // Temporary: select the BBCLL1(>0 tubes) trigger
	  // cout << "Found trigger " << minbias << " " << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1) << " with bit " << ilvl1 << endl;
	  //if(!strcmp(trRunLvl1->get_lvl1_trig_name_bybit(ilvl1),minbias))
	  //  cout << "Found scaled minbias trigger " << trRunLvl1->get_lvl1_trig_name_bybit(ilvl1) << endl;

	}
    }
  
  // Aerogel
  //==========

  AccHitMap *accmap = 0;
  AccCluster *acccl = 0;

  // This is in the new output file
  accmap  = findNode::getClass<AccHitMap>(topNode, "AccHit_comp");
  if (accmap)
    {
      //cout << "Event " << nevts-1 << " Found AccHit_comp" << endl;
    }
  else
    {
      cout << "Did not find AccHit_comp" << endl;

      // It must be the old style file
      acccl   =  findNode::getClass<AccCluster>(topNode,"AccCluster");
      if (acccl)
        {
          //cout << "Event " << nevts-1 << " Found AccCluster" << endl;
	}
    }

  if(!accmap && !acccl)
    {
      //cout << PHWHERE << "Did not find any Acc hits node, quit!" << endl;
      //exit(1); 

      if(nevts < 2)
	cout << PHWHERE << "Did not find any Acc hits node, will skip Acc" << endl;

    }
      
  PHCentralTrack *cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if (cnt)
    {
      //cout << PHWHERE << "Found PHCentralTrack" << endl;
      compactCNT = 1;
    }
  else
    {
      cout << PHWHERE << "Failed to find PHCentralTrack! Cannot proceed so quit!" << endl;
      exit(1);
    }

#ifdef DUMP
  dumpfile << "PHCentralTrack has " << cnt->get_npart() << " tracks " << endl;
#endif

  // Loop over central tracks and make histograms
  //==============================================

  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);

      // RICH histograms for this track
      //================================

      short int crkid = sngl->get_ring();

      // The field "ring" is set for both new and old data storage methods.
      if (crkid >= 0)
        {
          // If there are, we get the track line projections to the RICH from the node "TrackLineProjections"

#ifdef DUMP
	  dumpfile << "Track " << i << " rich pars straight: crkid " << crkid 
		   << " " << sngl->get_n0()
		   << " " << sngl->get_npe0()
		   << " " << sngl->get_n1()
		   << " " << sngl->get_npe1()
		   << " " << sngl->get_n2()
		   << " " << sngl->get_npe2()
		   << " " << sngl->get_n3()
		   << " " << sngl->get_npe3()
		   << " " << sngl->get_chi2()
		   << " " << sngl->get_disp()
		   << " " << sngl->get_tcrk()
		   << " " << sngl->get_cross_phi()
		   << " " << sngl->get_cross_z()
		   << " " << sngl->get_center_phi()
		   << " " << sngl->get_center_z()
		   << endl;
#endif
	  
	  hn0->Fill(sngl->get_n0());
	  hn1->Fill(sngl->get_n1());
	  hn2->Fill(sngl->get_n2());
	  hn3->Fill(sngl->get_n3());
	  hnpe0->Fill(sngl->get_npe0());
	  hnpe1->Fill(sngl->get_npe1());
	  hnpe2->Fill(sngl->get_npe2());
	  hnpe3->Fill(sngl->get_npe3());
	  hchi2->Fill(sngl->get_chi2());
	  hdisp->Fill(sngl->get_disp());
	  hcross_phi->Fill(sngl->get_cross_phi());
	  hcenter_phi->Fill(sngl->get_center_phi());
	  if(sngl->get_cross_z() > -300)        // for some reason this writes a rare -9998 into the histogram
	    hcross_z->Fill(sngl->get_cross_z());
	  hcenter_z->Fill(sngl->get_center_z());
	  
	}
      
      // The field "sring" is not set for the old data storage method, only for the new one, so I do not use it here
      // Check sn1 instead to see if there is a background ring for this track
      if(sngl->get_sn1() > 0)
	{

#ifdef DUMP
	  dumpfile << "Track " << i << " rich pars swapped: scrkid " << sngl->get_sring() 
		   << " " << sngl->get_sn0()
		   << " " << sngl->get_snpe0()
		   << " " << sngl->get_sn1()
		   << " " << sngl->get_snpe1()
		   << " " << sngl->get_sn2()
		   << " " << sngl->get_snpe2()
		   << " " << sngl->get_sn3()
		   << " " << sngl->get_snpe3()
		   << " " << sngl->get_schi2()
		   << " " << sngl->get_sdisp()
		   << " " << sngl->get_stcrk()
		   << endl;
#endif
	  
	  hsn0->Fill(sngl->get_sn0());
	  hsn1->Fill(sngl->get_sn1());
	  hsn2->Fill(sngl->get_sn2());
	  hsn3->Fill(sngl->get_sn3());
	  hsnpe0->Fill(sngl->get_snpe0());
	  hsnpe1->Fill(sngl->get_snpe1());
	  hsnpe2->Fill(sngl->get_snpe2());
	  hsnpe3->Fill(sngl->get_snpe3());
	  hschi2->Fill(sngl->get_schi2());
	  hsdisp->Fill(sngl->get_sdisp());
	}	      

      // TEC histograms for this track from the new readback code
      //=========================================================

      // For the old production code, the TEC fields used below will all be empty

      if(sngl->get_tecid() > 0)
	{
#ifdef DUMP
	  dumpfile << "Found TEC matched straight track " << i << endl;
#endif
	  int nplanes = 0;
	  for(int iplane=0;iplane<6;iplane++)
	    {
#ifdef DUMP
	      if(sngl->get_teccharge(iplane) > 0)
		dumpfile << "  plane " << iplane 
			 << " charge " << sngl->get_teccharge(iplane)
			 << " dphiplane " << sngl->get_tecdphiplane(iplane)
			 << " ntimebins " << sngl->get_tecntimebins(iplane)
			 << " avgtimebin " << sngl->get_tecavgtimebin(iplane)
			 << endl;
#endif
	      
	      if(sngl->get_teccharge(iplane) > 0.0)
		nplanes++;
	      
	      htc->Fill(sngl->get_teccharge(iplane));
	      htnt->Fill(sngl->get_tecntimebins(iplane));
	      htavgt->Fill(sngl->get_tecavgtimebin(iplane));
	      htdphi->Fill(sngl->get_tecdphiplane(iplane));
	    }
	  htnplane->Fill(nplanes);
	}
      
      if(sngl->get_stecid() > 0)
	{
#ifdef DUMP
	  dumpfile << "Found TEC matched swapped track " << i << endl;
#endif

	  int nplanes = 0;
	  for(int iplane=0;iplane<6;iplane++)
	    {
#ifdef DUMP
	      if(sngl->get_steccharge(iplane) > 0)
		dumpfile << "  plane " << iplane 
			 << " charge " << sngl->get_steccharge(iplane)
			 << " dphiplane " << sngl->get_stecdphiplane(iplane)
			 << " ntimebins " << sngl->get_stecntimebins(iplane)
			 << " avgtimebin " << sngl->get_stecavgtimebin(iplane)
			 << endl;
#endif
	      if(sngl->get_steccharge(iplane) > 0.0)
		nplanes++;
	      
	      hstc->Fill(sngl->get_steccharge(iplane));
	      hstnt->Fill(sngl->get_stecntimebins(iplane));
	      hstavgt->Fill(sngl->get_stecavgtimebin(iplane));
	      hstdphi->Fill(sngl->get_stecdphiplane(iplane));
	    }
	  
	  hstnplane->Fill(nplanes);
	}  
      
      //=========================================
      // Aerogel histograms for this track
      //=========================================

      if (acccl)
	{
	  // This is the old output file
	  
	  // Get the list of acc hits

#ifdef DUMP
	  dumpfile << "accmap has number of clusters " << acccl->get_ncluster() << endl;
#endif
	  int aerindex  = sngl->get_aerindex();
	  if(aerindex >= 0) 
	    { 
	      const AccSnglCluster * acchit = acccl->get_cluster(aerindex);
	      
	      int aerhitid     = acchit->get_aerhitid();
	      int aerhitconfig = acchit->get_aerhitconfig();
	      float aerph1     = acchit->get_aerph1(aerhitconfig);
	      float aerph2     = acchit->get_aerph2(aerhitconfig);
	      hid->Fill(aerhitid);
	      ha1->Fill(aerph1);
	      ha2->Fill(aerph2);
	    }
	  
	  int aersindex  = sngl->get_aersindex();
	  if(aersindex >= 0) 
	    { 
	      int aerhitid     = acccl->get_cluster(aersindex)->get_aerhitid();
	      int aerhitconfig = acccl->get_cluster(aersindex)->get_aerhitconfig();
	      float aerph1     = acccl->get_cluster(aersindex)->get_aerph1(aerhitconfig);
	      float aerph2     = acccl->get_cluster(aersindex)->get_aerph2(aerhitconfig);
	      hsid->Fill(aerhitid);
	      hsa1->Fill(aerph1);
	      hsa2->Fill(aerph2);
	    }
	}

      // This is the new output file
      if (accmap)
	{
	  // The new style file
      
	  // Get the list of acc hits

#ifdef DUMP
	  dumpfile << "accmap has Nentries " << accmap->GetNentries() << endl;
#endif
	  int aerindex  = sngl->get_aerindex();
	  if(aerindex >= 0) 
	    { 
	      const AccHitMapEntry *acchit = accmap->GetHit(aerindex);
	      
	      int aerhitid     = acchit->get_hitid();
	      int aerhitconfig = acchit->get_hitconfig();
	      float aerph1     = acchit->get_ph1(aerhitconfig);
	      float aerph2     = acchit->get_ph2(aerhitconfig);
	      hid->Fill(aerhitid);
	      ha1->Fill(aerph1);
	      ha2->Fill(aerph2);
	    }
	  
	  int aersindex  = sngl->get_aersindex();
	  if(aersindex >= 0) 
	    { 
	      const AccHitMapEntry *acchit = accmap->GetHit(aersindex);
	      
	      int aerhitid     = acchit->get_hitid();
	      int aerhitconfig = acchit->get_hitconfig();
	      float aerph1     = acchit->get_ph1(aerhitconfig);
	      float aerph2     = acchit->get_ph2(aerhitconfig);
	      hsid->Fill(aerhitid);
	      hsa1->Fill(aerph1);
	      hsa2->Fill(aerph2);
	    }
	}
    }

  // The HBD
  //=========
  // There are only two quantities per cell in HbdMiniCellList

  HbdHitMap *hbdmap  = findNode::getClass<HbdHitMap>(topNode, "HbdMiniCellList_comp");
  if(hbdmap)
    {
      hhbdentries->Fill( (float) hbdmap->get_nCells());

      for(int i=0; i<hbdmap->get_nCells();i++)
	{
	  const HbdHitMapEntry *hbdhit = hbdmap->get_cell(i);

	  short int adcch = hbdhit->get_adcch();
	  short int charge = hbdhit->get_charge();
	  
	  hhbdadcch->Fill( (float) adcch);
	  hhbdcharge->Fill( (float) charge);
	}
    }
  else
    {
      //cout << PHWHERE << "Did not find HbdMiniCellList_comp!" << endl;
    }

  // The EMCal matching
  //===================

  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);

      // Select electrons
      if(sngl->get_n0() > 1 && 
	 sngl->get_disp() < 5 )
	{
	  hecore->Fill(sngl->get_ecore());
	  hmom->Fill(sngl->get_mom());
	  heoverp->Fill(sngl->get_ecore()/sngl->get_mom());
	}
    }


  // DCH
  //=======

  for (unsigned int i = 0;i < cnt->get_npart();i++)
    {
      PHSnglCentralTrack *sngl = cnt->get_track(i);

      hnx1->Fill(sngl->get_nx1hits());
      hnx2->Fill(sngl->get_nx2hits());
      hqual->Fill(sngl->get_quality());
      hzed->Fill(sngl->get_zed());
      hphi->Fill(sngl->get_phi());
      hbeta->Fill(sngl->get_beta());      
    }


  // pad chambers - get the list of PC1 hits associated with tracks
  //================================================================

  // This is just to get the number of clusters
  int clusters[3] = {3*0};

  ostringstream PhobjectNodeName;
  for (short int j = 1; j <= 3; j++)
    {
      PhobjectNodeName.str("");
      PhobjectNodeName << "Pc" << j << "Hit_VarArray";
      
      VariableArray *hitarray = findNode::getClass<VariableArray>(topNode, PhobjectNodeName.str());
      
      if (hitarray)
        {
	  clusters[j-1] = hitarray->get_array_size()/4;
	  //cout << PhobjectNodeName.str().c_str() << " Variable array size = " << clusters[j-1] << " clusters " << endl;
	}
    }

  // This gets the hits information  

  PadHitMap *padmap[3];
  ostringstream tmpstream;
  for (int k = 0; k < 3;k++)
    {
      tmpstream.str(""); // reset tmpstream
      tmpstream << "Pc" << k + 1 << "Hit_comp";
      padmap[k]  = findNode::getClass<PadHitMap>(topNode, tmpstream.str());
      if (!padmap[k])
        {
          cout << "Did not find " << tmpstream.str().c_str()  << endl;
	  continue;
        }
    }

  //padmap[2]->identify();

  int index = 0;
  int npc1hits = 0;

  while(npc1hits < clusters[2])
    {
      //cout << "index " << index << " npc3hits " << npc1hits << " clusters " << clusters[2] << endl;

      const PadHitMapEntry *pchit = padmap[2]->GetHit(index);
      if(pchit)
	{
	  //cout << "  PC3 hit " << npc1hits << "  xyz(2) " << pchit->get_xyz(2) << endl;
	  npc1hits++;
	}
      index++;
    }

  //cout << " Number of PC3 hits was " << npc1hits << endl;

  hpc1assoc->Fill(npc1hits);


  // Get the charged veto information
  //===================================

  emcClusterContainer *emccont  = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");

  int emcid = 0;
  while(1)
    {
      emcClusterContent *emcclus = emccont->getCluster(emcid);
      if(!emcclus)
	break;

      if(emcclus->emcpc3() >= 0)
	{
	  hemcpc3dphi->Fill(emcclus->emcpc3dphi());
	  hemcpc3dz->Fill(emcclus->emcpc3dz());      
	  // index of closest high quality track to pc3 hit/closest pc3z track to emc hit/the dchtrack/cgltrack/phcentraltrack index
	  hemcpc3neartrk->Fill((float)emcclus->emcpc3neartrk());      
	  hemctrk->Fill((float)emcclus->emctrk());    // closest track to emc hit
	  hemctrkdz->Fill(emcclus->emctrkdz());  // distance dz (cm) of closest track
	  hemctrkdphi->Fill(emcclus->emctrkdphi());   // distance dphi (radians) of closest track
	  hpemctrk->Fill(emcclus->pemctrk());    // mom of closest track (not pt)
	  hemctrkquality->Fill((float)emcclus->emctrkquality());   // quality of closest track 
	}

      emcid++;
    }

  //===============================================
  // Get the CrHits_comp node and get the hits list
  //===============================================

  // set to true for testing with a few events only!
  bool print_details = false;

  CrkHitMap *crkmap;
  crkmap  = findNode::getClass<CrkHitMap>(topNode, "CrkHit_comp");
  if (!crkmap)
    {
      cout << PHWHERE << "CrkHit_comp not found!" << endl;
    }
  else
    {
      unsigned int ncrkhits = crkmap->GetNentries();

      if(print_details)  
	cout << "CrkHit_comp contains " << ncrkhits << " hits " << endl;
    
      if(ncrkhits > 0)
	{
	  for(unsigned int i=0;i<ncrkhits;i++)
	    {
	      const CrkHitMapEntry *crkhit = crkmap->GetHit(i);
	      if(crkhit)
		{
		  int pmtid = crkhit->get_pmtid();
		  float npe = crkhit->get_npe();
		  float time = crkhit->get_time();		  

		  if(print_details)
		    cout << "  pmtid " << pmtid
			 << " npe " << npe 
			 << " time " << time
			 << endl;
		}
	      else
		{
		  cout << PHWHERE << " CrkHitMap Hit " << i << " does not have a valid CrkHitMapEntry - Quit!" << endl;
		  exit(1);
		}
	    }
	}
    }



  //=========================
  // CrkAssocHits test code
  //=========================

  // Radius limits for n0 (these are d_Rmin and d_Rmax in CrkPID, Rmin and Rmax in the constructor call)
  //     SetPidParameters(5.9,3.4,8.4,11.0,8.4);
  //     SetPidParameters(float R0, float Rmin, float Rmax, float R1, float R2)
  double R0min = 3.4;
  double R0max = 8.4;

  // Get the associated hits node
  CrkAssocHits *crkassochits = findNode::getClass<CrkAssocHits>(topNode, "CrkAssocHits");
  if(crkassochits)
    {  
      int nentries = crkassochits->GetNentries();
            
      if(print_details)
	cout << endl << "TestNewout: event " << nevts-1 << " Number of CrkAssocHits entries " << nentries << endl;

      for(int i=0;i<nentries;i++)
	{
	  // Note that a single track can occasionally have two entries in CrkAssocHits - one straight and one swapped

	  const CrkAssocHitsEntry *assocentry = crkassochits->GetHit(i);
	  
	  int trackid = assocentry->get_trackid();
	  bool swapped = assocentry->get_swapped();
	  int npmts = assocentry->get_npmts();

	  if(print_details)
	    {	  
	      cout << "Entry " << i << ":" << endl;
	      cout << "  trackid " << trackid
		   << " swapped " << swapped
		   << "  npmts " << npmts
		   << endl;
	    }

	  // Reconstruct the n1, n0 and npe1 and npe0 for the ring 
	  int n0_assoc = 0;
	  int n1_assoc = 0;
	  double npe0_assoc = 0.0;
	  double npe1_assoc = 0.0;

	  for(int ipmt = 0;ipmt<npmts;ipmt++)
	    {
	      if(print_details)
		cout << "    pmtid " << assocentry->get_pmtid(ipmt)
		     << " npe " << assocentry->get_npe(ipmt)
		     << " time " << assocentry->get_time(ipmt)
		     << " rpmt " << assocentry->get_rpmt(ipmt)
		     << endl;

	      n1_assoc++;
	      npe1_assoc += assocentry->get_npe(ipmt);

	      if(assocentry->get_rpmt(ipmt) < R0max && assocentry->get_rpmt(ipmt) > R0min)
		{
		  n0_assoc++;
		  npe0_assoc += assocentry->get_npe(ipmt);
		}
	    }
	  
	  if(print_details)
	    cout << "  Reconstructed: n0 " << n0_assoc
		 << " npe0 " << npe0_assoc
		 << " n1 " << n1_assoc
		 << " npe1 " << npe1_assoc
		 << endl;


	  // Find this track in PHCentralTrack
	  PHSnglCentralTrack *sngl = cnt->get_track(assocentry->get_trackid());  

	  int n0,n1;
	  float npe0,npe1;
	  int cnt_nswapped = 2;

	  if(swapped)
	    {
	      // get the swapped ring fields for comparison

	      n0=sngl->get_sn0();
	      n1=sngl->get_sn1();
	      npe0=sngl->get_snpe0();
	      npe1=sngl->get_snpe1();
	      if(sngl->get_sring() >= 0)
		cnt_nswapped=1;
	      
	      if(print_details)
		cout << "  PHCentralTrack: n0 " << sngl->get_sn0()
		     << " npe0 " << sngl->get_snpe0()
		     << " n1 " << sngl->get_sn1()
		     << " npe1 " << sngl->get_snpe1()
		     << endl;
	    }
	  else
	    {
	      // get the straight ring fields for comparison
	      
	      n0=sngl->get_n0();
	      n1=sngl->get_n1();
	      npe0=sngl->get_npe0();
	      npe1=sngl->get_npe1();
	      if(sngl->get_ring() >= 0)
		cnt_nswapped=0;
	      
	      if(print_details)
		cout << "  PHCentralTrack: n0 " << sngl->get_n0()
		     << " npe0 " << sngl->get_npe0()
		     << " n1 " << sngl->get_n1()
		     << " npe1 " << sngl->get_npe1()
		     << endl;
	    }


	  // Complain if any discrepancy is found between CrkAssocHits results and CNT
	  if(n0_assoc != n0)
	    cout << "  Event " << nevts-1 << " mismatch: n0 " << n0 << " n0_assoc " << n0_assoc << endl; 
	  if(n1_assoc != n1)
	    cout << "  Event " << nevts-1 << " mismatch: n1 " << n1 << " n1_assoc " << n1_assoc << endl; 
	  if(npe0_assoc != npe0)
	    cout << "  Event " << nevts-1 << " mismatch: npe0 " << npe0 << " npe0_assoc " << npe0_assoc << endl; 
	  if(npe1_assoc != npe1)
	    cout << "  Event " << nevts-1 << " mismatch: npe1 " << npe1 << " npe1_assoc " << npe1_assoc << endl; 
	  if(swapped != cnt_nswapped)
	    cout << "  Event " << nevts-1 << " mismatch: swapped " << swapped << " cnt_nswapped " << cnt_nswapped << endl; 

	}
    }
  
  else
    cout << "Failed to find the CrkAssocHits node" << endl;
  
  // PHGlobal_CENTRAL Z vertex
  //===========================
  
  PHGlobal_Central *global_central = findNode::getClass<PHGlobal_Central>(topNode, "PHGlobal_CENTRAL");
  hpc1hits->Fill( global_central->getNumberPC1Hits() );

  // PHGlobal

  PHGlobal *global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");

  hzvert->Fill( global->getBbcZVertex() );

  return EVENT_OK;
}

int
TestNewOut::End(PHCompositeNode *topNode)
{

#ifdef DUMP
  dumpfile.close();
#endif

  cout << "TestNewout::End - processed " << nevts << " events" << endl;

  std::string histname;
  if(compactCNT)    
    {
      histname.assign("testnewout_new.root");
    }
  else
    {
      histname.assign("testnewout_old.root");
    }

  cout << "TestNewOut path is " << histoutpath.c_str() << " and filename is " << histname.c_str() << endl;
  std::string newhist = histoutpath + histname;

  if(nevts == 0)
    {
      cout << "There were no events in the file, I will not write any histograms" << endl;
      return 0;
    }

  cout << "Writing TestNewOut histogram to " << newhist.c_str() << endl;

  histout = new TFile(newhist.c_str(),"recreate");

  //RICH

  hn0->Write();
  hn1->Write();
  hn2->Write();
  hn3->Write();
  hnpe0->Write();
  hnpe1->Write();
  hnpe2->Write();
  hnpe3->Write();
  hchi2->Write();
  hdisp->Write();
  hcross_phi->Write();
  hcenter_phi->Write();
  hcross_z->Write();
  hcenter_z->Write();

  hsn0->Write();
  hsn1->Write();
  hsn2->Write();
  hsn3->Write();
  hsnpe0->Write();
  hsnpe1->Write();
  hsnpe2->Write();
  hsnpe3->Write();
  hschi2->Write();
  hsdisp->Write();

  // TEC

  htc->Write();
  htnt->Write();
  htavgt->Write();
  htdphi->Write();
  htnplane->Write();

  hstc->Write();
  hstnt->Write();
  hstavgt->Write();
  hstdphi->Write();
  hstnplane->Write();

  // Aerogel

  hid->Write();
  ha1->Write();
  ha2->Write();
  
  hsid->Write();
  hsa1->Write();
  hsa2->Write();

  // HBD

  hhbdadcch->Write();
  hhbdcharge->Write();
  hhbdentries->Write();
  
  // EMCal

  hecore->Write();
  hmom->Write();
  heoverp->Write();

  // DCH

  hnx1->Write();
  hnx2->Write();
  hqual->Write();
  hzed->Write();
  hphi->Write();
  hbeta->Write();

  // EmcPc3

  hemcpc3dphi->Write();
  hemcpc3dz->Write();
  hemcpc3neartrk->Write();
  hemctrk->Write();
  hemctrkdz->Write();
  hemctrkdphi->Write();
  hpemctrk->Write();
  hemctrkquality->Write();

  // Global

  hzvert->Write();
  hpc1hits->Write();
  hpc1assoc->Write();
  
  histout->Close();

  cout << "Wrote histograms to file " <<  newhist.c_str() << endl;

  // Output the trigger statistics now

  cout << endl << endl << "Summary of Level 1 scaled triggers:" << endl << endl;

  cout << setw(12) << "Lvl1 bit" << setw(36) << "Name" << setw(12) << "Scaled"
       << setw(12) << endl;

  for (int ilvl1 = 0; ilvl1 < 32; ilvl1++)
    {
      cout << setw(12) << ilvl1
           << setw(36) << Lvl1Name[ilvl1]
           << setw(12) << Lvl1Scaled[ilvl1]
           << endl;
    }

  return 0;
}

void
TestNewOut::SetHistPath(const std::string &path)
{
  histoutpath.assign(path);  
  cout << "Set the TestNewOut  histogram output directory to " << histoutpath.c_str() << endl;

  return;
}
