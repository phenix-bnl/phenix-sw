//******************************************************************************
//
// HbdWisClusterizer class
//
// Purpose: clustering for HBD Cherenkov response
//
// Authors:
//
// Ilia Ravinovich (WIS) - Ilia.Ravinovich@weizmann.ac.il
// Yosuke Watanabe(Tokyo University) - yosuke@nucl.phys.s.u-tokyo.ac.jp
// Mihael Makek (WIS) - mihael.makek@weizmann.ac.il
//
// Revisions: 07/23/09, v1.0, IR
//	      04/29/10,
//            26/11/10
//            03/02/11
//            08/03/12
//
// Please contact the authors before committing any changes
// to this file to the PHENIX repository.
//
//******************************************************************************

#include "HbdWisClusterizer.h"

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <algorithm>
#include <cmath>

//
// Fun4All and PHENIX headers
//
#include "phool.h"
#include "PHCompositeNode.h"
#include "PHNodeIterator.h"
#include "getClass.h"
#include "recoConsts.h"
#include "Fun4AllReturnCodes.h"
#include "Fun4AllServer.h"
#include "PHGlobal.h"
#include "PHParticle.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "PHAngle.h"

#include "HbdCellList.h"
#include "HbdBlobList.h"
#include "HbdBlobListv1.h"
#include "HbdCell.h"
#include "HbdFinalSimSupport.h"
#include "HbdNeighboursV2.h"


using namespace std;
using namespace findNode;

HbdWisClusterizer::HbdWisClusterizer(const char *NodeName,
		const char *CNTName): SubsysReco("HbdWisClusterizer")
{
	d_NodeName = NodeName;
	d_CNTName=CNTName;
}

int HbdWisClusterizer::Init(PHCompositeNode *topNode)
{ // Init
	simsupport.Init();

	//
	// Initialize the flags for MC and data
	//
	isSimulation = isEmbedding = isHijing = false;
	Run9_pp = Run10_AuAu = false;


	//
	// Fetch alignment from the correct run number ...
	//
	recoConsts *rc = recoConsts::instance();
	int set = rc->get_IntFlag("HBD_MC",0);

	//
	// Default run # for the geometry
	//
	runnum = 215034;

	//
	// Data ...
	//
	if (set==0)
	{
		int runtmp = rc->get_IntFlag("RUNNUMBER");

		//
		// Run-10?
		if (runtmp >= 299769)
		{
			Run10_AuAu = true;
			runnum = runtmp;
		}
		//
		// Run-9?
		//
		else
		{
			Run9_pp = true;
		}
	}
	//
	// MC simulation ...
	//
	else if (set==1)
	{ // Single particle simulation
		isSimulation = true;
	}
	else if (set==2)
	{ // Single particle simulation + embedding
		isEmbedding = true;
	}
	else if (set==3)
	{ // For HIJING events
		isHijing = true;
	}

	hbdgeo.fetch(runnum);
	hbdgeo.fetchPad(runnum);


	//Cluster merging
	int mrg = 1;
	mrg = rc->get_IntFlag("HBD_WISCL_MERGE",1);

	//Default is to merge (Run-9)
	MERGE = true;
	if (mrg == 0) MERGE = false; //only if set explicitly to 0

	//Use center of gravity (default is false)
	USE_COG = false;
	int cog_flag = 0;
	cog_flag = rc->get_IntFlag("HBD_WISCL_COG",0);
	if (cog_flag == 1) USE_COG = true;

	//
	// Define the pad areas
	//
	pad_area[0]   = 698.279;
	pad_area[1]   = 698.279;
	pad_area[2]   = 312.791;
	pad_area[3]   = 631.056;
	pad_area[4]   = 631.056;
	pad_area[5]   = 631.056;
	pad_area[6]   = 631.056;
	pad_area[7]   = 631.056;
	pad_area[8]   = 631.056;
	pad_area[9]   = 631.056;
	pad_area[10]  = 312.791;
	pad_area[11]  = 623.175;
	pad_area[12]  = 623.175;
	pad_area[13]  = 623.175;
	pad_area[14]  = 623.175;
	pad_area[15]  = 623.175;
	pad_area[16]  = 623.175;
	pad_area[17]  = 623.175;
	pad_area[18]  = 623.175;
	pad_area[19]  = 308.496;
	pad_area[20]  = 623.175;
	pad_area[21]  = 623.175;
	pad_area[22]  = 623.175;
	pad_area[23]  = 623.175;
	pad_area[24]  = 623.175;
	pad_area[25]  = 623.175;
	pad_area[26]  = 623.175;
	pad_area[27]  = 308.496;
	pad_area[28]  = 623.175;
	pad_area[29]  = 623.175;
	pad_area[30]  = 623.175;
	pad_area[31]  = 623.175;
	pad_area[32]  = 623.175;
	pad_area[33]  = 623.175;
	pad_area[34]  = 623.175;
	pad_area[35]  = 623.175;
	pad_area[36]  = 308.496;
	pad_area[37]  = 623.175;
	pad_area[38]  = 623.175;
	pad_area[39]  = 623.175;
	pad_area[40]  = 623.175;
	pad_area[41]  = 623.175;
	pad_area[42]  = 623.175;
	pad_area[43]  = 623.175;
	pad_area[44]  = 308.496;
	pad_area[45]  = 603.632;
	pad_area[46]  = 623.175;
	pad_area[47]  = 623.175;
	pad_area[48]  = 623.175;
	pad_area[49]  = 623.175;
	pad_area[50]  = 623.175;
	pad_area[51]  = 623.175;
	pad_area[52]  = 603.632;
	pad_area[53]  = 308.496;
	pad_area[54]  = 623.175;
	pad_area[55]  = 623.175;
	pad_area[56]  = 623.175;
	pad_area[57]  = 623.175;
	pad_area[58]  = 623.175;
	pad_area[59]  = 623.175;
	pad_area[60]  = 623.175;
	pad_area[61]  = 308.496;
	pad_area[62]  = 623.175;
	pad_area[63]  = 623.175;
	pad_area[64]  = 623.175;
	pad_area[65]  = 623.175;
	pad_area[66]  = 623.175;
	pad_area[67]  = 623.175;
	pad_area[68]  = 623.175;
	pad_area[69]  = 623.175;
	pad_area[70]  = 308.496;
	pad_area[71]  = 623.175;
	pad_area[72]  = 623.175;
	pad_area[73]  = 623.175;
	pad_area[74]  = 623.175;
	pad_area[75]  = 623.175;
	pad_area[76]  = 623.175;
	pad_area[77]  = 623.175;
	pad_area[78]  = 308.496;
	pad_area[79]  = 623.175;
	pad_area[80]  = 623.175;
	pad_area[81]  = 623.175;
	pad_area[82]  = 623.175;
	pad_area[83]  = 623.175;
	pad_area[84]  = 623.175;
	pad_area[85]  = 623.175;
	pad_area[86]  = 623.175;
	pad_area[87]  = 291.857;
	pad_area[88]  = 597.443;
	pad_area[89]  = 597.443;
	pad_area[90]  = 597.443;
	pad_area[91]  = 577.626;
	pad_area[92]  = 597.443;
	pad_area[93]  = 597.443;
	pad_area[94]  = 597.443;
	pad_area[95]  = 291.857;
	pad_area[96]  = 593.261;
	pad_area[97]  = 597.443;
	pad_area[98]  = 597.443;
	pad_area[99]  = 587.834;
	pad_area[100] = 587.834;
	pad_area[101] = 597.443;
	pad_area[102] = 597.443;
	pad_area[103] = 593.261;
	pad_area[104] = 308.496;
	pad_area[105] = 623.175;
	pad_area[106] = 623.175;
	pad_area[107] = 623.175;
	pad_area[108] = 623.175;
	pad_area[109] = 623.175;
	pad_area[110] = 623.175;
	pad_area[111] = 623.175;
	pad_area[112] = 308.496;
	pad_area[113] = 623.175;
	pad_area[114] = 623.175;
	pad_area[115] = 623.175;
	pad_area[116] = 623.175;
	pad_area[117] = 623.175;
	pad_area[118] = 623.175;
	pad_area[119] = 623.175;
	pad_area[120] = 623.175;
	pad_area[121] = 308.496;
	pad_area[122] = 623.175;
	pad_area[123] = 623.175;
	pad_area[124] = 623.175;
	pad_area[125] = 623.175;
	pad_area[126] = 623.175;
	pad_area[127] = 623.175;
	pad_area[128] = 623.175;
	pad_area[129] = 308.496;
	pad_area[130] = 623.175;
	pad_area[131] = 623.175;
	pad_area[132] = 623.175;
	pad_area[133] = 623.175;
	pad_area[134] = 623.175;
	pad_area[135] = 623.175;
	pad_area[136] = 623.175;
	pad_area[137] = 623.175;
	pad_area[138] = 288.679;
	pad_area[139] = 623.175;
	pad_area[140] = 623.175;
	pad_area[141] = 623.175;
	pad_area[142] = 623.175;
	pad_area[143] = 623.175;
	pad_area[144] = 623.175;
	pad_area[145] = 623.175;
	pad_area[146] = 288.679;
	pad_area[147] = 623.175;
	pad_area[148] = 623.175;
	pad_area[149] = 623.175;
	pad_area[150] = 623.175;
	pad_area[151] = 623.175;
	pad_area[152] = 623.175;
	pad_area[153] = 623.175;
	pad_area[154] = 623.175;
	pad_area[155] = 308.496;
	pad_area[156] = 623.175;
	pad_area[157] = 623.175;
	pad_area[158] = 623.175;
	pad_area[159] = 623.175;
	pad_area[160] = 623.175;
	pad_area[161] = 623.175;
	pad_area[162] = 623.175;
	pad_area[163] = 308.496;
	pad_area[164] = 623.175;
	pad_area[165] = 623.175;
	pad_area[166] = 623.175;
	pad_area[167] = 623.175;
	pad_area[168] = 623.175;
	pad_area[169] = 623.175;
	pad_area[170] = 623.175;
	pad_area[171] = 623.175;
	pad_area[172] = 308.496;
	pad_area[173] = 623.175;
	pad_area[174] = 623.175;
	pad_area[175] = 623.175;
	pad_area[176] = 623.175;
	pad_area[177] = 623.175;
	pad_area[178] = 623.175;
	pad_area[179] = 623.175;
	pad_area[180] = 308.496;
	pad_area[181] = 631.056;
	pad_area[182] = 631.056;
	pad_area[183] = 631.056;
	pad_area[184] = 631.056;
	pad_area[185] = 631.056;
	pad_area[186] = 631.056;
	pad_area[187] = 631.056;
	pad_area[188] = 631.056;
	pad_area[189] = 470.138;
	pad_area[190] = 454.503;
	pad_area[191] = 470.138;

	for (int k=0;k<192;k++) pad_area[k] *= 0.01; //[mm2]-->[cm2]

	//
	// Create output node in the node tree
	//
	PHNodeIterator iter(topNode);
	PHCompositeNode *dstNode = static_cast<PHCompositeNode *>(iter.findFirst("PHCompositeNode","DST"));

	if(!dstNode)
	{
		cout << "dstNode not found"<<endl;
		return -1;
	}

	BlobList = findNode::getClass<HbdBlobList>(topNode, "HbdBlobList");

	//
	// HbdBlob Container
	//
	if (!BlobList)
	{
		HbdBlobList * d_blob = new HbdBlobListv1();
		PHIODataNode<HbdBlobList> *blob = new PHIODataNode<HbdBlobList>(d_blob, "HbdBlobList","PHObject");
		dstNode->addNode(blob);
	}
	else
	{
		cout << PHWHERE << "HbdWisClusterizer:: HbdBlobList is already there. Funny!" << endl;
		return -1;
	}

	ClusterList = findNode::getClass<HbdWisClusterList>(topNode, "HbdWisClusterList");

	d_clust_list = 0;

	if (!ClusterList)
	{

		d_clust_list = new HbdWisClusterList();
		clust_node = new PHIODataNode<HbdWisClusterList>(d_clust_list, "HbdWisClusterList", "PHObject");
		dstNode->addNode(clust_node);

		cout << PHWHERE << "Adding HbdWisClusterList"  << endl;
	}
	else
	{
		cout << PHWHERE << "HbdWisClusterizer:: HbdClusterList is already there. Funny!" << endl;
	}


	return 0;
}

int HbdWisClusterizer::process_event(PHCompositeNode *topNode)
{

       //PHGlobal *global = getClass<PHGlobal>(topNode,"PHGlobal");
       //float vertex = global->getBbcZVertex();

       //if (fabs(vertex)>20) return EVENT_OK;

	CellList = findNode::getClass<HbdCellList>(topNode, "HbdCellList");
	if (!CellList) CellList = findNode::getClass<HbdCellList>(topNode, "MEGACellList");
	if (!CellList) cout << PHWHERE << "HbdWisClusterizer:: No HbdCellList!" << endl;

	BlobList = findNode::getClass<HbdBlobList>(topNode, "HbdBlobList");
	if (!BlobList) cout << PHWHERE << "HbdWisClusterizer:: No HbdBlobList!" << endl;

	PHCentralTrack *trk = getClass<PHCentralTrack>(topNode,"PHCentralTrack");
	if(!trk) trk = getClass<PHCentralTrack>(topNode,"EWGCentralTrack");
	if(!trk) trk = getClass<PHCentralTrack>(topNode,"MEGACentralTrack");
	if(!trk) trk = getClass<PHCentralTrack>(topNode,d_CNTName.c_str());
	if(!trk) cout << PHWHERE << "HbdWisClusterizer:: No PHCentralTrack!" << endl;




	//
	// Get the number of CA tracks
	//
	int ntrk = trk->get_npart();

	//
	// Test if run looks uncalibrated
	//
	if (CellList->get_nCells()==0 or
			std::isnan(CellList->get_cell(0)->get_charge()) or
			std::isinf(CellList->get_cell(0)->get_charge()))
		return EVENT_OK;

	int Ncells = CellList->get_nCells();
	if (Ncells > 2304)
	{
		cout << PHWHERE << " too many cells: " << Ncells
			<< " max is 2304" << endl;
		exit(1);
	}


	//
	// Here is the departure point for the clustering
	//
	if (ntrk<1) return EVENT_OK;

	fill_n(CellIndex[0], sizeof(CellIndex)/sizeof(**CellIndex), -1);

	//
	// These are the threshold for the clusterization
	//
	float pad_threshold = 1.;
	float master_pad_threshold = 3.;

	for (int icl=0; icl<CellList->get_nCells(); icl++)
	{
		int pn = CellList->get_cell(icl)->get_padnum();
		int sc = CellList->get_cell(icl)->get_sector();
		float ch = CellList->get_cell(icl)->get_charge();

		if (pn<0) continue;
		if (sc<0) continue;
		if (ch < pad_threshold) continue;

		CellIndex[CellList->get_cell(icl)->get_padnum()][CellList->get_cell(icl)->get_sector()] = icl;
	}

	int seed_ind=-999;
	int nHbdBlobs = 0;
	int nHbdBlobsTmp;
	BlobList->set_nBlobs(0);
	int SeedId = -1;
	int padnum = -1;

	fill_n(CellInClstIndex[0], sizeof(CellInClstIndex)/sizeof(**CellInClstIndex), -1);

	//
	// Loop over all HBD fired cells
	//
	for (int icl=0; icl<CellList->get_nCells(); icl++)
	{ // cell loop

		padnum = CellList->get_cell(icl)->get_padnum();
		if (padnum==0 || padnum==1 || padnum==189 || padnum==190 || padnum==191) continue;

		//
		// Threshold amplitude for the closest pad to the projection point of the Central arm electron
		//
		if (CellList->get_cell(icl)->get_charge()<=master_pad_threshold) continue; // threshold amplitude

		seed_ind = icl;
		float SeedCharge = -9999.;
		int SeedPads = -9999;
		int SeedSector = -1;
		int SeedPadNumber = -1;
		float SeedPosY = 0.0;
		float SeedPosZ = 0.0;
		int SeedPadKey = -1;

		//
		// Look around the seed pad
		//
		if (seed_ind<0) continue; // look around the seed pad

		SeedPadNumber = CellList->get_cell(seed_ind)->get_padnum();
		if (SeedPadNumber<0) continue;
		SeedSector = CellList->get_cell(seed_ind)->get_sector();
		if (SeedSector<0) continue;
		SeedCharge = CellList->get_cell(seed_ind)->get_charge();
		SeedPosY += (simsupport.get_pad_center(SeedPadNumber,1)+22.9*SeedSector)*SeedCharge ;
		SeedPosZ += simsupport.get_pad_center(SeedPadNumber,0)*SeedCharge;
		SeedPads = 1;
		SeedPadKey = SeedSector*192+SeedPadNumber;
		SeedId++;

		CellInClstIndex[nHbdBlobs][SeedPads-1] = seed_ind;
		for (int i=0; i<8; i++)
		{ // sum 6 pads around the central pad

			int padkey = pad_neighbours[SeedPadKey][i];
			if (padkey<=0) continue;
			int pad_temp = padkey%192;
			if (pad_temp<0) continue;
			int sect_temp = padkey/192;
			if (sect_temp<0) continue;
			if (CellIndex[pad_temp][sect_temp]>=0)
			{
				SeedPosY += (simsupport.get_pad_center(pad_temp,1)+22.9*sect_temp)*CellList->get_cell(CellIndex[pad_temp][sect_temp])->get_charge();
				SeedPosZ += simsupport.get_pad_center(pad_temp,0)*CellList->get_cell(CellIndex[pad_temp][sect_temp])->get_charge();
				SeedCharge += CellList->get_cell(CellIndex[pad_temp][sect_temp])->get_charge();
				SeedPads++;
				CellInClstIndex[nHbdBlobs][SeedPads-1] = CellIndex[pad_temp][sect_temp];
			}
		}

		PadInCluster pad_in_preblob[SeedPads];
		for (int k=0; k<SeedPads; k++)
		{
			pad_in_preblob[k].index = CellInClstIndex[nHbdBlobs][k];
			pad_in_preblob[k].charge = CellList->get_cell(CellInClstIndex[nHbdBlobs][k])->get_charge();
			int padnum_tmp = CellList->get_cell(CellInClstIndex[nHbdBlobs][k])->get_padnum();
			pad_in_preblob[k].positionY = simsupport.get_pad_center(padnum_tmp,1);
			pad_in_preblob[k].positionZ = simsupport.get_pad_center(padnum_tmp,0);
			pad_in_preblob[k].sector = CellList->get_cell(CellInClstIndex[nHbdBlobs][k])->get_sector();
		}

		//
		// Sort the pads in the cluster by charge
		//
		std::sort(pad_in_preblob, pad_in_preblob + SeedPads, sort_by_charge);

		SeedPosY = SeedPosY/SeedCharge;
		SeedPosZ = SeedPosZ/SeedCharge;

		int BlobSector = static_cast<int> ((SeedPosY+11.45)/22.9);
		SeedPosY = (SeedPosY - 22.9*BlobSector);

		//
		// Local coordinates to global
		//
		double x_glob_m, y_glob_m, z_glob_m;

		hbdgeo.LocToGlob(SeedPosY, SeedPosZ, x_glob_m, y_glob_m, z_glob_m, BlobSector);

		//
		// Store the cluster information
		//
		nHbdBlobsTmp = BlobList->get_nBlobs();
		BlobList->AddBlob(nHbdBlobsTmp);
		BlobList->set_nBlobs(nHbdBlobsTmp+1);
		HbdBlob *d_blob = BlobList->get_blob(nHbdBlobsTmp);
		d_blob->set_charge(SeedCharge);
		d_blob->set_sector(BlobSector);
		d_blob->set_size(SeedPads);
		d_blob->set_id(SeedId);
		d_blob->set_bloby_local(SeedPosY);
		d_blob->set_blobz_local(SeedPosZ);
		d_blob->set_nlocalmax(0);
		d_blob->set_parentid(1);
		d_blob->set_blobx(x_glob_m);
		d_blob->set_bloby(y_glob_m);
		d_blob->set_blobz(z_glob_m);
		for (int k=0; k<SeedPads; k++)
		{
			d_blob->set_charge_pad(k,pad_in_preblob[k].charge);
			d_blob->set_pady_local(k,pad_in_preblob[k].positionY);
			d_blob->set_padz_local(k,pad_in_preblob[k].positionZ);
			d_blob->set_pad_sector(k,pad_in_preblob[k].sector);
		}
		nHbdBlobs++;
	}


	if (MERGE == true){ //Start merging

		int nHbdBlobsOld = BlobList->get_nBlobs();

		//
		// Initialize the cluster counters
		//
		int ClusterId = -1;
		int nHbdBlobsNew;
		BlobList->set_nBlobs(0);
		int nHbdBlobsNewTmp = 0;

		//
		// Arrays for the pads and clusters
		//
		fill_n(cluster_used, sizeof(cluster_used)/sizeof(*cluster_used), -1);
		fill_n(CellInClstIndexNew[0], sizeof(CellInClstIndexNew)/sizeof(**CellInClstIndexNew), -1);

		std::vector<short> blobSize_cache(nHbdBlobsOld, -1);
		for (int a = 0; a < nHbdBlobsOld; a++) blobSize_cache[a] = BlobList->get_blob(a)->get_size();

		//
		// Loop over all existing HBD clusters in this event
		//
		for (int j = 0; j < nHbdBlobsOld; j++)
		{ // cluster loop # 1

			//
			// Do not use the clusters from the 2nd cluster loop
			//
			bool RemakeCluster = false;
			bool ClustersFromLoop = false;
			for (int jjj = 0; jjj < nHbdBlobsOld; jjj++)
			{
				if (j==cluster_used[jjj])
				{
					ClustersFromLoop = true;
					break;
				}
			}
			if (ClustersFromLoop) continue;

			//
			// Initialize the cluster variables
			//
			float ClusterCharge = 0;
			int ClusterSize = 0;
			float ClusterPosY = 0.0;
			float ClusterPosZ = 0.0;

			//
			// Arrays for the used pads
			//
			fill_n(cell_index_used[0], sizeof(cell_index_used)/sizeof(**cell_index_used), -1);

			if (nHbdBlobsOld>1)
			{ // go to the cluster loop if the # of clusters > 1

				//
				// Loop over all existing HBD clusters in this event
				//
				for (int jj = j+1; jj < nHbdBlobsOld; jj++)
				{ // cluster loop # 2

					//
					// If the cell indices of the master pads of the 2 clusters are the same but COG
					// is different or the cell index of the master pad of the 2nd cluster is a neighbour of the 1st one
					// we will buid a new cluster from 2 summing up the pads from both but every
					// pad is used only once.
					// Actually it can be that we will produce 1 cluster out of several.
					//

					bool overlappping_cells = false;

					//
					// Loop over pads in the 1st cluster
					//
					for (short i = 0; i < blobSize_cache[j]; ++i)
					{ // cell loop in cluster # 1
						int cell_j = CellInClstIndex[j][i];

						//
						// Loop over pads in the 2nd cluster
						//
						for (short ii = 0; ii < blobSize_cache[jj]; ++ii)
						{ // cell loop in cluster # 2
							int cell_jj = CellInClstIndex[jj][ii];
							if (cell_jj==cell_j){
								overlappping_cells = true;
								break;
							}
						}
					}

					if (overlappping_cells)
					{ // overlapping cells, full merging !!!

						//
						// The flag for a new cluster
						//
						RemakeCluster = true;

						cluster_used[jj] = jj;

						//
						// Loop over pads in the 1st cluster
						//
						for (short i = 0; i < blobSize_cache[j]; ++i)
						{ // cell loop in cluster # 1
							int current_cell_index_j = CellInClstIndex[j][i];
							if (current_cell_index_j<0) continue;
							int current_pad_number_j =CellList->get_cell(current_cell_index_j)->get_padnum();
							if (current_pad_number_j<0) continue;
							int current_pad_sector_j = CellList->get_cell(current_cell_index_j)->get_sector();
							if (current_pad_sector_j<0) continue;

							//
							// Do not use the pads from the 2nd cluster
							//
							bool CellFromOtherCluster1 = false;

							for (int a = 0; a < nHbdBlobsOld; ++a)
							{
								const short blobSize = blobSize_cache[a];
								for (short iii = 0; iii < blobSize; iii++)
								{
									if (current_cell_index_j==cell_index_used[a][iii])
									{
										CellFromOtherCluster1 = true;
										break;
									}
								}
							}

							//
							// Check that the current pad was not used before
							//
							if (cell_index_used[j][i]==-1 and not CellFromOtherCluster1)
							{ // cell not used before

								//
								// The information of the new cluster
								//
								ClusterPosY += (simsupport.get_pad_center(current_pad_number_j,1)+22.9*current_pad_sector_j)*CellList->get_cell(current_cell_index_j)->get_charge();
								ClusterPosZ += simsupport.get_pad_center(current_pad_number_j,0)*CellList->get_cell(current_cell_index_j)->get_charge();
								ClusterCharge += CellList->get_cell(current_cell_index_j)->get_charge();
								ClusterSize++;
								CellInClstIndexNew[nHbdBlobsNewTmp][ClusterSize-1] = current_cell_index_j;

								//
								// Remember this pad
								//
								cell_index_used[j][i] = current_cell_index_j;

								//
								// Loop over pads in the 2nd cluster
								//
								for (short ii = 0; ii < blobSize_cache[jj]; ++ii)
								{ // cell loop in cluster # 2
									int current_cell_index_jj = CellInClstIndex[jj][ii];
									if (current_cell_index_jj<0) continue;
									int current_pad_number_jj =CellList->get_cell(current_cell_index_jj)->get_padnum();
									if (current_pad_number_jj<0) continue;
									int current_pad_sector_jj = CellList->get_cell(current_cell_index_jj)->get_sector();
									if (current_pad_sector_jj<0) continue;

									//
									// Do not use the pads from the 1st cluster
									//
									bool CellFromOtherCluster2 = false;
									for (int a = 0; a < nHbdBlobsOld; a++)
									{
										const short blobSize = blobSize_cache[a];
										for (short iii = 0; iii < blobSize; iii++)
										{
											if (current_cell_index_jj==cell_index_used[a][iii])
											{
												CellFromOtherCluster2 = true;
												break;
											}
										}
									}

									//
									// Check that the current pad was not used before
									//
									if (cell_index_used[jj][ii]==-1 and not CellFromOtherCluster2)
									{ // cell not used before

										//
										// The information of the new cluster
										//
										ClusterPosY += (simsupport.get_pad_center(current_pad_number_jj,1)+22.9*current_pad_sector_jj)*CellList->get_cell(current_cell_index_jj)->get_charge();
										ClusterPosZ += simsupport.get_pad_center(current_pad_number_jj,0)*CellList->get_cell(current_cell_index_jj)->get_charge();
										ClusterCharge += CellList->get_cell(current_cell_index_jj)->get_charge();
										ClusterSize++;
										CellInClstIndexNew[nHbdBlobsNewTmp][ClusterSize-1] = current_cell_index_jj;

										//
										// Remember this pad
										//
										cell_index_used[jj][ii] = current_cell_index_jj;
									}
								}
							}
						}
					}
				}
			}

			//
			// Store a new cluster in the BlobList
			//
			if (RemakeCluster)
			{ // this is a new cluster
				ClusterPosY = ClusterPosY/ClusterCharge;
				ClusterPosZ = ClusterPosZ/ClusterCharge;

				int BlobSector = static_cast<int>( (ClusterPosY+11.45)/22.9);

				ClusterPosY = (ClusterPosY - 22.9*BlobSector);

				//
				// Local coordinates to global
				//
				double x_glob_new_m, y_glob_new_m, z_glob_new_m;
				if (hbdgeo.LocToGlob(ClusterPosY, ClusterPosZ, x_glob_new_m, y_glob_new_m, z_glob_new_m, BlobSector))
				{
					//return ABORTEVENT;
					// use return 0 instead to keep up with event counting.
					return DISCARDEVENT;
				}

				//
				// Store the cluster information
				//
				ClusterId++;
				nHbdBlobsNew = BlobList->get_nBlobs();
				BlobList->AddBlob(nHbdBlobsNew);
				BlobList->set_nBlobs(nHbdBlobsNew+1);
				HbdBlob *d_blob = BlobList->get_blob(nHbdBlobsNew);
				d_blob->set_charge(ClusterCharge);
				d_blob->set_sector(BlobSector);
				d_blob->set_size(ClusterSize);
				d_blob->set_id(ClusterId);
				d_blob->set_bloby_local(ClusterPosY);
				d_blob->set_blobz_local(ClusterPosZ);
				d_blob->set_nlocalmax(0);
				d_blob->set_parentid(1);
				d_blob->set_blobx(x_glob_new_m);
				d_blob->set_bloby(y_glob_new_m);
				d_blob->set_blobz(z_glob_new_m);

				PadInCluster pad_in_blob[ClusterSize];
				for (int k=0; k<ClusterSize; k++)
				{
					pad_in_blob[k].index = CellInClstIndexNew[nHbdBlobsNewTmp][k];
					pad_in_blob[k].charge = CellList->get_cell(CellInClstIndexNew[nHbdBlobsNewTmp][k])->get_charge();
					int padnum_tmp = CellList->get_cell(CellInClstIndexNew[nHbdBlobsNewTmp][k])->get_padnum();
					pad_in_blob[k].positionY = simsupport.get_pad_center(padnum_tmp,1);
					pad_in_blob[k].positionZ = simsupport.get_pad_center(padnum_tmp,0);
					pad_in_blob[k].sector = CellList->get_cell(CellInClstIndexNew[nHbdBlobsNewTmp][k])->get_sector();
				}

				std::sort(pad_in_blob, pad_in_blob + ClusterSize, sort_by_charge);

				for (int k=0; k<ClusterSize; k++)
				{
					d_blob->set_charge_pad(k,pad_in_blob[k].charge);
					d_blob->set_pady_local(k,pad_in_blob[k].positionY);
					d_blob->set_padz_local(k,pad_in_blob[k].positionZ);
					d_blob->set_pad_sector(k,pad_in_blob[k].sector);
				}

				nHbdBlobsNewTmp++;

			} else { // this is an old cluster
				//
				// Store an old cluster cluster that did not require remaking in the BlobList
				//
				float charge_tmp = BlobList->get_blob(j)->get_charge();
				int sector_tmp = BlobList->get_blob(j)->get_sector();
				int size_tmp = BlobList->get_blob(j)->get_size();
				float y_loc_tmp = BlobList->get_blob(j)->get_bloby_local();
				float z_loc_tmp = BlobList->get_blob(j)->get_blobz_local();
				float x_glob_tmp = BlobList->get_blob(j)->get_blobx();
				float y_glob_tmp = BlobList->get_blob(j)->get_bloby();
				float z_glob_tmp = BlobList->get_blob(j)->get_blobz();
				float charge_pad_tmp[size_tmp];
				float charge_posY_tmp[size_tmp];
				float charge_posZ_tmp[size_tmp];
				int pad_sector_tmp[size_tmp];
				for (int k=0; k<size_tmp; k++)
				{
					charge_pad_tmp[k]  = BlobList->get_blob(j)->get_charge_pad(k);
					charge_posY_tmp[k] = BlobList->get_blob(j)->get_pady_local(k);
					charge_posZ_tmp[k] = BlobList->get_blob(j)->get_padz_local(k);
					pad_sector_tmp[k] = BlobList->get_blob(j)->get_pad_sector(k);
				}

				//
				// Store the cluster information
				//
				ClusterId++;
				nHbdBlobsNew = BlobList->get_nBlobs();
				BlobList->AddBlob(nHbdBlobsNew);
				BlobList->set_nBlobs(nHbdBlobsNew+1);
				HbdBlob *d_blob = BlobList->get_blob(nHbdBlobsNew);
				d_blob->set_charge(charge_tmp);
				d_blob->set_sector(sector_tmp);
				d_blob->set_size(size_tmp);
				d_blob->set_id(ClusterId);
				d_blob->set_bloby_local(y_loc_tmp);
				d_blob->set_blobz_local(z_loc_tmp);
				d_blob->set_nlocalmax(0);
				d_blob->set_parentid(1);
				d_blob->set_blobx(x_glob_tmp);
				d_blob->set_bloby(y_glob_tmp);
				d_blob->set_blobz(z_glob_tmp);
				for (int k=0; k<size_tmp; k++)
				{
					d_blob->set_charge_pad(k,charge_pad_tmp[k]);
					d_blob->set_pady_local(k,charge_posY_tmp[k]);
					d_blob->set_padz_local(k,charge_posZ_tmp[k]);
					d_blob->set_pad_sector(k,pad_sector_tmp[k]);
				}
			}
		}

	} // Stop merging

	std::vector<const HbdBlob*> blob_cache;
	for (unsigned int j = 0; j < BlobList->get_nBlobs(); j++) blob_cache.push_back(BlobList->get_blob(j));


	for (int jtrk=0; jtrk<ntrk; jtrk++)
	{

		//
		// Stuff for the electron
		//
		float hbd_px = trk->get_track(jtrk)->get_phbdx();
		float hbd_py = trk->get_track(jtrk)->get_phbdy();
		float hbd_pz = trk->get_track(jtrk)->get_phbdz();
		PHAngle hbdpphi_tmp(atan2(hbd_py,hbd_px));
		double hbdpphi = hbdpphi_tmp;
		short hbd_id = -9999;
		short hbd_size = -9999;
		short hbd_sector = -9999;
		float hbd_dz = -9999;
		float hbd_dphi = -9999;
		float hbd_charge = -9999;
		float hbd_x = -9999;
		float hbd_y = -9999;
		float hbd_z = -9999;
		float  space_dist_blob = 999999;


		//
		//Swapped variables
		float hbd_px_s = -hbd_px;//swapped track projection to hbd
		float hbd_py_s =  hbd_py;//swapped track projection to hbd
		float hbd_pz_s =  hbd_pz;//swapped track projection to hbd
		PHAngle hbd_pphi_tmp_s(atan2(hbd_py_s,hbd_px_s));
		double hbd_pphi_s = hbd_pphi_tmp_s;
		short hbd_id_s = -9999;
//		short hbd_size_s = -9999;
//		short hbd_sector_s = -9999;
		float hbd_dz_s = -9999;
		float hbd_dphi_s = -9999;
		float hbd_charge_s = -9999;
//		float hbd_x_s = -9999;
//		float hbd_y_s = -9999;
//		float hbd_z_s = -9999;
		float space_dist_blob_s = 999999;



		//
		// Loop over all HBD clusters
		//
		for (unsigned int j = 0; j < BlobList->get_nBlobs(); j++)
		{ // cluster loop
			const int high_cell = blob_cache[j]->get_parentid();
			if(high_cell==0) continue;

			//
			// Get the cluster variables from BlobList
			//
			const unsigned int blob_id = blob_cache[j]->get_id();
			float blob_x  = blob_cache[j]->get_blobx();
			float blob_y  = blob_cache[j]->get_bloby();
			float blob_z  = blob_cache[j]->get_blobz();
			const float blob_charge = blob_cache[j]->get_charge();
			const int blob_size = blob_cache[j]->get_size();
			const int blob_sector = blob_cache[j]->get_sector();

			//
			// Run-9 offsets are not in the DB, so we align the detector here
			//
			if (Run9_pp)
			{
				vector<float> offs = hbd_offsets_run9(blob_sector,blob_z);
				blob_x -= offs[0];
				blob_y -= offs[1];
				blob_z -= offs[2];
			}

			if (blob_id>=BlobList->get_nBlobs()) continue;

			//
			// Look for a closest cluster to the projection point of the electron
			//
			PHAngle blob_phi_tmp(atan2(blob_y,blob_x));
			double blob_phi = blob_phi_tmp;
			float space_dist_tmp = sqrt((hbd_px-blob_x)*(hbd_px-blob_x)+
					(hbd_py-blob_y)*(hbd_py-blob_y)+
					(hbd_pz-blob_z)*(hbd_pz-blob_z));
			if (space_dist_tmp<space_dist_blob)
			{ // closest blob
			  
			  if (!USE_COG){ //use center of the closest pad instead of center of gravity
			        //
				// New stuff starts here ...
				//
				float  space_dist_pad = 999999;
				for (int k = 0; k < blob_size; k++)
				{ // pad loop
					float pad_pos_y = BlobList->get_blob(j)->get_pady_local(k);
					float pad_pos_z = BlobList->get_blob(j)->get_padz_local(k);
					int pad_sector = BlobList->get_blob(j)->get_pad_sector(k);

					//
					// Local coordinates to global
					//
					double pad_pos_glob_x, pad_pos_glob_y, pad_pos_glob_z;
					hbdgeo.LocToGlob(pad_pos_y, pad_pos_z, pad_pos_glob_x, pad_pos_glob_y, pad_pos_glob_z, pad_sector);

					//
					// Run-9 offsets are not in the DB, so we align the detector here
					//
					if (Run9_pp)
					{
						vector<float> padoffs = hbd_offsets_run9(pad_sector,pad_pos_glob_z);
						pad_pos_glob_x -= padoffs[0];
						pad_pos_glob_y -= padoffs[1];
						pad_pos_glob_z -= padoffs[2];
					}

					//
					// Look for a closest pad to the projection point of the electron
					//
					PHAngle pad_phi_tmp(atan2(pad_pos_glob_y,pad_pos_glob_x));
					double pad_phi = pad_phi_tmp;
					float space_dist_pad_tmp = sqrt((hbd_px-pad_pos_glob_x)*(hbd_px-pad_pos_glob_x)+
							(hbd_py-pad_pos_glob_y)*(hbd_py-pad_pos_glob_y)+
							(hbd_pz-pad_pos_glob_z)*(hbd_pz-pad_pos_glob_z));
					if (space_dist_pad_tmp<space_dist_pad)
					{ // closest pad
						space_dist_pad = space_dist_pad_tmp;
						hbd_dphi = pad_phi - hbdpphi;
						hbd_dz = pad_pos_glob_z - hbd_pz;
					}
				}
			  } //end of if (!USE_COG)
			  
			        space_dist_blob = space_dist_tmp;
			        if (USE_COG){
				  hbd_dphi = blob_phi - hbdpphi;
				  hbd_dz = blob_z - hbd_pz;
				}
				hbd_charge = blob_charge;
				hbd_id = blob_id;
				hbd_sector = blob_sector;
				hbd_size = blob_size;
				hbd_x = blob_x;
				hbd_y = blob_y;
				hbd_z = blob_z;
			}


			//
			// Look for a closest cluster to the swapped projection point of the electron
			//
			float space_dist_tmp_s = sqrt((hbd_px_s-blob_x)*(hbd_px_s-blob_x)+
					(hbd_py_s-blob_y)*(hbd_py_s-blob_y)+
					(hbd_pz_s-blob_z)*(hbd_pz_s-blob_z));

			if (space_dist_tmp_s<space_dist_blob_s){
				space_dist_blob_s = space_dist_tmp_s;
				
				if (!USE_COG){
				//
				// New stuff starts here ...
				//
				float  space_dist_pad_s = 999999;
				for (int k = 0; k < blob_size; k++){
					float pad_pos_y = BlobList->get_blob(j)->get_pady_local(k);
					float pad_pos_z = BlobList->get_blob(j)->get_padz_local(k);
					int pad_sector = BlobList->get_blob(j)->get_pad_sector(k);

					//
					// Local coordinates to global
					//
					double pad_pos_glob_x, pad_pos_glob_y, pad_pos_glob_z;
					hbdgeo.LocToGlob(pad_pos_y, pad_pos_z, pad_pos_glob_x,
							pad_pos_glob_y, pad_pos_glob_z, pad_sector);

                                       if (Run9_pp)
                                        {
					vector <float> offs = hbd_offsets_run9(pad_sector,pad_pos_glob_z);
					pad_pos_glob_x -= offs[0];
					pad_pos_glob_y -= offs[1];
					pad_pos_glob_z -= offs[2];
					}

					//
					// Look for a closest pad to the swapped projection point of the electron
					//
					PHAngle pad_phi_tmp(atan2(pad_pos_glob_y,pad_pos_glob_x));
					double pad_phi = pad_phi_tmp;
					float space_dist_pad_tmp_s =
						sqrt((hbd_px_s-pad_pos_glob_x)*(hbd_px_s-pad_pos_glob_x)+
								(hbd_py_s-pad_pos_glob_y)*(hbd_py_s-pad_pos_glob_y)+
								(hbd_pz_s-pad_pos_glob_z)*(hbd_pz_s-pad_pos_glob_z));
					if (space_dist_pad_tmp_s<space_dist_pad_s)
					{
						space_dist_pad_s = space_dist_pad_tmp_s;
						hbd_dphi_s = pad_phi - hbd_pphi_s;
						hbd_dz_s = pad_pos_glob_z - hbd_pz_s;
					}
				}
				} //end of if(!USE_COG)
				
				if (USE_COG){
				  hbd_dphi_s = blob_phi - hbd_pphi_s;
				  hbd_dz_s = blob_z - hbd_pz_s;
			        }
				hbd_charge_s = blob_charge;
				hbd_id_s = blob_id;
//				hbd_sector_s = blob_sector;
//				hbd_size_s = blob_size;
//				hbd_x_s = blob_x;
//				hbd_y_s = blob_y;
//				hbd_z_s = blob_z;
			}



		}

		/*
		//Disable tracks for EN2.
		if(pphi_d>180.0 && pphi_d<180.0+22.5 && hbd_pz>0){
		hbd_dphi = -9999;
		hbd_dz = -9999;
		hbd_charge = -9999;
		hbd_id = -9999;
		hbd_sector = -9999;
		hbd_size = -9999;
		hbd_x = -9999;
		hbd_y = -9999;
		hbd_z = -9999;
		}

		//Disable swap for EN2.
		if(spphi_d>180.0 && spphi_d<180.0+22.5 && hbd_pz_s>0){
		hbd_dphi_s = -9999;
		hbd_dz_s = -9999;
		hbd_charge_s = -9999;
		hbd_id_s = -9999;
		hbd_sector_s = -9999;
		hbd_size_s = -9999;
		hbd_x_s = -9999;
		hbd_y_s = -9999;
		hbd_z_s = -9999;
		}

*/

		//
		// Refill HBD stuff in PHCentral with a new derived HBD variables
		//
		trk->get_track(jtrk)->set_hbdcharge(hbd_charge);
		trk->get_track(jtrk)->set_hbddphi(hbd_dphi);
		trk->get_track(jtrk)->set_hbddz(hbd_dz);
		trk->get_track(jtrk)->set_hbdid(hbd_id);
		trk->get_track(jtrk)->set_hbdsector(hbd_sector);
		trk->get_track(jtrk)->set_hbdsize(hbd_size);
		trk->get_track(jtrk)->set_hbdx(hbd_x);
		trk->get_track(jtrk)->set_hbdy(hbd_y);
		trk->get_track(jtrk)->set_hbdz(hbd_z);


		/*
		//swapped stuff
		trk->get_track(jtrk)->set_pc2dz(hbd_charge_s); //temporary fill into pc2dz
		trk->get_track(jtrk)->set_spc2dphi(hbd_dphi_s); //temporary fill into hbdsdphi
		trk->get_track(jtrk)->set_spc2dz(hbd_dz_s); //temporary fill into hbdsdz
		trk->get_track(jtrk)->set_nx1hits(hbd_sector_s); //temporary fill into nx1hits
		trk->get_track(jtrk)->set_nx2hits(hbd_size_s); //temporary fill into nx2hits
		trk->get_track(jtrk)->set_twrhit(hbd_id_s); //temporary fill into twrhit
		trk->get_track(jtrk)->set_pc2dphi(hbd_x_s); //temporary fill into
		trk->get_track(jtrk)->set_spc3dphi(hbd_y_s); //temporary fill into
		trk->get_track(jtrk)->set_spc3dz(hbd_z_s); //temporary fill into spc3dz
		*/


		////
		//
		// Initialize the clusters
		//
		HbdWisCluster * wiscluster = new HbdWisCluster();
		wiscluster->set_hbdsector(hbd_sector);   // using sector 0-11.
		wiscluster->set_localmax(0);
		wiscluster->set_hbdsize(hbd_size);
		wiscluster->set_hbdcharge(hbd_charge);
		wiscluster->set_hbdx(hbd_x);
		wiscluster->set_hbdy(hbd_y);
		wiscluster->set_hbdz(hbd_z);
		wiscluster->set_hbddphi(hbd_dphi);
		wiscluster->set_hbddz(hbd_dz);
		wiscluster->set_hbdsdphi(hbd_dphi_s);
		wiscluster->set_hbdsdz(hbd_dz_s);
		wiscluster->set_hbdscharge(hbd_charge_s);
		wiscluster->set_clusterid(hbd_id_s);


		if(d_clust_list){
			d_clust_list->addCluster(*wiscluster);
		}
		if (wiscluster) {delete wiscluster;}


	}


	return 0;
}

bool sort_by_charge(PadInCluster const& first, PadInCluster const& second){
	return first.charge < second.charge;
}

vector<float> hbd_offsets_run9(unsigned int b_sector, float b_z){

	vector<float> offs(3);
	for (unsigned int k=0; k<offs.size(); k++) offs[k] = 0.0;

	//
	// ES1
	//
	if (b_sector==1 && b_z<0)
	{
		offs[0] =  0.18;
		offs[1] = -0.26;
		offs[2] =  1.69;
	}
	//
	// EN1
	//
	if (b_sector==1 && b_z>0)
	{
		offs[0] =  0.17;
		offs[1] = -0.33;
		offs[2] =  1.44;
	}
	//
	// ES2
	//
	if (b_sector==2 && b_z<0)
	{
		offs[0] =  0.06;
		offs[1] = -0.26;
		offs[2] =  1.44;
	}
	//
	// EN2
	//
	if (b_sector==2 && b_z>0)
	{
		offs[0] =  0.53;
		offs[1] = -0.23;
		offs[2] =  1.54;
	}
	//
	// ES3
	//
	if (b_sector==3 && b_z<0)
	{
		offs[0] = -0.05;
		offs[1] = -0.27;
		offs[2] =  1.30;
	}
	//
	// EN3
	//
	if (b_sector==3 && b_z>0)
	{
		offs[0] = -0.05;
		offs[1] = -0.24;
		offs[2] =  1.44;
	}
	//
	// ES4
	//
	if (b_sector==4 && b_z<0)
	{
		offs[0] = -0.07;
		offs[1] = -0.11;
		offs[2] =  1.31;
	}
	//
	// EN4
	//
	if (b_sector==4 && b_z>0)
	{
		offs[0] = -0.08;
		offs[1] = -0.11;
		offs[2] =  1.53;
	}
	//
	// ES5
	//
	if (b_sector==5 && b_z<0)
	{
		offs[0] = -0.11;
		offs[1] = -0.07;
		offs[2] =  1.34;
	}
	//
	// EN5
	//
	if (b_sector==5 && b_z>0)
	{
		offs[0] = -0.07;
		offs[1] = -0.05;
		offs[2] =  1.94;
	}
	//
	// WS1
	//
	if (b_sector==7 && b_z<0)
	{
		offs[0] = -0.20;
		offs[1] = -0.28;
		offs[2] =  1.17;
	}
	//
	// WN1
	//
	if (b_sector==7 && b_z>0)
	{
		offs[0] = -0.22;
		offs[1] = -0.30;
		offs[2] =  1.24;
	}
	//
	// WS2
	//
	if (b_sector==8 && b_z<0)
	{
		offs[0] = -0.06;
		offs[1] = -0.25;
		offs[2] =  1.21;
	}
	//
	// WN2
	//
	if (b_sector==8 && b_z>0)
	{
		offs[0] = -0.05;
		offs[1] = -0.24;
		offs[2] =  1.29;
	}
	//
	// WS3
	//
	if (b_sector==9 && b_z<0)
	{
		offs[0] =  0.02;
		offs[1] = -0.20;
		offs[2] =  1.22;
	}
	//
	// WN3
	//
	if (b_sector==9 && b_z>0)
	{
		offs[0] =  0.03;
		offs[1] = -0.14;
		offs[2] =  1.32;
	}
	//
	// WS4
	//
	if (b_sector==10 && b_z<0)
	{
		offs[0] =  0.07;
		offs[1] = -0.14;
		offs[2] =  1.28;
	}
	//
	// WN4
	//
	if (b_sector==10 && b_z>0)
	{
		offs[0] =  0.05;
		offs[1] = -0.10;
		offs[2] =  1.35;
	}
	//
	// WS5
	//
	if (b_sector==11 && b_z<0)
	{
		offs[0] =  0.27;
		offs[1] =  0.02;
		offs[2] =  1.31;
	}
	//
	// WN5
	//
	if (b_sector==11 && b_z>0)
	{
		offs[0] =  0.10;
		offs[1] = -0.01;
		offs[2] =  1.44;
	}

	offs[2] += 0.45;

	return offs;
}

