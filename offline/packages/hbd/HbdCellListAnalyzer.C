// Class: HbdCellListAnalyzer (implementation)
//-------------------------------------------------------------------------
#include "HbdCellListAnalyzer.h"
#include "Hbd.h"
#include "pad_area.h"
#include "HbdCellList.h"
#include "HbdCell.h"

#include <PHGlobal.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHTimeStamp.h>
#include <PHGeometry.h>

#include <getClass.h>

#include <algorithm>
#include <iostream>
#include <fstream>

using namespace std;

HbdCellListAnalyzer::HbdCellListAnalyzer()
{
	verbose = 0;
	for(int i=0;i<24;i++){
		charge[i]=0;
		rms[i]=0;
		median[i] = 0;
		hitarea[i]=0;
		unit_hip_high_threshold[i]=1000;
		hip[i].clear();
		unit_threshold[i]=0;
	}
}

HbdCellListAnalyzer::~HbdCellListAnalyzer()
{
}
int 
HbdCellListAnalyzer::Process(PHCompositeNode *topNode){

  HbdCellList *CellList = findNode::getClass<HbdCellList>(topNode, "HbdCellList");
  if (!CellList) cout << PHWHERE << "HbdCellListAnalyzer:: No HbdCellList!" << endl;
  
  //Get the bbcq
  if (mc_flag == 1){ //1-PURE MC
    bbcq = 1.0;
  }
  else{
    PHGlobal *global = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
    if (!global) cout << PHWHERE << "HbdCellListAnalyzer:: No PHGlobal!" << endl;
    bbcq = global->getBbcChargeN()+global->getBbcChargeS();
  }
  if(verbose) cout<<"HbdCellListAnalyzer::Process() bbcq "<<bbcq<<endl;
  
  Init();
  Expand(CellList);
  if (mc_flag!=1){ 
    CalculateModuleProperty(CellList);
    Subtract(CellList);
  }

  return 0;
}

void
HbdCellListAnalyzer::Init()
{
	if(verbose) cout<<"HbdCellListAnalyzer::Init()"<<endl;
	for(int i=0;i<24;i++){
		charge[i]=0;
		rms[i]=0;
		hitarea[i]=0;
		unit_hip_high_threshold[i]=10000;
		hip[i].clear();
		unit_threshold[i]=0;
		unitpadcharge[i].clear();
	}
}

void
HbdCellListAnalyzer::Expand(HbdCellList *CellList)
{
	if(verbose) cout<<"HbdCellListAnalyzer::Expand()"<<endl;
	int nFiredCells = CellList->get_nCells();
	if(verbose) cout<<"HbdCellListAnalyzer::Expand() nFiredCells "<< nFiredCells<<endl;
	float * cellcharge = new float[2304];
	int * cellsector = new int[2304];
	int * cellpadnum = new int[2304];
	
	
	int icl = 0;
	int ncell;
	
	for (unsigned int isect = 0; isect<12; isect++)
	{
		for (unsigned int ipad = 0; ipad < 192; ipad++)
		{
			icl = isect*192+ipad;  // icl has values from 0-2303
			cellsector[icl]=isect;
			cellpadnum[icl]=ipad;
			cellcharge[icl] = -999;
		}
	}
	
	for (int icl=0; icl<nFiredCells; icl++)
	{ // loop over fired cells
		ncell = CellList->get_cell(icl)->get_sector()*192+CellList->get_cell(icl)->get_padnum();
		cellcharge[ncell] = CellList->get_cell(icl)->get_charge();
	}
	
	CellList->set_nCells(0);
	for (unsigned int icl=0; icl< 2304; icl++)
	{
		ncell = CellList->get_nCells();
		CellList->AddCell(ncell);
		CellList->set_nCells(ncell+1);
		HbdCell *hbdcell = CellList->get_cell(ncell);
		hbdcell->set_charge(cellcharge[ncell]);
		hbdcell->set_sector(cellsector[ncell]);
		hbdcell->set_padnum(cellpadnum[ncell]);
		ncell++;
	}
	
	// Needed to prevent memory leaks!!!
	delete [] cellcharge;
	delete [] cellsector;
	delete [] cellpadnum;
	
	return;
	
}

void
HbdCellListAnalyzer::CalculateModuleProperty(HbdCellList *CellList)
{
	if(verbose) cout<<"HbdCellListAnalyzer::CalculateModuleProperty()"<<endl;
   int Ncells = CellList->get_nCells();
   float cellcharge[2304];
   int cellsector[2304];
   int cellpadnum[2304];
	 int module = -999;
	 //HIP threshold
	  unit_hip_high_threshold[ES1] = (-0.03636+0.001675*bbcq+10);
		unit_hip_high_threshold[ES2] = (-0.04571+0.00193*bbcq+10);
		unit_hip_high_threshold[ES3] = (-0.04351+0.001903*bbcq+10);
		unit_hip_high_threshold[ES4] = (-0.03194+0.00168*bbcq+10);
		unit_hip_high_threshold[ES5] = (-0.04156+0.001452*bbcq+10);
		unit_hip_high_threshold[EN1] = (-0.04176+0.001818*bbcq+10);
		unit_hip_high_threshold[EN2] = (0.000000+0.000000*bbcq+10);
		unit_hip_high_threshold[EN3] = (-0.03001+0.001406*bbcq+10);
		unit_hip_high_threshold[EN4] = (-0.002814+0.001306*bbcq+10);
		unit_hip_high_threshold[EN5] = (-0.03791+0.001643*bbcq+10);

		unit_hip_high_threshold[WS1] = (-0.03381+0.001296*bbcq+10);
		unit_hip_high_threshold[WS2] = (-0.02776+0.001548*bbcq+10);
		unit_hip_high_threshold[WS3] = (-0.02679+0.001512*bbcq+10);
		unit_hip_high_threshold[WS4] = (-0.0287+0.001477*bbcq+10);
		unit_hip_high_threshold[WS5] = (-0.02513+0.001296*bbcq+10);
		unit_hip_high_threshold[WN1] = (-0.03665+0.001521*bbcq+10);
		unit_hip_high_threshold[WN2] = (-0.03897+0.001576*bbcq+10);
		unit_hip_high_threshold[WN3] = (-0.04122+0.001727*bbcq+10);
		unit_hip_high_threshold[WN4] = (-0.03777+0.001701*bbcq+10);
		unit_hip_high_threshold[WN5] = (-0.04382+0.001567*bbcq+10);

	 //Find HIP and Sum charge loop
   for (int icl=0; icl<Ncells; icl++){ // cell loop
		 module = -999;
     cellcharge[icl] = CellList->get_cell(icl)->get_charge();
     cellsector[icl] = CellList->get_cell(icl)->get_sector();
     cellpadnum[icl] = CellList->get_cell(icl)->get_padnum();
   
     int pdnm = cellpadnum[icl];
		 if (cellsector[icl]==0 && pdnm<96) module = ES0;
		 if (cellsector[icl]==1 && pdnm<96) module = ES1;
		 if (cellsector[icl]==2 && pdnm<96) module = ES2; 
		 if (cellsector[icl]==3 && pdnm<96) module = ES3; 
		 if (cellsector[icl]==4 && pdnm<96)	module = ES4; 
		 if (cellsector[icl]==5 && pdnm<96) module = ES5; 
		 if (cellsector[icl]==0 && pdnm>95) module = EN0; 
		 if (cellsector[icl]==1 && pdnm>95) module = EN1; 
		 if (cellsector[icl]==2 && pdnm>95) module = EN2; 
		 if (cellsector[icl]==3 && pdnm>95) module = EN3; 
		 if (cellsector[icl]==4 && pdnm>95) module = EN4; 
		 if (cellsector[icl]==5 && pdnm>95) module = EN5; 
		 if (cellsector[icl]==6 && pdnm>95) module = WS0; 
		 if (cellsector[icl]==7 && pdnm>95) module = WS1; 
		 if (cellsector[icl]==8 && pdnm>95) module = WS2; 
		 if (cellsector[icl]==9 && pdnm>95) module = WS3; 
		 if (cellsector[icl]==10 &&pdnm>95) module = WS4; 
		 if (cellsector[icl]==11 &&pdnm>95) module = WS5; 
		 if (cellsector[icl]==6 && pdnm<96) module = WN0; 
		 if (cellsector[icl]==7 && pdnm<96) module = WN1; 
		 if (cellsector[icl]==8 && pdnm<96) module = WN2; 
		 if (cellsector[icl]==9 && pdnm<96) module = WN3; 
		 if (cellsector[icl]==10 &&pdnm<96) module = WN4; 
		 if (cellsector[icl]==11 &&pdnm<96) module = WN5; 

		 if (module==WS0||module==WN0||module==ES0||module==EN0||module==EN2) continue;
			

     if (pdnm==0 || pdnm==1 || pdnm==189 || pdnm==190 || pdnm==191) continue;

		 if(cellcharge[icl]<unit_hip_high_threshold[module]*pad_area[pdnm]){
				 if(cellcharge[icl]>-999) charge[module] += cellcharge[icl];
				 unitpadcharge[module].push_back(cellcharge[icl]/pad_area[pdnm]);
			 }else{
			 	 hip[module].insert(pdnm);
			 }
   }
	 //Effective area calculation
	 for(int mod = 0;mod<24;mod++){
		  if (mod==WS0||mod==WN0||mod==ES0||mod==EN0||mod==EN2) continue;
			if((mod>=0&&mod<6)||(mod>=18&&mod<24)){
				 for(int i=0;i<96;i++){
					 if (i==0 || i==1) continue;
						if(hip[mod].find(i)==hip[mod].end()) hitarea[mod] += pad_area[i];
				 }
			}
			if((mod>=6&&mod<12)||(mod>=12&&mod<18)){
				 for(int i=96;i<192;i++){
					 if (i==189 || i==190 || i==191) continue;
						if(hip[mod].find(i)==hip[mod].end()) hitarea[mod] += pad_area[i];
				 }

			}
	 }
	 //Mean calculation
	 for(int mod =0;mod<24;mod++){
		 if(mod==ES0 || mod ==EN0 || mod==EN2 ||mod == WS0 ||mod==WN0){
			 charge[mod] = 0;
		 }else{
			 charge[mod] = charge[mod]/hitarea[mod];
		 }
		 if(verbose)cout<<"HbdCellListAnalyzer()::CalculateModuleProperty(): module mean "<<mod<<" "<<charge[mod]<<endl;
	 }

	 //RMS calculation
   for (int icl=0; icl<Ncells; icl++){ // cell loop
		 module = -999;
     cellcharge[icl] = CellList->get_cell(icl)->get_charge();
     cellsector[icl] = CellList->get_cell(icl)->get_sector();
     cellpadnum[icl] = CellList->get_cell(icl)->get_padnum();

     int pdnm = cellpadnum[icl];
		 if (cellsector[icl]==0 && pdnm<96) module = ES0;
		 if (cellsector[icl]==1 && pdnm<96) module = ES1;
		 if (cellsector[icl]==2 && pdnm<96) module = ES2; 
		 if (cellsector[icl]==3 && pdnm<96) module = ES3; 
		 if (cellsector[icl]==4 && pdnm<96)	module = ES4; 
		 if (cellsector[icl]==5 && pdnm<96) module = ES5; 
		 if (cellsector[icl]==0 && pdnm>95) module = EN0; 
		 if (cellsector[icl]==1 && pdnm>95) module = EN1; 
		 if (cellsector[icl]==2 && pdnm>95) module = EN2; 
		 if (cellsector[icl]==3 && pdnm>95) module = EN3; 
		 if (cellsector[icl]==4 && pdnm>95) module = EN4; 
		 if (cellsector[icl]==5 && pdnm>95) module = EN5; 
		 if (cellsector[icl]==6 && pdnm>95) module = WS0; 
		 if (cellsector[icl]==7 && pdnm>95) module = WS1; 
		 if (cellsector[icl]==8 && pdnm>95) module = WS2; 
		 if (cellsector[icl]==9 && pdnm>95) module = WS3; 
		 if (cellsector[icl]==10 &&pdnm>95) module = WS4; 
		 if (cellsector[icl]==11 &&pdnm>95) module = WS5; 
		 if (cellsector[icl]==6 && pdnm<96) module = WN0; 
		 if (cellsector[icl]==7 && pdnm<96) module = WN1; 
		 if (cellsector[icl]==8 && pdnm<96) module = WN2; 
		 if (cellsector[icl]==9 && pdnm<96) module = WN3; 
		 if (cellsector[icl]==10 &&pdnm<96) module = WN4; 
		 if (cellsector[icl]==11 &&pdnm<96) module = WN5; 

		 if (module==WS0||module==WN0||module==ES0||module==EN0||module==EN2) continue;
     if (pdnm==0 || pdnm==1 || pdnm==189 || pdnm==190 || pdnm==191) continue;

		 if(hip[module].find(pdnm)==hip[module].end() && cellcharge[icl]>-999)	 rms[module] += (cellcharge[icl]-pad_area[pdnm]*charge[module])*(cellcharge[icl]-pad_area[pdnm]*charge[module])/pad_area[pdnm];
   }
	 for(int mod=0;mod<24;mod++){
		  if(mod==ES0 || mod ==EN0 || mod==EN2 ||mod == WS0 ||mod==WN0) continue;

			if((mod>=0&&mod<6)||(mod>=18&&mod<24)){
				 rms[mod] = sqrt(rms[mod]/(94.-(float)hip[mod].size()));
			}
			if((mod>=6&&mod<12)||(mod>=12&&mod<18)){
				 rms[mod] = sqrt(rms[mod]/(93.-(float)hip[mod].size()));
			}

		 if(verbose)cout<<"HbdCellListAnalyzer()::CalculateModuleProperty(): module rms "<<mod<<" "<<rms[mod]<<endl;
	 }
	 //Median calculation
	 for(int mod = 0;mod<24;mod++){
		  if(mod==ES0 || mod ==EN0 || mod==EN2 ||mod == WS0 ||mod==WN0) continue;
		 if(verbose)
		 {
			cout<<"HbdCellListAnalyzer():CalculateModuleProperty(): before sort"<<endl;
			vector<float>::iterator it = unitpadcharge[mod].begin();
			while(it != unitpadcharge[mod].end()){
				cout<<*it<<endl;
				it ++;
			}
		 }
		 sort(unitpadcharge[mod].begin(),unitpadcharge[mod].end(),SortUnitPadCharge());
		 if(verbose)
		 {
			cout<<"HbdCellListAnalyzer():CalculateModuleProperty(): after sort"<<endl;
			vector<float>::iterator it2 = unitpadcharge[mod].begin();
			while(it2 != unitpadcharge[mod].end()){
				cout<<*it2<<endl;
				it2 ++;
			}
		 }

		 if(unitpadcharge[mod].size() % 2 == 1){
				median[mod] = unitpadcharge[mod][(unitpadcharge[mod].size()-1.)/2.];
		 }else{
			 float temp1 = unitpadcharge[mod][unitpadcharge[mod].size()/2. -1.];
			 float temp2 = unitpadcharge[mod][unitpadcharge[mod].size()/2.];
			 median[mod] = (temp1 + temp2)/2.; 
		 }

	 }
}

void
HbdCellListAnalyzer::Subtract(HbdCellList *CellList)
{
	if(verbose) cout<<"HbdCellListAnalyzer::Subtract()"<<endl;
   int Ncells = CellList->get_nCells();
   float cellcharge[2304];
   int cellsector[2304];
   int cellpadnum[2304];
	 int module = -999;
   CellList->set_nCells(0);
   for (int c=0; c<Ncells; c++)
   {
       cellcharge[c] = -9999.;
       cellsector[c] = -9999;
       cellpadnum[c] = -9999;
   }

	 if(verbose){
		 cout<<"HbdCellListAnalyzer::Subtract() before subtraction ES0: 50 ";
		 cout<< CellList->get_cell(50)->get_charge()<<endl;
		 cout<<"HbdCellListAnalyzer::Subtract() before subtraction ES1: 50 ";
		 cout<< CellList->get_cell(242)->get_charge()<<endl;
	 }

	 //cout<<"HbdCellListAnalyzer::Subtract() nCells "<<Ncells<<endl;
   int ncell = 0;
   for (int icl=0; icl<Ncells; icl++){ // cell loop
		 module = -999;
     cellcharge[icl] = CellList->get_cell(icl)->get_charge();
     cellsector[icl] = CellList->get_cell(icl)->get_sector();
     cellpadnum[icl] = CellList->get_cell(icl)->get_padnum();
		 if(verbose){
			 if(cellsector[icl]*192+cellpadnum[icl]==1878){
				 cout<<"HbdCellListAnalyzer  before subtraction: "<<cellcharge[icl]<<endl;
			 }
		 }
   
     //Subtract the charge
     int pdnm = cellpadnum[icl];
		 if (cellsector[icl]==0 && pdnm<96) module = ES0;
		 if (cellsector[icl]==1 && pdnm<96) module = ES1;
		 if (cellsector[icl]==2 && pdnm<96) module = ES2; 
		 if (cellsector[icl]==3 && pdnm<96) module = ES3; 
		 if (cellsector[icl]==4 && pdnm<96)	module = ES4; 
		 if (cellsector[icl]==5 && pdnm<96) module = ES5; 
		 if (cellsector[icl]==0 && pdnm>95) module = EN0; 
		 if (cellsector[icl]==1 && pdnm>95) module = EN1; 
		 if (cellsector[icl]==2 && pdnm>95) module = EN2; 
		 if (cellsector[icl]==3 && pdnm>95) module = EN3; 
		 if (cellsector[icl]==4 && pdnm>95) module = EN4; 
		 if (cellsector[icl]==5 && pdnm>95) module = EN5; 
		 if (cellsector[icl]==6 && pdnm>95) module = WS0; 
		 if (cellsector[icl]==7 && pdnm>95) module = WS1; 
		 if (cellsector[icl]==8 && pdnm>95) module = WS2; 
		 if (cellsector[icl]==9 && pdnm>95) module = WS3; 
		 if (cellsector[icl]==10 &&pdnm>95) module = WS4; 
		 if (cellsector[icl]==11 &&pdnm>95) module = WS5; 
		 if (cellsector[icl]==6 && pdnm<96) module = WN0; 
		 if (cellsector[icl]==7 && pdnm<96) module = WN1; 
		 if (cellsector[icl]==8 && pdnm<96) module = WN2; 
		 if (cellsector[icl]==9 && pdnm<96) module = WN3; 
		 if (cellsector[icl]==10 &&pdnm<96) module = WN4; 
		 if (cellsector[icl]==11 &&pdnm<96) module = WN5; 
		 if(module<0||module>23){
			 cerr<<"HbdCellListAnalyzer::Subtract() module classify error"<<endl;
			 exit(1);
		 }

		 if(module == EN2||module ==ES0||module==EN0||module==WS0||module==WN0){
			 cellcharge[icl] = -999;
		 }else{
			 if(cellcharge[icl]>-999)
			 {
					cellcharge[icl] -= charge[module] * pad_area[pdnm];
			 } 
			 if(verbose && pdnm == 50)cout<<"HbdCellListAnalyzer::Subtract now subtracting.."<<module<<" "<<charge[module]<<" "<<pad_area[pdnm]<<endl;
			 if(verbose){
				 if(cellsector[icl]*192+cellpadnum[icl]==1878){
					 cout<<"HbdCellListAnalyzer  after subtraction: "<<cellcharge[icl]<<endl;
				 }
			 }
		 }
     
     ncell = CellList->get_nCells();
     CellList->AddCell(ncell);
     CellList->set_nCells(ncell+1);
     HbdCell *hbdcell = CellList->get_cell(ncell);
     hbdcell->set_charge(cellcharge[icl]);
     hbdcell->set_sector(cellsector[icl]);
     hbdcell->set_padnum(cellpadnum[icl]);
		 if(verbose){
			 if(cellpadnum[icl]==50){
				 cout<<"after "<<cellsector[icl]<<" "<<cellcharge[icl]<<endl;
			 }
		 }
	 }

}

