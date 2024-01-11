 
//#############################################################################
// E.Kistenev         6/10/99 
// send comments to kistenev@bnl.gov 
//#############################################################################

#include <EmcStaticData.h>
#include <PbScCalibrationData.h>
#include <PbScSector.h> 
#include <PbScSuperModule.h>

#include <iostream> 
#include <string>
#include <cstdio>
#include <cstdlib>
#include <cstring>

using std::cerr;
using std::cout;
using std::endl;

//____________________________________________________________________
PbScCalibrationData::PbScCalibrationData() 
{ 
	Status    = kFALSE;
//  Read list of files with calibration data accuired in 902 
  FILE * fp = 0; 
  int     dummy, supn; 
  char    filename[80]; 
  std::string    file;
  char * var = getenv("EMCAL_DATA_PBSC");

  if ( !var ) 
    {
      cerr << "<EMCAL-FATAL> PbScCalibrationData::PbScCalibrationData : "
	   << "Environment variable EMCAL_DATA_PBSC not set !"
	   << "Exit likely to happen soon."
	   << endl;
      return;
    }

  file = var; file += "/Cal_902/PbScSMCalibrationList";
  if( (fp =fopen(file.c_str(),"r")) != 0 ) { 
    while(fscanf(fp,"%d,%d,%d",&dummy,&supn,&dummy)!=EOF){ 
			int c;
      while( (c = fgetc(fp)) != '\n'  &&  c != EOF ){;} 
      fgets(filename,80,fp); 
      fName[supn-1]=new char[strlen(filename)+1];
      filename[strlen(filename)-1]='\0'; 
      strcpy(fName[supn-1],filename);
			//	cout<<fName[supn-1]<<endl;
    } 
  } else {
    cout << "Cannot open file : " << file << endl;
    return;
  }    
  fclose (fp); 
  PbScSMList=new int[108];
  int SMList[108]  = { 
    49,54,42,43,53,41,38,46,37,51,47,50,26,30,44,45,52,48, 
    34,27,67,68,60,21,63,71,55,65,70,28,74,77,82,72,79,57, 
    23,22,19,20,24,35,69,66,56,61,29,33,36,58,59,62,64,31, 
    86,78,84,25,76,88,87,75,81,83,94,98,109,73,90,80,85,32, 
    8,13,10,4,5,17,18,9,11,7,14,3,39,40,15,12,16,6, 
    108,105,107,89,104,100,101,95,93,97,91,1,102,106,92,103,96,99 
  }; 
  for(int i=0;i<108;i++)   	PbScSMList[i]=SMList[i];
	Status  = kTRUE;
} 

//____________________________________________________________________
PbScCalibrationData::~PbScCalibrationData() 
{ 
  for(int i=0;i<109; i++) delete [] fName[i];
  delete [] PbScSMList;
}; 
 
//____________________________________________________________________
int PbScCalibrationData::getSMNumber(int SMId) 
{
	for(int i=0;i<108; i++){ if(SMId==PbScSMList[i])return i; }
	return -1;
}
 
//____________________________________________________________________
void  PbScCalibrationData::LoadPMTDataBase(const char * fName)
{
	FILE *  fp;
	std::string file;
	float   Line[100];
	EmcStaticData * gData = EmcStaticData::buildEmcStaticData();
	file = getenv("EMCAL_DATA_PBSC"); file += "Cal_902/"; file += fName;
	cout<<file<<endl;
	int linesRead=0;
	if( (fp =fopen(file.c_str(),"r")) != 0 ) { 
		while(fscanf(fp,"%f",&Line[0])!=EOF){
			//			cout<<"Next Line "<<endl;
		        for(int i =1; i<PMTWordsPerTower+7; i++) assert( fscanf(fp,"%f ",&Line[i]) == 1 );
			linesRead++;
			// cout<<linesRead<<" "<<Line[0]<<" "<<Line[1]<<endl;
			// check if this is an installed PMT
			int SMId         = static_cast<int>(Line[0]);
			int Twr          = static_cast<int>(Line[1]-1);
			int SMNumber     = this->getSMNumber(SMId);
			int SectorNumber = SMNumber/18;
			SMNumber         = SMNumber%18;
			//	cout<<"Out to load "<<linesRead<<" "<<SMId<<" "<<SMNumber<<" "<<SectorNumber<<" Tower "<<Twr<<endl;
			// check if this sector exists
			if(EmcSector * sData = gData->getSector(SectorNumber)){
				// Sector exists - find supermodule pointer
				EmcSuperModule * smData = sData->getSuperModule(SMNumber);
				((PbScSuperModule *) smData)->LoadDataBase902(Twr, Line);
			}			
		}
		fclose(fp);
		return;
	} else {
		cout<<"File "<<file<<" not found. PMT data-base is missing"<<endl;
	}
}

