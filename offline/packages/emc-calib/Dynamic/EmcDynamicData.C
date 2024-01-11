/*------------------------------------------------------------------------
| This file describes EmcDynamicData class members. Constructor of this
| class parses EMCal electronic configuration file and creates arrays for data
| storage, map of channels that correspond to data arrays elements, array 
| of Reference structures that describe monitoring references and array of
| SuperModule structures that is used by class EMCalFEE to determine what 
| channels should be read out. See example of it usage in EmcParseTest.cc file.
| Created by Sergei Belikov 04/20/99.
-------------------------------------------------------------------------*/
/*==============================================================================
| The purpose of EmcDynamicData class is:                                       |
| 1. to Parse script file describing EMCalchannels and references connections   |
|    to FEMs;                                                                   |
| 2. to create storage arrays for only active channels and references data;     |
| 3. to create routers that explain Event Iterating routines/classes what       |
|    channels should be read out and where these data should be stored.         |
|    Actually router creates "connections" between electronics channels and     |
|    elements of data arrays.                                                   |
|                                                                               |
|  To make your own EmcDynamicData object, just do:                             |
|_____________________________________________________________________________  |
| int status;                                                                   |
| EmcDynamicData myobject(status,ConfigFileName,false,true,true);//constructor  |
|                                                          // does all          |
| if(status) exit(1); //non-zero status means there were script errors          |
| const int* dataMap=myobject.getEmcMap(); // get list of detector cells        |
| int nchannel=myobject.getEmcSize(); // get size of data arrays                |
| float* myHighGain=myobject.getEmcHG();//access to EMCal HighGain pre-post     |
|// the same for getEmcLG, getEmcTAC. You can also use                          |
| EmcData* allEMCdata=myobject.getEmcData();//to get structure for all EMC data |
| EmCData* allREFdata=myobject.getRefData();//to get all reference data         |
|_____________________________________________________________________________  |
| Boolean 3-nd parameter to constructor defines if raw data arrays should be    |
| created too. By default it is false. Boolean 4-th parameter defines if 24 or  |
| 32 channels from each ASIC board will be read. Default value is true that     |
| means that only 24 channles connected to towers will be readout.              |
| Boolean as a 5-th parameter to constructor defines channel mapping style.     |
| Default value is true that means that we use EMCal mapping style. False means |
| that in smMap array  electronics channel numbers are stored instead of tower  |
| numbers.                                                                      |
|_____________________________________________________________________________  |
| Slang in this program:                                                        |
| abs position-means position in the EMCal detector. For towers it is 0-24767,  |
|         for FEM or reference - this is a position of the supermodule (12x12   |
| towers) in EMCal : 0-171. EMCal is treated as 8 sectors, PbSc1, ...,PbSc6,    |
| PgGl1,PbGl2. In this program names of corresponding sectors are: W0,W1,W2,    |
| W3,E2,E3,E0,E1. Reason - Edouard asked to push PbGl as far as possible :)     |
===============================================================================*/

#include <iostream>
#include <cstdlib>
#include "EmcDynamicData.h"
#include "emcGlobals.h"
#include "EmcIndexer.h"
#include <cstring>
#include <cassert>

using std::cout;
using std::cerr;
using std::endl;

int EmcDynamicData::ScriptErrors=0;
bool EmcDynamicData::FEMchErr=false;

#define EMCcode 8001        //lowest packet identifier for EMCal
#define FEMChannels 192     // as it is stated now FEM will send data for 32x6=192 channels
#define ASICs   6           // number of ASIC boards in FEM
#define FEM     0
#define CHAN    1
#define REF     2
#define DELAY   3
#define MAP     4
#define DATA    5
#define EMC_UNKNOWN -1      


const char* dev[]=
{
  "FEM:",
  "CHAN:",
  "REF:",
  "DELAY:",
  "MAP:",
  "DATA:"
};
//Starting address of each sector in calibration array.
unsigned long SectorBases[]=
{
  0,
  3456,
  6912,
  10368,
  13824,
  17280,
  20736,
  26880,
  33024

};
//Sector names
const char* sectors[]=
{
  "W0",
  "W1",
  "W2",
  "W3",
  "E2",
  "E3",
  "E0",
  "E1",
  "NO"
};
//FEM keywords
const char *FEMkeywords[]=
{
  "PIN=",
  "POS=",
  "SECTOR=",
  "PACKET="
};
const char *DELAYkeywords[]=
{
  "TAC=",
  "POST="
};

long SectorFEM[]=
{
  0,
  18,
  36,
  54,
  72,
  90,
  108,
  140,
  172
};

const char* REFkeywords[]=
{
  "TYPE=",		//type of reference
  "POS=",	//position of reference in sector
  "SECTOR=",		//	in what sector
  "CHAN="		// FEM channel
};

const char* DATAkeywords[]=
{
  "TOW",
  "ALL"
};

const char* MAPkeywords[]=
{
  "EMC",
  "FEM"
};

int  EmcDynamicData::cmpSM(const void *a, const void *b)
{
  SuperModule* sa=(SuperModule*)a;
  SuperModule* sb=(SuperModule*)b;
  if(sa->absPosition<sb->absPosition)return -1;
  else if(sa->absPosition>sb->absPosition)return 1;
  cout<<"FEM with pin="<<sa->femPin<<" and FEM with pin="<<sb->femPin;
  cout<<" have the same absolute position="<<sa->absPosition<<endl;
  ScriptErrors++;
  return 0;
}

int EmcDynamicData::cmpRef(const void *a, const void *b)
{
  Reference* sa=(Reference*)a;
  Reference* sb=(Reference*)b;
  if(sa->type<sb->type)      return -1;
  else if(sa->type>sb->type) return 1;
  if(sa->absPosition<sb->absPosition) return -1;
  if(sa->absPosition>sb->absPosition) return  1;



//    if(sa->absFEM<sb->absFEM)return -1;
//    else if(sa->absFEM>sb->absFEM)return 1;
//    if(sa->chan<sb->chan)return -1;
//    if(sa->chan>sb->chan)return 1;

  cout<<"ERROR!!! Two reference are servicing same supermodule "<<sa->absPosition<<endl;
  ScriptErrors++;
  return 0;
}

int EmcDynamicData::cmpCh(const void *c1, const void *c2)
{
  FEMchErr=false;
  int sa=*(int*)c1;
  int sb=*(int*)c2;
  if(sa<sb)return -1;
  if(sa>sb)return 1;
  cout<<"ERROR!!! Two channels have the same number: "<<sa<<"-"<<sb<<endl;
  FEMchErr=true;
  ScriptErrors++;
  return 0;
}

//*************************************************************************

char* EmcDynamicData::strupr(char* s)
{
  for(int i=0;i<=(int)strlen(s);i++)
    if(s[i]>='a' && s[i]<='z')s[i]^=0x20;
  return s;
}


//*************************************************************************



void EmcDynamicData::MBCleanStr(char *String)
{
  strupr(String); // convert string in upper case
  char* str=String;
  if((str=strchr(str,'#')))*str='\0'; //cut off coments.
  str=String;
  while((str=strchr(str,',')))*str=' '; // replace all ',' with ' '
  while(strlen(String))
    {
      if ((String[0] == ' ') || (String[0] == '\t') || (String[0] == '\n'))
	strcpy(String,String+1); // remove blank spaces before keyword
      else break;
    }
}

//*************************************************************************
int EmcDynamicData::MBControl(char *String, char*& RestString)
{
  int size=0;
  for(int i=0;i<int(sizeof(dev)/sizeof(*dev));i++)
    {
      size=strlen(dev[i]);
      if(!strncmp(String,dev[i],size))
	{
	  RestString=String+size;
	  return i;
	}
    }
  RestString=NULL;
  return EMC_UNKNOWN;
}
//*************************************************************************
int EmcDynamicData::MBControl(char *String, char*& RestString, const char ** names, int n)
{
  int size=0;
  //  cout<<sizeof(names)<<" "<<sizeof(*names)<<" "<<sizeof(**names)<<endl;
  for(int i=0;i<n;i++)
    {
      //      cout<<"NAMES "<<i<<" "<<names[i]<<endl;
      size=strlen(names[i]);
      if(!strncmp(String,names[i],size))
	{
	  RestString=String+size;
	  return i;
	}
    }
  RestString=NULL;
  return EMC_UNKNOWN;
}
//*************************************************************************
int EmcDynamicData::FindSector(char *String)
{
  for(int i=0;i<int(sizeof(sectors)/sizeof(*sectors));i++)
    {
      if(!strncmp(String,sectors[i],strlen(sectors[i])))
	return i;
    }
  return -1;
}
//*************************************************************************
int EmcDynamicData::ParseDELAY(char* String)
{
  char* point;
  if(!(point=strstr(String,DELAYkeywords[0]))) //Find TAC delay
    {
      cout<<"DALAY: ERROR!!! LINE "<<line<<": "<<DELAYkeywords[0];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else if(sscanf(point+strlen(DELAYkeywords[0]),"%d",&tacDelay)!=1)
    {
      cout<<"DELAY: ERROR!!! LINE "<<line<<": "<<DELAYkeywords[0];
      cout<<" has wrong value:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else
    {
      if(curSM>=0)smMap[curSM].tac_pre=tacDelay;
    }

  if(!(point=strstr(String,DELAYkeywords[1]))) //Find POST delay
    {
      cout<<"DALAY: ERROR!!! LINE "<<line<<": "<<DELAYkeywords[1];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else if(sscanf(point+strlen(DELAYkeywords[1]),"%d",&postDelay)!=1)
    {
      cout<<"DELAY: ERROR!!! LINE "<<line<<": "<<DELAYkeywords[1];
      cout<<"wrong value:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else
    {
      if(curSM>=0)smMap[curSM].post_pre=postDelay;
    }
  return ScriptErrors;
}

int EmcDynamicData::ParseMAP(char* String)
{
  char* point;
  if((point=strstr(String,MAPkeywords[0])))  channelMap=true; //If MAP: EMCAL was set in configuration file
  else if((point=strstr(String,MAPkeywords[1]))) channelMap=false;//If MAP: FEM was set in configuration file
  else
    {
      cout<<"MAP: ERROR!!! LINE "<<line<<": ";
      cout<<" wrong mapping style - should be EMCAL or FEM:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;		
    }
  return ScriptErrors;
}
int EmcDynamicData::ParseDATA(char* String)
{
  char* point;
  if((point=strstr(String,DATAkeywords[0])))  Style=true; //If DATA: TOWERS was set in configuration file
  else if((point=strstr(String,DATAkeywords[1]))) Style=false;//If DATA: ALL was set in configuration file
  else
    {
      cout<<"MAP: ERROR!!! LINE "<<line<<": ";
      cout<<" wrong data style - should be TOWERS or ALL:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;		
    }
  return ScriptErrors;
}
int EmcDynamicData::ParseFEM(char* String)
{
  char* point;
  int FEMpin=0;

  if(!(point=strstr(String,FEMkeywords[0]))) //Find PID
    {
      cout<<"FEM: ERROR!!! LINE "<<line<<": "<<FEMkeywords[0];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else if(sscanf(point+strlen(FEMkeywords[0]),"%d",&FEMpin)!=1)
    {
      cout<<"FEM: ERROR!!! LINE "<<line<<": "<<FEMkeywords[0];
      cout<<"wrong value:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }

  if(!(point=strstr(String,FEMkeywords[1]))) //Find Position
    {
      cout<<"FEM: ERROR!!! LINE "<<line<<": "<<FEMkeywords[1];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else if(sscanf(point+strlen(FEMkeywords[1]),"%d",&Position)!=1)
    {
      cout<<"FEM: ERROR!!! LINE "<<line<<": "<<FEMkeywords[1];
      cout<<"wrong value:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }

  if(!(point=strstr(String,FEMkeywords[2]))) //Find Sector
    {
      cout<<"FEM: ERROR!!! LINE "<<line<<": "<<FEMkeywords[2];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else 
    {
      Sector=FindSector(point+strlen(FEMkeywords[2]));
      if(Sector<0)
	{
	  cout<<"FEM: ERROR!!! LINE "<<line<<": Wrong sector name: ";
	  cout<<point<<endl<<"-->"<<fLine<<endl;
	  ScriptErrors++;
	}
    }
  //  3/10/2009
  //  the option to change packet # from default one is included to allow 
  //  calorimeter software become usable to quickly review status of the data
  //  collected using calorimeter FEM's from any other PHENIX subsystem
  int FEMpacket(-1);
  if( (point=strstr(String,FEMkeywords[3])) ) //Find packet
    {
      if(sscanf(point+strlen(FEMkeywords[3]),"%d",&FEMpacket)==1) {
	cout<<"FEM: PACKET SUBSTITUTION!!! LINE "<<line<<": "<<FEMkeywords[3]<<" "<<FEMpacket<<endl;
      }
    }

  if((Position<0 || Position>17) && Sector<6)
    {
      cout<<"FEM: ERROR!!! LINE "<<line<<": wrong position for PbSc FEM"<<endl;
      cout<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  if((Position<0 || Position>31) && (Sector==6 || Sector==7))
    {
      cout<<"FEM: ERROR!!! LINE "<<line<<": wrong position for PbGl FEM"<<endl;
      cout<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
			
  if(!ScriptErrors)
    {
      curSM=nSM;
      smMap=(SuperModule* )realloc(smMap,(++nSM)*sizeof(SuperModule));
      if(!smMap)
	{
	  cout<<"Can't allocate memory for FEM addresses!"<<endl;
	  ScriptErrors++;
	  return -1;
	}
      smMap[curSM].absPosition=SectorFEM[Sector]+Position;
      smMap[curSM].femPin=FEMpin;
      smMap[curSM].packet=FEMpacket!=-1? FEMpacket : SectorFEM[Sector]+Position+EMCcode;
      smMap[curSM].nch=0;
      smMap[curSM].startTad=0;
      smMap[curSM].startRad=0;
      smMap[curSM].femCh=NULL; 
      smMap[curSM].nref=0;  
      smMap[curSM].adRef=NULL;
      smMap[curSM].nrefCh=0;
      smMap[curSM].refCh=NULL;
      smMap[curSM].tac_pre=tacDelay;
      smMap[curSM].post_pre=postDelay;
      for(int si=0;si<curSM;si++)
	{
	  if(smMap[si].femPin==FEMpin)
	    {
	      cout<<"ERROR!!! LINE "<<line<<": FEM "<<si;
	      cout<<" has the same PIN as a current FEM"<<endl;
	      cout<<"-->"<<fLine<<endl;
	      ScriptErrors++;
	    }
	}
    }
  return ScriptErrors;
}
int EmcDynamicData::ParseCHAN(char* str)
{
  int chn;
  int start=-1;
  int series=0;
  int number=-1;
  int number2=-1;
  for(int i=0;i<(int)(strlen(str)+1);i++)
    {
      if(str[i]>='0' && str[i]<='9')
	{
	  if(start<0)start=i;
	}
      else if(str[i]=='-')
	{
	  if(start<0)
	    {
	      cout<<"ERROR!!! LINE "<<line<<": no number before '-'"<<endl;
	      cout<<"-->"<<fLine<<endl;
	      ScriptErrors++;
	      return ScriptErrors;
	    }
	  if(sscanf(str+start,"%d",&number)!=1)
	    {
	      cout<<"ERROR!!! LINE "<<line<<": wrong value before '-'"<<endl;
	      cout<<"-->"<<fLine<<endl;
	      ScriptErrors++;
	      return ScriptErrors;
	    }
	  if(Style && number>=FEMChannels-ASICs*8)
	    {
	      cout<<"ERROR!!! LINE "<<line<<": "<<number;
	      cout<<" If you use 24 channel mode channel number must be <"<<FEMChannels-ASICs*8<<endl;
	      cout<<"-->"<<fLine<<endl;
	      ScriptErrors++;
	      return ScriptErrors;
	    }
	  if(!Style && number>=FEMChannels)
	    {
	      cout<<"ERROR!!! LINE "<<line<<": "<<number;
	      cout<<" If you use 32 channel mode channel number must be <"<<FEMChannels<<endl;
	      cout<<"-->"<<fLine<<endl;
	      ScriptErrors++;
	      return ScriptErrors;
	    }

	  series=1;
	  start=-1;
	}
      else if(str[i]==' ' || str[i]=='\t' || str[i]=='\n' || str[i]=='\0')
	{
	  if(start>=0)
	    {
	      if(series)
		{
		  if(sscanf(str+start,"%d",&number2)!=1)
		    {
		      cout<<"ERROR!!! LINE "<<line<<": wrong value after '-'"<<endl;
		      cout<<"-->"<<fLine<<endl;
		      ScriptErrors++;
		      return ScriptErrors;
		    }
		  if(Style && number2>=FEMChannels-ASICs*8)
		    {
		      cout<<"ERROR!!! LINE "<<line<<": "<<number<<"-"<<number2;
		      cout<<" If you use 24 channel mode channel number must be <"<<FEMChannels-ASICs*8<<endl;
		      cout<<"-->"<<fLine<<endl;
		      ScriptErrors++;
		      return ScriptErrors;
		    }
		  if(!Style && number2>=FEMChannels)
		    {
		      cout<<"ERROR!!! LINE "<<line<<": "<<number<<"-"<<number2;
		      cout<<" If you use 32 channel mode channel number must be <"<<FEMChannels<<endl;
		      cout<<"-->"<<fLine<<endl;
		      ScriptErrors++;
		      return ScriptErrors;
		    }

		  if(number2<number)
		    {
		      cout<<"ERROR!!! LINE "<<line<<": "<<number<<"-"<<number2;
		      cout<<" : second value should be greater then first"<<endl;
		      cout<<"-->"<<fLine<<endl;
		      ScriptErrors++;
		      return ScriptErrors;
		    }
		  if(!ScriptErrors)
		    {
		      smMap[curSM].femCh=(int* )realloc(smMap[curSM].femCh, (smMap[curSM].nch+number2-number+1)*sizeof(int));
		      if(!smMap[curSM].femCh)
			{
			  cout<<"Can't allocate memory for FEM addresses!"<<endl;
			  ScriptErrors++;
			  return ScriptErrors;
			}
		      DataMap=(int* )realloc(DataMap, (Data.size+number2-number+1)*sizeof(int));
		      if(!DataMap)
			{
			  cout<<"Can't allocate memory for data addresses!"<<endl;
			  ScriptErrors++;
			  return ScriptErrors;
			}
		      for(int j=number;j<=number2;j++)
			{
			  if(Style)
			    chn=j+j/12*4;
			  else chn=j;

			  DataMap[Data.size++]=SectorBases[Sector]+Position*FEMChannels+chn;
			  smMap[curSM].femCh[smMap[curSM].nch++]=chn;
			}
		      start=-1;
		      series=0;
		    }
		}
	      else
		{
		  if(sscanf(str+start,"%d",&number)!=1)
		    {
		      cout<<"ERROR!!! LINE "<<line<<": wrong channel number"<<endl;
		      cout<<"-->"<<fLine<<endl;
		      ScriptErrors++;
		      return ScriptErrors;
		    }

		  start=-1;
		  if(!ScriptErrors)
		    {
		      smMap[curSM].femCh=(int* )realloc(smMap[curSM].femCh, (smMap[curSM].nch+1)*sizeof(int));
		      if(!smMap[curSM].femCh)
			{
			  cout<<"Can't allocate memory for FEM addresses!"<<endl;
			  return ScriptErrors;
			}
		      DataMap=(int* )realloc(DataMap, (Data.size+1)*sizeof(int));
		      if(!DataMap)
			{
			  cout<<"Can't allocate memory for data addresses!"<<endl;
			  return ScriptErrors;
			}
		      if(Style)
			chn=number+number/12*4;
		      else chn=number;

		      DataMap[Data.size++]=SectorBases[Sector]+Position*FEMChannels+chn;
		      smMap[curSM].femCh[smMap[curSM].nch++]=chn;
		    }
		}
	    }
	}
      else
	{
	  cout<<"ERROR!!! Line "<<line;
	  cout<<": Wrong character in string: '"<<char(str[i])<<"'"<<endl;
	  cout<<"-->"<<fLine<<endl;
	  ScriptErrors++;
	  return ScriptErrors;
	}
    }
  return ScriptErrors;
}
int EmcDynamicData::ParseREF(char* String)
{
  int type=0; // to pacifiy coverity
  int rPosition=0; // to pacifiy coverity
  int rSector=-1;
  int rCh;
  char* point;
  if(!(point=strstr(String,REFkeywords[0]))) //Find type
    {
      cout<<"REF: ERROR!!! LINE "<<line<<": "<<REFkeywords[0];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else if(sscanf(point+strlen(REFkeywords[0]),"%d",&type)!=1)
    {
      cout<<"REF: ERROR!!! LINE "<<line<<": "<<REFkeywords[0];
      cout<<"wrong value:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }

  if(!(point=strstr(String,REFkeywords[1]))) //Find Position
    {
      cout<<"REF: ERROR!!! LINE "<<line<<": "<<REFkeywords[1];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else if( sscanf(point+strlen(REFkeywords[1]),"%d",&rPosition)!=1)
    {
      cout<<"REF: ERROR!!! LINE "<<line<<": "<<REFkeywords[1];
      cout<<"wrong value:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }

  if(!(point=strstr(String,REFkeywords[2]))) //Find Sector
    {
      cout<<"REF: ERROR!!! LINE "<<line<<": "<<REFkeywords[2];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else
    {
      rSector=FindSector(point+strlen(REFkeywords[2]));
      if(rSector<0)
	{
	  cout<<"REF: ERROR!!! LINE "<<line<<": Wrong sector name: ";
	  cout<<point<<endl<<"-->"<<fLine<<endl;
	  ScriptErrors++;
	}
    }
  assert( 0 <= ScriptErrors && ScriptErrors <= 100000000 ); // to pacify scanbuild
  if(!(point=strstr(String,REFkeywords[3]))) //Find channel
    {
      cout<<"REF: ERROR!!! LINE "<<line<<": "<<REFkeywords[3];
      cout<<" is not specified:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  else if(sscanf(point+strlen(REFkeywords[3]),"%d",&rCh)!=1)
    {
      cout<<"REF: ERROR!!! LINE "<<line<<": "<<REFkeywords[3];
      cout<<"wrong value:"<<endl<<"-->"<<fLine<<endl;
      ScriptErrors++;
    }
  if(!ScriptErrors)
    {
      refMap=(Reference *)realloc(refMap,(Refs.size+1)*sizeof(Reference));
      if(!refMap)
	{
	  cout<<"Can't allocate memory for reference!"<<endl;
	  return ScriptErrors;
	}
      assert(rSector>=0) ;
      refMap[Refs.size].absPosition=SectorFEM[rSector]+rPosition;
      refMap[Refs.size].type=type;
      refMap[Refs.size].absFEM=smMap[curSM].absPosition;
      if(Style)rCh=rCh+rCh/12*4;
      refMap[Refs.size].chan=rCh;
      refMap[Refs.size].nSMch=0;
      refMap[Refs.size].startSMad=0;
      smMap[curSM].refCh=(int*)realloc(smMap[curSM].refCh,sizeof(int)*(smMap[curSM].nrefCh+1));
      if(!smMap[curSM].refCh)
	{
	  cout<<"Can't allocate memory for new reflist!!!"<<endl;
	  ScriptErrors++;
	  return ScriptErrors;
	}
      smMap[curSM].refCh[smMap[curSM].nrefCh]=rCh;
      smMap[curSM].nrefCh++;

      Refs.size++;
    }
  return ScriptErrors;
}

EmcDynamicData::EmcDynamicData(int& status, char* filename, bool GetRaw, 
			       bool fem24ch, bool EmcalMapStyle)
{ 
  emcConfigurationFile configFile(filename) ;

  if (!configFile.IsValid()) {
    cerr << "<E> EmcDynamicData::EmcDynamicData(...) : Error opening file " << filename << endl ;
    status = 1 ;
  }
  else {
    ParseConfigurationFile(status,configFile,GetRaw,fem24ch,EmcalMapStyle) ;
  }
}

EmcDynamicData::EmcDynamicData(int& status, emcConfigurationFile& configFile, 
			       bool GetRaw, bool fem24ch, 
			       bool EmcalMapStyle)
{ 
  if (configFile.IsValid()) {
    ParseConfigurationFile(status,configFile,GetRaw,fem24ch,EmcalMapStyle) ;
  }
  else {
    cerr << "<E> EmcDynamicData::EmcDynamicData(...) : Config. File invalid." << endl ;
    status = 1 ;
  }
}

void 
EmcDynamicData::ParseConfigurationFile(int& status, 
				       emcConfigurationFile& configFile, 
				       bool GetRaw, bool fem24ch, 
				       bool EmcalMapStyle)
{
  char String[300];
  char *restString=NULL;
  line=0;
  fLine=NULL;
  status=0;
  Style=fem24ch;
  outputRaw=GetRaw;
  channelMap=EmcalMapStyle;
  Data.hg=NULL;
  Data.lg=NULL;
  Data.tac=NULL;
  Data.rawdata=NULL;
  DataMap=NULL;
  Data.size=0;

  Refs.hg=NULL;
  Refs.lg=NULL;
  Refs.tac=NULL;
  Refs.rawdata=NULL;
  refMap=NULL;
  Refs.size=0;

  smMap=NULL;
  nSM=0;
  curSM=-1;

  FEMchErr=false;

  ScriptErrors=0;
  int MBControlVal;

	
  // Start read configuration.
 
  //  while(fgets(String,sizeof(String),fdin) != NULL)

  configFile.Rewind() ;

  while ( configFile.GetLine(String,sizeof(String)) )
    {
      line++;
      fLine=new char[strlen(String)+1];
      strcpy(fLine,String);
      MBCleanStr(String);
      if(strlen(String))
	{
	  MBControlVal = MBControl(String,restString);
	  switch(MBControlVal)
	    {
	    case FEM:
	      if(ParseFEM(restString)<0)
		{
		  delete [] fLine;
		  status=-1;
		  return;
		}
	      break;

	    case MAP:
	      if(nSM)
		{
		  cout<<"ERROR!!! LINE "<<line;
		  cout<<": MAP must be specified before any FEM description"<<endl;
		  cout<<"-->"<<fLine;
		  ScriptErrors++;
		}
	      ParseMAP(restString);
	      break;

	    case DATA:
	      if(nSM)
		{
		  cout<<"ERROR!!! LINE "<<line;
		  cout<<": DATA must be specified before any FEM description"<<endl;
		  cout<<"-->"<<fLine;
		  ScriptErrors++;
		}
	      ParseDATA(restString);
	      break;

	    case CHAN:
	      if(!nSM)
		{
		  cout<<"ERROR!!! LINE "<<line;
		  cout<<": FEM is not specified for these channels"<<endl;
		  cout<<"-->"<<fLine;
		  ScriptErrors++;
		}
	      ParseCHAN(restString);
	      break;

	    case REF:
	      if(!nSM)
		{
		  cout<<"ERROR!!! LINE "<<line;
		  cout<<": FEM is not specified for these references"<<endl;
		  cout<<"-->"<<fLine;
		  ScriptErrors++;
		}
	      ParseREF(restString);  
	      break;

	    case DELAY:
	      ParseDELAY(restString);  
	      break;

	    default:
	      cout<<"ERROR!!! LINE "<<line;
	      cout<<": no keyword(like FEM:, CHAN:, REF:) or unknown keyword"<<endl;
	      cout<<"-->"<<fLine<<endl;
	      ScriptErrors++;
	      break;
	    } // end switch
	}// end if string is not empty
      delete [] fLine;
    } // end while not EOF
  //  fclose(fdin);
  cout<<"Total errors in script: "<<ScriptErrors<<endl;
  status=ScriptErrors;
  if(!nSM)
    {
      cout<<"Empty or non ASCII configuration file! Number of FEM=0."<<endl;
      status=3;
      return;
    }
  if(status)return;
  // Sorting FEM, REF, channels.
  //  ::qsort(smMap,nSM,sizeof(SuperModule),cmpSM);
  if(nSM > 0) ::qsort(smMap,nSM,sizeof(SuperModule),cmpSM);
  for(int ns=0;ns<nSM;ns++)
    {
      //      ::qsort(smMap[ns].femCh,smMap[ns].nch,sizeof(int),cmpCh);
      if(smMap[ns].nch > 0) ::qsort(smMap[ns].femCh,smMap[ns].nch,sizeof(int),cmpCh);
      if(ScriptErrors) cout<<"FEM PIN="<<smMap[ns].femPin<<endl<<endl;
      //      ::qsort(smMap[ns].refCh,smMap[ns].nrefCh,sizeof(int),cmpCh);
      if(smMap[ns].nrefCh > 0) ::qsort(smMap[ns].refCh,smMap[ns].nrefCh,sizeof(int),cmpCh);
      if(ScriptErrors) cout<<"FEM PIN="<<smMap[ns].femPin<<endl;
    }
  //  //  ::qsort(refMap,Refs.size,sizeof(Reference),cmpRef);
  //  ::qsort(DataMap,Data.size,sizeof(int),cmpCh);
  if(Refs.size > 0) ::qsort(refMap,Refs.size,sizeof(Reference),cmpRef);
  if(Data.size > 0) ::qsort(DataMap,Data.size,sizeof(int),cmpCh);
  status=ScriptErrors;
  if(status)return;
  Data.hg=(float *)calloc(Data.size,sizeof(float));
  if(!Data.hg)
    {
      cout<<"Can't allocate memory for HighGain!!!"<<endl;
      status=1000;
      return;
    }
  Data.lg=(float *)calloc(Data.size,sizeof(float));
  if(!Data.lg)
    {
      cout<<"Can't allocate memory for LowGain!!!"<<endl;
      status=1001;
      return;
    }
  Data.tac=(float *)calloc(Data.size,sizeof(float));
  if(!Data.tac)
    {
      cout<<"Can't allocate memory for TAC!!!"<<endl;
      status=1002;
      return;
    }
  if(outputRaw)
    {
      Data.rawdata=(float **)calloc(5,sizeof(float*));
      for(int rc=0;rc<5;rc++)
	{
	  Data.rawdata[rc]=(float *)calloc(Data.size,sizeof(float));
	  if(!Data.rawdata[rc])
	    {
	      cout<<"Can't allocate memory for raw data!!!"<<endl;
	      status=1012;
	      return;
	    }
	}
    }
  Refs.hg=(float *)calloc(Refs.size,sizeof(float));
  if(!Refs.hg)
    {
      cout<<"Can't allocate memory for references HighGain!!!"<<endl;
      status=1003;
      return;
    }
  Refs.lg=(float *)calloc(Refs.size,sizeof(float));
  if(!Refs.lg)
    {
      cout<<"Can't allocate memory for references LowGain!!!"<<endl;
      status=1004;
      return;
    }
  Refs.tac=(float *)calloc(Refs.size,sizeof(float));
  if(!Refs.tac)
    {
      cout<<"Can't allocate memory for references TAC!!!"<<endl;
      status=1005;
      return;
    }
  if(outputRaw)
    {
      Refs.rawdata=(float **)calloc(5,sizeof(float*));
      for(int rf=0;rf<5;rf++)
	{
	  Refs.rawdata[rf]=(float *)calloc(Refs.size,sizeof(float));
	  //	  if(Refs.size > 0) Refs.rawdata[rf]=(float *)calloc(Refs.size,sizeof(float));
	  if(!Refs.rawdata[rf])
	    {
	      cout<<"Can't allocate memory for references raw data!!!"<<endl;
	      status=1022;
	      return;
	    }// else {cout<<"Ref allocated "<<rf<<"   "<<(int)Refs.rawdata[rf]<<endl;}
	}
    }
  setSMad();
  // Find Supermodule that correponds to reference
  setREFtoSM();
  if(channelMap)
    {
      for(int im=0;im<Data.size;im++)
	DataMap[im] = EmcIndexer::absFEMCHiPX(DataMap[im]);
    }
  return;
}
void EmcDynamicData::setSMad()
{
  int ctotal=0;
  int rtotal=0;
  for(int i=0;i<nSM;i++)
    {
      smMap[i].startTad=ctotal;
      ctotal+=smMap[i].nch;
      smMap[i].startRad=rtotal;
      rtotal+=smMap[i].nrefCh;
    }
}
void EmcDynamicData::setREFtoSM() //makes crossreferences between SuperModule and Reference objects
{
  for(int r=0;r<Refs.size;r++)
    {
      for(int s=0;s<nSM;s++)
	{
	  //			if(refMap[r].absPosition<smMap[s].absPosition)break;
	  if(refMap[r].absPosition==smMap[s].absPosition)
	    {
	      refMap[r].nSMch=smMap[s].nch; //set number of SM channels in reference structure
	      refMap[r].startSMad=smMap[s].startTad; // set starting address of SM data in data array
	      smMap[s].adRef=(int*)realloc(smMap[s].adRef,(smMap[s].nref+1)*sizeof(int));
	      if(!smMap[s].adRef)
		{
		  cout<<"Can't reallocate memory for SM reference list!!!"<<endl;
		  ScriptErrors++;
		  return;
		}
	      smMap[s].adRef[smMap[s].nref]=r;  //add reference to SM reference list
	      smMap[s].nref++;		 //increment nnumber of references
				//				break;
	    }
	  if(refMap[r].absFEM==smMap[s].absPosition)
	    {
	      int absRefAd=refMap[r].chan+refMap[r].absFEM*FEMChannels;
	      int i;
	      for(i=smMap[s].startTad;i<(smMap[s].startTad+smMap[s].nch);i++)
		{
		  if(absRefAd==DataMap[i])
		    {
		      refMap[r].chan=i;
		      break;
		    }
		}
	      if(i==(smMap[s].startTad+smMap[s].nch))
		{
		  refMap[r].chan=-1;
		}
	    }
	}
    }
}


EmcDynamicData::~EmcDynamicData()
{
  free(Data.hg);
  free(Data.lg);
  free(Data.tac);
  free(Refs.hg);
  free(Refs.lg);
  free(Refs.tac);
  free(DataMap);
  for(int i=0;i<nSM;i++)
    {
      free(smMap[i].femCh);
      free(smMap[i].refCh);
      free(smMap[i].adRef);
    }
  free(smMap);
  free(refMap);
  if(Data.rawdata)
    {
      for(int rc=0;rc<5;rc++)
	free(Data.rawdata[rc]);
      free(Data.rawdata);
    }
  if(Refs.rawdata)
    {
      for(int rc=0;rc<5;rc++) {
	//	cout<<"Releasing "<<rc<<"   "<<(int)Refs.rawdata[rc]<<endl;
	free(Refs.rawdata[rc]);
      }
      free(Refs.rawdata);
    }
}

	

