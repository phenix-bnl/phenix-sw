// This macro handle setting all of the output nodes for the various types 
// of output files.

#include <stdio.h> 
#include <time.h> 
#include <string>
#include <vector>

vector<string> Output_Files;
vector<string> Output_Types;

string OUTPUT;
string EXTN;
string FILE;

void addCommon(Fun4AllDstOutputManager *manager)
{
  manager->AddNode("EventHeader");
  manager->AddNode("Sync");
  manager->AddNode("TrigLvl1");
  manager->AddNode("PreviousEvent");
}

void MakeOutput(const char* path="./",const char* dir="", const char* NUM="", const char *file ="")
{
  string PATH(path);
  string DIR(dir);
  string FILE(file);
  string SYST="_run11ZdcCalib";
  string EXTN=".root";

  OUTPUT = PATH + DIR + FILE + SYST + NUM + EXTN;

  Output_Files.push_back(OUTPUT);
  Output_Types.push_back(FILE);

  char cmd[1000];
  sprintf(cmd,"mkdir -p %s",(PATH+DIR).c_str());
  gSystem->Exec(cmd);
}

void DST_EVE_NodeAdd(Fun4AllDstOutputManager *manager)
{
  manager->AddNode("BbcOut");
  manager->AddNode("BbcRaw");
  manager->AddNode("ZdcOut");
  manager->AddNode("ZdcRaw");
  manager->AddNode("SmdOut");
  //manager->AddNode("SpinDataEventOut");
  manager->AddNode("lpcRaw");
  //manager->AddNode("ErtOut");
}

void DST_EVE_IOManager(const char* path="./", const char* NUM="",const char *file = "DST_EVE")
{
  MakeOutput(path,"DST_EVE/",NUM,file);
  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllDstOutputManager *dst_EVE_Manager  = new Fun4AllDstOutputManager("DST_EVE",OUTPUT.c_str());
  addCommon(dst_EVE_Manager);
  DST_EVE_NodeAdd(dst_EVE_Manager);
  se->registerOutputManager(dst_EVE_Manager);
}

void QA_IOManager(const char* path="./", const char* NUM="", const char *file = "qaRoot")
{
  MakeOutput(path,"qaRoot/",NUM,file);

  QAEnd(OUTPUT.c_str());
}

void RunSummary(const char* path="./", const char* NUM="", const char *file = "RunSummary")
{
  string PATH(path);
  string FILE(file);
  string NUMBER(NUM);
  string SYST="_run10AuAu_mpc";
  string EXTN=".txt";
  string OUTPUT = FILE + SYST + NUM + EXTN;

  ofstream fp_out;
  fp_out.open(OUTPUT.c_str(), ios::out);
  fp_out << OUTPUT << endl;
  for (unsigned int i=0; i<Output_Files.size(); i++)
    {
      fp_out << Output_Files[i] << endl;
    }
  fp_out.close();

}

