/*
 * ROOT macro
 */
 
#include <string.h>
void
dstQaSummary (const char *histIFile = "qaout.root",
      	      const char *outfilename = "dstqasummary.txt",
      	      const char *statusfilename = "dstqastatus.txt",
              Int_t runNumber=1234)
{

  gROOT->Reset ();

  ofstream textFile(outfilename);
  ofstream statusFile(statusfilename);

  TFile qafile(histIFile,"READ");

    textFile << " ----------------------------------------------------" << endl;
    textFile << " ************ QA Summary for run  "<< runNumber <<"  ************" <<endl;

    statusFile << runNumber <<" ";

    gROOT->Macro ("bbcQaSummary.C");
    gROOT->Macro ("dchQaSummary.C");
    gROOT->Macro ("emcQaSummary.C");
    gROOT->Macro ("padQaSummary.C");
    gROOT->Macro ("tecQaSummary.C");
    gROOT->Macro ("tofQaSummary.C");

    gROOT->Macro ("crkQaSummary.C");
    gROOT->Macro ("mutrQaSummary.C");
    gROOT->Macro ("muiQaSummary.C");
    gROOT->Macro ("zdcQaSummary.C");
    gROOT->Macro ("fclQaSummary.C");
    gROOT->Macro ("mvdQaSummary.C");
    gROOT->Macro ("electronQaSummary.C");
    gROOT->Macro ("ntcpQaSummary.C");

  qafile.Close();

}

