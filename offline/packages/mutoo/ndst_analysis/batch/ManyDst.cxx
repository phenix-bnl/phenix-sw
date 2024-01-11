// $Id: ManyDst.cxx,v 1.1 2003/10/27 17:20:16 zhangc Exp $
#include <string>
#include <vector.h>
#include <fstream.h>
//______________________________________________________________________
int main()
{
  //cout << "Usage: Many <DST1> [<DST2> <...>]" << endl;

  string workPath(   "/phenix/workarea/zhangc/sandbox/mutoo/ndst_analysis" );
  string NtuoutPath("/phenix/data07/zhangc/mwgntu_pp_north/ntuple/");
  string logPath(    "/phenix/data22/zhangc/pass_mwg/logfiles/" );
  string scriptPath( "/phenix/workarea/zhangc/sandbox/mutoo/ndst_analysis/batch/bsub_scripts/");
  string outScript(  "many_dst.bsub" );
	
  ofstream out( outScript.c_str(), ios::out );
  out << "# automaticaly generated script using ManyDst \n";
  out << "#!/bin/csh\n";
  // load inputfile
  char *filename="filelist.txt";
  ifstream fin;
  fin.open(filename);
  string input;
  while(!fin.eof()){
    fin>>input;
    int shortStart = input.rfind("/");
    string shortInput = input.substr( shortStart+1, input.size() );
    int extStart = shortInput.rfind(".");
    string head = shortInput.substr(0, extStart );
    string ntuout  = NtuoutPath+"mwgntu_"+head+".root";
    string name = head;
    string log = logPath+head+".log";
    string script = scriptPath+head+".bsub";
    
    // generate output script
    ofstream script_out( script.c_str(), ios::out );
    script_out << "# automaticaly generated script using ManyDst \n";
    script_out << "#BSUB -q phenix_cas\n";
    script_out << "#BSUB -L /usr/local/bin/tcsh\n";
    script_out << "#BSUB -R linux\n";
    script_out << "#BSUB -J " << name << endl;
    script_out << "cd " << workPath << endl;
    script_out << "analyze "<<input << " " << ntuout << "\n";
    script_out.close();
    cout << "ManyDst - \"" << script << "\" done.\n";
    
    // append to main script
    out << "rm -f " << log << ";";
    out << "bsub -o " << log << " < " << script << "; sleep 1 \n";
  }
  cout << "ManyDst - \"" << outScript << "\" done.\n";
  
  return 0;
}



















