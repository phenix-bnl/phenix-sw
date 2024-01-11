#include <stdio.h>
#include <stream.h>
#include "EmcCalibrator.hh"
#include "EmcCalibratorQA.hh"

ClassImp(EmcCalibrator)

//------------------------------------------------------------
EmcCalibrator::EmcCalibrator(): TNamedDir(){
  _cur_run = 0;
  _cur_seq = 0;
  _nch = 0;
  _debug = DEFAULT_DEBUG;
  _qa_bit = 0;
  return;
}
//------------------------------------------------------------
EmcCalibrator::EmcCalibrator(const char* name, const char* title) : TNamedDir(name,title){
  char hname[128],htitle[128];
  _cur_run = 0;
  _cur_seq = 0;
  _nch = 0;
  _qa_bit = 0;
  _debug = DEFAULT_DEBUG;
  sprintf(hname,"%s_gra_qa_twr",GetName());
  sprintf(htitle,"%s QA twr",GetTitle());
  _gra_qa_twr.SetName(hname);
  _gra_qa_twr.SetName(htitle);
  sprintf(hname,"%s_gra_qa_run",GetName());
  sprintf(htitle,"%s QA run",GetTitle());
  _gra_qa_run.SetName(hname);
  _gra_qa_run.SetName(htitle);
  return;
};
//------------------------------------------------------------
EmcCalibrator::EmcCalibrator(const char* name, const char* title,int nch) : TNamedDir(name,title){
  char hname[128],htitle[128];
  _cur_run = 0;
  _cur_seq = 0;
  _nch = nch;
  _qa_bit = 0;
  _debug = DEFAULT_DEBUG;
  sprintf(hname,"%s_gra_qa_twr",GetName());
  sprintf(htitle,"%s QA twr",GetTitle());
  _gra_qa_twr.Set(nch);
  _gra_qa_twr.SetName(hname);
  _gra_qa_twr.SetName(htitle);
  int n = nch;
  while( n--  )
    _gra_qa_twr.SetPoint(n,n,0);
  sprintf(hname,"%s_gra_qa_run",GetName());
  sprintf(htitle,"%s QA run",GetTitle());
  _gra_qa_run.SetName(hname);
  _gra_qa_run.SetName(htitle);
  return;
};
//------------------------------------------------------------
EmcCalibrator::~EmcCalibrator(){
  return;
};
//------------------------------------------------------------
int EmcCalibrator::SetEmcCalibratorQA(EmcCalibratorQA& calibqa){
  int status = 1;
  int n;
  double *x,*y;
  n = calibqa._gra_qa_run.GetN();
  x = calibqa._gra_qa_run.GetX();
  y = calibqa._gra_qa_run.GetY();
  while( n-- )
    SetQArun((int)x[n],(int)y[n]);
  if( _nch == calibqa._nch && _gra_qa_twr.GetN() == calibqa._nch ){
    //double* x = calibqa._gra_qa_twr.GetX();
    double* y = calibqa._gra_qa_twr.GetY();
    int n = _nch;
    while( n-- )
      AddQAtwr(n,(int)y[n]);
  } else
    status = 2;
  return status;
};
//------------------------------------------------------------
int EmcCalibrator::AddEmcCalibratorQA(EmcCalibratorQA& calibqa){
  int status = 1;
  int n,run;
  double *x,*y;
  n = calibqa._gra_qa_run.GetN();
  x = calibqa._gra_qa_run.GetX();
  y = calibqa._gra_qa_run.GetY();
  while( n-- )
    AddQArun((int)x[n],(int)y[n]);
  if( _nch == calibqa._nch && _gra_qa_twr.GetN() == calibqa._nch ){
    //x = calibqa._gra_qa_twr.GetX();
    y = calibqa._gra_qa_twr.GetY();
    n = _nch;
    while( n-- )
      AddQAtwr(n,(int)y[n]);
  } else
    status = -1;
  return status;
};
//------------------------------------------------------------
int EmcCalibrator::GetQAtwr(int ch){
  if( ch < 0 || ch >= _nch || _gra_qa_twr.GetN() != _nch )
    return 0;  // 0 means no error.....
  double* y = _gra_qa_twr.GetY();
  return (int)y[ch];
};
//------------------------------------------------------------
int EmcCalibrator::GetNQAtwr(int qabit=0) const{
  if( qabit ==0 )
    qabit = _qa_bit;
  int n = _gra_qa_twr.GetN();
  double* y = _gra_qa_twr.GetY();
  int nqa = 0;
  while( n-- ) if( ((int)y[n]) & qabit ) nqa++;
  return nqa;
};
//------------------------------------------------------------
int EmcCalibrator::GetNQAtwr_all() const{
  int n = _gra_qa_twr.GetN();
  double* y = _gra_qa_twr.GetY();
  int nqa = 0;
  while( n-- ) if( y[n] > 0 ) nqa++;
  return nqa;
};
//------------------------------------------------------------
bool EmcCalibrator::SetQAtwr(int ch,int value=-1){
  if( ch < 0 || ch >= _nch )
    return false;
  if( value==-1 )
    value = _qa_bit;
  double* y = _gra_qa_twr.GetY();
  y[ch] = value;
  return true;
};
//------------------------------------------------------------
bool EmcCalibrator::AddQAtwr(int ch,int value=-1){
  if( ch < 0 || ch >= _nch )
    return false;
  if( value==-1 )
    value = _qa_bit;
  double* y = _gra_qa_twr.GetY();
  value = value | ((int)y[ch]);  // Bit SUM
  y[ch] = value;
  return true;
};
//------------------------------------------------------------
int EmcCalibrator::GetQArun(int run){
  if( _gra_qa_run.GetN() == 0 )
    return 0;  // 0 means no error.....
  int n = _gra_qa_run.GetN();
  double* x = _gra_qa_run.GetX();
  double* y = _gra_qa_run.GetY();
  int error = 0;
  while( n-- ){
    if( run == (int) x[n] ){
      error += (int) y[n];
    }
  }
  return error;
};
//------------------------------------------------------------
int EmcCalibrator::GetNQArun(int qabit=0) const{
  if( qabit==0 )
    qabit = _qa_bit;
  int n = _gra_qa_run.GetN();
  double* y = _gra_qa_run.GetY();
  int nqa = 0;
  while( n-- ) if( ((int)y[n]) & qabit ) nqa++;
  return nqa;
};
//------------------------------------------------------------
int EmcCalibrator::GetNQArun_all() const{
  int n = _gra_qa_run.GetN();
  double* y = _gra_qa_run.GetY();
  int nqa = 0;
  while( n-- ) if( y[n] > 0 ) nqa++;
  return nqa;
};
//------------------------------------------------------------
bool EmcCalibrator::SetQArun(int run,int value=-1){
  int n =     _gra_qa_run.GetN();
  double* x = _gra_qa_run.GetX();
  double* y = _gra_qa_run.GetY();
  if( value==-1 )
    value = _qa_bit;
  int found = 0;
  int found_bin = 0;
  while( n-- ){
    if( run == (int) x[n] ){
      found_bin = n;
      found++;
    }
  }
  if( found == 0 ){
    n = _gra_qa_run.GetN();
    _gra_qa_run.Set(n+1);
    _gra_qa_run.SetPoint(n,run,value);
  } else if( found == 1 ) {
    _gra_qa_run.SetPoint(found_bin,run,value);
  } else if( found > 1 ){
    cout<<" "<<GetName()<<"::SetQArun() found two same runs of QA data base..."<<endl;
    return false;
  }
  return true;
};
//------------------------------------------------------------
bool EmcCalibrator::AddQArun(int run,int value=-1){
  int n =     _gra_qa_run.GetN();
  double* x = _gra_qa_run.GetX();
  double* y = _gra_qa_run.GetY();
  if( value==-1 )
    value = _qa_bit;
  int found = 0;
  int found_bin = 0;
  while( n-- ){
    if( run == (int) x[n] ){
      found_bin = n;
      found++;
    }
  }
  if( found == 0 ){
    n = _gra_qa_run.GetN();
    _gra_qa_run.Set(n+1);
    _gra_qa_run.SetPoint(n,run,value);
  } else if( found == 1 ) {
    value = value | ((int) y[found_bin]);  // Bit SUM
    _gra_qa_run.SetPoint(found_bin,run,value);
  } else if( found > 1 ){
    cout<<" "<<GetName()<<"::SetQArun() found two same runs of QA data base..."<<endl;
    return false;
  }
  return true;
};
//------------------------------------------------------------
bool EmcCalibrator::ResetQArun(){
  _gra_qa_run.Set(0);
  return true;
};
//------------------------------------------------------------
bool EmcCalibrator::ResetQAtwr(){
  int n = _gra_qa_twr.GetN();
  double* y = _gra_qa_twr.GetY();
  while( n-- )
    y[n] = 0;
  return true;
};
//------------------------------------------------------------
int EmcCalibrator::AnalyzeCalibRuns(CalibRuns* calibruns,char* opt){
  char* opt_d;
  opt_d = strstr(opt,"d"); // DEBUG option
  char* opt_qa;
  opt_qa = strstr(opt,"q"); // Skip if Run QA tells bad run.
  char* opt_cut;
  opt_cut = strstr(opt,"c"); // Analyze only the runs within the cut.
  char hname[256];
  TIterator* itnext;
  CalibObj* calibobj;
  TKey* key;
  itnext  = calibruns->_keymap->MakeIterator();
  int status = 0;
  int num = 0;
  while( (key = (TKey*) itnext->Next() ) != NULL ){
    cout<<" "<<GetName()<<"::----------------------------------------------------------------------"<<endl;
    cout<<" "<<GetName()<<"::AnalyzeCalibRuns():: Processing. "<<num++<<" / "<<calibruns->_keymap->GetSize()<<" : ";
    cout<<key->GetName()<<endl;
    TFile* file = (TFile*)calibruns->_keymap->GetValue(key);
    if( file ){
      file->cd();
      calibobj = (CalibObj*) key->ReadObj();
      sprintf(hname,"%s_%%d_%%d",calibruns->GetName());
      sscanf(calibobj->GetName(),hname,&_cur_run,&_cur_seq);
      if( opt_d ) cout<<" "<<GetName()<<"::AnalyzeCalibRuns()........... this is run:seq = "<<_cur_run<<":"<<_cur_seq<<endl;
      if( opt_qa && GetQArun(_cur_run) > 0 )
	cout<<" "<<GetName()<<"::AnalyzeCalibRuns() skip a run("<<_cur_run<<") due to the BAD run of run data base."<<endl;
      else if( opt_cut && ( _cur_run > _cut_run[1] || _cur_run < _cut_run[0] ) )
	cout<<" "<<GetName()<<"::AnalyzeCalibRuns() skip a run("<<_cur_run<<") because out of cut ("
	    <<_cut_run[0]<<","<<_cut_run[1]<<") "<<endl;
      else
	status += AnalyzeCalibObj(calibobj,opt);
      delete calibobj;
    }
  }
  _cur_run = 0;
  _cur_seq = 0;
  delete itnext;
  return status;
};
//------------------------------------------------------------
int EmcCalibrator::AnalyzeCalibObj(CalibObj* claibobj,char* opt=""){
  cout<<" "<<GetName()<<" is calling abstruct method AnalyzeCalibObj..."<<endl;
  return 0;
};
//------------------------------------------------------------
int EmcCalibrator::AnalyzeTwr(char* opt=""){
  cout<<" "<<GetName()<<" is calling abstruct method AnalyzeTwr..."<<endl;
  return 0;
};
//------------------------------------------------------------
int EmcCalibrator::Reset(char* opt=""){
  char* opt_qa;
  opt_qa = strstr(opt,"q");
  _cur_run = 0;
  _cur_seq = 0;
  if( opt_qa ){
    cout<<" "<<GetName()<<"::Reset skip resetting of _gra_qa_twr and _gra_qa_run"<<endl;
  } else {
    int n = _nch;
    while( n-- )
      _gra_qa_twr.SetPoint(n,n,0);
    _gra_qa_run.Set(0);
  }
  return 1;
};
//------------------------------------------------------------
bool EmcCalibrator::WriteFileQArun(char* qa_run_file){
  int ch,qa;
  double *x,*y;
  //
  if(_debug) cout<<" "<<GetName()<<"::WriteFileQAtwr() writing QA table for SICK run : "<<qa_run_file<<endl;
  ofstream fout_run(qa_run_file);
  fout_run<<"#Sick run list "<<endl;
  ch = _gra_qa_run.GetN();
  x = _gra_qa_run.GetX();
  y = _gra_qa_run.GetY();
  while( ch-- ){
    qa = (int) y[ch];
    if( qa > 0 )
      fout_run<<(int)x[ch]<<" "<<qa<<endl;
  }
  fout_run.close();
  //
  return true;
};
//------------------------------------------------------------
bool EmcCalibrator::ReadFileQArun(char* qa_run_file){
  int ch,qa;
  double *x,*y;
  char cline[128];
  if(_debug) cout<<" "<<GetName()<<"::ReadFileQArun() reading QA table for SICK run : "<<qa_run_file<<endl;
  ifstream fin_run(qa_run_file);
  _gra_qa_run.Set(0);
  while( fin_run.getline(cline,128) ){
    if( cline[0] != '#' ){
      //fin_run>>ch>>qa
      sscanf(cline,"%d %d",&ch,&qa);
      ch = _gra_qa_run.GetN();
      _gra_qa_run.Set(ch+1);
      _gra_qa_run.SetPoint(ch,ch,qa);
    }
  }
  fin_run.close();
  //
  return true;
};
//------------------------------------------------------------
bool EmcCalibrator::WriteFileQAtwr(char* qa_twr_file){
  int ch,qa;
  double *x,*y;
  if(_debug) cout<<" "<<GetName()<<"::WriteFileQAtwr() writing QA table for SICK twr : "<<qa_twr_file<<endl;
  ofstream fout_twr(qa_twr_file);
  fout_twr<<"#SICK Tower list "<<endl;
  ch = _nch;
  y = _gra_qa_twr.GetY();
  while( ch-- ){
    qa = (int) y[ch];
    if( qa > 0 )
      fout_twr<<ch<<" "<<qa<<endl;
  }
  fout_twr.close();
  //
  return true;

};
//------------------------------------------------------------
bool EmcCalibrator::ReadFileQAtwr(char* qa_twr_file){
  int ch,qa;
  double *x,*y;
  char cline[128];
  if(_debug) cout<<" "<<GetName()<<"::ReadFileQAtwr() reading QA table for SICK twr : "<<qa_twr_file<<endl;
  ifstream fin_twr(qa_twr_file);
  y = _gra_qa_twr.GetY();
  while( fin_twr.getline(cline,128) ){
    if( cline[0] != '#' ){
      sscanf(cline,"%d %d",&ch,&qa);
      if( ch < _nch && ch >= 0 ){
	//fin_twr>>ch>>qa
	y[ch] = qa;
      }
    }
  }
  fin_twr.close();
  //
  return true;

};
//------------------------------------------------------------
//
//

