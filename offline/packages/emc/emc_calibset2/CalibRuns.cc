#include <stdio.h>
#include <stream.h>
#include "CalibRuns.hh"

ClassImp(CalibRuns)

int CalibRuns::_glb_run = -1;
int CalibRuns::_glb_seq = -1;

//------------------------------------------------------------
CalibRuns::CalibRuns(): TNamedDir(){
  _run = -1;
  _seq = -1;
  _nch = 0;
  _initialized = false;
  ResetGlobal();
  InitializeBuffer(0);
  //
  _calibobj = NULL;
  _keymap = new TMap();
  _objarray = new TObjArray();
  _filearray = new TObjArray();
  _writedir = gDirectory;
  _debug = false;
  return;
}
//------------------------------------------------------------
CalibRuns::CalibRuns(const char* name, const char* title,int nch) : TNamedDir(name,title){
  _run = -1;
  _seq = -1;
  _nch = nch;
  _initialized = false;
  ResetGlobal();
  InitializeBuffer(DEFAULT_BUFFERSIZE);
  //
  _calibobj = NULL;
  _keymap = new TMap();
  _objarray = new TObjArray();
  _filearray = new TObjArray();
  _writedir = gDirectory;
  _debug = false;

  return;
};
//------------------------------------------------------------
CalibRuns::~CalibRuns(){
  TDirectory* currentdir = gDirectory;
  ResetGlobal();
  SetBuffersize(0);
  TIterator* itnext;

  //--- Delete objects in file
  if( _calibobj != NULL )
    if( _keymap->Contains(_calibobj) )
      _calibobj->Delete();
  //  itnext = _keymap->MakeIterator();
  //  while( TObject* obj =  itnext->Next() ){
  //    TFile* file = (TFile*)_keymap->GetValue(obj);
  //    delete file;
  //  }
  _keymap->RemoveAll();
  delete _keymap;

  //--- Delete objects in memory
  itnext  = _objarray->MakeIterator();
  while( CalibObj* obj = (CalibObj*) itnext->Next() ) obj->Delete();
  _objarray->RemoveAll();
  delete _objarray;
  delete itnext;

  //--- Delete opened TFile*
  itnext  = _filearray->MakeIterator();
  while( TFile* f = (TFile*) itnext->Next() ){
    //f->ls();
    delete f;
  }
  _filearray->RemoveAll();
  delete _filearray;
  //
  currentdir->cd();
  return;
};
//------------------------------------------------------------
void CalibRuns::ResetBuffer(){
  _buf_size = 0;
  return;
}
//------------------------------------------------------------
void CalibRuns::InitializeBuffer(int buffersize){
  _buf_size = 0;
  if( buffersize > 0 ){
    _buf_totsize = buffersize;
    _buf_ch = new int[_buf_totsize];
    _buf_x = new float[_buf_totsize];
    _buf_y = new float[_buf_totsize];
    _buf_z = new float[_buf_totsize];
  } else {
    _buf_totsize = 0;
    _buf_ch = NULL;
    _buf_x = NULL;
    _buf_y = NULL;
    _buf_z = NULL;
  }
  return;
}
//------------------------------------------------------------
void CalibRuns::SetBuffersize(int buffersize){
  if( _buf_totsize > 0 ){
    delete[] _buf_ch;
    delete[] _buf_x;
    delete[] _buf_y;
    delete[] _buf_z;
    //_buf_ch = NULL;
    //_buf_x = NULL;
    //_buf_y = NULL;
    //_buf_z = NULL;
  }
  _buf_size = 0;
  if( buffersize > 0 ){
    _buf_totsize = buffersize;
    _buf_ch = new int[_buf_totsize];
    _buf_x = new float[_buf_totsize];
    _buf_y = new float[_buf_totsize];
    _buf_z = new float[_buf_totsize];
  } else
    _buf_totsize = 0;
  return;
};
//------------------------------------------------------------
int CalibRuns::FillBuffer(int ch,float x,float y,float z){
  int stat = 0;
  if( _buf_size < _buf_totsize ){
    _buf_ch[_buf_size] = ch;
    _buf_x[_buf_size] = x;
    _buf_y[_buf_size] = y;
    _buf_z[_buf_size] = z;
    _buf_size++;
    stat = 1;
  }
  return stat;
};
//------------------------------------------------------------
void CalibRuns::ResetGlobal(){
  _all_num = 0;
  _all_sum = 0;
  _all_sum2 = 0;
  _all_low = 0;
  _all_high = 0;
  return;
}
//------------------------------------------------------------
int CalibRuns::FillGlobal(float x){
  _all_num++;
  _all_sum += x;
  _all_sum2 += x*x;
  if( x < _all_low ) _all_low = x;
  if( x > _all_high ) _all_high = x;
  return 1;
};
//------------------------------------------------------------
Int_t CalibRuns::Fill(int run,int seq,int ch,float x,float y=1.,float z=1.){
  //cout<<" CalibRuns::Fill(int run,int ch,Axis_t x) "<<endl;
  int stat;
  if( _run != run || _seq != seq ){
    //cout<<" "<<GetName()<<" :: Found new run "<<_run<<":"<<_seq<<" --> "<<run<<":"<<seq<<endl;
    if( _glb_run != run || _glb_seq != seq ){
      cout<<" CalibRuns:: Found new run "<<_run<<":"<<_seq<<" --> "<<run<<":"<<seq<<endl;
      _glb_run = run;
      _glb_seq = seq;
    }
    if( _all_num > 0 )
      Write(_writedir);
    if( _initialized ){
      cout<<" Warning:: "<<GetName()<<"Write function didn't work... "<<endl;
      _initialized = false;
    }
    _run = run;
    _seq = seq;
    ResetGlobal();
    ResetBuffer();
  }
  FillGlobal(x);
  //  cout<<GetName()<<"::_initialized  = "<<_initialized<<" GetBuffersize() = "<<GetBuffersize()<<endl;
  if( _initialized ) {
    stat = _calibobj->Fill(ch,x,y,z);
  } else {
    stat = FillBuffer(ch,x,y,z);
    if(! stat ){
      stat = CalibRuns::Initialize();
      if(! stat )
	stat = Initialize(); // This is virtual function
      if( stat ){
	char hname[256],htitle[256];
	sprintf(hname,"%s_%d_%d",GetName(),_run,_seq);
	sprintf(htitle,"%s run %d:%d",GetTitle(),_run,_seq);
	if(! _objarray->Contains(hname) ) _objarray->Add(_calibobj);
	while( _buf_size-- ) {
	  _calibobj->Fill(_buf_ch[_buf_size],_buf_x[_buf_size],_buf_y[_buf_size],_buf_z[_buf_size]);
	}
	_buf_size = 0;
      }
    }
  }

  return stat;
};
//------------------------------------------------------------
int CalibRuns::Initialize(){
  int stat = 0;
  int num;
  if( 0 ){
    cout<<" "<<GetName()<<" is already initialized "<<endl;
    return 1;
  }
  //************************************************************************************************
  //cout<<" CalibRuns:: Saving existing histograms into file. "<<endl;
  TDirectory* currentdir = gDirectory;
  if( _initialized 
      && ( _objarray->Contains(_calibobj) || _keymap->Contains(_calibobj->GetName()) ) ){
    if( _writedir->IsWritable() ){
      _writedir->cd();
      TKey* key;
      if( _keymap->Contains(_calibobj->GetName()) ){
	TAssoc* ass = (TAssoc*) _keymap->FindObject(_calibobj->GetName());
	key = (TKey*) ass->Key();
	_keymap->Remove(key);
	key = _writedir->GetKey(_calibobj->GetName());
	if( key ) key->Delete();
      }
      _calibobj->Write();
      key = _writedir->GetKey(_calibobj->GetName());
      if( key ){
	_keymap->Add(key,_writedir);
	_objarray->Remove(_calibobj);
	_calibobj->Delete();
	_initialized = false;
      } else {
	cout<<" "<<GetName()<<"::Initialize() Fatal Errors:: Failed to save existing histograms .... "<<endl;
      }
    } else {
      cout<<" "<<GetName()<<"::Initialized() Warning: Keeping existing histograms in memory may exhaust huge memory. "<<endl;
    }
  }
  //************************************************************************************************
  //cout<<" CalibRuns:: Finding histograms in memory and file. "<<endl;
  char hname[256],htitle[256];
  sprintf(hname,"%s_%d_%d",GetName(),_run,_seq);
  sprintf(htitle,"%s run %d:%d",GetTitle(),_run,_seq);
  if( _keymap->Contains(hname) ){
    TAssoc* ass = (TAssoc*) _keymap->FindObject(hname);
    TKey* key = (TKey*) ass->Key();
    TFile* file = (TFile*) ass->Value();
    if( file->IsWritable() ){
      file->cd();
      _calibobj = (CalibObj*) key->ReadObj();
      _initialized = true;
      stat = 1;
    } else {
      cout<<" "<<GetName()<<"::Initialize() Fatal Error:: Existing histgrams in the non-writable file..."<<endl;
      _calibobj = NULL;
      _initialized = false;
      stat = 0;
    }
  } else if( _objarray->Contains(hname) ) {
    _calibobj = (CalibObj*) _objarray->FindObject(hname);
    _initialized = true;
    stat = 1 ;
  }
  return stat;
};
//------------------------------------------------------------
void CalibRuns::Reset(char* option){
  CalibObj* calibobj;
  TKey* key;
  TFile* file;
  TObject* obj;
  TIterator* itnext;
  // Reset current objects................
  if(_debug) cout<<" "<<GetName()<<"::Reset() current object.......... "<<endl;
  if( _calibobj ){
    if( _keymap->Contains(_calibobj->GetName()) ){ //if _calibobj in file.
      _calibobj->Delete();
      _calibobj = NULL;
    }
  }
  // Reset all objects in memory...........
  if(_debug) cout<<" "<<GetName()<<"::Reset() all objects in memory........... "<<endl;
  itnext = _objarray->MakeIterator();
  while( calibobj = (CalibObj*) itnext->Next() ){
    //    cout<<" Delete ... "<<endl;
    //    calibobj->Print();
    calibobj->Delete();
  }
  _objarray->RemoveAll();
  // Reset all objects in file and close the file...........
  if(_debug) cout<<" "<<GetName()<<"::Reset all objects in file and close the file..........."<<endl;
  delete itnext;
  itnext = _keymap->MakeIterator();
  TObjArray filecol(_keymap->GetSize());
  while( obj =  itnext->Next() ){
    file = (TFile*)_keymap->GetValue(obj);
    //file->Close();
    //delete key;
    //    cout<<" Delete ... "<<endl;
    //    file->Print();
    if( file != 0 && _debug ){
      cout<<" "<<GetName()<<":: Reset() :: ------------------------------"<<endl;
      obj->Print();
      file->Print();
      cout<<" "<<GetName()<<":: Reset() :: ----------------------------- "<<endl;
    }
    if( (! filecol.Contains(file) ) && file != 0 )
      filecol.Add(file);
  }
  delete itnext;
  itnext = filecol.MakeIterator();
  while( file = (TFile*) itnext->Next() ){
    file->Close();
    delete file;
  }
  delete itnext;

  _keymap->RemoveAll();
  
  return;
};
//------------------------------------------------------------
int CalibRuns::Append(CalibRuns* calibr,float scale){
  if(_debug) cout<<" "<<GetName()<<"::Append(TDirectory*) ......... "<<endl;
  // Access current object .........................
  if( _calibobj != NULL ){
    if( _keymap->Contains(_calibobj->GetName()) ){
      _calibobj->Delete();
    }
  }
  _calibobj = NULL;
  //
  int read_num = 0;
  TIterator* itnext = calibr->_keymap->MakeIterator();
  TFile* tfile;
  TKey* key;
  int run,seq;
  while ( key = (TKey *) (itnext->Next()) ) {
    if( strncmp(key->GetName(),calibr->GetName(),strlen(calibr->GetName())) == 0 ){
      char hname[256];
      sprintf(hname,"%s_%%d_%%d",calibr->GetName());
      sscanf(key->GetName(),hname,&run,&seq);
      if(_debug) cout<<" "<<GetName()<<"::Append() scan : "<<key->GetName()<<"   run = "<<run<<endl;
      if( run > 0 ){
	tfile = (TFile*) calibr->_keymap->GetValue(key);
	if( tfile != 0 ){
	  if(_debug) cout<<" "<<GetName()<<"::Append()   tfile = "<<tfile<<" : "<<tfile->GetName()<<endl;
	  tfile->cd();
	  TObject* obj = key->ReadObj();
	  CalibObj* cobj = dynamic_cast<CalibObj*>( obj );
	  if( obj ){
	    if( Append(cobj,run,seq,scale) )
	      read_num++;
	    obj->Delete();
	  }
	} else {
	  cout<<" "<<GetName()<<"::Append() can't read health object in "<<calibr->GetName()<<endl;
	}
      }
    }
  }
  delete itnext;
  cout<<" "<<GetName()<<"::Append() append "<<read_num<<" object...."<<endl;
  return(read_num);
};
//------------------------------------------------------------
int CalibRuns::Append(TDirectory* dir,float scale){
  if(_debug) cout<<" "<<GetName()<<"::Append(TDirectory*) ......... "<<endl;
  // Access current object .........................
  if( _calibobj != NULL ){
    if( _keymap->Contains(_calibobj->GetName()) ){
      _calibobj->Delete();
    }
  }
  _calibobj = NULL;
  //
  int read_num = 0;
  TIter* itnext = new TIter( dir->GetListOfKeys() );
  TKey* key;
  int run,seq;
  while ( key = (TKey *) (itnext->Next()) ) {
    if( strncmp(key->GetName(),GetName(),strlen(GetName())) == 0 ){
      char hname[256];
      sprintf(hname,"%s_%%d_%%d",GetName());
      sscanf(key->GetName(),hname,&run,&seq);
      if(_debug) cout<<" "<<GetName()<<"::Append() scan : "<<key->GetName()<<"   run = "<<run<<endl;
      if( run > 0 ){
	dir->cd();
	TObject* obj = key->ReadObj();
	CalibObj* cobj = dynamic_cast<CalibObj*>( obj );
	if( obj ){
	  if( Append(cobj,run,seq,scale) )
	    read_num++;
	  obj->Delete();
	}
      }
    }
  }
  delete itnext;
  cout<<" "<<GetName()<<"::Append() append "<<read_num<<" object...."<<endl;
  return(read_num);
};
//------------------------------------------------------------
bool CalibRuns::Append(CalibObj* calibo,int run,int seq,float scale){
  if(_debug) cout<<" "<<GetName()<<"::Append(CalibObj*,int,int,float) "<<endl;
  bool status = false;
  char hname[128];
  sprintf(hname,"%s_%d_%d",GetName(),run,seq);
  //
  if(_debug) cout<<" "<<GetName()<<"::Append() try to append "<<calibo->GetName()<<"  (run,seq) = ("<<run<<","<<seq<<") "<<endl;
  if(_debug) cout<<"\t";
  if( Read(run,seq) > 0  ) {
    if(_debug) cout<<" "<<GetName()<<"::Append() calling CalibObj::Add() method into "<<_calibobj->GetName()<<endl;
    if( _calibobj->Add(calibo,scale) > 0 )
      status = true;
    else
      if(_debug) cout<<" "<<GetName()<<"::Append() Error in CalibObj::Add() method "<<endl;
    CalibRuns::Initialize();
  } else {
    if(_debug) cout<<" "<<GetName()<<"::Append() new run object "<<endl;
    if( _writedir->IsWritable() ){
      _writedir->cd();
      if( scale != 1.0 )
	calibo->Scale(scale);
      calibo->Write(hname);
      TKey* key = _writedir->GetKey(hname);
      if( key != 0 ){
	_keymap->Add(key,_writedir);
	//if( _initialized && _calibobj ) cout<<" initialized "<<endl;
	if( _initialized && _calibobj ) _calibobj->Delete(); // FIX.ME....
	_calibobj = (CalibObj*) key->ReadObj();
	_calibobj->SetName(hname);
	_initialized = true;
	status = true;
	_run = run;
	_seq = seq;
	_nch = calibo->GetNch();
      } else {
	cout<<" "<<GetName()<<"::Append() Error!! getting TKey "<<endl;
      }
    } else {
      cout<<" "<<GetName()<<"::Append() "<<_writedir->GetName()<<"is not writable "<<endl;
    }
  }
  return status;
};
//------------------------------------------------------------
//int CalibRuns::Add(CalibRuns* calibh){
//  int add_num = 0;
//  if( _run == calibh->GetRun() && _seq == calibh->GetSeq() ){
//    _calibobj->Add(calibh->_calibobj);
//    add_num++;
//  } else {
//    cout<<" Can't add with different run and sequence "<<endl;
//  }
//  return(add_num);
//};
//------------------------------------------------------------
bool CalibRuns::Read(int run,int seq,TDirectory* dir){
  TDirectory* current_dir = gDirectory;
  //
  char hname[256];
  sprintf(hname,"%s_%d_%d",GetName(),run,seq);
  // From Existing object .....................
  if( _initialized && _calibobj ){
    if(_debug) cout<<" "<<GetName()<<"::Read() hname = "<<hname<<endl;
    if(_debug) cout<<" "<<GetName()<<"::Read() _calibobj = "<<(CalibObj*)_calibobj<<endl;
    if(_debug) cout<<" "<<GetName()<<"::Read() _calibobj->GetName() = "<<_calibobj->GetName()<<endl;
    if( strcmp(_calibobj->GetName(),hname) != 0 ){
      _calibobj->Delete();
      _calibobj = NULL;
      _initialized = false;
    }
  }
  // From memory...............................
  if( _calibobj == NULL ){
    //cout<<" "<<GetName()<<"::Read() ....... From memory "<<endl;
    //CalibRuns::Initialize();
    if( _objarray->Contains(hname) )
      _calibobj = (CalibObj*) _objarray->FindObject(hname);
  }
  // From File..................................
  if( _calibobj == NULL ){
    //cout<<" "<<GetName()<<"::Read() ....... From file "<<endl;
    TAssoc* ass = (TAssoc*) _keymap->FindObject(hname);
    if( ass != 0 ){
      TKey* key = (TKey*) ass->Key();
      TFile* file = (TFile*) ass->Value();
      file->cd();
      _calibobj = (CalibObj*) key->ReadObj();
    }
  }
  // From Directory..............................
  if( _calibobj == NULL && dir != NULL ){
    //cout<<" "<<GetName()<<"::Read() ....... From directory "<<endl;
    TKey* key = dir->GetKey(hname);
    if( key != 0 ){
      _keymap->Add(key,dir);
      _calibobj = (CalibObj*) key->ReadObj();
    }
  }

  // Summary......................................
  if( _calibobj == NULL ){
    if(_debug) cout<<" "<<GetName()<<"::Read() Can't read run# "<<run<<":"<<seq<<endl;
    return false;
  }
  _initialized = true;
  _run = run;
  _seq = seq;
  _nch = _calibobj->GetNch();
  ResetGlobal();
  ResetBuffer();
  //
  current_dir->cd();
  return true;
};
//------------------------------------------------------------
int CalibRuns::ReadList(char* filename){
  int status = 0 ;
  int line = 0;
  int all_line = 0;
  char cline[1024];
  ifstream ftmp(filename);
  while( ftmp.getline(cline,1024) ) all_line++;
  ftmp.close();
  //
  ifstream fin(filename);
  while( fin.getline(cline,1024) ){
    cout<<" "<<GetName()<<"::ReadList() reading file ("<<line++<<"/"<<all_line<<") : "<<cline<<endl;
    status += Read(cline);
  }
  return status;
};
//------------------------------------------------------------
int CalibRuns::Read(char* filename){
  //cout<<" "<<GetName()<<"Read(char* filename) ......... "<<endl;
  int ret = 0;
  TFile* tfile = new TFile(filename);
  _filearray->Add(tfile);
  if( tfile ){
    ret = Read( (TDirectory*) tfile );
    if( ret == 0 ){
      delete tfile;
    }
  }
  return ret;
}
//------------------------------------------------------------
int CalibRuns::Read(TDirectory* dir){
  if(_debug) cout<<" "<<GetName()<<"::Read(TDirectory*) ......... "<<endl;
  // Access current object .........................
  if( _calibobj != NULL ){
    if( _keymap->Contains(_calibobj->GetName()) ){
      _calibobj->Delete();
    }
  }
  _calibobj = NULL;
  //
  int read_num = 0;
  TIter* itnext = new TIter( dir->GetListOfKeys() );
  TKey* key;
  int run,seq;
  TKey* readkey = NULL;
  while ( key = (TKey *) (itnext->Next()) ) {
    //TObject* obj = dir->Get(key->GetName());
    //if( obj->InheritsFrom(CalibObj::Class()) ) {
    //    cout<<" "<<GetName()<<"::Read() .. try to "<<key->GetName()<<endl; 
    if( strncmp(key->GetName(),GetName(),strlen(GetName())) == 0 ){
      char hname[256];
      sprintf(hname,"%s_%%d_%%d",GetName());
      sscanf(key->GetName(),hname,&run,&seq);
      //cout<<" "<<GetName()<<"::Read() scan : "<<key->GetName()<<"   run = "<<run<<endl;
      //      if( _keymap->Contains(key->GetName()) == 0  ){
      if( run > 0 ){
	_keymap->Add(key,dir);
	readkey = key;
	read_num++;
      }
	//      } else {
	//	if( Read(run,seq) ){
	//	  cout<<" "<<GetName()<<"::Read() call CalibObj::Add() method.. "<<endl;
	//	  CalibObj* cobj = (CalibObj*) key->ReadObj();
	//	  _calibobj->Add(cobj);
	//	  delete _calibobj;
	//	  delete cobj;
	//	} else {
	//	  cout<<" "<<GetName()<<"::Read() Failed to call CalibObj::Add() method.. "<<endl;
	//	}
	//      }

    }
  }
  delete itnext;
  if( read_num == 0 ){
    cout<<" "<<GetName()<<"::Read() Can't read any data "<<endl;
    return(0);
  }
  //
  //  Print();
  //  cout<<" -------------------------------------------------- "<<endl;
  //  dir->ls();
  //  cout<<" "<<GetName()<<"::Read() read_num = "<<read_num<<endl;
  //  cout<<" "<<GetName()<<"::Read() readkey = "<<readkey->GetName()<<endl;
  //  cout<<" -------------------------------------------------- "<<endl;
  //
  dir->cd();
  _calibobj = (CalibObj*) readkey->ReadObj(); // copy object to memory
  if( _calibobj == NULL) {
    cout<<" "<<GetName()<<"::Read() read data is wrong type.. "<<endl;
    return(0);
  }
  _initialized = true;
  _run = run;
  _seq = seq;
  _nch = _calibobj->GetNch();
  ResetGlobal();
  ResetBuffer();

  cout<<" "<<GetName()<<"::Read()  read "<<read_num<<" object...."<<endl;
  return(read_num);
};
//------------------------------------------------------------
int CalibRuns::Readable(TDirectory* dir){
  char hname[256];
  int read_num = 0;
  TIter* itnext = new TIter( dir->GetListOfKeys() );
  TKey* key;
  int run,seq;
  while ( key = (TKey *) (itnext->Next()) ) {
    //if(_debug) cout<<" "<<GetName()<<"::Readable() .. try to "<<key->GetName()<<endl; 
    if( strncmp(key->GetName(),GetName(),strlen(GetName())) == 0 ){
      sprintf(hname,"%s_%%d_%%d",GetName());
      sscanf(key->GetName(),hname,&run,&seq);
      if(_debug) cout<<" "<<GetName()<<"::Readable() scan : "<<key->GetName()<<"   run = "<<run<<endl;
      if( run > 0 ){
	read_num++;
      }
    }
  }
  delete itnext;
  return read_num;
};
//------------------------------------------------------------
int CalibRuns::Write(TDirectory* dir){
  if(! dir->IsWritable() ){
    cout<<" "<<GetName()<<"::Write() directory("<<dir->GetName()<<") is not writable. "<<endl;
    return 0;
  }
  TDirectory* current_dir = gDirectory;
  dir->cd();
  TKey* key;
  CalibObj* calibobj;
  TIterator* itnext;
  // If the initialization is not completed,
  if( _initialized == false && _all_num > 0 ){
    //cout<<" Creating histgrams from existing events ...... "<<endl;
    int stat = CalibRuns::Initialize();
    if(! stat )
      stat = Initialize(); // This is virtual function
    if(! stat ){
      cout<<" "<<GetName()<<"::Write() can't finish the run "<<_run<<":"<<_seq<<"'s job... "<<endl;
      cout<<"                   need expert ..... "<<endl;
      current_dir->cd();
      return 0;
    }
  }
  //
  // Writing objects in file .........................
  //cout<<" Writing objects in file ........................."<<endl;
  itnext = _keymap->MakeIterator();
  while( key = (TKey*) itnext->Next() ){
    TFile* file = (TFile*) _keymap->GetValue(key);
    // cout<<" "<<GetName()<<"   file = "<<file->GetName()<<endl;
    if( strcmp(file->GetName(), dir->GetName()) != 0 ){
      file->cd();
      calibobj = (CalibObj*) key->ReadObj();
      dir->cd();
      if( calibobj != NULL ){
	if(_debug) cout<<" "<<GetName()<<" is writing: "<< calibobj->GetName()<<endl;
	if(_debug) cout<<"   from "<<file->GetName()<<" to "<<dir->GetName()<<endl;
	calibobj->Write();
      }
      calibobj->Delete();
    }
  }
  delete itnext;
  //
  // Writing objects in memory .........................
  //cout<<" Writing objects in memory ........................."<<endl;
  if( _calibobj != NULL ){
    if(! _keymap->Contains(_calibobj->GetName()) )
      if(! _objarray->Contains(_calibobj) ){
	_objarray->Add(_calibobj);
    }
    _calibobj = NULL;
  }
  itnext = _objarray->MakeIterator();
  while( calibobj = (CalibObj*) itnext->Next() ){
    if(_debug) cout<<" "<<GetName()<<" is writing: "<<calibobj->GetName()<<endl;
    calibobj->Write();
    key = dir->GetKey(calibobj->GetName());
    if( key != NULL ){
      _keymap->Add(key,dir);
    }
    calibobj->Delete();
  }
  _objarray->RemoveAll();
  delete itnext;
  //
  _initialized = false;
  current_dir->cd();
  return(_objarray->GetEntries()+_keymap->GetSize() );
};
//------------------------------------------------------------
void CalibRuns::Print(Option_t* option) const{
  cout<<" ---CalibRuns:: "<<_keymap->GetSize()<<" objects in file --------------------------------- "<<endl;
  _keymap->Print(option);
  cout<<" ---CalibRuns:: "<<_objarray->GetEntries()<<" objects in memory ------------------------------- "<<endl;
  _objarray->Print(option);
  if( _calibobj ){
    cout<<" ---CalibRuns:: current objects ----------------------------------- "<<endl;
    _calibobj->Print();
  }
  cout<<" ------------------------------------------------------------------ "<<endl;
  return;
};
//------------------------------------------------------------
//
//

