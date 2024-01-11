{
// this macro is an example of how to use hardware/software map for pixels

gSystem->Load("libsvx");

  svxAddress* a = new svxAddress();
  a->set_Verbose(1);

// read hard/soft map from database
// please use master database for now (setenv ODBCINI /opt/phenix/etc/odbc.ini.master) 12/15/2010
//  PHTimeStamp Tsearch; Tsearch.setToSystemTime();
//  a->Fetch(&Tsearch);  

// use hard-coded map
  a->Initialize(); 

// convert from hardware to software
  int layer  = 0;
  int ladder = 0;
  int roc    = 0;
  int module = 0;
  int sensor = 1-(roc/4) + (1-module%2)*2;
//  int sensor = 3;
  int channel = 10;
  cout << "hardware: " << layer << " " << ladder << " " << sensor << " " << roc << " " << module << " " << channel << endl;
  int x = a->getSensorIX0(layer, ladder, sensor, channel, roc, module);
  int z = a->getSensorIZ0(layer, ladder, sensor, channel, roc, module);
  cout << "software: " << x << " " << z << endl;
 
// convert from software to hardware
  int mylayer = 1;
  int myladder = 1;
  int mysensor = 1;
  int myx = 15;
  int myz = 10;
  cout << endl << "   software: " << mylayer << " " << myladder << " " << mysensor << " " << myx << " " << myz << endl;
  int mymodule = a->svxAddress::getModuleSensor0(mylayer, myladder, mysensor, myx, myz);
  int myroc = a->svxAddress::getROCSensor0(mylayer, myladder, mysensor, myx, myz);
  int mychannel = a->svxAddress::getChannelSensor0(mylayer, myladder, mysensor, myx, myz);
  cout << "   hardware: " << myroc << " " << mymodule << " " << mychannel << endl;
  

}

