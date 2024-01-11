void mrpc_address_checkDB()
{
  // __________________________________________________________
  //
  //  This macro does the DB check for MRPC address.
  //  
  //  Run 146651: the first run for MRPC (TOF.W) time-in.
  //  (2005-01-06 04:51:34). 
  //  The end run is not specified, i.e. put the "FarFuture"
  //  EndTime by default. -- T.Chujo (1/31/2005)
  // __________________________________________________________

  gSystem->Load("libPgCalInstance.so");
  gSystem->Load("/phenix/data20/chujo/MRPC/offline/install/lib/libmrpc.so");

  // MRPC Address
  MrpcAddress* address = new MrpcAddress();
  address->set_debug(1);
  
  // DB Check for Run 146651
  address->fetch(146651);
  address->print(10);
}
