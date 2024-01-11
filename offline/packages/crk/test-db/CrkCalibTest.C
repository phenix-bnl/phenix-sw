{
gROOT->Macro("CrkCalibInit.C");
CrkCalib *cal = new CrkCalib();
PHTimeStamp time(2000,6,1,0,0,0);
cal->fetch_DB_adc_phe(time);
cal->print_adc_phe();
cal->fetch_DB_adc_ped(time);
cal->print_adc_ped();
cal->fetch_DB_t0(time);
cal->print_t0();
cal->fetch_DB_tac(time);
cal->print_tac();
cal->fetch_DB_slew(time);
cal->print_slew();
}
