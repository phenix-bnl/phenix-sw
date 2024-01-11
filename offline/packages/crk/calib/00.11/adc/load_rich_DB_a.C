{
cal = new CrkCalib();

cal->read_file_adc("crk_adc_10126.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10242.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10360.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10364.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10427.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10545.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10603.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10645.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10744.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_10767.txt");
cal->store_DB_adc();


}
