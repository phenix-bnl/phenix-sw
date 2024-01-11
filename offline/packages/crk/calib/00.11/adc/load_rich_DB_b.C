{
cal = new CrkCalib();

cal->read_file_adc("crk_adc_11285.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11291.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11305.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11797.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11801.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11805.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11891.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11893.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11928.txt");
cal->store_DB_adc();

cal->read_file_adc("crk_adc_11970.txt");
cal->store_DB_adc();

cal->store_DB_adc();

}
