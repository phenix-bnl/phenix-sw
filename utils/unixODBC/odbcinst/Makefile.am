lib_LTLIBRARIES = libodbcinst.la

INCLUDES = -I../include \
	-DSYSTEM_FILE_PATH=\"@sysconfdir@\" \
	-DDEFLIB_PATH=\"@libdir@\" \
	-DSHLIBEXT=\"@SHLIBEXT@\" \
	$(INCLTDL)

libodbcinst_la_LDFLAGS = \
    -version-info @LIB_VERSION@ \
    -export-dynamic 

libodbcinst_la_LIBADD = \
	../ini/libinilc.la \
	../log/libloglc.la \
	../lst/liblstlc.la \
	$(LIBLTDL) $(LIBADD_DL) 

libodbcinst_la_DEPENDENCIES = \
    ../log/libloglc.la \
    ../ini/libinilc.la \
    ../lst/liblstlc.la \
    $(LIBLTDL) 

sysconf_DATA=

libodbcinst_la_SOURCES = \
	ODBCINSTConstructProperties.c \
	ODBCINSTDestructProperties.c \
	ODBCINSTSetProperty.c \
	ODBCINSTValidateProperties.c \
	ODBCINSTValidateProperty.c \
	SQLConfigDataSource.c \
	SQLConfigDriver.c \
	SQLCreateDataSource.c \
	SQLGetAvailableDrivers.c \
	SQLGetConfigMode.c \
	SQLGetInstalledDrivers.c \
	SQLGetPrivateProfileString.c \
	SQLGetTranslator.c \
	SQLInstallDriverEx.c \
	SQLInstallDriverManager.c \
	SQLInstallTranslatorEx.c \
	SQLInstallerError.c \
	SQLManageDataSources.c \
	SQLPostInstallerError.c \
	SQLReadFileDSN.c \
	SQLRemoveDSNFromIni.c \
	SQLRemoveDriver.c \
	SQLRemoveDriverManager.c \
	SQLRemoveTranslator.c \
	SQLSetConfigMode.c \
	SQLValidDSN.c \
	SQLWriteDSNToIni.c \
	SQLWriteFileDSN.c \
	SQLWritePrivateProfileString.c \
	_logging.c \
	_odbcinst_ConfigModeINI.c \
	_odbcinst_UserINI.c \
	_odbcinst_SystemINI.c \
	_odbcinst_GetSections.c \
	_odbcinst_GetEntries.c \
	_SQLGetInstalledDrivers.c \
	_SQLWriteInstalledDrivers.c

libodbcinstlc_la_LDFLAGS =

noinst_LTLIBRARIES = libodbcinstlc.la

libodbcinstlc_la_SOURCES = \
	ODBCINSTConstructProperties.c \
	ODBCINSTDestructProperties.c \
	ODBCINSTSetProperty.c \
	ODBCINSTValidateProperties.c \
	ODBCINSTValidateProperty.c \
	SQLConfigDataSource.c \
	SQLConfigDriver.c \
	SQLCreateDataSource.c \
	SQLGetAvailableDrivers.c \
	SQLGetConfigMode.c \
	SQLGetInstalledDrivers.c \
	SQLGetPrivateProfileString.c \
	SQLGetTranslator.c \
	SQLInstallDriverEx.c \
	SQLInstallDriverManager.c \
	SQLInstallTranslatorEx.c \
	SQLInstallerError.c \
	SQLManageDataSources.c \
	SQLPostInstallerError.c \
	SQLReadFileDSN.c \
	SQLRemoveDSNFromIni.c \
	SQLRemoveDriver.c \
	SQLRemoveDriverManager.c \
	SQLRemoveTranslator.c \
	SQLSetConfigMode.c \
	SQLValidDSN.c \
	SQLWriteDSNToIni.c \
	SQLWriteFileDSN.c \
	SQLWritePrivateProfileString.c \
	_logging.c \
	_odbcinst_ConfigModeINI.c \
	_odbcinst_UserINI.c \
	_odbcinst_SystemINI.c \
	_odbcinst_GetSections.c \
	_odbcinst_GetEntries.c \
	_SQLGetInstalledDrivers.c \
	_SQLWriteInstalledDrivers.c
