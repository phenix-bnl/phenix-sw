/**************************************************
 * _SQLWriteInstalledDrivers
 *
 * Added to allow ODBC Config programs and the iODBC
 * driver manager to access system information without
 * having to worry about where it is... just like accessing
 * Data Source information. So no surprise... its just
 * like SQLWritePrivateProfileString()!
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign.
 * Released under LGPL 28.JAN.99
 *
 * Contributions from...
 * -----------------------------------------------
 * Peter Harvey		- pharvey@codebydesign.com
 **************************************************/

#include <odbcinstext.h>

BOOL _SQLWriteInstalledDrivers(
								LPCSTR	pszSection,
								LPCSTR	pszEntry,
								LPCSTR	pszString )
{
	HINI	hIni;
    char    szIniName[ INI_MAX_OBJECT_NAME + 1 ];

	/* SANITY CHECKS */
	if ( pszSection == NULL )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
		return FALSE;
	}
	if ( pszSection[0] == '\0' )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "" );
		return FALSE;
	}

	/* OK */

#ifdef VMS
    sprintf( szIniName, "%sODBCINST.INI", odbcinst_system_file_path() );
#else
    sprintf( szIniName, "%s/odbcinst.ini", odbcinst_system_file_path() );
#endif

	if ( iniOpen( &hIni, szIniName, "#;", '[', ']', '=', TRUE  ) != INI_SUCCESS )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_REQUEST_FAILED, "" );
		return FALSE;
	}

	/* delete section */
	if ( pszEntry == NULL )
	{
		if ( iniObjectSeek( hIni, (char *)pszSection ) == INI_SUCCESS )
			iniObjectDelete( hIni );
	}
	/* delete entry */
	else if	( pszString == NULL )
	{
		if ( iniPropertySeek( hIni, (char *)pszSection, (char *)pszEntry, "" ) == INI_SUCCESS )
			iniPropertyDelete( hIni );
	}
	else
	{
		/* add section */
		if ( iniObjectSeek( hIni, (char *)pszSection ) != INI_SUCCESS )
		{
			iniObjectInsert( hIni, (char *)pszSection );
		}

		/* update entry */
		if ( iniPropertySeek( hIni, (char *)pszSection, (char *)pszEntry, "" ) == INI_SUCCESS )
		{
/*			iniObjectSeek( hIni, (char *)pszSection ); */
			iniPropertyUpdate( hIni, (char *)pszEntry, (char *)pszString );
		}
		/* add entry */
		else
		{
			iniObjectSeek( hIni, (char *)pszSection );
			iniPropertyInsert( hIni, (char *)pszEntry, (char *)pszString );
		}

	}

	if ( iniCommit( hIni ) != INI_SUCCESS )
	{
		iniClose( hIni );
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_REQUEST_FAILED, "" );
		return FALSE;
	}

	iniClose( hIni );

	return TRUE;
}




