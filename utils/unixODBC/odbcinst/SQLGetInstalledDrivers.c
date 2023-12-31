/**************************************************
 * SQLGetInstalledDrivers
 *
 **************************************************
 * This code was created by Peter Harvey @ CodeByDesign.
 * Released under LGPL 28.JAN.99
 *
 * Contributions from...
 * -----------------------------------------------
 * Peter Harvey		- pharvey@codebydesign.com
 **************************************************/
#include <odbcinstext.h>

BOOL SQLGetInstalledDrivers(	LPSTR	pszBuf,
								WORD	nBufMax,
								WORD	*pnBufOut )
{
	HINI	hIni;
	WORD	nBufPos		= 0;
	WORD	nToCopySize	= 0;
	char	szObjectName[INI_MAX_OBJECT_NAME+1];
    char    szIniName[ INI_MAX_OBJECT_NAME + 1 ];

#ifdef VMS
    sprintf( szIniName, "%sODBCINST.INI", odbcinst_system_file_path() );
#else
    sprintf( szIniName, "%s/odbcinst.ini", odbcinst_system_file_path() );
#endif

	if ( iniOpen( &hIni, szIniName, "#;", '[', ']', '=', TRUE ) != INI_SUCCESS )
	{
        inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_COMPONENT_NOT_FOUND, "" );
		return FALSE;
	}
	
	memset( pszBuf, '\0', nBufMax );

	iniObjectFirst( hIni );
	while ( iniObjectEOL( hIni ) == FALSE )
	{
		iniObject( hIni, szObjectName );
		if ( strcmp( szObjectName, "ODBC" ) == 0 )
		{
		    iniObjectNext( hIni );
		    continue;
		}

		if ( (strlen( szObjectName )+1) > (nBufMax - nBufPos) )
		{
			nToCopySize = nBufMax - nBufPos;
			strncpy( &(pszBuf[nBufPos]), szObjectName, nToCopySize );
			nBufPos = nBufMax;
			break;
		}
		else
		{
			strcpy( &(pszBuf[nBufPos]), szObjectName );
			nBufPos += strlen( szObjectName )+1;
		}
		iniObjectNext( hIni );
	}
	iniClose( hIni );

	if ( pnBufOut )
		*pnBufOut = nBufPos-1;
	
	return TRUE;
}


