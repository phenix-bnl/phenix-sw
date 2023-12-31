/**************************************************
 * ODBCINSTConstructProperties
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

static const char *aYesNo[] =
{
	"Yes",
	"No",
	NULL
};


int ODBCINSTConstructProperties( char *pszDriver, HODBCINSTPROPERTY *hFirstProperty )
{
	char 				szError[LOG_MSG_MAX+1];
	char 				szDriverSetup[ODBC_FILENAME_MAX+1];
	HINI 				hIni;
	int					(*pODBCINSTGetProperties)( HODBCINSTPROPERTY );
	void 				*hDLL	= NULL;
	HODBCINSTPROPERTY	hLastProperty;
	char				szSectionName[INI_MAX_OBJECT_NAME+1];
    char                szIniName[ INI_MAX_OBJECT_NAME + 1 ];

	/* SANITY CHECKS */
	if ( pszDriver == NULL )
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "Need a driver name. Make it the friendly name." );
		return ODBCINST_ERROR;
	}
#ifdef VMS
    sprintf( szIniName, "%sODBCINST.INI", odbcinst_system_file_path());
#else
    sprintf( szIniName, "%s/odbcinst.ini", odbcinst_system_file_path());
#endif

	/* GET DRIVER SETUP FILE NAME FOR GIVEN DRIVER */
	if ( iniOpen( &hIni, szIniName, "#;", '[', ']', '=', FALSE ) != INI_SUCCESS )
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "Could not open odbcinst.ini" );
		return ODBCINST_ERROR;
	}

	/* ASSUME USER FRIENDLY NAME FOR STARTERS */
	if ( iniPropertySeek( hIni, pszDriver, "Setup", "" ) != INI_SUCCESS )
	{
		/* NOT USER FRIENDLY NAME I GUESS SO ASSUME DRIVER FILE NAME */
		if ( iniPropertySeek( hIni, "", "Driver", pszDriver ) != INI_SUCCESS )
		{
			sprintf( szError, "Could not find driver (%s) in system information", pszDriver );
			inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, szError );
			iniClose( hIni );
			return ODBCINST_ERROR;
		}
		else
		{
			iniObject( hIni, szSectionName );
			if ( iniPropertySeek( hIni, szSectionName, "Setup", "" ) != INI_SUCCESS )
			{
				sprintf( szError, "Could not find Setup property for (%s) in system information", pszDriver );
				inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, szError );
				iniClose( hIni );
				return ODBCINST_ERROR;
			}
		}
	}

	iniValue( hIni, szDriverSetup );
	iniClose( hIni );

    /*
     * initialize libtool
     */

    lt_dlinit();

	/* TRY GET FUNC FROM DRIVER SETUP */
	if ( !(hDLL = lt_dlopen( szDriverSetup ))  )
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "Could not load library" );
		return ODBCINST_ERROR;
	}

	pODBCINSTGetProperties = (int(*)(struct tODBCINSTPROPERTY*)) lt_dlsym( hDLL, "ODBCINSTGetProperties" );

/*	PAH - This can be true even when we found the symbol.
    if ( lt_dlerror() != NULL ) 
*/
	if ( pODBCINSTGetProperties == NULL )
	{
		inst_logPushMsg( __FILE__, __FILE__, __LINE__, LOG_CRITICAL, ODBC_ERROR_GENERAL_ERR, "Could not find ODBCINSTGetProperties()" );
		return ODBCINST_ERROR;
	}
	
	/* MANDATORY PROPERTIES */
	(*hFirstProperty) 						= (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
	memset( (*hFirstProperty), 0, sizeof(ODBCINSTPROPERTY) );
	(*hFirstProperty)->nPromptType			= ODBCINST_PROMPTTYPE_TEXTEDIT;
	(*hFirstProperty)->pNext				= NULL;
    (*hFirstProperty)->bRefresh				= 0;
    (*hFirstProperty)->hDLL					= hDLL;
    (*hFirstProperty)->pWidget				= NULL;
    (*hFirstProperty)->pszHelp				= NULL;
	(*hFirstProperty)->aPromptData			= NULL;
	strncpy( (*hFirstProperty)->szName, "Name", INI_MAX_PROPERTY_NAME );
	strcpy( (*hFirstProperty)->szValue, "" );
	hLastProperty = (*hFirstProperty);

	(*hFirstProperty)->pNext 				= (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
	hLastProperty 							= (*hFirstProperty)->pNext;
	memset( hLastProperty, 0, sizeof(ODBCINSTPROPERTY) );
	hLastProperty->nPromptType				= ODBCINST_PROMPTTYPE_TEXTEDIT;
	hLastProperty->pNext					= NULL;
    hLastProperty->bRefresh					= 0;
    hLastProperty->hDLL						= hDLL;
    hLastProperty->pWidget					= NULL;
    (*hFirstProperty)->pszHelp				= NULL;
	(*hFirstProperty)->aPromptData			= NULL;
	strncpy( hLastProperty->szName, "Description", INI_MAX_PROPERTY_NAME );
	strncpy( hLastProperty->szValue, pszDriver, INI_MAX_PROPERTY_VALUE );

	hLastProperty->pNext 				= (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
	hLastProperty 							= hLastProperty->pNext;
	memset( hLastProperty, 0, sizeof(ODBCINSTPROPERTY) );
	hLastProperty->nPromptType				= ODBCINST_PROMPTTYPE_LABEL;
	hLastProperty->pNext					= NULL;
    hLastProperty->bRefresh					= 0;
    hLastProperty->hDLL						= hDLL;
    hLastProperty->pWidget					= NULL;
    (*hFirstProperty)->pszHelp				= NULL;
	(*hFirstProperty)->aPromptData			= NULL;
	strncpy( hLastProperty->szName, "Driver", INI_MAX_PROPERTY_NAME );
	strncpy( hLastProperty->szValue, pszDriver, INI_MAX_PROPERTY_VALUE );
/*
	hLastProperty->pNext 				= (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
	hLastProperty 						= hLastProperty->pNext;
	memset( hLastProperty, 0, sizeof(ODBCINSTPROPERTY) );
	hLastProperty->nPromptType			= ODBCINST_PROMPTTYPE_LISTBOX;
    hLastProperty->aPromptData			= malloc( sizeof(aYesNo) );
	memcpy( hLastProperty->aPromptData, aYesNo, sizeof(aYesNo) );
	strncpy( hLastProperty->szName, "Trace", INI_MAX_PROPERTY_NAME );
    strcpy( hLastProperty->szValue, "No" );

	hLastProperty->pNext 				= (HODBCINSTPROPERTY)malloc( sizeof(ODBCINSTPROPERTY) );
	hLastProperty 						= hLastProperty->pNext;
	memset( hLastProperty, 0, sizeof(ODBCINSTPROPERTY) );
	hLastProperty->nPromptType			= ODBCINST_PROMPTTYPE_FILENAME;
	strncpy( hLastProperty->szName, "TraceFile", INI_MAX_PROPERTY_NAME );
	strncpy( hLastProperty->szValue, "", INI_MAX_PROPERTY_VALUE );
*/

	/* APPEND OTHERS */
	pODBCINSTGetProperties( hLastProperty );

	return ODBCINST_SUCCESS;
}


