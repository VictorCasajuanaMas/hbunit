//-- copyright
// hbunit is a unit-testing framework for the Harbour language.
//
// Copyright (C) 2014 Enderson maia <endersonmaia _at_ gmail _dot_ com>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
//
// See COPYRIGHT for more details.
//++

#include "hbunit.ch"

//---------------------------------------------------------------------------//

CLASS TestResultData

   CLASSDATA aErrors
   CLASSDATA aAsserts
   CLASSDATA aFailures
   CLASSDATA nTestCases
   CLASSDATA nAssertCount

   METHOD new() CONSTRUCTOR

   METHOD countErrors()
   METHOD countFailures()
   METHOD addError( oError )
   METHOD addFailure( oFailure )
   METHOD addAssert( oFailure )
   METHOD incrementAssertCount()
   METHOD addTestCaseCount( nCount )
   METHOD getErrors()
   METHOD getFailures()
   METHOD getTestCasesCount()
   METHOD getAssertCount()

ENDCLASS

//---------------------------------------------------------------------------//

METHOD new() CLASS TestResultData

   iif( ::aErrors == nil, ::aErrors := {}, )
   iif( ::aFailures == nil, ::aFailures := {}, )
   iif( ::aAsserts == nil, ::aAsserts := {}, )
   iif( ::nTestCases == nil, ::nTestCases := 0, )
   iif( ::nAssertCount == nil, ::nAssertCount := 0, )

RETURN ( SELF )

//---------------------------------------------------------------------------//

METHOD countErrors() CLASS TestResultData

RETURN ( len( ::aErrors ) )

//---------------------------------------------------------------------------//

METHOD countFailures() CLASS TestResultData

RETURN ( len( ::aFailures ) )

//---------------------------------------------------------------------------//

METHOD addError( oError ) CLASS TestResultData

RETURN ( aadd( ::aErrors, oError ) )

//---------------------------------------------------------------------------//

METHOD addFailure( oFailure ) CLASS TestResultData

RETURN ( aadd( ::aFailures, oFailure ) )

//---------------------------------------------------------------------------//

METHOD addAssert( cAssert ) CLASS TestResultData

RETURN ( aadd( ::aAsserts, cAssert ) )

//---------------------------------------------------------------------------//

METHOD incrementAssertCount() CLASS TestResultData

RETURN ( ::nAssertCount++ )

//---------------------------------------------------------------------------//

METHOD addTestCaseCount( nCount ) CLASS TestResultData

RETURN ( ::nTestCases += nCount )

//---------------------------------------------------------------------------//

METHOD getErrors() CLASS TestResultData

RETURN ( ::aErrors )

//---------------------------------------------------------------------------//

METHOD getFailures() CLASS TestResultData

RETURN ( ::aFailures )

//---------------------------------------------------------------------------//

METHOD getTestCasesCount() CLASS TestResultData

RETURN ( ::nTestCases )

//---------------------------------------------------------------------------//

METHOD getAssertCount() CLASS TestResultData

RETURN ( ::nAssertCount )

//---------------------------------------------------------------------------//