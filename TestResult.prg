//-- copyright
// hbunit is a unit-testing framework for the Harbour language.
//
// Copyright (C) 2019 Manuel Calero Solis <manuelcalerosolis _at_ gmail _dot_ com>
//
// Based on hbunit from Enderson maia <endersonmaia _at_ gmail _dot_ com>
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

CLASS TestResult

   DATA oData

   METHOD new() CONSTRUCTOR

   METHOD run()

   HIDDEN:
      METHOD invokeTestMethod()
      METHOD getTestMethods()

ENDCLASS

//---------------------------------------------------------------------------//

METHOD new() CLASS TestResult

   ::oData  := TestResultData():new()

RETURN ( self )

//---------------------------------------------------------------------------//

METHOD run( oTest ) CLASS TestResult

   local cMethod
   local aTestMethods   := ::getTestMethods( oTest )

   ::oData:addTestCaseCount( len( aTestMethods ) )
      
   ::invokeTestMethod( oTest, "BEFORECLASS" )

   for each cMethod in aTestMethods
      ::invokeTestMethod( oTest, "BEFORE" )
      ::invokeTestMethod( oTest, cMethod )
      ::invokeTestMethod( oTest, "AFTER" )
   next

   ::invokeTestMethod( oTest, "AFTERCLASS" )

RETURN ( nil )

//---------------------------------------------------------------------------//

METHOD getTestMethods( oTest ) CLASS TestResult

   local aTestMethods   := {}
   local aMethods       := __objGetMethodList( oTest )

   aeval( aMethods, {| cMethod | if( left( cMethod, 5 ) == "TEST_", aadd( aTestMethods, cMethod ), ) } )

RETURN ( aTestMethods )

//---------------------------------------------------------------------------//

METHOD invokeTestMethod( oTest, cMethod ) CLASS TestResult

   local oError

   BEGIN SEQUENCE

      __objSendMsg( oTest, cMethod )

   RECOVER USING oError

      oError:Args := oTest:ClassName() + ":" + cMethod

      ::addError( oError )

   END 

RETURN ( nil )

//---------------------------------------------------------------------------//