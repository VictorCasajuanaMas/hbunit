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

CLASS TestCase FROM TestBase

   METHOD Run()
   METHOD before()                     VIRTUAL
   METHOD beforeClass()                VIRTUAL
   METHOD after()                      VIRTUAL
   METHOD afterClass()                 VIRTUAL

   DATA aCategories                    INIT { "all" }

   PROTECTED:
      DATA oAssert                      

      METHOD Assert()                  INLINE ( if( empty( ::oAssert ), ::oAssert := TAssert():new( ::Result() ), ), ::oAssert )

ENDCLASS

//---------------------------------------------------------------------------//

METHOD Run() CLASS TestCase
  
   ::Result():Run( self )

RETURN ( ::oResult() )

//---------------------------------------------------------------------------//

