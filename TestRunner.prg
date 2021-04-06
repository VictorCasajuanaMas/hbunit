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
#include "fivewin.ch"

//---------------------------------------------------------------------------//

CLASS TestRunner
   
   METHOD new() CONSTRUCTOR

   METHOD run( oTest )

   PROTECTED:
      METHOD  showResults() VIRTUAL

ENDCLASS

//---------------------------------------------------------------------------//

METHOD New() CLASS TestRunner

RETURN ( self )

//---------------------------------------------------------------------------//

METHOD Run( oTest ) CLASS TestRunner

   ::showResults( oTest:run() )

RETURN ( nil )

//---------------------------------------------------------------------------//

