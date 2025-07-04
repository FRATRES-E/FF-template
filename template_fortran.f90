!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!>      VUA and IPSL/LSCE by the iLOVECLIM / LUDUS coding group / Within the LUDUS code environement

!       LICENSING TERMS:
!>      \copyright
!!      This file is part of [insert sub-component name here, in following Foobar]
!!      Foobar is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
!!      as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

!!      Foobar is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
!!      of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

!!      You should have received a copy of the GNU General Public License along with Foobar.
!!      If not, see <http://www.gnu.org/licenses/>.
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!dmr -- Adding the choice of components through the pre-processing options:
#include 'choixcomposantes.h'

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      MODULE: [Module_Name here]

!>     @author  The First Author (tfa)
!>     @author  The Second Author (tsa)
!>     @date Creation date: November, 41st, 2999

!>     @brief This module [Foo_mod] is handling the creation of a particuliar type of fried noodles ...

!>     DESCRIPTION : Here add the long_description of the module ...
!        - Subroutine [NAME] : This subroutine is for blabla
!        - Formula: \f$ \frac{d\lambda}{dt} , \frac{d\phi}{dt},  \frac{dz}{dt} \f$
!        - References: papers or other documents to be cited...(including link when possible)

!>     @date Last modification: SLastChangedDate$
!>     @author Last modified by : tfa, tsa!
!>     @version This is svn version: $LastChangedRevision$

!>     REVISION HISTORY:
!        YYYY-MM-DD - Initial Version
!        TODO_YYYY-MM-DD - TODO_describe_appropriate_changes to be done or discussed - TODO_name
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!> help -- Hello, this is just a template for writing a clean file in Fortran. 
!> help -- Please follow the steps by declaring who is writing.
!> help --  Please delete the comments starting with ‘!> help -- ’ when the file is clean.

!> help -- Bellow are the stop to follow : 
!> help -- 1) Complete the paragraph from lines 20 to 40

!> help -- 2) Creation du module : 
!>                - a) Name of the module (l.75) + End of the module (l.150)
!>                - b) Declare variables or functions that are required in this module, but are declared in other modules 
!>                      (l.77 to 79) (/!\ this variables or functiuns need to be PUBLIC)
!>                - c)  Declare the variables required in the module (l.84 to 91). If they are useful elsewhere, 
! >                      these variables or functions must be made public (l.85 to 87). 

!> help -- 3) Create subroutine (maybe more) = lines 98 to 119
!>                - a) Put the name of the subroutine and close the subroutine (l.99 and 118)
!>                - b) Precise the author, the goal etc (l.102 to 105)
!>                - c) Declare the parameters for the subroutine (in) (l.108 and 108)
!>                - d) Declare the output variable (out) (l. 109)
!>                - e) If necessary, you can create the “intermediate” variables required for the subroutine. (l.112)
!>                - f) Enter the equations and, if possible, quote the sources. You can use some functions.  (l.114 to 116)

!> help -- 4) Create function (maybe more) = lines 124 to 144
!>                - a) Put the name of the functions and close the it (l.125 nd 143)
!>                - b) Precise the author, the goal etc (l.128 to 135)
!>                - c) Declare the variables for the function (in and out (l.138 and 139)
!>                - d) If necessary, you can create the “intermediate” variables = "local variables"
!>                - e) Enter the equations and, if possible, quote the sources (l.141)

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|


      module Foo_mod 

       use global_constants_mod, only: dblp=>dp, ip
       use AnotherModule_mod, only: some_variable
       use AnotherModule_mod, only: some_otherfunction

       implicit none
       private

       ! people -- PUBLIC variables, functions, subroutines. 
       public :: someFunction, function_a
       real(kind=dblp), parameter, public    :: var = 0.1_dblp    ![unit]
       integer(kind=ip), parameter, public  :: var2 = 0             ![unit]
       
       ! people -- Variables only required for this module 
      integer(kind=ip), PARAMETER :: var3 = 4                         ![unit]
      real(kind=dblp)                           :: var4                             
      
      contains
      
      
! <<<<<<<<<<<<<<<<<<<<<<<<<<  SUBROUTINE PART >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

! *******************************************************************************************************************
      SUBROUTINE name_subroutine(parameters1, parameters2, output)
! *******************************************************************************************************************

!>      AUTHOR : people
!>      DESCRIPTION: Subroutine is for blablabla --> Here add the long_description of the module ...
!>      REFERENCES: papers or other documents to be cited...(including link when possible) 
!>      CALL : This subroutine is call in "module_file"

       real(kind=dblp), intent(in) 	 :: parameters1     ! variablea [unit]
       real(kind=dblp), intent(in) 	 :: parameters2     ! variableb [unit]
       real(kind=dblp), intent(out)  :: output                ! variablec [unit]

       ! Local variables
       real(kind=dblp) :: intermediate1
       
       ! Begin of the subroutine
       intermediate1 = someFunction(parameters1, parameters2)
       output = intermediate1 + parameters1   

      END SUBROUTINE name_subroutine
! *******************************************************************************************************************
      

! <<<<<<<<<<<<<<<<<<<<<<<<<<  FUNCTIONS PART >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!=======================================================================
      function someFunction(inParam1, inParam2) result(returnValue)
!=======================================================================

!>      AUTHOR : people
!>      DESCRIPTION: Subroutine is for blablabla --> Here add the long_description of the module ...
!>      REFERENCES: papers or other documents to be cited...(including link when possible) 

!>      Input variable :
!>         - inParam1 = blabla 
!>         - inParam2 = blabla2
!>      Output variable : GPP_O2.


       real(kind=ip), intent(in)    :: inParam1; inParam2     ![unit]
       real, (kind=dblp),               :: returnValue                   ![unit]
       
       returnValue = inParam1 * inParam2

      end function someFunction
!=======================================================================


!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      The End of All Things (op. cit.)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
end module Foo_mod
