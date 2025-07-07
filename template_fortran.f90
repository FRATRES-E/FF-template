!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!
!   Copyright 2025 FRATRES-E (https://github.com/FRATRES-E)
!     FRamework for fAst TRansient Earth-system Studies and Education

!   Licensed under the Apache License, Version 2.0 (the "License");
!   you may not use this file except in compliance with the License.
!   You may obtain a copy of the License at

!       http://www.apache.org/licenses/LICENSE-2.0

!   Unless required by applicable law or agreed to in writing, software
!   distributed under the License is distributed on an "AS IS" BASIS,
!   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!   See the License for the specific language governing permissions and
!   limitations under the License.


!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!dmr -- Adding the choice of components through the pre-processing options:
#include 'choixcomposantes.h'

      module Foo_mod

       !! version: v1.0
       !! display: public private protected
       !! proc_internals: true


!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      MODULE: [Module_Name here]

!!     @author  Emeline Clermont (ec)
!!     @author  Didier M. Roche  (dmr)
!!     @date Creation date: July, 03rd, 2024

!!     @brief This module [Foo_mod] is handling the creation of a particuliar type of fried noodles ...

!>
!>     DESCRIPTION : Here add the long_description of the module ...
!>        - Subroutine [NAME] : This subroutine is for blabla
!>        - Formula:
!> $$ \frac{d\lambda}{dt} , \frac{d\phi}{dt},  \frac{dz}{dt} $$
!>     @reference References: papers or other documents to be cited... [Site the website if possible](https://iloveclim.eu)
!>     @date Last modification: SLastChangedDate$
!>     @author Last modified by : tfa, tsa!

!!     REVISION HISTORY:
!!        YYYY-MM-DD - Initial Version
!!        TODO_YYYY-MM-DD - TODO_describe_appropriate_changes to be done or discussed - TODO_name
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!! @note
!! <pre>
!! help -- Hello, this is just a template for writing a clean file in Fortran.
!! help -- Please follow the steps by declaring who is writing.
!! help --  Please delete the comments starting with ‘!! help -- ’ when the file is clean.

!! help -- Bellow are the stop to follow :
!! help -- 1) Complete the paragraph from lines 20 to 40

!! help -- 2) Creation du module :
!!                - a) Name of the module (l.75) + End of the module (l.150)
!!                - b) Declare variables or functions that are required in this module, but are declared in other modules
!!                      (l.77 to 79) (/!\ this variables or functiuns need to be PUBLIC)
!!                - c)  Declare the variables required in the module (l.84 to 91). If they are useful elsewhere,
! >                      these variables or functions must be made public (l.85 to 87).

!! help -- 3) Create subroutine (maybe more) = lines 98 to 119
!!                - a) Put the name of the subroutine and close the subroutine (l.99 and 118)
!!                - b) Precise the author, the goal etc (l.102 to 105)
!!                - c) Declare the parameters for the subroutine (in) (l.108 and 108)
!!                - d) Declare the output variable (out) (l. 109)
!!                - e) If necessary, you can create the “intermediate” variables required for the subroutine. (l.112)
!!                - f) Enter the equations and, if possible, quote the sources. You can use some functions.  (l.114 to 116)

!! help -- 4) Create function (maybe more) = lines 124 to 144
!!                - a) Put the name of the functions and close the it (l.125 nd 143)
!!                - b) Precise the author, the goal etc (l.128 to 135)
!!                - c) Declare the variables for the function (in and out (l.138 and 139)
!!                - d) If necessary, you can create the “intermediate” variables = "local variables"
!!                - e) Enter the equations and, if possible, quote the sources (l.141)
!! </pre>
!! @endnote
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|




       use global_constants_mod, only: dblp=>dp, ip
       use AnotherModule_mod, only: some_variable
       use AnotherModule_mod, only: some_otherfunction

       implicit none
       private

       ! people -- PUBLIC variables, functions, subroutines.
       public :: someFunction, function_a

         !> Description variable var  [unit]
       real(kind=dblp),  parameter, public  :: var = 0.1_dblp
       integer(kind=ip), parameter, public  :: var2 = 0           !! Description variable var2 [unit]

       ! people -- Variables only required for this module
       integer(kind=ip), PARAMETER :: var3 = 4                     !! Description variable var2 [unit]
       real(kind=dblp)                           :: var4

      contains


! <<<<<<<<<<<<<<<<<<<<<<<<<<  SUBROUTINE PART >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

! *******************************************************************************************************************
      SUBROUTINE name_subroutine(parameters1, parameters2, output)
! *******************************************************************************************************************

!!      AUTHOR : people
!!      DESCRIPTION: Subroutine is for blablabla --> Here add the long_description of the module ...
!!      REFERENCES: papers or other documents to be cited...(including link when possible)
!!      CALL : This subroutine is call in "module_file"

       real(kind=dblp), intent(in)         :: parameters1     ! variablea [unit]
       real(kind=dblp), intent(in)         :: parameters2     ! variableb [unit]
       real(kind=dblp), intent(out)  :: output                ! variablec [unit]

     ,  ! Local variables
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


end module Foo_mod

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      The End of All Things (op. cit.)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
