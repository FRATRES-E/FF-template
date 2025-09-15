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

!ecl -- Begin of the module here, add the name : 
      module garden_mod

       !! version: v1.0
       !! display: public private protected
       !! proc_internals: true


!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      MODULE: [garden_mod]

!!     @author  Emeline Clermont (ec)
!!     @author  Didier M. Roche  (dmr)
!!     @date Creation date: Sept, 15th, 2025

!!     @brief This module [garden_mod] serves as an example. It is useful for nothing. 
!>
!>     DESCRIPTION : In this module, the objective is to create a subroutine which takes the radius of the garden plot and returns 
!>     a "productivity" index (area/perimeter x factor). We only need 1 subroutine and 2 functions. 
!>        - Subroutine [garden_info] : This subroutine is for return the "prductivity" index of the garden.
!>        - Function [circle_area] : This function calculate the area of the circle garden. 
!>        - Function [circle_perimeter] : This function calculate the perimeter of the circle garden. 

!>     @reference References: https://www.ac-versailles.fr/sites/ac_versailles/files/2021-10/disque-cercle-au-c3-91-25043.pdf
!>     @date Last modification: SLastChangedDate$
!>     @author Last modified by : ecl

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
!! help -- 1) Complete the paragraph before

!! help -- 2) Creation du module :
!!                - a) Name of the module (at the begining) + End of the module (at the end)
!!                - b) Declare variables or functions that are required in this module, but are declared in other modules
!!                       (/!\ this variables or functions need to be PUBLIC)
!!                - c)  Declare the variables required in the module. If they are useful elsewhere,
! >                      these variables or functions must be made public.

!! help -- 3) Create subroutine (maybe more) 
!!                - a) Put the name of the subroutine and close the subroutine 
!!                - b) Precise the author, the goal etc 
!!                - c) Declare the parameters for the subroutine (in)
!!                - d) Declare the output variable (out) 
!!                - e) If necessary, you can create the “intermediate” variables required for the subroutine.
!!                - f) Enter the equations and, if possible, quote the sources. You can use some functions.  

!! help -- 4) Create function (maybe more) 
!!                - a) Put the name of the functions and close the it 
!!                - b) Precise the author, the goal etc 
!!                - c) Declare the variables for the function (in and out)
!!                - d) If necessary, you can create the “intermediate” variables = "local variables"
!!                - e) Enter the equations and, if possible, quote the sources 
!! </pre>
!! @endnote
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|


       ! ecl -- Import the necessary variables/functions, which have already been created in other files: 
       use global_constants_mod, only: dblp=>dp, ip

       implicit none
       private

       ! ecl -- The subroutine will be called in another file, so it must be defined as public
       public :: garden_info

       ! ecl -- Constant declaration: Constants declared here do no need to be declare elsewhere in the file. 
       real(kind=dblp),  parameter, public  :: PI = 3.141592653589793_dblp

      contains


! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  SUBROUTINE PART >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

! **********************************************************************************************************************************
      SUBROUTINE garden_info(radius, indice)
! **********************************************************************************************************************************

!!      AUTHOR : ecl
!!      DESCRIPTION: Subroutine is for return a "productivity" index (area/perimeter x factor) of the garden.
!!      CALL : This subroutine is call in "patate_file"

       real(kind=dblp), intent(in)   :: radius        ! [cm]
       real(kind=dblp), intent(out)  :: indice        ! [-]

       ! Local variables
       real(kind=dblp) :: area, perimeter

       ! Begin of the subroutine
       area = circle_area(radius)
       perimeter = circle_perimeter(radius)

       indice = area/perimeter

      END SUBROUTINE garden_info
! **********************************************************************************************************************************


! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<  FUNCTIONS PART >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!===================================================================================================================================
      function circle_area(radius) result(a)
!===================================================================================================================================

!>      AUTHOR : ecl
!>      DESCRIPTION: This function allows to calculate the area of a circle with radius "radius".
!>      REFERENCES: https://www.ac-versailles.fr/sites/ac_versailles/files/2021-10/disque-cercle-au-c3-91-25043.pdf

!>      Input variable :
!>         - radius = radius of the circle [cm]
!>      Output variable : a.


       real(kind=dblp), intent(in)  :: radius     ![cm]
       real, (kind=dblp),           :: a          ![cm^2]

       a = PI * radius**2

      end function circle_area
!===================================================================================================================================

!===================================================================================================================================
      function circle_perimeter(radius) result(p)
!===================================================================================================================================

!>      AUTHOR : ecl
!>      DESCRIPTION: This function allows to calculate the perimeter of a circle with radius "radius".
!>      REFERENCES: https://www.ac-versailles.fr/sites/ac_versailles/files/2021-10/disque-cercle-au-c3-91-25043.pdf

!>      Input variable :
!>         - radius = radius of the circle [cm]
!>      Output variable : p.


       real(kind=dblp), intent(in)  :: radius     ![cm]
       real, (kind=dblp),           :: p          ![cm]

       ! Local variables
       real(kind=dblp), parameter :: double = 2.0_dblp

       p = double * PI * radius

      end function circle_perimeter
!===================================================================================================================================

end module garden_mod

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      The End of All Things (op. cit.)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
