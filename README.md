# Fortran Input Parser

This is the Fortran Input Parser of the nextnano³ software.

(c) 2025, nextnano GmbH, Germany, www.nextnano.com (BSD license)

The Fortran Input Parser is compatible to the [nextnanomat](https://www.nextnano.com/nextnanomat/) graphical user interface.

For details, please refer to: https://github.com/nextnano/FortranInputParser

Further explanation on the features (Syntax definition, Macro, Function Parser) are available here:
https://www.nextnano.com/documentation/tools/nextnano3/input_syntax/macro.html

For questions, please contact stefan.birner@nextnano.com.

## Applications

A typical application is the reading in of input parameters (*input file*) and material parameters (*database*) from two text files for e.g. a scientific code.


### Example 1

```
!---------------------
! Variable definitions
!---------------------
%x_min    =  0.0                      ! (DisplayUnit:nm)(DoNotShowInUserInterface)
%x_max    = 50.0                      ! (DisplayUnit:nm)(HighlightInUserInterface)
%length   = 20.0                      ! (DisplayUnit:nm)(ListOfValues:20.0,30.0,40.0)
%width    = 20.0                      ! (DisplayUnit:nm)(RangeOfValues:From=20.0,To=30.0,Steps=2.0)
%FILENAME = doping_concentration.dat  !

%MagneticField = .TRUE.               ! conditional comment: either .TRUE. or .FALSE. or 1 or 0


!-----------------------------------------------------------------------------!
$simulation-dimension                                                         ! keyword
 dimension   = 2                                                              ! specifier with integer
 orientation = 1 1 0                                                          ! specifier with integer array
$end_simulation-dimension                                                     ! end of keyword
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$global-parameters                                                            !
 debug-level = 2                                                              ! integer
 temperature = 300.0                                                          ! double
$end_global-parameters                                                        !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$material                                                                     !
 material-number = 1                                                          ! separation specifier (allows loop)
 material-name   = GaAs                                                       ! character
 
 material-number = 2                                                          ! separation specifier (2nd loop)
 material-name   = Al(x)Ga(1-x)As                                             !
 alloy-content-x = 0.30                                                       !

 material-number = 3        material-name = AlAs                              ! several items in one line

 material-number = 4        material-name = GaAs                              !

 material-number = 4        material-name =
                                            GaAs                              ! line break is allowed
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$regions                                                                      !
 region-number = 1                                                            ! separation specifier
 x-coordinates = -100.0  0.0                                                  ! double array
                                                                              !
 region-number = 2                                                            !
 x-coordinates = %x_min   %x_max                                              ! double array
                                                                              !
 region-number = 3                                                            !
 x-coordinates = 30.0                                                         ! double array with line break
                 40.0                                                         !
$end_regions                                                                  !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! A CHOICE limits the allowed values that can be entered.                     !
!-----------------------------------------------------------------------------!
$electric-field                                                               !
 electric-field = on                                                          ! CHOICE[on,off]
 direction      = x                                                           ! CHOICE[x,y,z]
 strength       = 1.0e-1                                                      !
$end_electric-field                                                           !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! Conditional comments are uncommented if %MagneticField = .TRUE.             !
!-----------------------------------------------------------------------------!
!IF %MagneticField $magnetic-field                                            !
!IF %MagneticField  magnetic-field = on                                       ! CHOICE[on,off]
!IF %MagneticField  direction      = x                                        ! CHOICE[x,y,z]
!IF %MagneticField  strength       = 1.0e-1                                   !
!IF %MagneticField $end_magnetic-field                                        !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! This is the output for the material parameters, e.g. doping concentration.  !
!-----------------------------------------------------------------------------!
$output-material-parameters                                                   !
!directory            = ./                                                    !
 directory            = material_parameters/                                  !
 doping-concentration = %FILENAME                                             !
$end_material-parameters                                                      !
!-----------------------------------------------------------------------------!

```

### Example 2

- See `inputfile.in` in this folder.
- See `database.in` in this folder.


### Documentation

- See `Documentation.md` in this folder.


### How to compile and run the Fortran Input Parser.

These source files produce a simple test example for the Fortran Input Parser.

- Compatibility: The Fortran Input Parser works on Windows, Linux and macOS.
- Use `make` (i.e. read in `Makefile`) to compile the `*.f90` files.
  Alternatively, a solution file for Microsoft Visual Studio Community for the Intel Fortran Compiler is included.
- The `keywords_inputfile.val` file contains the name of the input file to be read in (here: `inputfile.in`).
- The `keywords_inputfile.val` file contains the syntax definiton of arbitrary input files.
- Once compiled, type `.\FortranInputParser.exe` (Windows) or `./FortranInputParser.exe` (Linux/macOS) to execute the program, which reads in input file definition
  (`keywords_inputfile.val`) and input file (`inputfile.in`).

- The following screen output will be produced:

```
  +++++++++++++++++++++++++++++++++++++++++++++ 

 ------------------------------------------------------------------------------
 Reading in input file: 'inputfile.in'

 ------------------------------------------------------------------------------
 Reading in database:   'database.in'

===============================================================================
 Content of database:
-------------------------------------------------------------------------------

  The electron charge is  -1.602176620800000E-019  [As].
  Planck's constant is     6.626070040000000E-034  [Js].

===============================================================================


===============================================================================
 Content of inputfile:
-------------------------------------------------------------------------------

 ------------------------------------------------------------------------------
 Material info.
 ------------------------------------------------------------------------------
    material-number                       =            1
    cluster-numbers                       =            1           3           5
           6
 ...
```
