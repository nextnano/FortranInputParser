# FortranInputParser

This is the Fortran Input Parser of the nextnano3 software.

(c) 2020, nextnano GmbH, Germany

Applications:

A typical application is the reading in of input flags and material parameters from a text file for e.g. scientific codes.

The input parser is compatible to the [nextnanomat](https://www.nextnano.com/nextnanomat/) graphical user interface.

### Example

```
%x_min    =  0.0                      ! (DisplayUnit:nm)(DoNotShowInUserInterface)
%x_max    = 50.0                      ! (DisplayUnit:nm)(HighlightInUserInterface)
%length   = 20.0                      ! (DisplayUnit:nm)(ListOfValues:20.0,30.0,40.0)
%width    = 20.0                      ! (DisplayUnit:nm)(RangeOfValues:From=20.0,To=30.0,Steps=2.0)
%FILENAME = doping_concentration.dat                            !

!-----------------------------------------------------------------------------!
$simulation-dimension                                                         ! (keyword)
 dimension   = 2                                                              ! (specifier) (integer)
 orientation = 1 1 0                                                          ! (specifier) (integer array)
$end_simulation-dimension                                                     ! (end keyword)
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$global-parameters                                                            !
 debug-level = 2                                                              ! (integer)
 temperature = 300.0                                                          ! (double)
$end_global-parameters                                                        !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$material                                                                     !
 material-number = 1                                                          ! (loop over separation specifier)
 material-name   = GaAs                                                       ! (character)
 
 material-number = 2                                                          ! (2nd loop)
 material-name   = Al(x)Ga(1-x)As                                             !
 alloy-content-x = 0.30                                                       !

 material-number = 3        material-name = AlAs                              ! (several items in one line)

 material-number = 4        material-name = GaAs                              !

 material-number = 4        material-name =
                                            GaAs                              ! (line break is allowed)
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$regions                                                                      !
 region-number = 1                                                            ! (separation specifier)
 x-coordinates = -100.0  0.0                                                  ! (double array)

 region-number = 2                                                            !
 x-coordinates = %x_min   %x_max                                              ! (double array)
$end_regions                                                                  !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$electric-field                                                               !
 electric-field = on                                                          ! CHOICE(on,off)
 direction      = x                                                           ! CHOICE(x,y,z)
 strength       = 1.0e5                                                       !
$end_electric-field                                                           !
!-----------------------------------------------------------------------------!

!---------------------------------------------------------------------------!
! This is the output for the material parameters, doping concentration.     !
!---------------------------------------------------------------------------!
$output-material-parameters                                                 !
!directory            = ./                                                  !
 directory            = material_parameters/                                !
 doping-concentration = %FILENAME                                           !
$end_material-parameters                                                    !
!---------------------------------------------------------------------------!

```

### Example
```
! Syntax definition file
!-----------------------------------------------------------------------------!
$keyword 		                                                                   !
 specifier1 	 data_type  	required                                            !
 specifier2 	 data_type  	optional                                            !
 ...          ...         ... 		                                              !
$end_keyword                                                                  !
!-----------------------------------------------------------------------------!

$: special sign indicating a keyword  ($ can be replaced by the user)
!: special sign indicating a comment  (! can be replaced by the user)
%: special sign indicating a variable (% can be replaced by the user)
data_type:
  - integer
  - integer_array
  - double
  - double_array
  - character
  - logical is also supported but we recommend to using 'yes' or 'no' instead as this is more readable to the human eye.
```

Dr. Stefan Birner

nextnano GmbH

Managing Directer
