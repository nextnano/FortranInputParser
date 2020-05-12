# FortranInputParser
Fortran Input Parser

This is the Fortran Input Parser of the nextnano3 software.

(c) 2020, nextnano GmbH, Germany

Applications:

A typical application is the reading in of input flags and material parameters from a text file for e.g. scientific codes.

The input parser is compatible to the nextnanomat graphical user interface.

=======
Example
=======

!-----------------------------------------------------------------------------!
$simulation-dimension                                                         !
 dimension   = 2                                                              ! (integer)
 orientation = 1 1 0                                                          ! (integer array)
$end_simulation-dimension                                                     !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$global-parameters                                                            !
 debug-level = 2                                                              !
 temperature = 300.0                                                          ! (double)
$end_global-parameters                                                        !
!-----------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
$material                                                                     !
 material-number = 1                                                          !
 material-name   = GaAs                                                       ! (string)
 
 material-number = 2                                                          !
 material-name   = Al(x)Ga(1-x)As                                             !
 alloy-content-x = 0.30                                                       !

 material-number = 3        material-name = AlAs                              ! (several items in one line

 material-number = 4        material-name = GaAs                              !

 material-number = 4        material-name =
                                            GaAs                              ! (line brake is allowed)
!-----------------------------------------------------------------------------!


Dr. Stefan Birner
nextnano GmbH
Managing Directer
