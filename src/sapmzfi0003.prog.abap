*--------------------------------------------------------------------*
*           P R O J E T O   T A E S A  - B R A S I L                 *
*--------------------------------------------------------------------*
* Consultoria .....: Axxiom                                          *
* Res. ABAP   .....: Cláudio R.                                      *
* Res. Funcional...: Marcelo Forti                                   *
* Módulo...........: FI                                              *
* Programa    .....: SAPMZFI0003                                     *
* Transação   .....:                                                 *
* Tipo de prg .....: Module Pool                                     *
* Objetivo    .....: Lançamento de zeramento das contas de resultado *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version  Date       Who                        What                *
*    1.00  01/12/2011 Luciana Oliveira           Versão Inicial      *
**********************************************************************
*&---------------------------------------------------------------------*
*& PoolMóds.         SAPMZFI0002
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report SAPMZFI0003 MESSAGE-ID zfi_0003.

INCLUDE mzfi0003top                             .  " global Data
INCLUDE mzfi0003o01                             .  " PBO-Modules
INCLUDE mzfi0003i01                             .  " PAI-Modules
INCLUDE mzfi0003f01                             .  " FORM-Routines.
