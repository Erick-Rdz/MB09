      *----------------------------------------------------------------*
      * dfheiblk.cpy
      *----------------------------------------------------------------*
      * Copyright (C) 1998-2004 Micro Focus International Ltd.
      * All rights reserved.
      *----------------------------------------------------------------*
       01  DFHEIBLK.
           05  EIBTIME                 PIC S9(7)       COMP-3.
           05  EIBDATE                 PIC S9(7)       COMP-3.
           05  EIBTRNID                PIC  X(4).
           05  EIBTASKN                PIC S9(7)       COMP-3.
           05  EIBTRMID                PIC  X(4).
           05  DFHEIGDI                PIC S9(4)       COMP.
           05  EIBRSVD1                PIC S9(4)       COMP.
           05  EIBCPOSN                PIC S9(4)       COMP.
           05  EIBCALEN                PIC S9(4)       COMP.
           05  EIBAID                  PIC  X(1).
           05  EIBFN                   PIC  X(2).
           05  EIBRCODE                PIC  X(6).
           05  EIBDS                   PIC  X(8).
           05  EIBREQID                PIC  X(8).
           05  EIBRSRCE                PIC  X(8).
           05  EIBSYNC                 PIC  X(1).
           05  EIBFREE                 PIC  X(1).
           05  EIBRECV                 PIC  X(1).
           05  EIBFIL01                PIC  X(1).
           05  EIBSEND                 PIC  X(1).
           05  EIBATT                  PIC  X(1).
           05  EIBEOC                  PIC  X(1).
           05  EIBFMH                  PIC  X(1).
           05  EIBCOMPL                PIC  X(1).
           05  EIBSIG                  PIC  X(1).
           05  EIBCONF                 PIC  X(1).
           05  EIBERR                  PIC  X(1).
           05  EIBERRCD                PIC  X(4).
           05  EIBSYNRB                PIC  X(1).
           05  EIBNODAT                PIC  X(1).
           05  EIBRESP                 PIC S9(8)       COMP.
           05  EIBRESP2                PIC S9(8)       COMP.
           05  EIBRLDBK                PIC  X(1).

      * ADDED FOR COMPATIBILITY IN ALTAMIRA II.

           05    EIB-ENTITY                PIC X(04).
           05    EIB-ACC-BRANCH            PIC X(04).
           05    EIB-ACC-TERMINAL          PIC X(04).
           05    EIB-USERID                PIC X(08).
           05    EIB-STARTCODE             PIC X(02).
           05    EIB-LANGUAGE              PIC X(01).
           05    EIB-CHANNEL               PIC X(02).
           05    EIB-IND-COM-ABD           PIC X(01).
           05    EIB-SYSID                 PIC X(04).
           05    EIB-TX-MULT               PIC X(04).
           05    FILLER                    PIC X(05).

      ******************************************************************
      ******************************************************************
      ******************************************************************
      *----------------------------------------------------------------*
      * dfheiblk.cpy end
      *----------------------------------------------------------------*
