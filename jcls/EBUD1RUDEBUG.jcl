//EBUD0RUN JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=NOLIMIT,REGION=0M,COND=(16,LT)
//        JCLLIB ORDER=(RATCFG.IDZ.V14R2.#CUST.PROCLIB)
//*
//GO    EXEC PGM=EBUD0RUN
//******* ADDITIONAL RUNTIME JCL HERE ******
//STEPLIB  DD DISP=SHR,DSN=CEE.SCEERUN
//        DD    DISP=SHR,DSN=RDZ.V14R2.SFELLOAD
//        DD    DISP=SHR,DSN=DEBUG.V14R1.SEQAMOD
//        DD    DISP=SHR,DSN=DBEHM.DBBRET.LOAD
//CEEOPTS DD *
TEST(,,,DBMDT:*)
//SYSIN DD *
1984
01
21
/*
//
