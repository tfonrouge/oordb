#include "oordb.ch"
#include "hbthread.ch"

#define NUM_THREADS 10

REQUEST DBFCDX

PROCEDURE Main()
    LOCAL o
    LOCAL t1

    RddSetDefault( "DBFCDX" )

    IF hb_dbExists( "table1.dbf" )
        hb_dbDrop( "table1.dbf", "table1.cdx" )
    ENDIF

    /* builds table / index files */
    t1 := Table1():new()
    outStd( e"\nTable1:RecCoount():", t1:recCount() )
    outStd( e"\n" )

    o := MainThreadClass():new()

    o:Run()

    outStd( e"\nTable1:RecCoount():", t1:recCount() )
    outStd( e"\n" )

    outStd( e"\nCOUNTER:", o:counter )

RETURN

CLASS MainThreadClass
EXPORTED:
    DATA counter INIT 0
    DATA mtxQueue
    DATA aThreads
    METHOD Run()
ENDCLASS

METHOD PROCEDURE Run() CLASS MainThreadClass
    LOCAL i
    LOCAL mtx
    LOCAL jobs
    LOCAL queue

    ::mtxQueue := hb_mutexCreate()

    ::aThreads := {}

    FOR i := 1 TO NUM_THREADS
        outStd( e"\n", i )
        AAdd( ::aThreads, hb_threadStart( HB_THREAD_INHERIT_PUBLIC, @ProcessConnection(), i, self ) )
    NEXT

    mtx := hb_mutexCreate()
    hb_mutexQueueInfo( mtx, @jobs, @queue )

    outStd( e"\nhbhb_mutexQueueInfo:", "jobs", hb_nToS( jobs ), "queue", hb_nToS( queue ) )

    aEval( ::aThreads, {|threadId| hb_threadJoin( threadId ) } )

RETURN

STATIC FUNCTION ProcessConnection( threadNum, mainThread )
    LOCAL i
    LOCAL n
    LOCAL millis
    LOCAL mtx
    LOCAL table1

    table1 := Table1():new()

    millis := hb_milliSeconds()

    ++ mainThread:counter

    mtx := hb_mutexCreate()
    hb_mutexLock( mtx )
    outStd( e"\nstarting thread...", hb_nToS( threadNum ) )
    hb_mutexUnLock( mtx )

    FOR n := 1 TO 100
        IF table1:insert()
            FOR i := 1 TO 9
                table1:fieldByname( "STR" + hb_nToS( i ) ):value := hb_numToHex( i )
            NEXT
            table1:post()
        ENDIF
    NEXT

    mtx := hb_mutexCreate()
    hb_mutexLock( mtx )
    outStd( e"\nfinished thread...", hb_nToS( threadNum ), "millis:", hb_nToS( hb_milliSeconds() - millis ) )
    hb_mutexUnLock( mtx )

RETURN 0
